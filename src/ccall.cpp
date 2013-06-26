// --- the ccall intrinsic ---

// --- library symbol lookup ---

// map from "libX" to full soname "libX.so.ver"
#if defined(__linux__)
static std::map<std::string, std::string> sonameMap;
static bool got_sonames = false;

extern "C" DLLEXPORT void jl_read_sonames(void)
{
    char *line=NULL;
    size_t sz=0;
    FILE *ldc = popen("/sbin/ldconfig -p", "r");

    while (!feof(ldc)) {
        ssize_t n = getline(&line, &sz, ldc);
        if (n == -1)
            break;
        if (n > 2 && isspace((unsigned char)line[0])) {
            int i=0;
            while (isspace((unsigned char)line[++i])) ;
            char *name = &line[i];
            char *dot = strstr(name, ".so");
            i=0;

            if (NULL == dot)
                continue;

            // Detect if this entry is for the current architecture
            while (!isspace((unsigned char)dot[++i])) ;
            while (isspace((unsigned char)dot[++i])) ;
            int j = i;
            while (!isspace((unsigned char)dot[++j])) ;
            char *arch = strstr(dot+i,"x86-64");
            if (arch != NULL && arch < dot + j) {
#ifdef _P32
                continue;
#endif
            }
            else {
#ifdef _P64
                continue;
#endif
            }

            char *abslibpath = strrchr(line, ' ');
            if (dot != NULL && abslibpath != NULL) {
                std::string pfx(name, dot - name);
                // Do not include ' ' in front and '\n' at the end
                std::string soname(abslibpath+1, line+n-(abslibpath+1)-1);
                sonameMap[pfx] = soname;
            }
        }
    }

    free(line);
    pclose(ldc);
}

extern "C" DLLEXPORT const char *jl_lookup_soname(char *pfx, size_t n)
{
    if (!got_sonames) {
        jl_read_sonames();
        got_sonames = true;
    }
    std::string str(pfx, n);
    if (sonameMap.find(str) != sonameMap.end()) {
        return sonameMap[str].c_str();
    }
    return NULL;
}
#endif

// map from user-specified lib names to handles
static std::map<std::string, uv_lib_t*> libMap;

static uv_lib_t *get_library(char *lib)
{
    uv_lib_t *hnd;
#ifdef _OS_WINDOWS_
    if ((intptr_t)lib == 1)
        return jl_exe_handle;
    if ((intptr_t)lib == 2)
        return jl_dl_handle;
#endif
    if (lib == NULL)
        return jl_RTLD_DEFAULT_handle;
    hnd = libMap[lib];
    if (hnd != NULL)
        return hnd;
    hnd = jl_load_dynamic_library(lib, JL_RTLD_DEFAULT);
    if (hnd != NULL)
        libMap[lib] = hnd;
    return hnd;
}

extern "C" DLLEXPORT
void *jl_load_and_lookup(char *f_lib, char *f_name, uv_lib_t **hnd)
{
    uv_lib_t *handle = *hnd;
    if (!handle)
        *hnd = handle = get_library(f_lib);
    void *ptr = jl_dlsym_e(handle, f_name);
    if (!ptr)
        jl_errorf("symbol could not be found %s: %s\n", f_name, uv_dlerror(handle));
    return ptr;
}

static std::map<std::string, GlobalVariable*> libMapGV;
static std::map<std::string, GlobalVariable*> symMapGV;
static Value *runtime_sym_lookup(PointerType *funcptype, char *f_lib, char *f_name, jl_codectx_t *ctx)
{
    // in pseudo-code, this function emits the following:
    //   global uv_lib_t **libptrgv
    //   global void **llvmgv
    //   if (*llvmgv == NULL) {
    //       *llvmgv = jl_load_and_lookup(f_lib, f_name, libptrgv);
    //   }
    //   return (*llvmgv)
    Constant *initnul = ConstantPointerNull::get((PointerType*)T_pint8);

    uv_lib_t *libsym = NULL;
    bool runtime_lib = false;
    GlobalVariable *libptrgv;
#ifdef _OS_WINDOWS_
    if ((intptr_t)f_lib == 1) {
        libptrgv = prepare_global(jlexe_var);
        libsym = jl_exe_handle;
    }
    else if ((intptr_t)f_lib == 2) {
        libptrgv = prepare_global(jldll_var);
        libsym = jl_dl_handle;
    }
    else
#endif
    if (f_lib == NULL) {
        libptrgv = prepare_global(jlRTLD_DEFAULT_var);
        libsym = jl_RTLD_DEFAULT_handle;
    }
    else {
        runtime_lib = true;
        libptrgv = libMapGV[f_lib];
        if (libptrgv == NULL) {
            libptrgv = new GlobalVariable(*jl_Module, T_pint8,
               false, GlobalVariable::PrivateLinkage,
               initnul, f_lib);
            libMapGV[f_lib] = libptrgv;
            libsym = get_library(f_lib);
            assert(libsym != NULL);
#ifdef USE_MCJIT
            llvm_to_jl_value[libptrgv] = libsym;
#else
            *((uv_lib_t**)jl_ExecutionEngine->getPointerToGlobal(libptrgv)) = libsym;
#endif
        }
    }
    if (libsym == NULL) {
#ifdef USE_MCJIT
        libsym = (uv_lib_t*)llvm_to_jl_value[libptrgv];
#else
        libsym = *((uv_lib_t**)jl_ExecutionEngine->getPointerToGlobal(libptrgv));
#endif
    }

    assert(libsym != NULL);

    GlobalVariable *llvmgv = symMapGV[f_name];
    if (llvmgv == NULL) {
        // MCJIT forces this to have external linkage eventually, so we would clobber
        // the symbol of the actual function.
        std::string name = f_name;
        name = "ccall_" + name;
        llvmgv = new GlobalVariable(*jl_Module, T_pint8,
           false, GlobalVariable::PrivateLinkage,
           initnul, name);
        symMapGV[f_name] = llvmgv;
#ifdef USE_MCJIT
        llvm_to_jl_value[llvmgv] = jl_dlsym_e(libsym, f_name);
#else
        *((void**)jl_ExecutionEngine->getPointerToGlobal(llvmgv)) = jl_dlsym_e(libsym, f_name);
#endif
    }

    BasicBlock *dlsym_lookup = BasicBlock::Create(getGlobalContext(), "dlsym"),
               *ccall_bb = BasicBlock::Create(getGlobalContext(), "ccall");
    builder.CreateCondBr(builder.CreateICmpNE(builder.CreateLoad(llvmgv), initnul), ccall_bb, dlsym_lookup);

    ctx->f->getBasicBlockList().push_back(dlsym_lookup);
    builder.SetInsertPoint(dlsym_lookup);
    Value *libname;
    if (runtime_lib) {
        libname = builder.CreateGlobalStringPtr(f_lib);
    }
    else {
        libname = literal_static_pointer_val(f_lib, T_pint8);
    }
    Value *llvmf = builder.CreateCall3(prepare_call(jldlsym_func), libname, builder.CreateGlobalStringPtr(f_name), libptrgv);
    builder.CreateStore(llvmf, llvmgv);
    builder.CreateBr(ccall_bb);

    ctx->f->getBasicBlockList().push_back(ccall_bb);
    builder.SetInsertPoint(ccall_bb);
    llvmf = builder.CreateLoad(llvmgv);
    return builder.CreatePointerCast(llvmf,funcptype);
}
// --- ABI Implementations ---
// Partially based on the LDC ABI implementations licensed under the BSD 3-clause license

#ifdef _P64
#   ifdef _OS_WINDOWS_
#       define ABI_WIN64 1
#   else
#       define ABI_X86_64 1
#   endif
#else
#   define ABI_X86 1
#endif

#if ABI_X86_64

// used to track the state of the ABI generator during
// code generation
struct AbiState {
    unsigned char int_regs, sse_regs;
};

const AbiState default_abi_state = {6,8};

enum ArgClass { Integer, Sse, SseUp, X87, X87Up, ComplexX87, NoClass, Memory };

struct Classification {
    bool isMemory;
    ArgClass classes[2];

    Classification() : isMemory(false) {
        classes[0] = NoClass;
        classes[1] = NoClass;
    }

    void addField(unsigned offset, ArgClass cl) {
        if (isMemory)
            return;

        // Note that we don't need to bother checking if it crosses 8 bytes.
        // We don't get here with unaligned fields, and anything that can be
        // big enough to cross 8 bytes (cdoubles, reals, structs and arrays)
        // is special-cased in classifyType()
        int idx = (offset < 8 ? 0 : 1);

        ArgClass nw = merge(classes[idx], cl);
        if (nw != classes[idx]) {
            classes[idx] = nw;

            if (nw == Memory) {
                classes[1-idx] = Memory;
                isMemory = true;
            }
        }
    }

    static ArgClass merge(ArgClass accum, ArgClass cl) {
        if (accum == cl)
            return accum;
        if (accum == NoClass)
            return cl;
        if (cl == NoClass)
            return accum;
        if (accum == Memory || cl == Memory)
            return Memory;
        if (accum == Integer || cl == Integer)
            return Integer;
        if (accum == X87 || accum == X87Up || accum == ComplexX87 ||
            cl == X87 || cl == X87Up || cl == ComplexX87)
            return Memory;
        return Sse;
    }
};

/*else if (ty == jl_float80_type) { //if this is ever added
        accum.addField(offset, X87);
        accum.addField(offset+8, X87Up);
    } else if (ty->ty == Tcomplex80) {
        accum.addField(offset, ComplexX87);
        // make sure other half knows about it too:
        accum.addField(offset+16, ComplexX87);
    } */
void classifyType(Classification& accum, jl_value_t* ty, uint64_t offset) {
    if (jl_is_cpointer_type(ty)) {
        accum.addField(offset, Integer);
    } else if (jl_is_bitstype(ty) && jl_datatype_size(ty) == 16) {
        // Int128 or other 128bit wide INTEGER types
        accum.addField(offset, Integer);
        accum.addField(offset+8, Integer);
    }
    // Floating point types
    else if (ty == (jl_value_t*)jl_float64_type || ty == (jl_value_t*)jl_float32_type) {
        accum.addField(offset, Sse);
    }
    // Other integer types
    else if (jl_is_bitstype(ty))
    {
        if(jl_datatype_size(ty) > 8)
            jl_error("Bitstype of this size not supported in the C ABI");
        accum.addField(offset,Integer);
    } else if (jl_datatype_size(ty) > 16) {
        // This isn't creal, yet is > 16 bytes, so pass in memory.
        // Must be after creal case but before arrays and structs,
        // the other types that can get bigger than 16 bytes
        accum.addField(offset, Memory);
    } else if (jl_is_array_type(ty)) {
        jl_value_t* eltType = jl_tparam0(ty);
        uint64_t eltsize = jl_datatype_size(eltType);
        if (eltsize > 0) {
            uint16_t dim = 0;
            for (int i = 0; i<jl_array_ndims(ty); ++i)
                dim += jl_array_dim(ty,i);
            assert(dim <= 16
                    && "Array of non-empty type <= 16 bytes but > 16 elements?");
            for (int i = 0; i < dim; i++) {
                classifyType(accum, eltType, offset);
                offset += eltsize;
            }
        }
    } else if (jl_is_structtype(ty)) {
        for (int i = 0; i < jl_tuple_len(((jl_datatype_t*)ty)->types); ++i) {
            classifyType(accum, jl_tupleref(((jl_datatype_t*)ty)->types,i), offset + jl_field_offset(ty,i));
        }
    } else {
        jl_error("Unsupported type in C ABI");
    }
}

Classification classify(jl_value_t* ty) {
    Classification cl;
    classifyType(cl, ty, 0);
    return cl;
}

bool use_sret(AbiState *state,jl_value_t *ty)
{
    int sret = classify(ty).isMemory;
    if(sret) {
        assert(state->int_regs>0 && "WTF? No int regs available?");
        state->int_regs--;
    }
    return sret;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    Classification cl = classify(ty);
    if (cl.isMemory) {
        *byRefAttr = *byRef = true;
        return;
    }


    // Figure out how many registers we want for this arg:
    AbiState wanted = { 0, 0 };
    for (int i = 0 ; i < 2; i++) {
        if (cl.classes[i] == Integer)
            wanted.int_regs++;
        else if (cl.classes[i] == Sse)
            wanted.sse_regs++;
    }

    if (wanted.int_regs <= state->int_regs && wanted.sse_regs <= state->sse_regs) {
        state->int_regs -= wanted.int_regs;
        state->sse_regs -= wanted.sse_regs;
        *inReg = true;
    }else if(jl_is_structtype(ty))
    {
        // spill to memory even though we would ordinarily pass
        // it in registers
        *byRef = true;
    }
    *byRefAttr = *byRef;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    (void) isret;
    // no need to rewrite bitstypes or pointers (really only agregates are the problem)
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty))
        return NULL;

    int size = jl_datatype_size(ty);
    if(!(size == 1 || size == 2 || size == 4 || size == 8))
        return NULL;

    Classification cl = classify(ty);
    if(cl.isMemory)
        return NULL;
    ArgClass c = Classification::merge(cl.classes[0],cl.classes[1]);
    Type *target_type = NULL;

    // Make into an aggregate of
    if(c == Sse)
        target_type = Type::getDoubleTy(getGlobalContext());
    else if(c == Integer)
        target_type = T_int64;
    else
        assert("Don't know how to rewrite type");

    return target_type;
}

bool need_destructure_argument(jl_value_t *ty)
{
    return false;
}

bool need_private_copy(jl_value_t *ty, bool isRef)
{
    return false;
}

#elif ABI_WIN64

// Windows only uses the first four registers of either
struct AbiState {
    unsigned char int_regs, sse_regs;
};

const AbiState default_abi_state = {4,4};


bool use_sret(AbiState *state,jl_value_t *ty)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    size_t size = jl_datatype_size(ty);
    bool sret =  !(size == 1 || size == 2 || size == 4 || size == 8); // || jl_is_sse(ty) if every implemented
    if(sret)
        state->int_regs--;
    return sret;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    if ((jl_datatype_t*)ty == jl_float32_type || (jl_datatype_t*)ty == jl_float64_type) {
        state->sse_regs--;
        return;
    }
    size_t size = jl_datatype_size(ty);
    *byRef = !(size == 1 || size == 2 || size == 4 || size == 8); // but not sse types        
    *byRefAttr = *byRef;
    if(state->int_regs > 0) {
        state->int_regs--; //Windows passes these by pointer
        //*inReg = true;
    }
}

bool need_destructure_argument(jl_value_t *ty)
{
    return false;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    if(jl_is_bitstype(ty))
        return NULL;
    size_t size = jl_datatype_size(ty);
    if (size == 1 || size == 2 || size == 4 || size == 8)
        return T_int64;
    return NULL;
}

// Windows needs all types pased byRef to be passed in caller allocated memory
bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return byRef;
}


#elif ABI_X86

typedef bool AbiState;
AbiState default_abi_state = 0;

inline bool is_complex64(jl_value_t *ty)
{
    return jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float32_type;
}

inline bool is_complex128(jl_value_t *ty)
{
    return jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float64_type;
}

bool use_sret(AbiState *state,jl_value_t *ty)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return false;
    if(is_complex64(ty))
        return false;
    return jl_is_structtype(ty);
}

bool need_destructure_argument(jl_value_t *ty)
{
    return false;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg, bool *byRefAttr)
{
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return;
    if(jl_is_structtype(ty) && !need_destructure_argument(ty))
        *byRef = true;
    *byRefAttr = *byRef;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    if(!isret)
        return NULL;
    if(!jl_is_datatype(ty) || jl_is_abstracttype(ty) || jl_is_bitstype(ty) ||  jl_is_cpointer_type(ty) || jl_is_array_type(ty))
        return NULL;
    // special case Complex{Float32} as a return type
    if(jl_subtype(ty,(jl_value_t*)jl_complex_type,0) && jl_tparam0(ty) == (jl_value_t*)jl_float32_type)
        return T_int64;
    return NULL;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}


#elif ABI_LLVM

// Just do whatever LLVM decides is fine

typedef bool AbiState;
AbiState default_abi_state = 0;

bool use_sret(AbiState *state,jl_value_t *ty)
{
    return false;
}

void needPassByRef(AbiState *state,jl_value_t *ty, bool *byRef, bool *inReg)
{
    return;
}

Type *preferred_llvm_type(jl_value_t *ty, bool isret)
{
    return NULL;
}

bool need_destructure_argument(jl_value_t *ty)
{
    return false;
}

bool need_private_copy(jl_value_t *ty, bool byRef)
{
    return false;
}

#endif

Value *llvm_type_rewrite(Value *v, Type *target_type, jl_value_t *ty, bool isret)
{
    if(preferred_llvm_type(ty,isret) == NULL || target_type == NULL || target_type == v->getType())
        return v;

    assert(!v->getType()->isPointerTy());

    // LLVM doesn't allow us to cast values directly, so 
    // we need to use this alloca trick
    Value *mem = builder.CreateAlloca(target_type);
    builder.CreateStore(v,builder.CreateBitCast(mem,v->getType()->getPointerTo()));
    return builder.CreateLoad(mem);
}

// --- argument passing and scratch space utilities ---

static Function *value_to_pointer_func;

// TODO: per-thread
static char *temp_arg_area;
static const uint32_t arg_area_sz = 4196;
static uint32_t arg_area_loc;
#define N_TEMP_ARG_BLOCKS 1024
static void *temp_arg_blocks[N_TEMP_ARG_BLOCKS];
static uint32_t arg_block_n = 0;
static Function *save_arg_area_loc_func;
static Function *restore_arg_area_loc_func;

extern "C" DLLEXPORT uint64_t save_arg_area_loc()
{
    return (((uint64_t)arg_block_n)<<32) | ((uint64_t)arg_area_loc);
}

extern "C" DLLEXPORT void restore_arg_area_loc(uint64_t l)
{
    arg_area_loc = l&0xffffffff;
    uint32_t ab = l>>32;
    while (arg_block_n > ab) {
        arg_block_n--;
        free(temp_arg_blocks[arg_block_n]);
    }
}

static void *alloc_temp_arg_space(uint32_t sz)
{
    void *p;
    if (arg_area_loc+sz > arg_area_sz) {
#ifdef JL_GC_MARKSWEEP
        if (arg_block_n >= N_TEMP_ARG_BLOCKS)
            jl_error("internal compiler error: out of temporary argument space in ccall");
        p = malloc(sz);
        temp_arg_blocks[arg_block_n++] = p;
#else
#error "fixme"
#endif
    }
    else {
        p = &temp_arg_area[arg_area_loc];
        arg_area_loc += sz;
    }
    return p;
}

static void *alloc_temp_arg_copy(void *obj, uint32_t sz)
{
    void *p = alloc_temp_arg_space(sz);
    memcpy(p, obj, sz);
    return p;
}

// this is a run-time function
// warning: cannot allocate memory except using alloc_temp_arg_space
extern "C" DLLEXPORT void *jl_value_to_pointer(jl_value_t *jt, jl_value_t *v, int argn,
                                     int addressof)
{
    jl_value_t *jvt = (jl_value_t*)jl_typeof(v);
    if (addressof) {
        if (jvt == jt) {
            if (jl_is_bitstype(jvt)) {
                size_t osz = jl_datatype_size(jt);
                return alloc_temp_arg_copy(jl_data_ptr(v), osz);
            }
            else if (!jl_is_tuple(jvt) && jl_is_leaf_type(jvt) && !jl_is_array_type(jvt)) {
                return v + 1;
            }
        }
        goto value_to_pointer_error;
    }
    else {
        if (jl_is_cpointer_type(jvt) && jl_tparam0(jvt) == jt) {
            return (void*)jl_unbox_voidpointer(v);
        }
    }

    if (((jl_value_t*)jl_uint8_type == jt ||
         (jl_value_t*)jl_int8_type == jt) && jl_is_byte_string(v)) {
        return jl_string_data(v);
    }
    if (jl_is_array_type(jvt)) {
        if (jl_tparam0(jl_typeof(v)) == jt || jt==(jl_value_t*)jl_bottom_type)
            return ((jl_array_t*)v)->data;
        if (jl_is_cpointer_type(jt)) {
            jl_array_t *ar = (jl_array_t*)v;
            void **temp=(void**)alloc_temp_arg_space((1+jl_array_len(ar))*sizeof(void*));
            size_t i;
            for(i=0; i < jl_array_len(ar); i++) {
                temp[i] = jl_value_to_pointer(jl_tparam0(jt),
                                              jl_arrayref(ar, i), argn, 0);
            }
            temp[i] = 0;
            return temp;
        }
    }

 value_to_pointer_error:
    std::map<int, std::string>::iterator it = argNumberStrings.find(argn);
    if (it == argNumberStrings.end()) {
        std::stringstream msg;
        msg << "argument ";
        msg << argn;
        argNumberStrings[argn] = msg.str();
        it = argNumberStrings.find(argn);
    }
    jl_value_t *targ=NULL, *pty=NULL;
    JL_GC_PUSH2(&targ, &pty);
    targ = (jl_value_t*)jl_tuple1(jt);
    pty = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type,
                                     (jl_tuple_t*)targ);
    jl_type_error_rt("ccall", (*it).second.c_str(), pty, v);
    // doesn't return
    return (jl_value_t*)jl_null;
}

static Value *julia_to_native(Type *ty, jl_value_t *jt, Value *jv,
                              jl_value_t *aty, bool addressOf,
                              bool byRef, bool inReg,
                              bool needCopy,
                              int argn, jl_codectx_t *ctx,
                              bool *mightNeedTempSpace, bool *needStackRestore)
{
    Type *vt = jv->getType();

    // We're passing any
    if (ty == jl_pvalue_llvmt) {
        return boxed(jv,ctx);
    }
    else if (ty == vt && !addressOf && !byRef) {
        return jv;
    }
    else if (vt != jl_pvalue_llvmt) {
        // argument value is unboxed
        if (addressOf || (byRef && inReg)) {
            if (ty->isPointerTy() && ty->getContainedType(0)==vt) {
                // pass the address of an alloca'd thing, not a box
                // since those are immutable.
                *needStackRestore = true;
                Value *slot = builder.CreateAlloca(vt);
                builder.CreateStore(jv, slot);
                return builder.CreateBitCast(slot, ty);
            }
        }
        else if ((vt->isIntegerTy() && ty->isIntegerTy()) ||
                 (vt->isFloatingPointTy() && ty->isFloatingPointTy()) ||
                 (vt->isPointerTy() && ty->isPointerTy())) {
            if (vt->getPrimitiveSizeInBits() ==
                ty->getPrimitiveSizeInBits()) {
                if (!byRef) {
                    return builder.CreateBitCast(jv, ty);
                }
                else {
                    Value *mem = builder.CreateAlloca(ty);
                    builder.CreateStore(jv,builder.CreateBitCast(mem,vt->getPointerTo()));
                    return mem;
                }
            }
        }
        else if (vt->isStructTy())
        {
            if (!byRef) {
                return jv;
            }
            else {
                Value *mem = builder.CreateAlloca(vt);
                builder.CreateStore(jv,mem);
                return mem;
            }
        }

        // error. box for error handling.
        jv = boxed(jv,ctx);
    }
    else if (jl_is_cpointer_type(jt)) {
        assert(ty->isPointerTy());
        if (jl_is_array_type(aty) &&
            (jl_tparam0(jt) == jl_tparam0(aty) ||
             jl_tparam0(jt) == (jl_value_t*)jl_bottom_type)) {
            // array to pointer
            return builder.CreateBitCast(emit_arrayptr(jv), ty);
        }
        if (aty == (jl_value_t*)jl_ascii_string_type || aty == (jl_value_t*)jl_utf8_string_type) {
            return builder.CreateBitCast(emit_arrayptr(emit_nthptr(jv,1,tbaa_const)), ty);
        }
        if (jl_is_structtype(aty) && jl_is_leaf_type(aty) && !jl_is_array_type(aty)) {
            if (!addressOf) {
                emit_error("ccall: expected & on argument", ctx);
                return literal_pointer_val(jl_nothing);
            }
            return builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), ty); // skip type tag field
        }
        *mightNeedTempSpace = true;
        Value *p = builder.CreateCall4(prepare_call(value_to_pointer_func),
                                       literal_pointer_val(jl_tparam0(jt)), jv,
                                       ConstantInt::get(T_int32, argn),
                                       ConstantInt::get(T_int32, (int)addressOf));
        return builder.CreateBitCast(p, ty);
    }
    else if (jl_is_structtype(jt)) {
        if (addressOf)
            jl_error("ccall: unexpected & on argument"); // the only "safe" thing to emit here is the expected struct
        //ty->dump();
        //vt->dump();
        assert (ty->isStructTy() && (Type*)((jl_datatype_t*)jt)->struct_decl == ty);
        if (aty != jt) {
            std::stringstream msg;
            msg << "ccall argument ";
            msg << argn;
            emit_typecheck(jv, jt, msg.str(), ctx);
        }
        //TODO: check instead that prefix matches
        //if (!jl_is_structtype(aty))
        //    emit_typecheck(emit_typeof(jv), (jl_value_t*)jl_struct_kind, "ccall: Struct argument called with something that isn't a struct", ctx);
        // //safe thing would be to also check that jl_typeof(aty)->size > sizeof(ty) here and/or at runtime
        Value *pjv = builder.CreateBitCast(emit_nthptr_addr(jv, (size_t)1), PointerType::get(ty,0));
        if(byRef) {
            if(!needCopy)
                return pjv;
            else {
                Value *mem = builder.CreateAlloca(ty);
                builder.CreateMemCpy(mem,pjv,(uint64_t)jl_datatype_size(jt),(uint64_t)((jl_datatype_t*)jt)->alignment);
                return mem;
            }
        }
        else {
            return builder.CreateLoad(pjv, false);
        }
    }
    else if (jl_is_tuple(jt)) {
        return emit_unbox(ty,jv,jt);
    }
    // TODO: error for & with non-pointer argument type
    assert(jl_is_bitstype(jt));
    std::stringstream msg;
    msg << "ccall argument ";
    msg << argn;
    emit_typecheck(jv, jt, msg.str(), ctx);
    Value *p = data_pointer(jv);
    Value *pjv = builder.CreateBitCast(p, PointerType::get(ty,0));
    if (byRef) {
        if(!needCopy)
            return pjv;
        else {
            Value *mem = builder.CreateAlloca(ty);
            builder.CreateMemCpy(mem,pjv,(uint64_t)jl_datatype_size(jt),(uint64_t)((jl_datatype_t*)jt)->alignment);
            return mem;
        }
    }
    else
        return builder.CreateLoad(pjv,false);
}

static jl_value_t *jl_signed_type=NULL;

typedef struct {
    Value *jl_ptr;  // if the argument is a run-time computed pointer
    void *fptr;     // if the argument is a constant pointer
    char *f_name;   // if the symbol name is known
    char *f_lib;    // if a library name is specified
} native_sym_arg_t;

// --- parse :sym or (:sym, :lib) argument into address info ---
static native_sym_arg_t interpret_symbol_arg(jl_value_t *arg, jl_codectx_t *ctx, const char *fname)
{
    jl_value_t *ptr = NULL;
    Value *jl_ptr=NULL;

    ptr = static_eval(arg, ctx, true);
    if (ptr == NULL) {
        jl_value_t *ptr_ty = expr_type(arg, ctx);
        Value *arg1 = emit_unboxed(arg, ctx);
        if (!jl_is_cpointer_type(ptr_ty)) {
            emit_cpointercheck(arg1,
                               !strcmp(fname,"ccall") ?
                               "ccall: first argument not a pointer or valid constant expression" :
                               "cglobal: first argument not a pointer or valid constant expression",
                               ctx);
        }
        jl_ptr = emit_unbox(T_size, arg1, (jl_value_t*)jl_voidpointer_type);
    }

    void *fptr=NULL;
    char *f_name=NULL, *f_lib=NULL;
    if (ptr != NULL) {
        if (jl_is_tuple(ptr) && jl_tuple_len(ptr)==1) {
            ptr = jl_tupleref(ptr,0);
        }
        if (jl_is_symbol(ptr))
            f_name = ((jl_sym_t*)ptr)->name;
        else if (jl_is_byte_string(ptr))
            f_name = jl_string_data(ptr);
        if (f_name != NULL) {
            // just symbol, default to JuliaDLHandle
            // will look in process symbol table
#ifdef _OS_WINDOWS_
            f_lib = jl_dlfind_win32(f_name);
#endif
        }
        else if (jl_is_cpointer_type(jl_typeof(ptr))) {
            fptr = *(void**)jl_data_ptr(ptr);
        }
        else if (jl_is_tuple(ptr) && jl_tuple_len(ptr)>1) {
            jl_value_t *t0 = jl_tupleref(ptr,0);
            jl_value_t *t1 = jl_tupleref(ptr,1);
            if (jl_is_symbol(t0))
                f_name = ((jl_sym_t*)t0)->name;
            else if (jl_is_byte_string(t0))
                f_name = jl_string_data(t0);
            else
                JL_TYPECHKS(fname, symbol, t0);
            if (jl_is_symbol(t1))
                f_lib = ((jl_sym_t*)t1)->name;
            else if (jl_is_byte_string(t1))
                f_lib = jl_string_data(t1);
            else
                JL_TYPECHKS(fname, symbol, t1);
        }
        else {
            JL_TYPECHKS(fname, pointer, ptr);
        }
    }
    native_sym_arg_t r;
    r.jl_ptr = jl_ptr;
    r.fptr = fptr;
    r.f_name = f_name;
    r.f_lib = f_lib;
    return r;
}


#ifdef LLVM33
    typedef AttributeSet attr_type;
#else
    typedef AttrListPtr attr_type;
#endif

// --- code generator for cglobal ---

static Value *emit_cglobal(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGS(cglobal, 1, 2);
    jl_value_t *rt=NULL;
    Value *res;
    JL_GC_PUSH1(&rt);

    if (nargs == 2) {
        JL_TRY {
            rt = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                               &jl_tupleref(ctx->sp,0),
                                               jl_tuple_len(ctx->sp)/2);
        }
        JL_CATCH {
            jl_rethrow_with_add("error interpreting cglobal type");
        }

        JL_TYPECHK(cglobal, type, rt);
        rt = (jl_value_t*)jl_apply_type((jl_value_t*)jl_pointer_type, jl_tuple1(rt));
    }
    else {
        rt = (jl_value_t*)jl_voidpointer_type;
    }
    Type *lrt = julia_type_to_llvm(rt);
    if (lrt == NULL) lrt = T_pint8;

    native_sym_arg_t sym = interpret_symbol_arg(args[1], ctx, "cglobal");

    if (sym.jl_ptr != NULL) {
        res = builder.CreateIntToPtr(sym.jl_ptr, lrt);
    }
    else if (sym.fptr != NULL) {
        res = literal_static_pointer_val(sym.fptr, lrt);
        if (imaging_mode)
            JL_PRINTF(JL_STDERR,"warning: literal address used in cglobal for %s; code cannot be statically compiled\n", sym.f_name);
    }
    else {
        if (imaging_mode) {
            res = runtime_sym_lookup((PointerType*)lrt, sym.f_lib, sym.f_name, ctx);
        }
        else {
            void *symaddr = jl_dlsym_e(get_library(sym.f_lib), sym.f_name);
            if (symaddr == NULL) {
                std::stringstream msg;
                msg << "cglobal: could not find symbol ";
                msg << sym.f_name;
                if (sym.f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)sym.f_lib != 1 && (intptr_t)sym.f_lib != 2);
#endif
                    msg << " in library ";
                    msg << sym.f_lib;
                }
                emit_error(msg.str(), ctx);
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the address of the cglobal
            res = literal_static_pointer_val(symaddr, lrt);
        }
    }

    JL_GC_POP();
    return mark_julia_type(res, rt);
}

// --- code generator for ccall itself ---

Value *sanity_check(jl_value_t *rt, jl_tuple_t *tt, size_t nargs, bool &isVa, jl_codectx_t *ctx)
{
    Type *lrt = julia_struct_to_llvm(rt);
    if (lrt == NULL) {
        JL_GC_POP();
        emit_error("ccall: return type doesn't correspond to a C type", ctx);
        return literal_pointer_val(jl_nothing);
    }

    // some sanity checking and check whether there's a vararg
    size_t i;
    size_t nargt = jl_tuple_len(tt);
    for(i=0; i < nargt; i++) {
        jl_value_t *tti = jl_tupleref(tt,i);
        if (tti == (jl_value_t*)jl_pointer_type) {
            JL_GC_POP();
            emit_error("ccall: argument type Ptr should have an element type, Ptr{T}",ctx);
            return literal_pointer_val(jl_nothing);
        }
        if (jl_is_vararg_type(tti)) {
            isVa = true;
        }
    }

    return NULL;
}

#ifdef LLVM33
    typedef AttributeSet attr_type;
#else
    typedef AttrListPtr attr_type;
#endif

std::string generate_func_sig(Type **lrt, Type **prt, int &sret,
        std::vector<Type *> &fargt, std::vector<Type *> &fargt_sig,
        std::vector<bool> &inRegList,
        std::vector<bool> &byRefList, attr_type &attributes,
        jl_value_t *rt, jl_tuple_t *tt)
{
    size_t nargt = jl_tuple_len(tt);
    *lrt = julia_struct_to_llvm(rt);
    assert(lrt); // verified in sanity_check()
    *prt = preferred_llvm_type(rt,true);
    if(*prt == NULL)
        *prt = *lrt;

#if LLVM33
    AttrBuilder retattrs;
    std::vector<AttrBuilder> paramattrs;
#else
    AttrBuilder retattrs;
    std::vector<AttrBuilder> paramattrs;
    std::vector<AttributeWithIndex> attrs;
#endif
    AbiState abi = default_abi_state;
    sret = 0;
    if (jl_is_datatype(rt) && !jl_is_abstracttype(rt) && use_sret(&abi,rt)) {
#if LLVM32 || LLVM33
        paramattrs.push_back(AttrBuilder());
        paramattrs[0].clear();
        paramattrs[0].addAttribute(Attributes::StructRet);
#else
        attrs.push_back(AttributeWithIndex::get(1, Attribute::StructRet));
#endif
        fargt.push_back(PointerType::get(*prt,0));
        fargt_sig.push_back(PointerType::get(*prt,0));
        sret = 1;
    }

    size_t i;
    bool current_isVa = false;
    for(i=0; i < nargt; i++) {
#if LLVM32 || LLVM33
        paramattrs.push_back(AttrBuilder());
#endif
        jl_value_t *tti = jl_tupleref(tt,i);
        if (jl_is_vararg_type(tti)) {
            tti = jl_tparam0(tti);
            current_isVa = true;
        }
        paramattrs.push_back(AttrBuilder());
        if (jl_is_bitstype(tti)) {
            // see pull req #978. need to annotate signext/zeroext for
            // small integer arguments.
            jl_datatype_t *bt = (jl_datatype_t*)tti;
            if (bt->size < 4) {
                if (jl_signed_type == NULL) {
                    jl_signed_type = jl_get_global(jl_core_module,jl_symbol("Signed"));
                }
#if LLVM33
                Attribute::AttrKind av;
#elif LLVM32
                Attributes::AttrVal av;
#else
                Attribute::AttrConst av;
#endif
#if LLVM32 && !LLVM33
                if (jl_signed_type && jl_subtype(tti, jl_signed_type, 0))
                    av = Attributes::SExt;
                else
                    av = Attributes::ZExt;
#else
                if (jl_signed_type && jl_subtype(tti, jl_signed_type, 0))
                    av = Attribute::SExt;
                else
                    av = Attribute::ZExt;
#endif
#if LLVM32 || LLVM33
                paramattrs[i+sret].addAttribute(av);
#else
                attrs.push_back(AttributeWithIndex::get(i+1+sret, av));
#endif
            }
        }

        Type *t = julia_struct_to_llvm(tti);
        if (t == NULL || t == T_void) {
            JL_GC_POP();
            std::stringstream msg;
            msg << "ccall: the type of argument ";
            msg << i+1;
            msg << " doesn't correspond to a C type";
            return msg;
        }

        // Whether the ABI needs us to pass this by ref and/or in registers
        // Valid combinations are:
        bool byRefAttr = false;

        // Whether or not LLVM wants us to emit a pointer to the data
        bool byRef = false;

        // Whether or not to pass this in registers
        bool inReg = false;

        if(jl_is_datatype(tti) && !jl_is_abstracttype(tti))
            needPassByRef(&abi,tti,&byRef,&inReg,&byRefAttr);

        // Add the appropriate LLVM parameter attributes
        // Note that even though the LLVM argument is called ByVal
        // this really means that the thing we're passing is pointing to
        // the thing we want to pass by value
#if LLVM33 || LLVM32
        if(byRefAttr)
            paramattrs[i+sret].addAttribute(Attributes::ByVal);
        if(inReg)
            paramattrs[i+sret].addAttribute(Attributes::InReg);
#else
        if(byRefAttr)
            attrs.push_back(AttributeWithIndex::get(i+sret+1, Attribute::ByVal));
        if(inReg)
            attrs.push_back(AttributeWithIndex::get(i+sret+1, Attribute::InReg));
#endif

        byRefList.push_back(byRef);
        inRegList.push_back(inReg);

        fargt.push_back(t);

        if(byRef)
            t = PointerType::get(t,0);

        Type *pat = preferred_llvm_type(tti,false);
        if(pat != NULL)
            t = pat;

        if(!current_isVa) {
            if(!need_destructure_argument(tti))
                fargt_sig.push_back(t);
            else
            {
                for (size_t j = 0; j < jl_tuple_len(((jl_datatype_t*)tti)->types); ++j) {
                    jl_value_t *el = jl_tupleref(((jl_datatype_t*)tti)->types,j);
                    fargt_sig.push_back(julia_type_to_llvm(el));
                }
            }
        }

    }


#ifdef LLVM33
    if(retattrs.hasAttributes())
        attributes = Attributes::get(AttributeSet::ReturnIndex,retattrs);
    for(i = 0; i < nargt+sret; ++i)
        if(paramattrs[i].hasAttributes())
            attributes = attributes.addAttributes(i+1,paramattrs[i]);
#elif LLVM32
    if(retattrs.hasAttributes())
        attrs.push_back(AttributeWithIndex::get(0, Attributes::get(jl_LLVMContext,retattrs)));
    for(i = 0; i < nargt+sret; ++i)
        if(paramattrs[i].hasAttributes())
            attrs.push_back(AttributeWithIndex::get(i+1, Attributes::get(jl_LLVMContext,paramattrs[i])));
    attributes = AttrListPtr::get(getGlobalContext(), ArrayRef<AttributeWithIndex>(attrs));
#else
    attributes = AttrListPtr::get(attrs.data(),attrs.size());
#endif
    return "";
}

// ccall(pointer, rettype, (argtypes...), args...)
static Value *emit_ccall(jl_value_t **args, size_t nargs, jl_codectx_t *ctx)
{
    JL_NARGSV(ccall, 3);
    jl_value_t *rt=NULL, *at=NULL;
    JL_GC_PUSH2(&rt, &at);

    native_sym_arg_t symarg = interpret_symbol_arg(args[1], ctx, "ccall");
    Value *jl_ptr=NULL;
    void *fptr = NULL;
    char *f_name = NULL, *f_lib = NULL;
    jl_ptr = symarg.jl_ptr;
    fptr = symarg.fptr;
    f_name = symarg.f_name;
    f_lib = symarg.f_lib;
    bool isVa = false;

    if (f_name == NULL && fptr == NULL && jl_ptr == NULL) {
        JL_GC_POP();
        emit_error("ccall: null function pointer", ctx);
        return literal_pointer_val(jl_nothing);
    }

    {
        JL_TRY {
            rt  = jl_interpret_toplevel_expr_in(ctx->module, args[2],
                                                &jl_tupleref(ctx->sp,0),
                                                jl_tuple_len(ctx->sp)/2);
        }
        JL_CATCH {
            jl_rethrow_with_add("error interpreting ccall return type");
        }
    }

    if (jl_is_tuple(rt)) {
        std::string msg = "in " + ctx->funcName +
            ": ccall: missing return type";
        jl_error(msg.c_str());
    }
    if (rt == (jl_value_t*)jl_pointer_type)
        jl_error("ccall: return type Ptr should have an element type, Ptr{T}");

    {
        JL_TRY {
            at  = jl_interpret_toplevel_expr_in(ctx->module, args[3],
                                                &jl_tupleref(ctx->sp,0),
                                                jl_tuple_len(ctx->sp)/2);
        }
        JL_CATCH {
            jl_rethrow_with_add("error interpreting ccall argument tuple");
        }
    }

    JL_TYPECHK(ccall, type, rt);
    JL_TYPECHK(ccall, tuple, at);
    JL_TYPECHK(ccall, type, at);

    jl_tuple_t *tt = (jl_tuple_t*)at;

    // check for calling convention specifier
    CallingConv::ID cc = CallingConv::C;
    jl_value_t *last = args[nargs];
    if (jl_is_expr(last)) {
        jl_sym_t *lhd = ((jl_expr_t*)last)->head;
        if (lhd == jl_symbol("stdcall")) {
            cc = CallingConv::X86_StdCall;
            nargs--;
        }
        else if (lhd == jl_symbol("cdecl")) {
            cc = CallingConv::C;
            nargs--;
        }
        else if (lhd == jl_symbol("fastcall")) {
            cc = CallingConv::X86_FastCall;
            nargs--;
        }
        else if (lhd == jl_symbol("thiscall")) {
            cc = CallingConv::X86_ThisCall;
            nargs--;
        }
    }

    Value *err = sanity_check(rt,tt,nargs,isVa,ctx);
    if (err != NULL)
        return err;

    if ((!isVa && jl_tuple_len(tt)  != (nargs-2)/2) ||
        ( isVa && jl_tuple_len(tt)-1 > (nargs-2)/2))
        jl_error("ccall: wrong number of arguments to C function");

    // some special functions
    if (fptr == (void *) &jl_array_ptr ||
        (f_lib==NULL && f_name && !strcmp(f_name,"jl_array_ptr"))) {
        Value *ary = emit_expr(args[4], ctx);
        JL_GC_POP();
        return mark_julia_type(builder.CreateBitCast(emit_arrayptr(ary),julia_type_to_llvm(rt)),
                               rt);
    }
    if (fptr == (void *) &jl_value_ptr ||
        (f_lib==NULL && f_name && !strcmp(f_name,"jl_value_ptr"))) {
        jl_value_t *argi = args[4];
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }
        Value *ary = boxed(emit_expr(argi, ctx),ctx);
        JL_GC_POP();
        return mark_julia_type(
                builder.CreateBitCast(emit_nthptr_addr(ary, addressOf?1:0),julia_type_to_llvm(rt)),
                rt);
    }
    if (fptr == (void *) &jl_is_leaf_type ||
        (f_lib==NULL && f_name && !strcmp(f_name, "jl_is_leaf_type"))) {
        jl_value_t *arg = args[4];
        jl_value_t *ty = expr_type(arg, ctx);
        if (jl_is_type_type(ty) && !jl_is_typevar(jl_tparam0(ty))) {
            int isleaf = jl_is_leaf_type(jl_tparam0(ty));
            JL_GC_POP();
            return ConstantInt::get(T_int32, isleaf);
        }
    }


    // save place before arguments, for possible insertion of temp arg
    // area saving code.
    Value *saveloc=NULL;
    Value *stacksave=NULL;
    BasicBlock::InstListType &instList = builder.GetInsertBlock()->getInstList();
    Instruction *savespot;
    if (instList.empty()) {
        savespot = NULL;
    }
    else {
        // hey C++, there's this thing called pointers...
        Instruction &_savespot = builder.GetInsertBlock()->back();
        savespot = &_savespot;
    }

    std::vector<Type*> fargt(0);
    std::vector<Type*> fargt_sig(0);
    std::vector<bool> inRegList(0);
    std::vector<bool> byRefList(0);
    attr_type attrs;
    Type *lrt = NULL;
    Type *prt = NULL;
    int sret = 0;
    std::string err_msg = generate_func_sig(&lrt,&prt,sret,fargt,fargt_sig,inRegList,byRefList,attrs,rt,tt);
    if(!err_msg.empty()) {
        JL_GC_POP();
        emit_error(err_msg,ctx);
        return literal_pointer_val(jl_nothing);
    }


    if (0 && f_name != NULL) {
        // print the f_name before each ccall
        Value *zeros[2] = { ConstantInt::get(T_int32, 0),
                            ConstantInt::get(T_int32, 0) };
        std::stringstream msg;
            msg << "ccall: ";
            msg << f_name;
            msg << "(...)";
            if (f_lib != NULL && (intptr_t)f_lib != 1 && (intptr_t)f_lib != 2) {
                msg << " in library ";
                msg << f_lib;
            }
            msg << "\n";
        builder.CreateCall2(prepare_call(jlputs_func),
                            builder.CreateGEP(stringConst(msg.str()),
                                         ArrayRef<Value*>(zeros)),
                            prepare_global(jlstderr_var));
    }

    // emit arguments
    Value **argvals = (Value**) alloca(((nargs-3)/2 + sret)*sizeof(Value*));
    Value *result = NULL;

    // First, if the ABI requires us to provide the space for the return
    // argument, allocate the box and store that as the first argument type
    if (sret) {
        result = emit_newsym(rt,0,NULL,ctx);
        assert(result != NULL && "Type was not concrete");
        if (!result->getType()->isPointerTy()) {
            Value *mem = builder.CreateAlloca(lrt);
            builder.CreateStore(result, mem);
            result = mem;
        }
        argvals[0] = result;
    }

    // save argument depth until after we're done emitting arguments
    int last_depth = ctx->argDepth;

    // number of parameters to the c function
    size_t nargty = jl_tuple_len(tt);
    bool needTempSpace = false;
    bool needStackRestore = false;
    size_t i;
    for(i=4; i < nargs+1; i+=2) {

        // Current C function parameter
        size_t ai = (i-4)/2;

        // Julia (expression) value of current parameter
        jl_value_t *argi = args[i];

        // pass the address of the argument rather than the argument itself
        bool addressOf = false;
        if (jl_is_expr(argi) && ((jl_expr_t*)argi)->head == amp_sym) {
            addressOf = true;
            argi = jl_exprarg(argi,0);
        }

        // LLVM type of the current parameter
        Type *largty;

        // Julia type of the current parameter
        jl_value_t *jargty;
        if (isVa && ai >= nargty-1) {
            largty = fargt[nargty-1];
            jargty = jl_tparam0(jl_tupleref(tt,nargty-1));
        }
        else {
            largty = fargt[sret+ai];
            jargty = jl_tupleref(tt,ai);
        }

        Value *arg;
        bool needroot = false;
        if (largty == jl_pvalue_llvmt || largty->isStructTy()) {
            arg = emit_expr(argi, ctx, true);
            if (largty == jl_pvalue_llvmt && arg->getType() != jl_pvalue_llvmt) {
                arg = boxed(arg,ctx);
                needroot = true;
            }
        }
        else {
            arg = emit_unboxed(argi, ctx);
            if (jl_is_bitstype(expr_type(argi, ctx))) {
                Type *at = arg->getType();
                Type *totype = addressOf ? largty->getContainedType(0) : largty;
                if (at != jl_pvalue_llvmt && at != totype &&
                    !(at->isPointerTy() && jargty==(jl_value_t*)jl_voidpointer_type)) {
                    emit_type_error(arg, jargty, "ccall", ctx);
                    arg = UndefValue::get(totype);
                }
                else {
                    arg = emit_unbox(totype, arg, jargty);
                }
            }
        }

#ifdef JL_GC_MARKSWEEP
        // make sure args are rooted
        if (largty == jl_pvalue_llvmt && (needroot || might_need_root(argi))) {
            make_gcroot(arg, ctx);
        }
#endif

        bool mightNeed=false;
        if (!need_destructure_argument(jargty)) {
            argvals[ai+sret] = llvm_type_rewrite(julia_to_native(largty, jargty, arg, expr_type(argi, ctx), addressOf, byRefList[ai], inRegList[ai],
                                               need_private_copy(jargty,byRefList[ai]),ai+1, ctx, &mightNeed, &nSR),fargt_sig[ai+sret],jargty,false));
        } else {
            assert(0);
            assert(jl_is_structtype(jargty));
            assert(largty->isStructTy());
            StructType *sty = dyn_cast<StructType>(largty);
            Value *s = julia_to_native(largty, jargty, arg, expr_type(argi, ctx), addressOf, true, false, need_private_copy(jargty,byRefList[ai]), ai+1, ctx, &mightNeed);
            for (size_t j = 0; j < sty->getNumElements(); ++j)
            {
                std::vector<Value*> args;
                args.push_back(ConstantInt::get(T_size,0));
                args.push_back(ConstantInt::get(T_size,j));
                argvals[ai+sret] = builder.CreateLoad(builder.CreateGEP(s,args));
            }
        }
        needTempSpace |= mightNeed;
        needStackRestore |= nSR;

    //    if (!isVa || ai < nargty-1)
    //        fargt_sig.push_back(fargt[ai+sret]);
    }


    // make LLVM function object for the target
    // keep this close to the function call, so that the compiler can
    // optimize the global pointer load in the common case
    Value *llvmf;
    FunctionType *functype = FunctionType::get(sret?T_void:prt, fargt_sig, isVa);

    if (jl_ptr != NULL) {
        null_pointer_check(jl_ptr,ctx);
        Type *funcptype = PointerType::get(functype,0);
        llvmf = builder.CreateIntToPtr(jl_ptr, funcptype);
    }
    else if (fptr != NULL) {
        Type *funcptype = PointerType::get(functype,0);
        llvmf = literal_static_pointer_val(fptr, funcptype);
        if (imaging_mode)
            JL_PRINTF(JL_STDERR,"warning: literal address used in ccall for %s; code cannot be statically compiled\n", f_name);
    }
    else {
        assert(f_name != NULL);

        PointerType *funcptype = PointerType::get(functype,0);
        if (imaging_mode) {
            llvmf = runtime_sym_lookup(funcptype, f_lib, f_name, ctx);
        }
        else {
            void *symaddr = jl_dlsym_e(get_library(f_lib), f_name);
            if (symaddr == NULL) {
                JL_GC_POP();
                std::stringstream msg;
                msg << "ccall: could not find function ";
                msg << f_name;
                if (f_lib != NULL) {
#ifdef _OS_WINDOWS_
                    assert((intptr_t)f_lib != 1 && (intptr_t)f_lib != 2);
#endif
                    msg << " in library ";
                    msg << f_lib;
                }
                emit_error(msg.str(), ctx);
                return literal_pointer_val(jl_nothing);
            }
            // since we aren't saving this code, there's no sense in
            // putting anything complicated here: just JIT the function address
            llvmf = literal_static_pointer_val(symaddr, funcptype);
        }
    }

    if (needTempSpace) {
        // save temp argument area stack pointer
        // TODO: inline this
        saveloc = CallInst::Create(prepare_call(save_arg_area_loc_func));
        if (savespot)
            instList.insertAfter(savespot, (Instruction*)saveloc);
        else
            instList.push_front((Instruction*)saveloc);
        savespot = (Instruction*)saveloc;
    }

    if (needStackRestore) {
        stacksave = CallInst::Create(Intrinsic::getDeclaration(jl_Module,
                                                               Intrinsic::stacksave));
        if (savespot)
            instList.insertAfter((Instruction*)savespot, (Instruction*)stacksave);
        else
            instList.push_front((Instruction*)stacksave);
    }

    //llvmf->dump();
    //for (std::vector<Value *>::iterator it = argvals.begin() ; it != argvals.end(); ++it)
    //    (*it)->dump();

    // the actual call
    Value *ret = builder.CreateCall(
            prepare_call(llvmf),
            ArrayRef<Value*>(&argvals[0],(nargs-3)/2+sret));
#ifdef LLVM32
    ((CallInst*)ret)->setAttributes(AttrListPtr::get(getGlobalContext(), ArrayRef<AttributeWithIndex>(attrs)));
#else
    attributes = AttrListPtr::get(attrs.data(),attrs.size());
    ((CallInst*)ret)->setAttributes(attributes);
#endif

    if (cc != CallingConv::C)
        ((CallInst*)ret)->setCallingConv(cc);
    if (!sret)
        result = ret;
    if (needStackRestore) {
        assert(stacksave != NULL);
        builder.CreateCall(Intrinsic::getDeclaration(jl_Module,
                                                     Intrinsic::stackrestore),
                           stacksave);
    }
    if (needTempSpace) {
        // restore temp argument area stack pointer
        assert(saveloc != NULL);
        builder.CreateCall(prepare_call(restore_arg_area_loc_func), saveloc);
    }
    ctx->argDepth = last_depth;
    if (0) { // Enable this to turn on SSPREQ (-fstack-protector) on the function containing this ccall
#if LLVM32 && !LLVM33
        ctx->f->addFnAttr(Attributes::StackProtectReq);
#else
        ctx->f->addFnAttr(Attribute::StackProtectReq);
#endif
    }

    JL_GC_POP();
    // Finally we need to box the result into julia type
    // However, if we have already created a box for the return
    // type because we the ABI required us to pass a pointer (sret),
    // then we do not need to do this.
    if (!sret) {
        if (lrt == T_void)
            result = literal_pointer_val((jl_value_t*)jl_nothing);
        if (lrt->isStructTy()) {
            //fprintf(stderr, "ccall rt: %s -> %s\n", f_name, ((jl_tag_type_t*)rt)->name->name->name);
            assert(jl_is_structtype(rt));

            // Call jlallocobj_func with the appropriate size (argument size size_t)
            Value *strct =
            builder.CreateCall(prepare_call(jlallocobj_func),
                                   ConstantInt::get(T_size,
                                        sizeof(void*)+((jl_datatype_t*)rt)->size));
            // Store the type into the first field
            builder.CreateStore(literal_pointer_val((jl_value_t*)rt),
                                emit_nthptr_addr(strct, (size_t)0));
            builder.CreateStore(result,
                                builder.CreateBitCast(
                                    emit_nthptr_addr(strct, (size_t)1),
                                    PointerType::get(prt,0)));
            return mark_julia_type(strct, rt);
        } else {
            if(prt->getPrimitiveSizeInBits() == lrt->getPrimitiveSizeInBits())
                result = builder.CreateBitCast(result,lrt);
            else {
                assert(0 && "Unimplemented");
            }
        }
    }
    return mark_julia_type(result, rt);
}
