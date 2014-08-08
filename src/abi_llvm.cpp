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
