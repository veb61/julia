typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union(Vector{T}, Matrix{T})

typealias DenseVector{T} DenseArray{T,1}
typealias DenseMatrix{T} DenseArray{T,2}
typealias DenseVecOrMat{T} Union(DenseVector{T}, DenseMatrix{T})


typealias StridedArray{T,N,A<:DenseArray} Union(DenseArray{T,N}, SubArray{T,N,A})
typealias StridedVector{T,A<:DenseArray}  Union(DenseArray{T,1}, SubArray{T,1,A})
typealias StridedMatrix{T,A<:DenseArray}  Union(DenseArray{T,2}, SubArray{T,2,A})
typealias StridedVecOrMat{T} Union(StridedVector{T}, StridedMatrix{T})
