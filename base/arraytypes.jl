typealias StridedArray{T,N,A<:DenseArray} Union(DenseArray{T,N}, SubArray{T,N,A})
typealias StridedVector{T,A<:DenseArray}  Union(DenseArray{T,1}, SubArray{T,1,A})
typealias StridedMatrix{T,A<:DenseArray}  Union(DenseArray{T,2}, SubArray{T,2,A})
typealias StridedVecOrMat{T} Union(StridedVector{T}, StridedMatrix{T})
