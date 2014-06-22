## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function   
## Creates a matrix from the given set of values

makeCacheMatrix <- function(x = matrix(2:9, 4, 5)) {
    if (is.object(data) || !is.atomic(data)) 
        data <- as.vector(data)
    .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), 
        missing(ncol)))
}


## Write a short comment describing this function
## The function Calculates the Moore-Penrose generalized inverse of a matrix ## X and X is a Matrix for which the Moore-Penrose inverse is required

cacheSolve <- function (X, tol = sqrt(.Machine$double.eps)) 
{
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))

        ## Return a matrix that is the generalize o pseudoinverse inverse of 'x'
}
   