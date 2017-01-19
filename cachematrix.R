## This is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setMatInv <- function(pMat_inv) mat_inv <<- pMat_inv
    getMatInv <- function() mat_inv
    list(set = set, get = get,
         setMatInv =setMatInv,
         getMatInv = getMatInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## This function needs the package "MASS" to call the function "ginv" (Generalized inverse of a matrix)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getMatInv ()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    
    ## If x is a square matrix the "solve" function is used, otherwise the "ginv" function 
    ## of the 'MASS' package is used.
    if (ncol(x)==nrow(x)){
         return(solve(x))
    }
    ginv(x) 
}
