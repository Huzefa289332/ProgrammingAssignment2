## To write a pair of functions that cache the inverse of a matrix.

## This function creates a kind of matrix object that can cache its inverse.

createCacheMatrix <- function(inputMatrix = matrix()) {
    cachedInverse <- NULL
    
    setMatrix <- function(newMatrix) {
        inputMatrix <<- newMatrix
        cachedInverse <<- NULL
    }
    
    getMatrix <- function() inputMatrix
    
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    getInverse <- function() cachedInverse
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix
## returned by the above method makeCacheMatrix. If the inverse has 
## been calculated already without the change in matrix, then the 
## method below cachesolve should get the inverse from the cache memory.

computeCachedInverse <- function(cacheMatrix, ...) {
    cachedInverse <- cacheMatrix$getInverse()
    
    if (!is.null(cachedInverse)) {
        message("Fetching cached inverse")
        return(cachedInverse)
    }
    
    matrixData <- cacheMatrix$getMatrix()
    inverseResult <- solve(matrixData, ...)
    cacheMatrix$setInverse(inverseResult)
    inverseResult
}
