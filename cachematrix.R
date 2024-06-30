## To write a pair of functions that cache the inverse of a matrix.

## This function creates a kind of matrix object that can cache its inverse.

cacheMatrix <- function(initialMatrix = matrix()) {
    invMatrix <- NULL
    
    # Function to set the matrix and reset the cached inverse
    set <- function(matrix) {
        initialMatrix <<- matrix
        invMatrix <<- NULL
    }
    
    # Function to get the matrix
    get <- function() initialMatrix
    
    # Function to set the cached inverse
    setInverse <- function(inverse) invMatrix <<- inverse
    
    # Function to get the cached inverse
    getInverse <- function() invMatrix
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix
## returned by the above method makeCacheMatrix. If the inverse has 
## been calculated already without the change in matrix, then the 
## method below cachesolve should get the inverse from the cache memory.

computeInverse <- function(matrixObject, ...) {
    invMatrix <- matrixObject$getInverse()
    
    # Check if the inverse is already cached
    if (!is.null(invMatrix)) {
        message("Using cached inverse")
        return(invMatrix)
    }
    
    # Compute the inverse, cache it, and return it
    dataMatrix <- matrixObject$get()
    invMatrix <- solve(dataMatrix, ...)
    matrixObject$setInverse(invMatrix)
    invMatrix
}
