## To write a pair of functions that cache the inverse of a matrix.

## This function creates a kind of matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  # Return the matrix
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix
## returned by the above method makeCacheMatrix. If the inverse has 
## been calculated already without the change in matrix, then the 
## method below cachesolve should get the inverse from the cache memory.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
