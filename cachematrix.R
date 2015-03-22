## ===========================================================================
## Cache the inverse of a matrix
## JGulbert 2015.03.20
## ===========================================================================

## ---------------------------------------------------------------------------
## This function creates a special matrix object that can cache its inverse
## Args: x - matrix to be inversed
## ---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

    # inv_mat holds the inverse of the matrix x
    inv_mat <- NULL
    
    # Function set to inicialize the object
    # Always the matrix is "set", inv_mat is set to NULL too
    set <- function(y) {
        if (!identical(x, y)) {
            x <<- y             # Matrix
            inv_mat <<- NULL    # Inverse
        } else
            message("Matrix has not changed")
    }
    
    # Function get to return the matrix
    get <- function() x
    
    # Function setInverse to set the inverse of the matrix x
    setInverse <- function(inverse) inv_mat <<- inverse
    
    # Function getInverse to return the inverse of the matrix x
    getInverse <- function() inv_mat
    
    # The named list to be returned
    list(set = set, get = get, setInverse = setInverse, 
            getInverse = getInverse)
}


## ---------------------------------------------------------------------------
## This function computed the inverse of the special matrix returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then this function should retrieve the inverse
## from the cache.
##
## Args: x - list object returned by makeCacheMatrix
## ---------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    
    # Get inverse
    inv <- x$getInverse()
    
    # Has it been calculated?
    if (!is.null(inv)) {
        message("Getting Inverse from cache")
        return(inv)
    }
    
    # Nop. We need to do it
    data <- x$get()          # Get de matrix
    inv <- solve(data, ...)  # Compute de inverse with solve
    x$setInverse(inv)        # Cache de inverse matrix
    
    inv
    
}
