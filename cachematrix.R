## The purpose of the two functions in this script are to cache
## a matrix and its inverse so that repeated needs for the matrix
## inverse do not result in expensive matrix inverse calculations.
## This is useful when the matrix whose inverse is needed doesn't
## change or changes rarely while the inverse is needed often.

## This function, given an input invertible matrix, returns a
## function vector that includes setting/getting the matrix or
## its inverse. It uses the <<- operator to store the matrix and
## its inverse outside of the current environment.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    ## Cache the new matrix and clear its inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function, given the function vector returned from
## makeCacheMatrix, returns the matrix inverse of the matrix
## that was input into makeCacheMatrix. If the inverse was
## never calculated before, it calculates the inverse and
## caches it, otherwise it returns the cached copy of the
## matrix inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()

    ## Return the cached copy
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    ## Not cached, so calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)

    ## Cache the inverse now
    x$setinverse(inv)
    inv
}
