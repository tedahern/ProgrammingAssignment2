## The two functions defined below will create a matrix object that can cache its
## inverse, and then either retrieve the inverse from the cache, or solve for
## the inverse if it is not available in the cache

## The first function will create a matrix that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function(inverse) inv <<- inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse
             getinverse = getinverse)
}


## The following function will return the inverse of the matrix 'x'. It will
## first attempt to return this from the cache; if it is not in cache, then
## it will solve for the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}
