## The function makeCacheMatrix takes a matrix as an argument and 
## caches its inverse in a list of functions for later use. 
## The invere matrix until calculated by cacheSolve is NULL.
## The function cacheSolve computes the inverse of the argument matrix
## if it has not been calculated already. If already calculated and cached, 
## then cacheSolve retrieves it from the cache instead of recalculating the inverse matrix

## This function creates matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inversemat <- NULL
        setfunc <- function(y) {
                x <<- y
                inversemat <<- NULL
        }
        getfunc <- function() x
        setinversefunc <- function(solve) inversemat <<- solve
        getinversefunc <- function() inversemat
        list(set = setfunc, get = getfunc, 
             setinverse = setinversefunc, 
             getinverse = getinversefunc)
}


## This function computes, caches and returns matrix inverse 
#if not computed previously and 
#retrieves the cached inverse matrix
#if already previously computed

cacheSolve <- function(x, ...) {
        inversemat <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        data <- x$get()
        inversemat <- solve(data, ...)
        x$setinverse(inversemat)
        inversemat
}
