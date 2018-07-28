## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {x}
        setsolve <- function(solve) {i <<- solve}
        getsolve <- function() {i}
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cachesolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
