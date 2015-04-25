## These functions invert a square matrix and cache the results
## When Rerun, the cached results are used for efficiency

## This function caches the results of the matrix inversion from the cacheSolve function


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
            setinverse = setinverse,
             getinverse = getinverse)
}


## This function inverts a square matrix (an input of the makeCacheMatrix function)
## unless the inverted matrix has already been computed and cached
## if cached, the cached solution is returned


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}