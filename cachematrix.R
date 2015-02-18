# Thes functions crate a matrix then calculates the inverse of it.

## The makeCacheMatrix creates a matrix and contains a list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function first checks if the x matrix has been inverted and cached. 
## If yes, it gives a message. 
## If not, it calculates the inverse and updates the cached data.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m  
}
