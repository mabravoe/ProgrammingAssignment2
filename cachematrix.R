## The 2 following functions allow you to cache the inverse 
## of a square matrix in order to save time on an otherwise
## costly computation.


## This first function creates a "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) Inv <<- solve
        getInverse <- function() Inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## This second function either computes the inverse returned by the 
## function above (makeCacheMatrix) or retrieves the inverse of that
## matrix from the cache

cacheSolve <- function (x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
