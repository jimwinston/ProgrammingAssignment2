## Functions to create and invert objects containing square
## invertible matrixes. The cached matrix object can hold the matrix
## and its inverse. The cacheSolve function will retrieve
## the cached value of that inverse matrix or force the
## computation of that inverse matrix and then save it.

## Creates an object that holds a square invertible matrix
## and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
 
}

## Takes an object created by makeCacheMatrix() containing
## a square invertible matrix and returns the inverse of the matrix.
## The value returned may be cached or newly computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
