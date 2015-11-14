## The following functions are contained in this file:
## 1. makeCacheMatrix: Creates a matrix object with "getter" and "setter" functions
##    to store both matrix and its inverse
## 2. cacheSolve: Checks the inverse of the matrix object and solves for it (and
##    sets it in the object if and only if it is null

## Takes a matrix and returns a "matrix object" (really a list of getter and setter
## functions that use the <<- operator to store values in the parent function)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix  <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Takes the matrix object (list) as an argument, uses the getInverse functtion to
## check if the inverse is null.If it is, it solves for it and stores it in the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data)
        x$setInverse(i)
        i
}
