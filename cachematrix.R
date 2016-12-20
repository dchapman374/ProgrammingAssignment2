## Coursera/Johns Hopkins R Programming Course
## Programming Assignment 2
## 
## Author: Douglas Chapman
## Date: Dec 20, 2016
## 
## The following script contains two functions, namely
## makeCacheMatrix and cacheSolve.  Discriptions of each function are
## provided as headers for the functions.

## makeCacheMatrix: creates a special "matrix" object that is able to
##   cache it inverse.
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

## cacheSolve: computes the inverse of the special "matrix" returned
##   by makeCacheMatrix. If the inverse has already been calculated
##   (and the matrix has not changed), then cacheSolve retrieves
##   the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if (!is.null(m)) {
                message("inverting matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
