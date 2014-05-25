## This is my solution to Assignment 2 of the Coursera MOOC "R Programming"
## The assignment concerns: Caching the Inverse of a Matrix
##
## For this assignment two functions are used:
##
## a. makeCacheMatrix
## This function is very useful as it creates a special "matrix" object that can cache its inverse. 
##
## b. cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## Extra note: Computing the inverse of a square matrix can be done with the solve function in R.
## If, for instance, X is a square invertible matrix, then solve(X) returns its inverse.
## Reading tip on what can be done with matrices: http://en.wikibooks.org/wiki/R_Programming/Mathematics
##
## Let's start with the first function: makeCacheMatrix
## This function does 4 things:
## 1. sets the value of the matrix;
## 2. gets the value of the matrix;
## 3. sets the value of the inverse;
## 4. gets the value of the inverse;
##
## The R-codes are as follows:

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Let's continue with the second function: cacheSolve
## This functions calculates the inverse of the matrix created with the first function makeCacheMatrix.
## This function does the following:
## 1. It checks to see if the inverse has already been calculated;
## 2. If this is the case, it gets the inverse from the cache and skips the computation;
## 3. If this is NOT the case, it calculates the inverse of the matrix and setst the value of the matrix in the cache.
##
## The R-codes are as follows:

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

