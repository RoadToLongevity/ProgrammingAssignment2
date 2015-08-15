## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve are complementary functions that can create a
## new matrix, solve the inverse of a matrix, and cache a matrix's inverse.
## For large matrices, looking up a cached inverse for a matrix takes
## less time than solving the matrix inverse.  Using this function to cache an
## inverse matrix can save time if the inverse matrix needs to be looked up
## multiple times.

## Thanks to Gregory D. Horne for providing unit tests for this code.


## Write a short comment describing this function


## makeCacheMatrix provides methods for making a new matrix.  It also provides
## setinverse and getinverse, which are used in cacheSolve below to cache the
## inverse of a matrix if it has been solved at least once. The "<<-" assignment
## operator changes the lexical scope of a variable to be in the function's 
## parent environment so that it can be accessed by another function.

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


## Write a short comment describing this function

## cacheSolve is a function which can solve a matrix, i.e. calculate it's
## inverse using the "solve" function.  It uses callbacks to methods defined in
## makeCacheMatrix in order to cache a matrix inverse once it has been solved.


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
