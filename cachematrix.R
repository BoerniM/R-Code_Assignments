### File: cachematrix.R
### Author: B.Merkel - 21-June 2016
### Programming Assignment 2: Lexical Scoping
### --------------------------------------------------------------

### The first function makeCacheMatrix() creates a list with several functions:
### - set the values of a matrix A (cache)
### - get the values of a matrix A
### - set the values of the inverse matrix A_inv (cache)
### - get the values of the inverse matrix A_inv (cache)

makeCacheMatrix <- function(A = matrix()) {

    A_inv <- NULL
    set <- function(B) {
        A <<- B
        A_inv <<- NULL
    }
    
    get <- function() A
    setinverse <- function(inverse) A_inv <<- inverse
    getinverse <- function() A_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

### The second function cacheSolve() calculates the inverse of the "list" (matrix) created with makeCacheMatrix
### It first checks whether the inverse has already been calculated.
### If so, it gets the inverse from the cache. Otherwise it calculates the matrix inverse and stores it in the 
### cache via setinverse()


cacheSolve <- function(A, ...) {
    A_inv <- A$getinverse()
    if(!is.null(A_inv)) {
        message("getting cached data")
        return(A_inv)
    }
    data <- A$get()
    A_inv <- solve(data, ...)
    A$setinverse(A_inv)
    A_inv ## Return a matrix that is the inverse of 'A'
}
