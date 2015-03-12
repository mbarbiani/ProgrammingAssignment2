## cachematrix assignment no.2 
## Coursera - R Programming course r012
## Author: M. Barbiani
## Date: 12 March 2015
## Version: 1.0
##
## This file contains two functions defining a matrix object and
## computing and storing its inverse.
## The inverse is computed and cached to be available for
## following calls without need of recomputation.

## The "makeCacheMatrix" function creates a special "matrix"
## object that can cache its inverse.
##
## INPUT: x is a matrix (it is assumed it is invertible)
## OUTPUT: it is a list of functions 

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize to NULL the variable storing the inverse of
  ## the x matrix 
  invrs <- NULL
  
  ## "set" sets the matrix x and resets to NULL the invrs matrix.
  ## invrs must be reset in order to be recomputed, x is changed
  ## upon calling of the set function
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  
  ## "get" returns the stored matrix x
  get <- function() x
  
  ## "setinverse" sets the new value for the invrs variable
  setinverse <- function(inverse) invrs <<- inverse
  
  ## the getinverse function returns the value of the "invrs" variable  
  getinverse <- function() invrs
  
  ## a list of the functions is created as output
  list(set = set, get = get,
       setInverse = setinverse,
       getInverse = getinverse)
  
}


## The "cacheSolve" function computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function.
## INPUT: x is a list created by the "makeCacheMatrix" function
## OUTPUT: it is the inverse matrix of x 

cacheSolve <- function(x, ...) {
  
  ## storing into m the inverse matrix from the cache: it can be NULL
  m <- x$getInverse
  
  ## if m is not NULL then it is returned
  #  (the caller is notified about having used cached data)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if m is NULL instead  the inverse is computed and stored in the
  ## cache before returning it
  data <- x$get
  m <- solve(data, ...)
  x$setInverse(m)
  
  return(m)
}