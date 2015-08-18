## Put comments here that give an overall description of what your
## functions do

## Name: makeCacheMatrix
## Description:
##   Creates a special "matrix" object that encapsulates a
##   matrix and cache the inverse after it is computed.
##
## set: Assigns the encapsulated matrix.
##   Input: m2 - An orthogonal R matrix.
##
## get: Returns the encapsulated matrix.
##
## set_inverse: Sets the inverse variable and stores it in the cache.
##   Input: new_inverse - The matrix inverse to persist.
##
## get_inverse: Returns the pre-calculated matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  
  set <- function(m2) {
    x <<- m2
    inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  
  get_inverse <- function() inverse
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Name: cacheSolve
## Description:
##   Calculates the inverse of the given special "matrix",
##   or returns the cached one if it was calculated before.
##
## Input:
##   x: The special "matrix" which encapsulates the real matrix and its inverse.
## Output:
##   return: The matrix's inverse.

cacheSolve <- function(x, ...) {
  inverse <- matrix$get_inverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get()
  inverse <- solve(data)
  matrix$set_inverse(inverse)
  inverse
}
