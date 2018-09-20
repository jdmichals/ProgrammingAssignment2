## This is a pair of functions that will cache the inverse of a matrix.

## This function creates a special "matrix" object , which is actually a list 
## containing functions to (1) set the value of the matrix, (2) get the value of 
## the matrix, (3) set the value of its inverse,(4) get the value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" object returned by 
## makeCacheMatrix above. It first checks to see if the inverse has already been
## computed and if it has returns the inverse from the cache instead of computing
## it again. If the inverse has not already been computed, it computes the inverse
## and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  invr
}