## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix takes one matrix (x) and returns
# a list with this matrix and some functions for 
# using this matrix. The functions that are covered by
# this function are:
#   1. set - sets the value of the matrix
#   2. get - gets the inverse of the matrix
#   3. setinverse - sets the inverse of the matrix
#   4. getinverse - gets the inverse of the matrix

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


## Write a short comment describing this function
## cacheSolve takes two arguments a matrix and ... and 
# computes the inverse of "matrix" returned by 
## `makeCacheMatrix`. This function only calculates the 
## function result if it is not already calculated. If it
## is in cache, it will be returned from cache without any
## calculation

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