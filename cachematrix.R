## Put comments here that give an overall description of what your
## functions do

## This function gets the value of the matrix and cache's the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the inverse of the matrix has already been calculated by
## makeCacheMatrix above. It calculates it, if the matrix changed, or if the function above did not do so.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting inverse from cache")
    return(m)
  }
  nueva <- x$get()
  m <- solve(nueva, ...)
  x$setinverse(m)
  m
}



