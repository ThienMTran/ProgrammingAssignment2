## The functions cache the inverse of a matrix.

## This function creates a special 'matrix' onject that can
## store a matrix and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special 'matrix' returned by the
## makeCacheMatrix function. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}