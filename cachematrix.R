## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here).
## The functions are cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse<- function(inverse) invX <<-inverse
  getInverse <- function() invX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invX <- x$getInverse()
  if (!is.null(invX)) {
    message("Getting cached inverse matrix!")
    return(invX)
  } else {
    invX <- solve(x$get())
    x$setInverse(invX)
    return(invX)
  }
}
