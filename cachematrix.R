## makeCacheMatrix and cacheSolve functions 
## which help decrasing
## computation time of finding inverse of
## square matrix which content doesn't change

## creates cacheable matrix object from givex x squared matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## calculates inverse matrix x created using makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

