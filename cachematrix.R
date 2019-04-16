## Matrix inversion is usually a costly computation and there may be some benefit
#  to caching the inverse of a matrix rather than compute it repeatedly.  The
#  following pair of functions cache the inverse of a matrix and provide a means 
#  to called the cached inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Return a matrix that is the inverse of 'x'
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solve) i <<- solve
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
} ##/makeCacheMatrix


## This function computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix above. If the inverse has already been calculated (and the
#  matrix has not changed), then cacheSolve should retrieve the inverse from the
#  cache.
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
} ##/cacheSolve

