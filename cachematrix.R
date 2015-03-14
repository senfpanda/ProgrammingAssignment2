## compute the inverse matrix of a given matrix at the first time.
## If the given matrix is not changed, just return the cached inverse matrix,
##  otherwise, re-compute it and return.

## Make a special matrix with cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  # the inverse matrix
  mtrxInv <- NULL
  # methods to access the special matrix
  set <- function(y) {
    x <<- y
    mtrxInv <<- NULL
  }
  get <- function() x
  setinv <- function(m) mtrxInv <<- m
  getinv <- function() mtrxInv
  # return the methods' list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the cached inverse matrix, or re-compute it and return.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  # if the origin matrix is not changed, just return the cached data
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise, re-compute it
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
