## The function makeCacheMatrix creates a special "matrix" object
## that is a list containing functions to set and get the value of 
## the matrix and functions to set and get the value of the inverse

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(Y) {
    M <<- Y
    I <<- NULL
  }
  get <- function() M
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve computes the inverse of the matrix
## provided. In order to do so, it makes use of the "matrix" 
## returned by the function makeCacheMatrix above. If this inverse
## has already been calculated previously and can be found in the 
## cache, the function cacheSolve retrieves it from the cache.

## Note: we assume that the matrix is always invertible.

cacheSolve <- function(M, ...) {
  ## Return a matrix that is the inverse of 'M'
  I <- M$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- M$get()
  I <- solve(data, ...)
  M$setinv(I)
  I
}
