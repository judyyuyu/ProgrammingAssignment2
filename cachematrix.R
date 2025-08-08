## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # cache for inverse
  
  set <- function(y) {
    x <<- y       # update matrix
    inv <<- NULL  # reset cached inverse
  }
  
  get <- function() x  # return the matrix
  
  setinverse <- function(inverse) inv <<- inverse  # cache the inverse
  
  getinverse <- function() inv  # return the cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # compute inverse
  x$setinverse(inv)       # cache it
  inv
}
