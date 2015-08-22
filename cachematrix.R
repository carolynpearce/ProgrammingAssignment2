## These functions create a cache matrix for the inverse of the supplied matrix
## and generates the cache inverse matrix

## This function creates a cache matrix for the supplied matrix and provides 
## getters and setters for getting and setting the supplied matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    
}


## This function returns the inverse of a matrix by either getting the cache matrix
## or computing the inverse and setting the cache matrix (if it does not exist)

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
