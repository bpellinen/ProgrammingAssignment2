## These functions demonstrate scoping rules and utility by demonstrating how
## to cache time-consuming computations, in this case the inverse of a matrix.

## 'makeCacheMatrix' creates a "matrix" object that will cache the inverse 
## of the given matrix value

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


## 'cacheSolve' computes the inverse of the matrix originally given to the
## makeCacheMatrix above and caches the result. If called more than once,
## the function will return the previously computed value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    # message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
