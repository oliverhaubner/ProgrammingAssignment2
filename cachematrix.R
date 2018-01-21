## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The following 2 functions() will compute the inverse of a
## matrix, and store it. Subsequent access to the inverse will be simply
## returned from the cached result.

## the function makeCachematrix() creates a special "matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # setter function of the matrix itself
  set <- function(y) {
    x <<- y
    i <<- NULL
}
  # getter function of the matrix itself
  get <- function() x
  
  # setter function for the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  # getter function for the inverse of the matrix
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## the function cacheSolve() computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not been changed), then the cachesolve
## simply retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  ## if the inverse if already cached, just return it
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## otherwise, compute the inverse then store it
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
