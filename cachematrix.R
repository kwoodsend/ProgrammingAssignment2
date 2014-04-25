# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.
# In this set of functions, we invert the matrix once if needed,
# and put the result in a cache so repeated calls are less costly.

# This function creates a special list of functions held in a vector.
# These functions provide set and get functions for the original matrix x,
# and also setinv and getinv for setting and getting the inverse of x.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# cacheSolve calculates the inverse of the special "vector"
# created with the above function. It first checks to see if the
# inverse has already been calculated. If so, it retrieves the inverse matrix
# from the cache and skips the computation. 
# Otherwise, it calculates the inverse of
# the matrix and sets the value in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv() ## get the inverse if it is in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## get the original matrix
  m <- solve(data, ...) ## invert it
  x$setinv(m)
  m
}
