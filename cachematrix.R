Matrix inversion is usually a costly computation and there may be some benefit
to caching the inverse of a matrix rather than compute 
it repeatedly. The functions here are to cache the inverse of a matrix.

The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
set the value of the matrix
get the value of the matrix
set the value of the inversion
get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}

This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
If the inverse has already been calculated (and the matrix has not changed), 
then the cacheSolve should retrieve the inverse from the cache and skips the computation.
Otherwise it gets the inverse of the matrix and sets the inverse in the cache via
the setSolve function

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m ## Return a matrix that is the inverse of 'x'
}

