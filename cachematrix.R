# makeCacheMatrix is a function that creates a special "MATRIX" object
# The Inverse of the created special "MATRIX" is computed using cacheSolve
 
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  s <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setInverse <- function(INV) n <<- INV
  getInverse <- function() n
  list(set = s,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
 
 
## This function computes the inverse of the special "matrix" ( the one returned by makeCacheMatrix).
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverso <- x$getInverse()
  mat <- x$get()
  inverso <- solve(mat, ...)
  x$setInverse(inverso)
  inverso
}
