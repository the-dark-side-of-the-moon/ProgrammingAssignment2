## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix initialize a cacheMatrix object from a matrix.
# cacheSolve can solve a matrix and cache the result so next time the same matrix is solved it returns the cached value
# Usage:
# 1. Intialize a matrix m <- matrix(c(-1, 1.5, 1, -1), 2, 2)
# 2. Initialize a chaceMatrix cacheM <- makeCacheMatrix(m)
# 3. To get inverse of the matrix use cacheSolve(cacheM)
# 4. If needed, change matrix to be solved with cacheM$set(newMatrix)
# 5. Get inverse of newMatrix with cacheSolve(cacheM)

## Write a short comment describing this function
## Takes x (a matrix) and initialize a cachedMatrix
## Return a list of functions (get, set, getInverted, setInverted) that can be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverted <- function(inverted) i <<- inverted
  getInverted <- function() i
  list(set = set,
       get = get,
       setInverted = setInverted,
       getInverted = getInverted
       )
}


## Write a short comment describing this function
#Check if there is a cached version of inverted matrix and returns it, if not calculate, store and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverted()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverted(i)
  i
}
