## Use makeCacheMatrix and cacheSolve to get the inverse of a matrix. 
## makeCacheMatrix returns a list with functions that can be passed as input to cacheSolve. 
## cacheSolve calculates the inverse using 'solve' if not already calculated and caches it. 
## If it is already found in the cache, it returns the value.

## makeCacheMatrix returns a list with functions that can be passed as input to cacheSolve. 
## Input:
##    x = matrix  (Default: x = matrix() )
##
## Output: List with handles to the following functions
##    set: to set the matrix
##    get: to get the matrix
##    setInverse: Set the inverse of the matrix
##    getInverse: Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(minv) m <<- minv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve takes the list returned by cacheMatrix as input and returns the inverse if found in the cache with the message, "getting cached data". 
## If not, it calculates the inverse using SOLVE, caches it and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}