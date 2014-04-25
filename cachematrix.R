## These functions allow a matrix inverse to be cached alongside the matrix so
## it can be recalled instead of calculated repeatedly

## makeCacheMatrix creates a list of functions that enable the cacheSolve function
## to get the cached value of the inverse (if available) or calculate and cache

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) xInv <<- solve
  getsolve <- function() xInv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve checks to see if there is a cached value for the inverse - if so it
## gets it, otherwise it instructs makeCacheMatrix to calculate and cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getsolve()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setsolve(xInv)
  xInv
}
