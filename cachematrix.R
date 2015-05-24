## Put comments here that give an overall description of what your
## functions do

## Make an object representing Matrix that caches inverse of itself
makeCacheMatrix <- function(x = matrix()) {
  ## inverted matrix, NULL if not set
  inverted <- NULL 
  ## original matrix setter
  set <- function(y) {
    x <<- y
    inverted <<- NULL
  }
  ## original matrix getter
  get <- function() x
  ## inverted matrix setter
  setsolve <- function(value) inverted <<- value
  ## inverted matrix getter
  getsolve <- function() inverted
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverted <- x$getsolve()
  ## check if inverted matrix cached
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  }
  data <- x$get()
  ## calculate inverted matrix
  inverted <- solve(data, ...)
  ## cache inverted matrix
  x$setsolve(inverted)
  inverted
}
