## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a list of functions needed to produce a new inverse or hold the values outside the environment. 
# A square matrix must be used with the function.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

# Pulls the matrix data cached in the prior function. If a the inverse doesn't exist in the environment
# it will compute the inverse by utilizing the solve function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
