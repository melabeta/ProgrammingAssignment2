## makeCacheMatrix is creating a "matrix object" that can cache its inverse
## Calling cacheSolve on such a previously calculated "matrix object" will calculate its inverse
## If the inverse matrix has already been calculated and makeCacheMatrix contains a cache, cacheSolve will simply retrieve the cache

## First, makeCacheMatrix initialises x (as an empty matrix) and s.
## The set function will assign the input to x, which will be available also in the parent environment
## and clears any previously cached value of s, also for the parent environment
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## further, functions are defined that will be stored as named items in a list,
  ## which can be called upon in another function of the parent environment.
  ## naming values in list() will allow for accessing the functions by the $ operator
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve will return a matrix that is the inverse of 'x'
## while the input arguments needs to be of the type of makeCacheMatrix(), or else x$getinv and x$get are invalid
## If s is not empty but has been calculated and cached before, cacheSolve will return the cached data
## If s is empty, cacheSolve will calculate the inverse of the matrix in the input makeCacheMatrix() list 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
