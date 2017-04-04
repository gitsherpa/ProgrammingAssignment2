## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Closure of matrix variable in function as cached variable
  # initialization of values
  m <- NULL

  # Setting a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # Getting a matrix
  get <- function() x

  # Setting inverse matrix
  setinverse <- function(inv) m <<- inv

  # Getting inverse matrix
  getinverse <- function() m

  # list of values as result of function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Input x <-makeCacheMatrix(matrix)
  ## Return a matrix that is the inverse of 'x'
  # Time to inverse matrix matrix(c(1,21,31,2,22,23,3,23,33), nrow=3)
  # as cached matrix is 0.01299596 secs versus uncached with 0.003558874 secs

  m <- x$getinverse()

  # Looking for cached matrix...
  if(!is.null(m)) {
    # Simply return the computed inverse
    message("Getting cached matrix")
    return(m)
  }
  # Getting cached matrix
  data <- x$get()

  # Inverse of matrix
  m <- solve(data, ...)

  # Caching of inversed matrix
  x$setinverse(m)

  # Return of inversed matrix
  m
}
