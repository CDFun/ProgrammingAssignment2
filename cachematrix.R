## Put comments here that give an overall description of what your
## functions do
# Below is a pair of functions that cache the inverse of a matrix.  Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Write a short comment describing this function
# The makeCacheMatrix function computes a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialise inverse matrix
  inv <- NULL

  # Set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # Get value of the matrix
  get <- function() x

  # Set value of the inverse
  setinverse <- function(inverse) inv <<- inverse

  # Get value of the inverse
  getinverse <- function() inv

  # List the four subfunctions within this function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Get inverse of matrix
  inv <- x$getinverse()

  # Check if a cached version is available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Get matrix and assign to local variable "data"
  data <- x$get()

  # Calculate inverse of "data"
  inv <- solve(data, ...)

  # Set inverse of matrix
  x$setinverse(inv)

  # Return inverse of matrix as output of the function
  inv
}
