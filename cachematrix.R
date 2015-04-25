makeCacheMatrix <- function(x = matrix()) {
  # Allows for the caching of a matrix and its inverse
  #
  # Args:
  #   x: The matrix that will cached
  #
  # Returns:
  #   a list of functions defined below:
  #   set: setter for original matrix
  #   get: getter for cached matrix
  #   setinverse: setter for inverted matrix
  #   getinverse: getter for cached inverted matrix, may be NULL
  #
  
  # Set NULL default for inverse prior to caching
  inverse <- NULL
  
  # Define getter/setter for original matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  # Define getter/setter for inverse matrix
  setinverse <- function(inv) { inverse <<- inv }
  getinverse <- function() { inverse }
  
  # Wire up the getters/setters for function
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Performs the inversion of a matrix, using cache if available
  #
  # Args:
  #   x: The matrix that will be inverted
  #
  # Returns:
  #   The input matrix x inverted
  #

  # Check for cached inverse
  inverse = x$getinverse()
  
  # If already solved, return without recalculation
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  # Only will arrive here if the cache isn't populated
  unsolved = x$get()
  
  # Compute inverse and set in cache
  inverse = solve(unsolved, ...)
  x$setinverse(inverse)
  
  # Return the inverted matrix solution
  inverse
}
