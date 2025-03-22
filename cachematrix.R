## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache when a new matrix is assigned
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get cached inverse if available
  if (!is.null(inv)) {
    message("getting cached data")  # Display message if using cache
    return(inv)
  }
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}
