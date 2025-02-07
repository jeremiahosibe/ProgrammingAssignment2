## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache for inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Store new matrix
    inv <<- NULL  # Reset cache (invalidate inverse)
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set (store) the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get (retrieve) the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is cached
  
  if (!is.null(inv)) {
    message("Getting cached inverse")  # Inform user
    return(inv)  # Return cached inverse
  }
  
  mat <- x$get()  # Get the original matrix
  
  if (nrow(mat) != ncol(mat)) {
    stop("Error: The matrix must be square to find its inverse.")
  }
  
  inv <- solve(mat)  # Compute inverse
  x$setInverse(inv)  # Cache the result
  
  return(inv) ## Return a matrix that is the inverse of 'x'
}
