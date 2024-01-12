Put comments here that give an overall description of what your
## functions do

## Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the matrix and inverse as NULL
  mat <- y
  inv <- NULL
  
  # Function to set the matrix
  setMatrix <- function(matrix) {
    # Set the matrix
    mat <<- matrix
    # Reset the cached inverse
    inv <<- NULL
  }
  
  # Function to get the matrix
  getMatrix <- function() mat
  
  # Function to get the cached inverse or compute it if not cached
  getInverse <- function() {
    if (!is.null(inv)) {
      # If the inverse is already cached, return it
      message("Getting cached inverse")
      return(inv)
    } else {
      # If the inverse is not cached, compute it and cache it
      message("Computing and caching inverse")
      inv <<- solve(mat)
      return(inv)
    }
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}

## Function to compute the inverse of a matrix using caching
cacheSolve <- function(y, ...) {
  # Get the matrix from the cache matrix object
  mat <- y$getMatrix()
  
  # Get the cached inverse or compute it if not cached
  inv <- y$getInverse()
  
  # Return the computed inverse
  return(inv)
}
