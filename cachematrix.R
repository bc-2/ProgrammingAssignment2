## Functions to compute the inverse of a matrix and cache the result for later use.

## Creates a matrix that can also store its inverse based on an existing matrix.

  # Declare a variable to contain the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # A function to set the matrix value and reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # A function to retrieve the matrix value
  get <- function() x
  # A function to set the value of the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  # A function to get the value of the inverse matrix
  getinverse <- function() inverse
  # Return value is a list of the functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of a given matrix as defined above. If the inverse has already been computed,
## reuse the last computed value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the stored value of the inverse
  inverse <- x$getinverse()
  # If the stored value is available, return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # Stored value for inverse is not available: compute inverse, store for future use and return it.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
