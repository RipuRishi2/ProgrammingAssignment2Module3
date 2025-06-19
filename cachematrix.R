## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property to NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y       # Assign new matrix to x in parent environment
    inv <<- NULL  # Reset the inverse cache when the matrix is reset
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of the above methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Try to get the cached inverse
  
  # If the inverse is already cached, return it with a message
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()            # Get the matrix from the object
  inv <- solve(data, ...)    # Compute the inverse using solve()
  x$setinverse(inv)          # Cache the inverse for future use
  inv                        # Return the inverse
}
