## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 # Initialize the inverse property to NULL
  inv <- NULL  
  
  # Method to set the matrix
  set <- function(y) {

    # Assign new matrix to x in parent environment
    x <<- y       
    
    # Reset the inverse cache when the matrix is reset
    inv <<- NULL  
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

# cacheSolve: Computes the inverse of the square "matrix" ONLY returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then it retrieves the inverse from the cache.
# If the input matrix is NOT square, error message is provided.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Try to get the cached inverse
  
  # If the inverse is already cached, return it with a message
  if (!is.null(inv)) {
    message("Please wait ... fetching cached data")
    return(inv)
  }
  
  # Section for computing the inverse

  # Get the matrix from the object
  data <- x$get()            
  
  # Checking if the input matrix is square before attempting inversion
  if (nrow(data) != ncol(data)) {
    stop("Cannot invertas the input Matrix is not square.")
  }  

  # Compute the inverse using solve()
  inv <- solve(data, ...)    

  # Cache the inverse for future use
  x$setinverse(inv)          

  # Return the inverse
  inv                        
}
