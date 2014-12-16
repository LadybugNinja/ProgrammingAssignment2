## cacheMatrix.R
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (via the solve function). 
## Here, we write a pair of functions that cache the inverse of a matrix. 



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      # Clear cached inverse matrix
      m <- NULL
      
      # Set new matrix x, will also clear cached inverse matrix
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      # Retrieve the matrix x
      get <- function() {
            x
      }
      
      # Set the inverse matrix, this function is used by the cacheSolve function
      setInv <- function(solve) {
            m <<- solve
      }
      
      # Retrieve the cached inverse matrix
      getInv <- function() {
            m
      }
      
      # List of internal metods
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
      # Check to see if the inverse has been calculated
      m <- x$getInv()
      
      # If the inverse matrix is not NULL, retrieve and return it
      if(!is.null(m)) {
            message("getting cached data")
            return(m) # The function exits here
      }
      
      # If the inverse matrix is NULL, retrieve the original matrix
      data <- x$get()
      
      # Solve the inverse matrix
      m <- solve(data, ...)
      
      # Store the inverse
      x$setInv(m)
      
      # Return the inverse matrix
      m
}
