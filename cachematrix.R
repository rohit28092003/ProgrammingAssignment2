makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the cached inverse when the matrix changes
    }
    
    # Function to get the value of the matrix
    get <- function() x
    
    # Function to set the value of the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the cached inverse
    getInverse <- function() inv
    
    # Return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" created by makeCacheMatrix.
# If the inverse has already been calculated, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse
    inv <- x$getInverse()
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix
    mat <- x$get()
    
    # Compute the inverse
    inv <- solve(mat, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}
