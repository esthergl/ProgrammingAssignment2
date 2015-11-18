## Caching the Inverse of a Matrix

## makeCacheMatrix creates a list containing four functions.
## The argument of the function is a matrix.

makeCacheMatrix <- function(x = matrix()) {

	  # Initially the cache value is missing
        m <- NULL
	  # set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
	  # get the matrix
        get <- function() x
	  # set the inverse matrix (cache value is the inverse)
        setsolve <- function(solve) m <<- solve
	  # get the inverse matrix
        getsolve <- function() m
	  # list of elements that you obtain with this function
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The function cacheSolve calculates the inverse of the matrix created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  ## Check if it's a square matrix and return "Error" if not
        if(!isSymmetric(x$get())) return("Error: No square matrix")

	  # get the inverse matrix (the cache value)
        inv <- x$getsolve()

	  # check if it's available the inverse matrix and return the inverse matrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # If it's not available:
        # get the matrix
        data <- x$get()
        # obtain the inverse matrix
        inv <- solve(data, ...)
        # set the inverse matrix in the cache
        x$setsolve(inv)
        # return the inverse matrix
        inv
}
