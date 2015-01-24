## These two functions allow for creating a matrix object which contains a cace
## as well as a function which solves for the inverse using the cached matrix object
##
## sample usage:
##
## mat <- matrix (runif(16), nrow=4, ncol=4)
## y <- makeCacheMatrix(mat)
## inv <- cacheSolve(y)
##

## takes a matrix object as an input and returns a list containing function pointers
## that allow for get/set operations on the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the matrix inverse
  inverse <- NULL
  
  # set the matrix to a new value; reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the matrix value
  get <- function() x
  
  # set the inverse
  setInverse <- function(i) inverse <<- i
  
  # get the inverse
  getInverse <- function() inverse
  
  # return pointers to all four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of argument 'x' where 'x' is a cached matrix object
## returned by a call makeCacheMatrix()
cacheSolve <- function(x, ...) {  
  ## if a cached value exists, return it
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## otherwise, get the original matrix
  data <- x$get()
  
  ## calculate the inverse
  inverse <- solve(data, ...)
  
  ## set the inverse in the cache
  x$setInverse(inverse)
  
  ## and return the inverse to the caller
  inverse
}
