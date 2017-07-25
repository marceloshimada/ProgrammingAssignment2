## Put comments here that give an overall description of what your
## functions do

## This function returns an entity that represents a matrix
## with 4 related functions: set, get, setInverse and getInverse. 
## Its inverse matrix can be stored in cache

makeCacheMatrix <- function(x = matrix()) {
  #internal value
  inv <- NULL
  # sets the matrix value and resets the internal value
  set <- function(y) {
    x <<- y
    #reset internal value
    inv <<- NULL
  }
  # retrieves the matrix value
  get <- function() x
  # sets the inverse matrix (internal value)
  setInverse <- function(inverse) inv <<- inverse
  # gets the inverse matrix (internal value)
  getInverse <- function() inv
  # returns a list with 4 functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the matrix. 
## If the inverse value was stored before, it returns the cached value. 
## Otherwise, calculates the inverse, stores the value and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # try to get the inverse matrix 
  inv <- x$getInverse()
  # check if the inverse matrix is not null (available)
  if(!is.null(inv)) {
	# using cached value
    message("getting cached data: inverse")
    return(inv)
  }
  # cached value not available
  # get matrix
  data <- x$get()
  # calculate inverse of matrix
  inv <- solve(data, ...)
  # store the calculated inverse
  x$setInverse(inv)  
  # return calculated value
  inv
}

