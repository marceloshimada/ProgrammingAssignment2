## Put comments here that give an overall description of what your
## functions do

## This function returns an entity that represents a matrix
## with 4 related functions: set, get, setInverse and getInverse. 
## Its inverse matrix can be stored in cache

makeCacheMatrix <- function(x = matrix()) {
  #internal value
  inv <- NULL
  set <- function(y) {
    x <<- y
    #reset internal value
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the matrix. 
## If the inverse value was stored before, it returns the cached value. 
## Otherwise, calculates the inverse, stores the value and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data: inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  # store the calculated inverse
  x$setInverse(inv)  
  inv
}

