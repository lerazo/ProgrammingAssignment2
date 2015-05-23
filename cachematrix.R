## This functions checks if the inverted matrix was already
## calculated and the inverted matrix is saved. If it was calculated
## returns the cached value, if not it calculates it, returns it and
## saves it in cache.

## makeCacheMatrix creates a vector containing functions to set and
## get the value of a matrix, and set and get the value of the 
##inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverted matrix of the matrix created with
## makeCacheMatrix only if it is not saved in cache. 
## First, it checks if it was already calculated. If not, 
## it calculates the inverted matrix and sets the value in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
## Return a matrix that is the inverse of 'x' called inv
}
