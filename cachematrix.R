## There are 2 functions in this file. The purpose is to cache the
## inversion of a matrix to speed up inversion calculation. 

## The first function creates a list containing a function to set the value
## of a matrix, get the value of that matrix, set the inversion of that matrix,
## and get the inversion of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function creates the Inversion of above function. 
## Before doing so, it first checks to see if the inverstion has already been done. 
## If that is the case, it gets the inversion from the cache (no need to compute) 
## If not, it computes the inversion and puts it into the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}
