## Author: Molla Hunegnaw Asmare
## Date: 27 April 2014
## The script tries to use cashed value of inversed Matrix to reduce cost of computation and avoids 
##    repeated matrix inverstion. 
## There  are two functions in the script:
##    1. makeCacheMatrix creates a special "matrix" object that can cache its inverse
##    2. cacheSolve: This function computes the inverse of the special "matrix" returned 
##       by makeCacheMatrix above. If the inverse has already been calculated 
##       (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

##  makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # inversmx will store the cached inverse matrix
  inversmx <- NULL
  
  # Set and get the matrix
  set <- function(y) 
    {
      x <<- y
      inversmx <<- NULL
    }
  get <- function() x
  
  # Set and get the inverse
  setinversmx <- function(solve) inversmx <<- solve
  getinversmx <- function() inversmx
  
  # Return the matrix 
  list(set = set, get = get, setinversmx = setinversmx, getinversmx = getinversmx)
  
}


## CasheSolve returens an inversed matirx if cache exists or 
##      compute inverse and return the inversed matrix

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  inversmx <- x$getinversmx()
  
  # If already cached, return it
  if (!is.null(inversmx)) 
    {
        return(inversmx)
    }
  
  ## inverse it
  data <- x$get()
  inversmx <- solve(data, ...)
  x$setinversmx(inversmx)
  
  return(inversmx)
  
}
