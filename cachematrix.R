## The two functions makeCacheMatrix and cacheSolve together are used to calculate
## the inverse of a given input matrix and store it in cache and retrieve it if needed

## This function, makeCacheMatrix creates a special "matrix", which is a list of functions to
## set the value of the input matrix
## get the value of the input matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Sets the result to null everytime makeCacheMatrix is called
  result <- NULL
  
  ## This function can be invoked to directly set an input matrix instead of passing 
  ## the matrix as an argument to makeCacheMatrix function
  set <- function(y) {
    x <<- y
    result <<- NULL
  }
  
  ## Get the input matrix
  get <- function() x
  
  ## Store the result or inverse of the input matrix in cache
  setInverse <- function(inversematrix) result <<- inversematrix
  
  ## Retrieve the cached result
  getInverse <- function() result
  
  ## Making a list of the functions within makeCacheMatrix function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of a "special" matrix created with the help
## of makeCacheMatrix. It looks for the inverse in the cache and if it's not found
## it determines the inverse using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  result <- x$getInverse()
  
  ## Check if the inverse is already calculated and if so get the value from cache
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  ## Get the matrix whose inverse is to be determined and use the solve function 
  data <- x$get()
  result <- solve(data, ...)
  
  ## Store the result in cache and return it
  x$setInverse(result)
  result
}
