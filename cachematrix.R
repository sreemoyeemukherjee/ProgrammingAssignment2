## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix() is a function with which we create the matrix and cacheSolve() is the function with which we get the inverse of the matrix.
## We check the cache memory if we have already computed the inverse. 
## If we have, then we just display the result with getinv() and display the message "getting cached data"
## Else, we first compute the inverse and set it using setinv() and then show the result


## Write a short comment describing this function

## This function helps in creation of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function

## This function helps in displaying the inverse of the matrix. If it has already been computed, then it just displays the result
## with a message "getting cached data", else it first computes (using solve()) and then displays the inverse. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}
