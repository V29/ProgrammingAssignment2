## Put comments here that give an overall description of what your
## functions do

## This function creates a special Matrix Object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) { ##set is a function that changes the matrix stored 
                       ##in the main function. We don't need to use this function 
                       ##unless we want to change the matrix.
    x <<- y
    i <<- NULL ##Restores to null the value of the inverse i, because the old 
               ##inverse of the old matrix is not needed anymore. The new inverse
               ##needs to be recalculated through the function cacheSolve.
  }
  get <- function() x ##get is a function that returns the matrix x stored 
                      ##in the main function. It doesn't require any input.
  
  ## setinverse and getinverse are functions very similar to set and get. 
  ##They don't calculate the inverse, they simply store the value of the input 
  ##in a variable i into the main function makeCacheMatrix (setinverse) and 
  ##return it (getinverse).
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse() ##The first thing cachemean does is to verify the value i, 
                      ##stored previously with getinverse, exists and is not NULL. 
                      ##If it exists in memory, it simply returns a message and 
                      ##the value i.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()  ## Gets the matrix stored with makeCacheMatrix, i calculates 
                   ##the inverse of the matrix and x$setinverse(i) stores it in 
                   ##the object generated assigned with makeCacheMatrix. 
  message("solving inverse")
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}