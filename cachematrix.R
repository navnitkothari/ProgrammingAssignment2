## Here I am writing a function to cache the Inverse of a Matrix.
## If we need the same inverse again, then cache will help make the process faster.

## This function creates a matrix object that can cache its inverse.
## It contains 4 functions. The set and get functions are used for setting and getting the values.
## The setinverse and getinverse functions are used for setting ans getting the inverse.
  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) inverse <<- matrixinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by above function.
## But it first checks whether the inverse has already been computed in cache.
## If so, it gets the inverse from cache else the function calculates it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
