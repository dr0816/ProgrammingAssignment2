#The first function, makeCacheMatrix creates a special "matrix" object, which can cache its inverse

##set the value of the matrix
##get the value of the matrix 
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The second function cacheSolve Returns a matrix that is the inverse of 'x'
## X should be square invertible matrix, then solve(X) returns its inverse.

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache


cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if(!is.null(inv)) {
    msessage("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
