## Two functions, makeCacheMatrix and cacheSolve are designed to save
## computation time by cache the inverse of a square matrix

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix 
## If the inverse has already been calculated
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## given a square invertible matrix, return a list of functions:
## to set the matrix
## to get the matrix
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # <<- assignment operator assigns inputs to parent environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## given output of makeCacheMatrix(), 
## return inverse of original matrix x

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matx.data <- x$get()
  inv <- solve(matx.data, ...)
  x$setinv(inv)
  inv
}
