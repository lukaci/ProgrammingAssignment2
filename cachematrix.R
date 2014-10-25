## cachematrix.R
##
## functions in this file implements an inverse-matrix caching system, used to speed-up calculations
## when accessing multiple times the inverse of a given matrix.
##
## in order to benefit from the caching system, first you have to transform a matrix into a 'special'
## matrix (we name it CacheMatrix), calling the function 'makeCacheMatrix'
##
## the inverse of a CacheMatrix instance is obtained by calling 'cacheSolve' function.
##
## the call sequence to obtain the inverse of a matrix x is, for example:
##
## x <- matrix(1:4,2,2)
## y <- makeCacheMatrix(x) 
## inverse_x <- cacheSolve(y)
##
## first time call to cacheSolve(y) will calculate the inverse of x (using the 'solve' function) and caches 
## it in the CacheMatrix instance 
##
## consecutive calls to cacheSolve(y) will result in a simple access to the cached version of inverse matrix


## makeCacheMatrix(x)
## 
## x: matrix
## returns: list of functions
##
## this function create a 'special matrix' (or CacheMatrix) instance, starting from a given matrix x.
## this 'special matrix' is simply a container of functions (a list) used to store and access data related
## to the matrix: the inverse matrix.
##
## functions in the result list are:
##  - get(): returns the original matrix
##  - set(v): change the original matrix assigning it to 'v', and resets the cached inverse matrix
##  - getInverse(): returns the currently cached inverse matrix
##  - setInverse(v): sets the currently cached inverse matrix to 'v'

makeCacheMatrix <- function(x = matrix(), i = NULL) {
  set <- function(newx) {
    i <<- NULL
    x <<- newx
  }
  get <- function() x
  
  setInverse <- function(newi) {
    i <<- newi
  }
  getInverse <- function() i
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(x, ...)
## 
## x: 'special matrix' created using makeCacheMatrix function
## returns: the inverse of the matrix currently hosted in the 'special matrix'
##
## this functiovn is used to calculate the inverse of a matrix, hosted in the 'special matrix' x.
## if x doesn't contain a cached version of the inverse matrix, then the inverse of the matrix is calculated
## and set into the 'special matrix' instance
##
## if x contains a cached version of the inverse matrix, the cached value is returned

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(is.null(i)) {
    message("calculating inverse")
    i <- solve(x$get(), ...)
    x$setInverse(i)
  }
  
  i
}
