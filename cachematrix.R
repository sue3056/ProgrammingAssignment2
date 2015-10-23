## This function creates a special "matrix" object that can cache its inverse.
## This is efficient because matrix inversion is usually a costly computation.

## It involves two functions, the first one is for the inverse calculation.
## The second function does the cache.

## The first function, makeCacheMatrix creates "matrix = x" 


makeCacheMatrix <- function(x = matrix()) {
  
  ## return: a list used as the input to cacheSolve() containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  
  invm = NULL
  
  set = function (y) {
    # <<- used to asign a value to an object of different environment
    x <<- y
    invm <<- NULL
  }
  
  get = function()x
  
  setinvm = function(inverse) invm <<- inverse
  
  getinvm = function() invm
  list(set=set, get=get, setinvm=setinvm, getinvm=getinvm)
  
}


## takes output 'x' from makeCacheMatrix()
## returns the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Returns the inverse of 'x' from makeCacheMatrix
  
  invm=x$getinvm()
  
  ## with the inversed calculated
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  
  ##without the inversed calculated
  m.data=x$get()
  invm=solve(m.data, ...)
  
  ## set inverse in the cache
  x$setinvm(invm)
  
  return(invm)
}






## makeVector <- function(x = numeric()) {
##   m <- NULL
## set <- function(y) {
##  x <<- y
##  m <<- NULL
## }
## get <- function() x
## setmean <- function(mean) m <<- mean
## getmean <- function() m
## list(set = set, get = get,
##     setmean = setmean,
##     getmean = getmean)
## }

## cachemean <- function(x, ...) {
## m <- x$getmean()
## if(!is.null(m)) {
##  message("getting cached data")
##  return(m)
## }
## data <- x$get()
## m <- mean(data, ...)
##   x$setmean(m)
## m
## }
