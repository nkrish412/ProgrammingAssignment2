## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function defines functions to store and retrieve a matrix and its inverse.
#It is done by using the get and set functions

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }  
  



## Write a short comment describing this function
##The cacheSolve function accepts a matrix as an input, and returns the inverse from cache if present 
## otherwise it calculates the inverse and stores in into the cache. It the matrix is not invertable a
## an appropriate message is displayed

cacheSolve  <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()

  m <- try(solve(data),silent=T)

  if (class(m)=="matrix") 
{
  message("adding to cache")
  x$setinv(m)
  m
}
  else message("Input Matrix cannot be inverted")
}