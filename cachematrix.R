## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## replace with a new matrix
  set <- function (y) {
    x <<- y
    m <<- NULL
    
  }

  ## return the current matrix
  get <- function() x
  
  ## cache the inverse in this environment
  setinv <- function(inv) m <<- inv
  
  ## return the cached inverse value
  getinv <- function() m
  
  list(set=set, get=get, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get the current cached inverse matrix
  i <- x$getinv()
  
  ## checks if the cached inverse matrix has been calculated, returns the value only if it has been calculated
  if (!is.null(i)) {
    return(i)
  }
  
  ## get the stored matrix
  m <- x$get()
  
  ## calculates the inverse matrix
  i <- solve(m, ...)
  
  ## Store the newly calculated inverse matrix
  x$setinv(i)
  
  ## returns the newly calculated inverse matrix
  i
}
