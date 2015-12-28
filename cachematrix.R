## Put comments here that give an overall description of what your
## functions do
#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than computing it
#repeatedly

## Write a short comment describing this function
# 
# #The first function, `makeVector` creates a special "vector", which is
# really a list containing a function to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinv <<- inverse
  getinverse <- function() matinv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  matinv <- x$getinverse()
  if(!is.null(matinv)) {
    message("getting cached data.")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data)
  x$setinverse(matinv)
  matinv
}
