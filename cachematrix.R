## Put comments here that give an overall description of what your
## functions do
#Matrix inversion is usually a costly computation and there may be some
#benefit to caching the inverse of a matrix rather than computing it
#repeatedly

## Write a short comment describing this function
# 
# #The first function, `makeCacheMatrix` creates a special "matrix" that can cache its inverse, which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix' inverse
# 4.  get the value of the matrix' inverse

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
# 
#  The second function, "Cachesolve" computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` retrieves the inverse from the cache. The solve function computes the inverse of a square matrix

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
