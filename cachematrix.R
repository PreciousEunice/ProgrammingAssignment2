## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, `makeCacheMatrix` creates the following 

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of a square matrix
## 4.  get the value of the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function solves the inverse of a square matrix.
## If the inverse has already been calculated, 
## then cacheSolve should retrieve the stored inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
