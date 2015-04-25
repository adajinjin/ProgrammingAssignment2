## This script created a method to save the mean of a matrix in the cache
## and then pull this value out if neccessary. 

## The function "makeCacheMatrix" created a special matrix x that composes 
## of a series of functions that 
## 1. set the matrix
## 2. get the matrix
## 3. set the mean of the matrix
## 4. get the mean of the matrix

## Write a short comment describing this function
## This is a function mirrored the function of makeVector. Instead of getting the mean of a vector,
## this function was able to get the inverse of a matrix. However, the inner principle keeps
## the same, which is obtaining the inverse of the matrix only if it is checked that the inverse 
## doesn't exist already.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" return a matrix that is the inverse of x.
## If the inverse of a matrix has already existed, it gets the inverse
## of the matrix from the cache and skips the computation steps.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
