## Put comments here that give an overall description of what your
## functions do

## This function creates a makeCacheMatrix, a list of 4 methods together
## with a matrix x.  
## The 4 methods enable setting, getting, settingInverse and 
## gettingInverse of x.

## When the function is called a matrix as an argument,
## it remembers the value of the matrix and sets the inverse to NULL.
## When a request is made by cacheSolve below to access the inverse,
## cacheSolve checks whether x_inverse is NULL.  If it is NULL, the
## cacheSolve computes the matrix inverse using solve and calls the 
## setInverse function here to cache the computed result.

makeCacheMatrix <- function(x = matrix()) {
  
  x_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) x_inverse <<- inverse
  
  getInverse <- function() x_inverse
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## cacheSolve works on a matrix object of type cacheMatrix.
## It returns the inverse of its matrix.
## If the inverse has been computed already, it returns this cached
## result.
## If the inverse has not been cached, it computes the inverse using solve
## and then calls the setInverse method of makeCacheMatrix in order to 
## cache the matrix inverse for the next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  m <- solve(data)
  
  x$setInverse(m)
  m
  
  
}
