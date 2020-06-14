## Put comments here that give an overall description of what your
## functions do

## This function creates an object.
## The object stores the inverse of a matrix in a cache.

makeCacheMatrix <- function(z = matrix()) {
  m <- NULL 
  set <- function(x) {
     x <<- z
     m <<- NULL
  }
  
  get <- function() z
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the given matrix.
## If the solution already exists in the cache,
## it is retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <<- x$getInverse() 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m     
}
