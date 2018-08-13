# This is a solution to Programming Assignment 2: Lexical Scoping from the 
# Coursera R Programming Course

# The code used in this solution is based on the makeVector and CacheMean 
# functions provided as an example of a caching function. Assuming that they are
# essentially performing the same process the code below is similar with the 
# mean() calculation replace with source(), which was given as the required 
# calculation in the intstructions

# makeCacheMatrix: This function creates a special "matrix" object that 
# can cache its inverse.

# First change is to make x = matrix() as opposed to numeric
makeCacheMatrix <- function(x = matrix()) { 
  # m changed to i to indicate inverse. m changed to i throughout
  i = NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  #mean replaced with inverse
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i = x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  #calculation changed here from mean to solve, which will calculate the 
  # inverse
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
  }
