## Computing the inverse of a square matrix
## with caching enabled. 

## makeCacheMatrix creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve retrives the inverse of the matrix if it has already been computed
## otherwise it will call solve method to compute the inverse of matrix, save it in the cache
## and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  # solve computes the inverse of the matrix
  inv <- solve(data)
  x$setInverse(inv)
  inv
}


# Instructions to execute the program
# Create a square matrix Ex: c=matrix(c(100,20,30,80,30,30, 70,30,10), nrow=3, ncol=3)
# and then call 
# cacheMatrix <- makeCacheMatrix(c)
# cacheSolve(cacheMatrix)
#or
#cacheSolve(makeCacheMatrix(c))
