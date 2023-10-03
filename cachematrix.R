## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##define the argument 
  inv <- NULL  ## initialise inv as NULL
  set <- function (y){ ## define the set function
    x <<- y ##value of matrix in parent envieronment
    inv <<- NULL
  }
  get <- function() x ##returns the value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse ##assigns value of inv
  getinverse <- function() inv ##gets the value of inv
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheinverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
