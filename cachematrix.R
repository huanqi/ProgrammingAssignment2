## cachematrix.R contains two functions to calculate the inversiton of a matrix
## If the inversion of the matrix is precalculated, we can quickly obtain the calculated inversion
## If the inversion of the matrix is not precalculated, we can calculate the inversion and store the value
## I use "solve" command to compute the inversion of a matrix

## The makeCacheMatrix creates a special "vector", which is a list containing a function to
## 1, set the value of the matrix
## 2, get the value of the matrix
## 3, set the value of the inversion of the matrix
## 4, get the value of the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inversion of the special "matrix" created with the makeCacheMatrix
## First, if the inversion is already calculated, return the value
## Otherwise, calculate the inversion of the data and sets the value of the inversion
## in the cache via the "setinv" function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
      message('getting cached data')
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
