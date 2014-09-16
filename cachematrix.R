## This code is Programming Assignment 2 for the R Programming Course
## The purpose of the code is to cache the result from calculating the
## inverse of a matrix to potentially save computation time.  There are
## two functions in this code: 1) makeCacheMatrix and 2) cacheSolve.

## The purpose of makeCacheMatrix is to create a special "matrix" object
## that can cache its inverse.  The object is really a list to 1) set the
## values of the matrix, 2) get the value of the matrix, 3) set the value
## of the matrix inverse and 4) get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- matrix()
  set <- function(y) {
    x <<- y
    m_inv <<- matrix()
  }
  get <- function() x
  setm_inv <- function(solve_m) m_inv <<- solve_m
  getm_inv <- function() m_inv
  list(set = set, get = get,
       setm_inv = setm_inv,
       getm_inv = getm_inv)
}


## The purpose of cacheSolve is to return the value of the matrix inverse
## if it has already been calculated and cached.  If there is no matrix inverse
## cached, the function computes the matrix inverse using the solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getm_inv()
  if (!is.na(m_inv[1,1])) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data)
  x$setm_inv(m_inv)
  m_inv
}
