## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
