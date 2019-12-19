## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix creates a small matrix object that can cache it's inverse
#cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function

#creates a small matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialize cached value to NULL
  m <- NULL
  # when set is called, sets matrix to inputed value, m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get returns original matrix object is created around
  get <- function() x
  #set solve sets m to it's value(solve is called in other function)
  setSolve <- function(solve) m <<- solve
  #gets value stored for solved matrix
  getSolve <- function() m
  #sets return value as list with getter/setter functions as values
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

#cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #checks if cache object has stored solution
  m <- x$getSolve()
  if(!is.null(m)) {
    #if solution stored, prints message and returns cached value
    message("getting cached data")
    return(m)
  }
  #sets matrix to call solve on to value stored in object
  data <- x$get()
  #sets m to solution of stored matrix
  m <- solve(data, ...)
  #sets stored value for solution
  x$setSolve(m)
  #returns solution
  m
        ## Return a matrix that is the inverse of 'x'
}


#example test case / usage example, ignore
#mat <-rbind(c(0,-3,-2),c(1,-4,-2),c(-3,4,1))
#cacmat<-makeCacheMatrix(mat)
#cacheSolve(cacmat)
