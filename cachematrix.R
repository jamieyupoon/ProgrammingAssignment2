## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  #set matrix as Null
  m <- NULL
  #Set matrix to input matrix y
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get method to return x
  get <- function() x
  #set m as inverse matrix
  setinverse <- function(inverse) m <<- inverse
  #get method returns m
  getinverse <- function() m
  #return the 4 methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #if m is not null, then return the cached matrix m
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  #if m is null, then get x
  data <- x$get()
  #set m as inverse matrix of x
  m <- solve(data, ...)
  # set m as inverse matrix
  x$setinverse(m)
  m
}
