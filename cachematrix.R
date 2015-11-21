## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #matrix(data=NA,dim(x)[1],dim(x)[2])
  set <- function(y) {
    x <<- y
    m <<- NULL #matrix(data=NA,dim(x)[1],dim(x)[2])
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x[getinverse = getinverse()]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x[get = get()]
  m <- solve(data, ...)
  x[setinverse = setinverse(m)]
  m
}