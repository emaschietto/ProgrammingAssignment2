## Caching the Inverse of a Matrix, this function works to inverte the Matrix
## Using a pair of functions creating a special object that stores a matrix
## And its inverse.

## Here below, this function will inverte the Matrix and store it, and inverte it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function will compute the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and not changed, ## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
