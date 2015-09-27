## makeCachematrix creates a special list of functions
## that calculate inverse of a matrix and caches it

## This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(solve) I <<- solve
  getinv <- function () I
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  I <-x$getinv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <-x$get()
  I <-solve(data,...)
  x$setinv(I)
  I
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}