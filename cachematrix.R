## Makes matrix object with cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  # inverse value
  inverseX <- NULL
  
  # set value and remove cached inverse value (since value has changed)
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  
  # get value
  get <- function() x
  
  # set inverse value
  setInverse <- function(solve) inverseX <<- solve
  
  # get inverse value
  getInverse <- function() inverseX

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  # get cached inverse value
  m <- x$getInverse()
  
  # if cached value is not null return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if cached value is null, calculate it
  data <- x$get()
  m <- solve(data, ...)
  
  # cache calculated inverse value
  x$setInverse(m)
  m
}

