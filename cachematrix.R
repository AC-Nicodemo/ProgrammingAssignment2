#These functions are used to make matrix inversion. First caching

## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #initialise inv setting it's initial value
  inv <- NULL
  
  #set matrix x as y, sets inv (in the parent environment) as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #return matrix x
  get <- function() x
  
  #set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  #return inverse
  getinverse <- function() inv
  
  #list of the  functions is returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  #if there already is an inverse cached in inv from previous function, return the value
  if(!is.null(inv)) {
    inv
  }
  
  #if not, calculates inverse of x
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

