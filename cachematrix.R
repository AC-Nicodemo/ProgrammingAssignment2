
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  #initialize inv setting it's initial value as NULL
  inv <- NULL
  
  #set matrix x as y, sets inv (in the parent environment) as NULL
  set <- function(a) {
    m <<- a
    inv <<- NULL
  }
  
  #return matrix x
  get <- function() m
  
  #sets the inverse and then returns it
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #list of the  functions is returned
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(m, ...) {
  
  inv <- m$getinverse()
  
  #if there already is an inverse cached in inv from previous function, return the value
  if(!is.null(inv)) {
    inv
  }
  
  #if not, calculates inverse of x through function solve
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv
}

