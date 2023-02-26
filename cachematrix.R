# As example, to invert a matrix take the example:
  #A =
      # 1  2  3
      # 0  1  4
      # 5  6  0

#Inverted, it turns to:
  #A^-1 =
      # -24   18    5
      #  20  -15   -4
      #  -5   4     1

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  #initialize inv setting it's initial value as NULL
  inv <- NULL
  
  #set matrix x as y, sets inv (in the parent environment) as NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  #return matrix x
  get <- function() m
  
  #sets the inverse and then returns it
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  #list of the  functions is returned
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(m, ...) {
  
  inv <- m$getinv()
  
  #if there already is an inverse cached in inv from previous function, return the value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #if not, calculates inverse of x through function solve
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}

