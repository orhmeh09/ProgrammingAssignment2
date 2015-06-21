
## makeCacheMatrix: Decorate the input matrix with the
## getter/setter functions
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  
  # Return list with the above functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve: Get the inverse of the matrix x; if this is done more
## than once, return a precomputed version of the inverse
## to speed things up.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse() 
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv) # There is a precomputed inverse, so return that
  }
  
  data = x$get() # Get matrix data
  inv = solve(data, ...) # and solve it
  x$setinverse(inv) # and save it 
  inv # and return it
}
