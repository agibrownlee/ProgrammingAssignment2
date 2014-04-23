## Function creates a list of Functions which sets, gets a Matrix and sets, gets the inverse of the Matrix

## Inputs a Matrix and returns a list of 4 functions
## MATRIX MUST BE SQUARE

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                     #set m to null
  set <- function(y) {          #sets the value of the Matrix
    x <<- y
    m <<- NULL                  #resets m to null when Matrix is reset
  }
  get <- function() x           #returns matrix x
  setInverse <- function() {
    if (is.null(m)) m <<- solve(x)      #solves inverse of x, stores in m
    m                                   #returns m
    }
  getInverse <- function() m            #returns m if calculated
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function inputs the list that is created in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()           #assigns the inverse of x if it has been calculated already        
  if(!is.null(m)) {           #if it exists
    message("getting cached data") 
    return(m)                #returns the inverse value
  }
  data <- x$get()             #if there's no cache
  m <- solve(data, ...)        #compute here
  x$setInverse()                #save the result
  m                           #return the result
}
