## Two functions to calculate the inverse of a matrix
## and cache it ifor quicker access.

## Create an object to store, set and retreive values of matrix
## and values for its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Clear any cached values for inverse of matrix
  inv <- NULL
  
  ## Define local function for setting values of matrix object
  set <- function(y) {
    ## Set original values of matrix to passed values
    x <<-y
    ## Clear any cached values for inverse of matrix
    inv<<-Null
  }
  
  ## Return values of original matrix
  get <- function() x
  
  ## Set local variable to inverted matrix
  setinverse <- function(solved) inv <<- solved
  
  ## Return value of local value with inverted matrix
  getinverse <- function() inv
  
  ## Concatonate above local functions into list
  ## object with corresponding names to allow direct referencing
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Perform matrix inversion and store in cache, or retrieve
## existing values if applicable.

cacheSolve <- function(x, ...) {
  ## Retrieve cached value for inverse matrix and place in local variable
  inv <- x$getinverse()
  
  ## If value is not null, return stored value and message...
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## ...Else get original matrix data
  data <- x$get()
  
  ## Perform inverse of matrix
  inv <- solve(data, ...)
  
  ## Store inverse matrix in cache
  x$setinverse(inv)
  
  inv
  ## Return a matrix that is the inverse of 'x'
}


