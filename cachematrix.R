## This function returns a list of functions
## get() and getinverse() functions returns matrix data and matrix inverse
## set() and setinverse() functions sets matrix date and matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  ## Func 1: This function sets the matrix         
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Func 2: This function returns the Matrix object
  get <- function() x
  
  ## Func 3: This function sets the inverse object	
  setinverse <- function(inverse) inv <<- inverse
  
  ## Func 4: This function returns the inverse
  getinverse <- function() inv
  
  # makeCacheMatrix returns a list of all above functions 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function actually returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  inv <- x$getinverse()
  
  ## if inverse is in the cache, it will be returned and function
  ## will terminate here
  if(!is.null(inv)) {
    message("getting cached data for inverse")
    return(inv)
  }
  
  ## if the inverse is not in the cache
  ## Execution will continue for the following statements
    
  ## get the matrix object
  matrixdata <- x$get()
  
  ## Calculate inverse of the matrix object
  inv <- solve(matrixdata, ...)
  
  ## Store the inverse of matrix in cache
  x$setinverse(inv)
  
  ## Function will return the inverse object
  inv 
}
