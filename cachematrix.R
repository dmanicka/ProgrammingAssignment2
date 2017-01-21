## Put comments here that give an overall description of what your
## functions do

## Function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  
  # inverse property initialization
  inv <- NULL
  
  ## function to set the matrix
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  
  get <- function() {
    ## return the matrix
    m
  }
  
  ## method to set inverse of the matrix
  setinverse <- function(inverse){
    inv <<- inverse
  }
    
  ## method to get the inverse of the matrix
  getinverse <- function() {
    ## return the inverse
    inv
  }
  
  ## returns the list of all methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of special matrix created by makecachematrix function
## If the inverse has been already computed the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  ## function returns inverse if already computed
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculates the inverse
  m <- solve(data) %*% data
  
  ## set inverse to object
  x$setinverse(m)
  
  ## return matrix
  m
}
