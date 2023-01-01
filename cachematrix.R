# Writing a function to cache the Inverse of a Matrix
## Creating a matrix object
### `makeCacheMatrix` is a function which creates a special 
### `matrix` object that can cache its inverse for the input 
### (which is an invertible square matrix).


makeCacheMatrix <- function( m = matrix() ) {
  ## Initializing the matrix
  j <- NULL
  
  ## Setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
  }
  
  ## Getting the matrix
  get <- function() {
    ## return the matrix
    m
  }
  
  ## setting inverse of the matrix
  setInverse <- function(inverse) {
    j <<- inverse
  }
  
  ##  getting inverse of the matrix
  getInverese <- function() {
    ## returns the inverse of the matrix
    j
  }
  
  ## return a list containing methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getinverse = getInverese)
}

## Retrieving the Inverse of the Matrix
### This function will compute the inverse of the matrix returned 
### by `makeCacheMatrix`.If the inverse has already been calculated 
### (and the matrix has not changed), then the `cachesolve`' retrieves 
### the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculating the inverse of the matrix using the matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to object
  x$setInverse(m)
  
  ## Returns the matrix
  m
}
