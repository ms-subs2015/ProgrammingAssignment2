## The first function, makeCacheMatrix creates a special "Matrix", 
## which is a list containing a function to
### 1. set: set the value of the Matrix
### 2. get: return the value of the Matrix
### 3. setInverse: set/cahche the inverse of the Matrix
### 4. getInverse: return the inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
## First testing GIT cmds
  mx <- mx_makeCacheMatrix(x)
  return (mx)
}


## The following function calculates the inverse of the special "Matrix" 
## created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the Matrix using solve, a library function and 
## sets/cache the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mxi <- mx_cacheSolve(x, ...)
  
  return (mxi)
}



mx_makeCacheMatrix <- function(data = NA, nrow_or_ncol = NA) {
  
  # data: must be valid square matirx or 
  # numric vector that can be converted to square matrix.
  
  
  x <- validateSquareMatrix(data, nrow_or_ncol)
  
  if(is.null(x) || !is.matrix(x)) {  
    return (NULL)
  }
  
  m <- NULL
  set <- function(data = NA, nrow_or_ncol = NA) {
    
    y <- validateSquareMatrix(data, nrow_or_ncol)
    
    if(!is.null(y) && is.matrix(y)) {
      x <<- y
      m <<- NULL 
      return (TRUE)
    } else {
      return (FALSE)
    }
    
    
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


mx_cacheSolve <- function(x, ...) {
  
  if(is.null(x) || is.na(x) || !is.list(x) ) {
    message("Error: Invalid parameter/s")
    return (NULL)
  }
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached Inverse Matrix")
    return(m)
  }
  
  mx <- x$get()
  
  m <- solve(mx, ...)
  message("setting cache Inverse Matrix")
  x$setInverse(m)
  m
}


validateSquareMatrix <- function(data = NA, nrow_or_ncol = NA) {
  x = matrix()
  
  if(is.null(data) || is.na(data)  ) {
    message("Error: Invalid parameter/s")
    return (NULL)
  } else if(is.matrix(data)) {
    
    if(nrow(data) == ncol(data)) {
      x <- data
    } else {
      message("Error: Not a square Matrix")
      return (NULL)
    }
    
  } else if(is.numeric(data)) {
    
    if(is.na(nrow_or_ncol)) {
      nrow_or_ncol = sqrt(length(data))
      nrow_or_ncol = as.integer(nrow_or_ncol)
    }
    
    if((length(data) == (nrow_or_ncol*nrow_or_ncol))) {
      x <- matrix(data, nrow_or_ncol, nrow_or_ncol)
    } else {
      message("Error: It cannot be converted to square Matrix")
      return (NULL)
    }
    
  } else {
    return (NULL)
  }
  
  if(det(x) == 0) {
    message("Error: Not an invertible Matrix: det = 0")
    return (NULL)
  } else {
    return (x)
  }
  
  
}

