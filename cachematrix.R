## This function returns a special matrix from a matrix with 4 functions that:
## (1) Set the matrix (2) get the original matrix (3) set the inverse of the matrix and (4) return
## the inverse of the matrix. Used in conjunction with cacheSolve, the two functions return
## the inverse of a matrix, either from memory (if already calculated) or using the Solve function

## This code creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #set inv to null first time
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x ## this is a function that returns the source data of the matrix
  setinv <- function(inverse) inv <<- inverse  ## this function assigns the inverse of a matrix to a global inv variable
  getinv <- function() inv ## this function returns the inverse matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function either returns the inverse of the special matrix in memory if already calculated or it calculates
## the inverse of the special matrix object using Solve if not already calculated

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) { ## check to see if the inverse matrix already exists
    message("getting cached data") 
    return(inv)  ## return the inverse matrix in memory
  }
  original <- x$get() ## if inverse is not in memory, get the original matrix
  inv <- solve(original) # return the inverse of the original matrix ## calculate the inverse of the matrix
  x$setinv(inv) ## set the inverse of the matrix in memory
  inv ## return the matrix
}

