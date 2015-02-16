## This function creates a special matrix object
## from a matrix object

## This function creates a special matrix object
## from a matrix that includes the source matrix
## and that is used to assign the inverse of the matrix
## to a global variable holding the inverse of the matrix

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


## This function checks to see if an inverse matrix exists for an input matrix and, if so
## it returns the inverse matrix.  IF an inverse matrix doesn't exist, it calculates the 
## inverse of the matrix and stores it to a global variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinv()
if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  original <- x$get()
  inv <- solve(original) # return the inverse of the original matrix
  x$setinv(inv)
  inv
}
