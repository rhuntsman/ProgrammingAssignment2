## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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

