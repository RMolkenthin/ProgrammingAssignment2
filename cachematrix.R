## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix x1 with NULL
  x1 <- NULL
  
  set <- function(mat) {
    ## !!!!! PRüfen ob inverse gebildet werden kann!!!!
    x <<- mat
    x1 <<- NULL
  }
  
  get <-function(){
    x
  }
  
  ## caches inverse matrix
  setMatrix <- function(xinv) {
    x1 <<- xinv
  }
  
  ## retrieves cached inverse matrix
  getMatrix <- function() {
    x1
  }
  
  ## return "methods" for matrix
  list (set= set, get= get, setMatrix = setMatrix, getMatrix = getMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ## already cached? -> returning cached matrix
  x1 <- x$getMatrix
  if(!is.null(x1)){
    message("Getting cached inverse matrix")
    return(x1)
  }
  mat <- x$get()
  x1 <- solve(mat)
  x$setMatrix(x1)
  x1
}