## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a matrix with a list of functions (set, get the given matrix and 
## setMatrix, getMatrix to cache and retrieve the cached inverted matrix
## returns the list of functions

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix x1 with NULL
  x1 <- NULL
  set <- function(mat) {
    x <<- mat
    x1 <<- NULL
  }
  
  get <-function(){x}
  
  ## caches inverse matrix
  setMatrix <- function(xinv) {x1 <<- xinv}
  
  ## retrieves cached inverse matrix
  getMatrix <- function() {x1}
  
  ## return "methods" for matrix
  list (set= set, get= get, setMatrix = setMatrix, getMatrix = getMatrix)

}

## Write a short comment describing this function
## inverts the matrix stored
## if the inverted matrix is already cache the $getMatrix() retrieves it otherwise the result is null
## if no cached matrix is available, the matrix stored with the $set earlier is retrieved by $get = mat
## and the the inverse matrix x1 is returned by solve(mat)
## the result is cached by the $setMatrix(x1)
## finally the inverse is returned

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ## already cached? -> return cached matrix
  x1 <- x$getMatrix()
  if(!is.null(x1)){
    message("Getting cached inverse matrix")
    return(x1)
  }
  mat <- x$get()
  x1 <- solve(mat)
  x$setMatrix(x1)
  x1
}