makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL ## initializes variable InvMat to store the result of inverse matrix
  set <- function(y) ## set function sets the value of the input matrix
  {
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x #returns the input matrix
  setinverse <- function(inverse) InvMat <<- inverse #sets the inverse matrix 
  getinverse <- function() InvMat #returns the inverse matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  # list returns all the objects/functions created in makeCacheMatrix function
}

cacheSolve <- function(x, ...) {
  # Return an inverse matrix of the input matrix x
  InvMat <- x$getinverse() #uses the get method to calcualte inverse matrix from input matrix
  if(!is.null(InvMat)) { #check if the inverse matrix already exists, if it does them it returns the cached data
    message("getting cached data")
    return(InvMat)
  }
  data <- x$get() #if inverse matrix doesn't exist it uses get method to get the input matrix
  InvMat <- solve(data, ...)
  x$setinverse(InvMat)
  InvMat
}




