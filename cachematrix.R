#makeCasheMatrix is a function which is comparable to a class object
#it has 4 member functions which are comparable to methods
# member functions are:set, get, setinverse and getinverse
# <<- assignment operator causes a search to made through parent environments for an existing definition of the variable being assigned

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
  # returns all the objects/functions created in makeCacheMatrix function
}

#cacheSolve calculates the inverse of matrix created with makeCacheMatrix function
#first it checks if the inverse matrix already exists
#if it does then it gets the inverse from cache and skips calculations
#if it doesn't, it calculates the inverse of the inupt matrix using solve() function
#then it sets the inverse matrix in the cache using setinverse() function

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




