## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  invMatrix <- NULL
  setMatrix <- function(y)  #set value of the Matrix
  {
    x <<- y
    invMatrix=NULL
  }
  getMatrix <- function() x                                #get value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse    #set value of the invertible matrix
  getInverse <- function() invMatrix                        #get value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix))              #if inverse matrix is not NULL
  {
    message("getting cached data")
    return(invMatrix)                  #return the cached invertible matrix
  }
  
  #if condition is false  
  Matrixdata <- x$getMatrix()
  invMatrix <- solve(Matrixdata, ...)        # solve function to inverse the matrix
  x$setInverse(invMatrix)                     # set the invertible matrix 
  invMatrix                                    #return the invertible matrix
}
