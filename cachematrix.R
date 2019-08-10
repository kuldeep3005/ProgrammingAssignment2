# Function makeCacheMatrix takes a matrix as input. Then, set value of matrix,
#get Value of matrix, set the inverse matrix, get inverse matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function CacheSolve takes output of above function makeCacheMatrix(matrix) as input.
# and checks if matrix has any value or not. In case, inverse matrix is empty, then it takes  
# original matrix data from function makeCacheMatrix(matrix) and set invertible matrix using solve() 
# function. If it gets Inverse matrix has some value, it returns message "getting cached Invertible Matrix" 
# and cached object.   

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached Invertible Matrix")
    return(invMatrix)
  }
  matrixData <- x$getMatrix()
  invMatrix <- solve(matrixData, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
}