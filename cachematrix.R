## these functions create custom matrix objects that can cache their inverse

## generates a special "matrix" object that can cache its inverse
## "matrix" is actually a list containing functions to 
## (1) set the value of the matrix, 
## (2) get the value of the matrix, 
## (3) set the inverse of matrix, and
## (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  
  getinverse <- function() inverse

  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function solves for the inverse of the matrix x passed to it
## however, because this can be a computationally intensive comptuation, it first
## checks to see if the inverse is already computed, and returns that if it is
## if not, it solves for the matrix inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
}
