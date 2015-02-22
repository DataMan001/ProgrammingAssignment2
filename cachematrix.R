## This program contains two functions, makeCacheMatrix and cacheSolve. 
## The makeCacheMatrix creates an object that stores a matrix and can cache the 
## inverse of it, to avoid recomputing the inverse, which is a time-consuming computation.
## The cacheSolve funtion first checks whether the inverse to be computed for a matrix
## has already been computed. If yes, then it retrieves the inverse from the cache, else
## it computes the inverse and stores it in the cache, then prints it.


## This funtion takes an argument of type matrix, and creates an object which stores
## the matrix as well as caches the inverse of the matrix. It conctains four funtions
## to set and return the matrix, and to set and return the inverse of the matrix.



makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  setmatrix <- function(y) {
    
      x <<- y
      inv <<- NULL
  }
  
  getmatrix <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinv = setinv, getinv = getinv)

}


## This funtion first checks if the inverse is already cached.
## If yes, it returns the cached inverse. Else it computes the inverse, stores it in
## the cache, and returns it.

cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  
    if(!is.null(i)) {
      
        message("Retreiving Inverse from Cache")
        return(i)
    }
  
  data <- x$getmatrix()
  i <- solve(data)
  x$setinv(i)
  i

  
}
