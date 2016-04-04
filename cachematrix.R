## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #create cached inverse
    inv <- NULL
    
    #functions to set and get matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    
    #functions to set and get matrix invert
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    #returning list of get/set functions for matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
    
       inv <- x$getinv()
       
       #return the cached inverse if it's already been computed
       if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
       }
       
       #compute inverse
       matrix <- x$get
       inv <- solve(matrix,...)
       
       #cache inverse
       x$setinverse(inv)
       
       #return inverse
       inv
       
}
