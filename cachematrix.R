 ##  To create a function makeCacheMatrix
##  This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
 


}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
 ##If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m <- x$getinverse()
     if(!is.null(m)) {
         message("the inverse from the cache")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m

        ## Return a matrix that is the inverse of 'x'
}
