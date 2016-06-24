## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
## matrix must be invertible (not singular ....)
##
## set : set matrix
## get : get matrix
## setinverse : set inverse matrix using solve
## getinverse : get inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x<<-y
            m<<-NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


##The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}




