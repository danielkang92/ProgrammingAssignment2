## This functions will cache the inverse of a matrix rather than simply compute
## it and delete the content. Hence, once we request the inverse of same matrix
## again we don't ask the memory to do same extra work.

## This function, upon being passed a matrix, will compute its inverse
## and cache it. It also allows retrieval via get functions and setting new values
## with set functions

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y){
      x <<- y
      inv <<- NULL
   }
   
   get <- function() x
   setInv <- function(inverse) inv <<- inverse
   getInv <- function() inv
   list(set = set, get = get, 
      setInv = setInv, getInv = getInv)
}


## This function, upon being called, will see if the matrix's inverse had
## been computed and cached. If it had been cached, simply return that value.
## If not cached, compute the inverse and return it.

cacheSolve <- function(x, ...) {
   inv <- x$getInv()
   if(!is.null(inv)){
      message("getting cached data")
      return(inv)
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInv(inv)
   ## Return a matrix that is the inverse of 'x'
   inv
}
