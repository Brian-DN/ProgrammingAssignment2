## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
       n <- NULL
       set <- function(y){
              x <<- y
              n <<- NULL
       }
       get <- function() x
       setInverse <- function(solve) n <- solve
       getInverse <- function() n
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
       n <- x$getInverse()
       if(!is.null(n)){
              message("getting cached data")
              return(n)
       }
       data <- x$get()
       n <- solve(data,...)
       x$setInverse(n)
       n
}
