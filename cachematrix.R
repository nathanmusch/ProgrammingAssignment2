## Caching values can prove to be a more efficient means of programming, especially for
## repetitive tasks.

## This function will determine the inverse of a matrix and store it as a cache value.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<-y
          inv <<- NULL
     }
     get <-function()x
     setInverse <- function(inverse)inv <<- inverse
     getInverse <- function()inv
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## This will pull data from the cache created above.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
if (!is.null(inv)){
     message("getting cached data")
     return(inv)
}
mat <-x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}