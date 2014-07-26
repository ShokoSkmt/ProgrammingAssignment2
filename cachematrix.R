## The purpose of these two functions is to avoid calculate the inverse Matrix more than once in a row for the same Matrix.
## Save inverse Matrix to cache and get it 
## Matrix is assumed to be inversible Matrix

## makeCacheMatrix function creates "matrix" object that can cache its inverse.
## Create set, get, setinverse, getinverse object of matrix
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve    
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  ## return value

}


##  return inverse of matrix
##  If the inverse already calculated (and the matrix not changed) -> retrieve from the cache
##  else calculate the inverse using solve()
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv ## return value
}
