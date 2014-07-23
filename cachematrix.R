## makeCacheMatrix function creates a invertible "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the invertible "matrix" returned by makeCacheMatrix.
##  If the inverse already calculated (and the matrix not changed) -> retrieve from the cache
##  else calculate the inverse using solve()


## Create set, get, setinverse, getinverse object fot matrix
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
       getinverse = getinverse)

}


## return inverse of matrix - search cache data first
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
