## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {                                ## set inv matrix
      x <<- y
      inv <<- NULL
    }
    get <- function() x                                 ## get inv matrix
    setinv <- function(inverse) inv <<- inverse         ## set function inverse
    getinv <- function() inv                            ## get inv matrix
    list(set = set, get = get,
         setmat = setinv,
         getmat = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)                             ##solve inv matrix
    x$setinv(inv)
    inv                                                 ## return inv matrix
  }
}
