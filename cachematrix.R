## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a funtion to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinv <- function(y) { inv <<- y }  
  getinv <- function() { inv }
  list(
    set = set, get = get,
    setinv = setinv,
    getinv = getinv
  )

}


## Write a short comment describing this function
# The function, cacheSolve calculates the inverse of the special
# "matrix" created with the function makeCacheMatrix. It first
# chechs to see if the inverse has already been calculated. if so,
# it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value
#  of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
