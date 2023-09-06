## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  inv <- NULL
  get <- function() x
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment.
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}





## Write a short comment describing this function



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  # if the inverse has already been calculated
  if (!is.null(inverse)) {
    return(inverse)
  }
  # otherwise, calculates the inverse 
  m <- solve(x$get())
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(m)
  return(m)
}
 
