## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function takes an argumetn x of type matrix
##and returns a list with 4 list items (each item is a function)
##Functions:
##set: Set the value of the matrix
##get: Get the value of the matrix
##setInverse: Set the value of the matrix inverse
##getInverse: Get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

##This function expects a "special matrix", made from makeCacheMatrix
##The output is the inverse of that matrix, either coming from cache or computation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}
