##makeCacheMatrix is a function to store cached data
makeCacheMatrix <- function(x = matrix()) {
  ##sets the initial value of m to null
  m <- NULL
  ##changes the vector in the function to the new value (calculated by cacheSolve)
  set <- function(y) {
    ##sets and stores the new value
    x <<- y
    ##erases the old value, which is no longer needed
    m <<- NULL
  }
  ##returns the numeric (or vector) inputted into the function
  get <- function() x
  ##setInverse stores the inverse of m
  setInverse <- function(solve) m <<- solve
  ##getInverse returns the inverse of m
  getInverse <- function() m
  ##stores all of the values into a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve is a function which calculates the inverse of a matrix
##and stores the number for future use in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ##assign m to the value in makeCacheMatrix (stored value)
  m <- x$getInverse()
  ##checks if there is a stored value (if NULL, no value saved)
  if(!is.null(m)) {
    ##if stored value detected, prints "getting cached data"
    message("getting cached data")
    ##then prints the stored value
    return(m)
  }
  ##if there is no stored (cached) value, takes the numeric value(s) entered into the program in makeCacheMatrix
  data <- x$get()
  ##takes the inverse of the square matrix
  m <- solve(data, ...)
  ##stores the inverse matrix calculation for future use within makeCacheMatrix
  x$setInverse(m)
  ##prints the inverse matrix
  m
}
