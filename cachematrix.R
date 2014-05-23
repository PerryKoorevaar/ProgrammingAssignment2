# There are 2 functions here, makeCacheMatrix and cacheSolve.
#
# Function makeCacheMatrix takes a matrix, calculates its inverse
# and puts the inverse in the cache memory (m). The output
# of makeCacheMatrix is a list of 4 functions, that can 1) set and 2) get the
# value of a matrix, and 3) calculate and 4) get the inverse of the matrix.
#
# Function cacheSolve takes as input the output of function makeCacheMatrix.
# It first checks if the inverse of the matrix, that served as input into
# function makeCacheMatrix, has already been calculated and stored in the 
# cache. If yes, the function returns this value and stops. If no, it will 
# itself calculate the inverse of the matrix, and return the result.

# Additional comments for function makeCacheMatrix. 
# The sub-function "set"basically assign stores its input into the variable 
# x, with the super assignment operator "<<-". This changes the value of x
# also outside of the environment of sub-funtion "set". So the changed value
# of x can be passed on to other sub-functions, e.g. sub-function "get".
# Sub-function "get" reads variable x and returns it.
# Sub-function "setinverse" calculates the inverse of its input matrix
# and stores the result in the cache (m).
# Sub-function "getinverse" returns the cache.
#

makeCacheMatrix <- function(x = matrix()) {
#
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
#
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Additional comments for function cacheSolve. 
# The first cacheSolve does is calling the the "getinverse" sub-function
# of makeCacheMatrix to read the value of the cache. It then checks if
# the cache is non-empty, and if so it returns the value of the cache and
# stops. If not, it will itself get the matrix with the subfunction "get"
# of function makeCacheMatrix, calcultas the inverse, stores the inverse
# of the inverse (i.e. the original) into the cache using sub-function 
# "setinverse", and returns the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
#  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
#
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
