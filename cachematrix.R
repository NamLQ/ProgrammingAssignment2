## Caching the Inverse of a Matrix
# comments are based on Bill Hilton at 
# https://class.coursera.org/rprog-009/forum/thread?thread_id=457

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {	# input x will be a matrix
  m <- NULL		 # m will be our invertible matrix and it's reset to NULL every 
                 # time makeCacheMatrix is called

  set <- function(y) { 	# set the value of the matrix
    x <<- y				# Assign new value (y) to the matrix x
    m <<- NULL			# Reset matrix m to NULL every time setting the new value
  }
                 # note these next three functions are not run when makeCacheMatrix is called.
                 # instead, they will be used by cacheSolve() to get values for x or for
                 # m and for setting the invertible matrix.

  get <- function() {x} 		# returns the value of the original matrix
  setinvert <- function(solve) {m <<- solve}	# set the value of the invertible matrix
                  # this is called by cacheSolve() during the first cacheSolve() access and it will
                  # store the value using superassignment
 
  getinvert <- function() {m}	# this will return the cached value to cacheSolve() on subsequent accesses
  list(set = set, get = get,	# This is a list of the internal functions ('methods') so a calling 
       setinvert = setinvert,	# function knows how to access those methods.
       getinvert = getinvert)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {	# the input x is an object created by makeCacheMatrix

  m <- x$getinvert()				# accesses the object 'x' and gets the value of the invertible matrix
  if(!is.null(m)) {					# if the invertible matrix was already cached (not NULL) ...
    message("getting cached data")	# ... send this message to the console
    return(m)						# ... and return the invertible matrix ... "return" ends 
                        # the function cachemean()
  }
  data <- x$get()					# we reach this code only if x$getinvert() returned NULL
  m <- solve(data, ...)				# if m was NULL then we have to calculate the invertible matrix
  x$setinvert(m)					# store the calculated invertible value in x
  m									# return the invertible matrix to the code that called this function
}
