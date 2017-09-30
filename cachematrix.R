## This function does the following: 1. Set the value of x. 
makeCacheMatrix <- function(x = matrix()) {  
  y <- NULL
  set <- function(m) {
    x <<- m
    y <<- NULL
  }
  ##Gets the value of x
  get <- function() x
  ##Sets the inverse of x (y)
  setsolve <- function(solve) y <<- solve
  ##Gets the value of y.
  getsolve <- function() y
  list(set = set,
       get = get,
       setsolve = setsolve,
       getmean = getmean)
}
##Checks if matrix have been inverted if not inverts the matrix
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    y <- getsolve()
    if (!is.null(y)) {
      message('getting stuff')
      return(y)
    }
    data <- get()
    y <- solve(data)
    setsolve(y)
  }
  
#Makes a randomly generated 3x3 matrix and inverts it with solve
set.seed(42)
x <- matrix(rnorm(9,3,4), 3, 3)
cacheSolve(x)
