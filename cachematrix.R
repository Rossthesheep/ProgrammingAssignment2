makeCacheMatrix <- function(x = matrix()) {

  y <- NULL
  set <- function(m) {
    x <<- m
    y <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) y <<- solve
  getsolve <- function() y
  list(set = set,
       get = get,
       setsolve = setsolve,
       getmean = getmean)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
  y
}

#Makes a randomly generated 3x3 matrix and inverts it with solve
set.seed(42)
x <- matrix(rnorm(9,3,4), 3, 3)
cacheSolve(x)
