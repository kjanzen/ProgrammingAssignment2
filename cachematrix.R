## These functions work together caching the inverse of a matrix.

# 1 - Create a squared matrix (M). Bear in mind that it has to be invertible.
# 2 - Pass the matrix created in step one as an argument for makeCacheMatrix
#     and assign the result to a variable (C). This will return a list of functions.
# 3 - Run the function cacheSolve using C as the argument. This will check if there
#     already is an inverse cached for M. If there is not, it will calculate the inverse,
#     store it, and return its value. If there is one already, it will retrieve it and return
#     its value as well.


## This function initializes the variables needed to store the cached values of an invertible matrix
## and creates the methods required to set/change the matrix, retrieve the matrix, set its inverse
## and retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function checks if the matrix we want to invert has already a cached value. If it hasn't
## it uses the get() and setsolve() functions to obtain the matrix to be inverted and cache its
## inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## You can use the following lines to test the function.

# M <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
# C <- makeCacheMatrix(M)
# cacheSolve(C)

## Outputs:

#      [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

## You can now use getsolve() to access the cached value
# C$getsolve()

## We can also set the value to a different matrix and test with new data

# C$set(matrix(1:4, 2, 2))
# cacheSolve(C)

## Outputs:

#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


