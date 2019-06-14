## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Recall inverse of a matrix: let A be a matric, adj(A) its adjugate. The inverse of A is:
## A^-1 = 1/det(A)*adj(A)
## ES: A = |a b|; adj(A) = |d -b|; det(A) = ad - bc
##         |c d|           |-c a|
## For this exercise let's assume that the matrix supplied is always invertible
makeCacheMatrix <- function(x = matrix()) {
  # set inv to NULL
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inv
  setinv <- function(solve) inv <<- solve
  
  # get the inv
  getinv <- function() inv
  
  # build the list
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # let's get inv from cache
  inv <- x$getinv()
  # check if inv is not a null matrix and return cached inv
  if (!all(is.na(inv))) {
    message("getting cached inv")
    return(inv)
  }
  
  matrix <- x$get()
  
  # compute the inv
  inv <- solve(matrix, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
