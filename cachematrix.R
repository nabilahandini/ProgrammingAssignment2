# assume that the matrix supplied is always invertible it means det != 0
# if X is a square invertible matrix, then solve(X) returns its inverse

# if X not a square then solve return an error
# So if X not square use ginv function 
# ginv can be used in both square and not square matrix

# firstly install MASS
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x <<- y
    n <<- NULL
  }
  
  get <- function() x
  setinv <- function(ginv) n <<- ginv
  getinv <- function() n
  list(set=set, get=get, setinv = setinv, getinv = getinv)
}

#make cacheSolve function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinv()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  dat <- x$get()
  n <- ginv(dat, ...)
  x$setinv(n)
  n
}

#try

#not square matrix
#a <- rbind(c(5,2),c(8,4),c(1,6))
#b <- makeCacheMatrix(a)
#b$get()
#b$getinv()
#cacheSolve(b)

#square matrix
#x <- rbind(c(2,3),c(1,2))
#cacheSolve(makeCacheMatrix(x))
