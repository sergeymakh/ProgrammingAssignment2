## Put comments here that give an overall description of what your
## functions do

## !! Usage example

#> x <- matrix(1:4, 2, 2)
#> ttt <- makeCacheMatrix(x)
#> cacheSolve(ttt)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(ttt)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> y <- matrix(1:6, 2, 3)
#> uuu <- makeCacheMatrix(y)
#> cacheSolve(uuu)
#Matrix is not invertible!
#  [,1]
#[1,]   NA

##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv_mat <- matrix()
  
  #set - resets the object with the new input, NULLs the invese
  set <- function(y) 
  {
    x <<- y
    inv_mat <<- matrix()
  }
  
  #get - returns the matrix
  get <- function() x
  
  #setinverse - sets the inverse
  setinverse <- function(inverse_m) inv_mat <<- inverse_m
  
  #getinverse - returns the inverse of the matrix
  getinverse <- function() inv_mat
  
  #the return of the function - a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  i_m <- x$getinverse()   
  if (!identical(i_m, matrix()))
  {
    message("getting cached data")
    return(i_m)
  }
  
  matrix_data <- x$get()
  
  ## checking if matrix is invertible
  if (nrow(matrix_data) != ncol(matrix_data))
  {
    message("Matrix is not invertible!")
    return(i_m)
  }
  
  i_m <- solve(matrix_data)
  x$setinverse(i_m)
  
  ## Return a matrix that is the inverse of 'x'
  i_m
}
