### In this code two functions are defined: makeCacheMatrix and cachesolve. 
### MakeCacheMatrix takes an invertible matrix (M) as its argument and creates an special object of type list 
### (e.g. makeCacheMatrix.object) which contains of four functions that are explained below in detail. 

### Cachesolve takes this special object as its argument and returns the inverse of the matrix M. 
### It either calculates the inverse of M and stores it to the environtment of the special object 
### (makeCacheMatrix.object) or retrieves it from the cache if it is already stored.

### In conclusion: When makeCacheMatrix and cachesolve are run, the inverse of M is stored to the memory
### and can be retrieved by running cachesolve another time without calculating it again.



### The Function makeCacheMatrix creates a special object from type list with four elements which are functions:

# 1th element: setmatrix(): When running the function makeCacheMatrix or cachesolve the function set.matrix ist not used
# because the matrix which should be inverted is given as an argument to the function. However, if one wants to cache 
# and invert a different matrix, one can use makeCacheMatrix.object$setmatrix(NEWMATRIX) without running makeCacheMatrix
# again. setmatrix() defines the NEWMATRIX as wenn as it sets the inverse to null.

# 2nd element: getmatrix(): Retrieves the matrix M which is stored in the environment of the special matrix object 
# (makeCacheMatrix.object).

# 3rd element: setinverse(): Takes the argument "I" which is the inverse of the matrix which should be inverted and 
# assigns it to the variable "Inverse" in the parent environtment of setinverse() and therefore stores it of the 
# environtment of the special matrix object (makeCacheMatrix.object).

# 4th element: getinverse: Retrieves the Value of the variable Inverse out of the environment of the special matrix
# object (makeCacheMatrix.object).

makeCacheMatrix <- function(M = matrix() )
{
  Inverse <- NULL
  set.Matrix <- function(newM)
  {
    M <<- newM
    Inverse <<- NULL
  }
  
  get.Matrix <- function() {M} 
  set.Inverse <- function(I) {Inverse <<- I}
  get.Inverse <- function() {Inverse}
  list(setmatrix = set.Matrix, getmatrix = get.Matrix,
       setinverse = set.Inverse,
       getinverse = get.Inverse)
}


### The function cachesolve creates the inverse of the matrix M and stores it to the cache. It takes the 
# special matrix object created by makeCacheMatrix as argument and does the follwing steps:
# First it calls getinverse() from the special matrix object which is NULL at the first time or the inverse of M if 
# there is already an inverse which is cached. The inverse is stored in an local variable called "local.inverse".
# Second: It checks wether the "local.inverse" is NULL. If this variable is not NULL, 
# the value is returned from the cache (environment of the special object). Otherwise
# the function calls getmatrix() to retrieve the matrix to be inverted and stores it in the local variable "data".
# Afterwards, the inverse of "data" is calculated and stored to the environment of the special matrix object by calling
# setinverse(loca.Inverse).




cacheSolve <- function(makeCacheMatrix.object, ...)
{
  local.Inverse <- makeCacheMatrix.object$getinverse()
  if(!is.null(local.Inverse))
  {
    message("getting cached data")
    return(local.Inverse)
  }
  data <- makeCacheMatrix.object$getmatrix()
  local.Inverse <- solve(data, ...)
  makeCacheMatrix.object$setinverse(local.Inverse)
  local.Inverse
}




