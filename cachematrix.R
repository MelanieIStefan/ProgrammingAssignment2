## Functions makeCacheMatrix and cacheSolve allow a user to cache the inverse of a matrix,
## so that once it has been computed, users can just retrieve the stored value and do not
## have to compute it again. This reduced computatoinal cost. The two functions provided
## are 
## - makeCacheMatrix (creates a matrix object that can cache its inverse)
## - cacheSolve (retrieves inverse from cache or computes and returns it)

## usage example
## A <- rbind(c(2,3),c(1,2))
## B <- makeCacheMatrix(A)
## C <- cacheSolve(B)
 
## gives:
## C
## [,1] [,2]
## [1,]    2   -3
## [2,]   -1    2

## A %*% C
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## makeCacheMatrix creates a special matrix object that can cache its inverse
## Argument: Matrix x
## Four functions: set, get, set.inv, get.inv (to set and get the matrix and its inverse)

makeCacheMatrix <- function(x = matrix()) {
 
  ## sets inv to an empty vector upon first defining the matrix
  inv <- NULL
  
  ## sets the matrix to some other matrix y; sets inv to NULL 
  ## (because the matrix itself has changed)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## retrieves the matrix
  get <- function() x
  
  ## sets the inverse (computed using the cacheSolve function below)
  set.inv <- function(inverse) inv <<- inverse
  
  ## retrieves the inverse (returns a number if an inverse has been computed, 
  ## returns NULL otherwise)  
  get.inv <- function() inv
  
  ## lists the functions used 
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
  
}


## cacheSolve returns a matrix that is the inverse of matrix x (if x was created with the
## special makeCacheMatrix function above)
## Argument: Matrix x (created using makeCacheMatrix)
## checks if inv exists in case, otherwise computes it 

cacheSolve <- function(x, ...) {
  ## check if inverse is already cached using get.inv from makeCacheMatrix above 
  inv <- x$get.inv()
  
  ## if inverse exists (is non-empty), print message that it is being retrieved from cache, 
  ## return inv, and we are done (function exits after completing return() )
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## this part is only executed if inv is empty:
  ## get the actual matrix using get from makeCacheMatrix
  data <- x$get()
  ## compute the inverted matrix using the solve function
  inv <- solve(data, ...)
  # sets inverse of x to value just computed
  x$set.inv(inv)
  # returns the inverse of x
  inv
}
