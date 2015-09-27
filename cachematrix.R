## Put comments here that give an overall description of what your
## functions do

##  In these functions, a matrix and its inverse are stored. When a new 
##  matrix comes up, the function first checks whether the inverse has
##  already been calculated. If yes, it just gets the inverse stored in
##  the cache. If not, it would compute the inverse and store it in the 
##  cache.



## Write a short comment describing this function

##  makeCacheMatrix is a function which stores four functions:
##  set, get, setinv and getinv. Set function substitutes matrix x 
##  with y in the makeCacheMatrix rather than only in the set function.
##  Similarly, setinv is used to store a input into object inv
##  of the makeCacheMatrix function. The set function is not needed 
##  until we want to change the matrix. Functions get and getinv return
##  matrix x and variable inv stored in makeCacheMatrix, respectively.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
  
   
}


## Write a short comment describing this function

##  The input of cacheSolve function is the result from makeCacheMatrix.
##  The inverse returned by getinv from makeCacheMatrix is assigned to 
##  object inv here. If it is not null, cacheSolve just returns a message
##  and inv. Then the function ends. If this is not the case, cacheSolve
##  gets the matrix stored in makeCacheMatrix and calculates its inverse.
##  Then the function stores the inverse into setinv and returns its value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

