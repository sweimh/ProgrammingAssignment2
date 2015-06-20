
## "makeCacheMatrix" creates a special "matrix" object that can ... 
##   1. it can set value of the metrix
##   2. it can set inverse value of the metrix
##   3. it can get value of the metrix
##   4. it can get inverse value of the metrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  ## 1. set value of matrix (inverse is null)
  set <- function(y) x <<- y
  
  ## 2. set inverse value of matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## 3. get value of matrix
  get <- function() x
  
  ## 4. get inverse value of matrix
  getinverse <- function() inv
    
  ## bind results, otherwise "object of type 'closure' is not subsettable"
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()

  ## detect if inverse is already cached
  if(!is.null(inv)) {
    
    ## show this message when result is pull from cache
    message("Getting cached data...")
    
    ## return cache (this terminates function here)
    return(inv)
  }

  ## inverse is not cached, run normal calculation
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)

  return(inv)
}
