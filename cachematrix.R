## Create inverse from cache

## create a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    ##create getter/setter functions
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    ##assign to environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## return the inverse of a cached matrix 

cacheSolve <- function(x, ...) {
        ## get the inverse (if exists)
    inv <- x$getinverse()
    ##if inverse already exists, return existing inverse
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    ##calculate inverse matrix
    inv <- solve(data, ...)
    ##set inverse matrix
    x$setinverse(inv)
    ##return inverse Matrix
    inv
  }

