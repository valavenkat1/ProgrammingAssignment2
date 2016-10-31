 
#The two functions written below are used to cache the inverse of a matrix.

# makeCacheMatrix, our first function is creating a list which contains the function which let's us set, get the value of a matrix and also set, get the value of inverse of a matrix.



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The cacheinv function will return the inverse of the matrix. It initially checks if the inverse is already computed or not. If the inverse is already computed, the computation is skipped and the inverse is returned directly.
## If the inverse is not computed, it computes the inverse and assigns the value in the cache through the setinverse function.


cacheinv <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
