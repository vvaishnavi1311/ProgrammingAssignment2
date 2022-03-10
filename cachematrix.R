## makeCacheMatrix creates a matrix object that will cache its Inverse 
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x<<-y
    j<<-NULL
  }
  get<-function() x
  setInverse <- function(Inverse) j <<- Inverse
  getInverse <- function() j
  list(set=set, 
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}
## cacheSolve does the inverse of a matrix returned by makCacheMatrix above, 
##if it has already been inverse then it is retrieved from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the Inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("grabbing cached values")
    j
  }
  data <- x$get()
  j <- solve(data, ...) ##inverse the matrix and stores in j object
  x$setInverse(j)
  j
}
