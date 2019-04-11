makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  ##setting the value of the matrix
  set <- function(y) {
    x <<- y
    ##this <<- operator is to assign value in an environment outside the parent
    im <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ## set the value of inverse of matrix
  setinverse <- function(inverse) im <<- inverse
  ## here we get the inverse of the matrix
  getinverse <- function() im
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ##output of the makeCacheMatrix function
  ## if the ivnerse has already existed in cache
  im <- x$getinverse()
  if(!is.null(im)) {
    ##get it from cache and skip computation
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  ##otherwise solve returns the inverse of the matrix
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
##example run
x<- matrix(1:4,2,2)
x
x1<-makeCacheMatrix(x)
cacheSolve(x1)
