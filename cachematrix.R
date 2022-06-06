#the functions are able to cache the inverse of a matrix after
#it is computed the first time, in order to save time in case of
#future need

#this function creates a list of functions that can get and set the
#value of the matrix, as well as get and set its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinv <- function(inv) i<<-inv
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#this function provides the inverse of the matrix to the function above when
#such operation is performed for the first time. In future usages, it will
#just get the stored value (cache) and skip computing the inverse again
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cache data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}