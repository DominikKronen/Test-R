makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function() {x}
  Inverse <-function(inverse) {inv<<-inverse}
  calcInverse <- function() {inv}
  list(set = set, get=get,Inverse=Inverse,calcInverse=calcInverse)
}

cacheSolve <- function(x,...){
  inv<-x$calcInverse()
  if(!is.null(inv)){
    message("data is cached")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$Inverse(inv)
  inv
}

