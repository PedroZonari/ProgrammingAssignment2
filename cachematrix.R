# makeCacheMatrix sets the value of the matrix, gets the
# value of the matrix, sets the value of the inverse of the matrix
# and gets the value of the inverse of the matrix. 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function() x
  setinverse <- function(inverse) inv <<-inverse
  getinverse<- function() inv
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)

}


# Returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data<- x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}

