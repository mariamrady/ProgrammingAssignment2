## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  #set values of a matrix
  #get values of a matrix
  #set the transpose of a matrix
  #get the transpose of a matrix
  transpose<-NULL
  set<-function(y)
  {
    x<<-y
    transpose<<-NULL
  }
  get<-function() x
  settranspose<-function(t) transpose<<-t
  gettranspose<-function() transpose
  list(set=set,get=get, settranspose=settranspose,gettranspose=gettranspose)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  #If the inverse has already been calculated (and the matrix has not changed),
  #then the cachesolve should retrieve the inverse from the cache.
  transpose<-x$gettranspose()
  if(!is.null(transpose)){
    message("getting cached data")
    return(transpose)
  }
  data<-x$get()
  transpose<-t(data,...)
  x$settranspose(transpose)
  transpose
}
