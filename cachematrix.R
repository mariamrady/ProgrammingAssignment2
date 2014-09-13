## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  #set values of a matrix
  #get values of a matrix
  #set the inverse of a matrix
  #get the inverse of a matrix
  inv<-NULL
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  settranspose<-function(inverse) inv<<-inverse
  gettranspose<-function() inv
  list(set=set,get=get, settranspose=settranspose,gettranspose=gettranspose)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  #If the inverse has already been calculated (and the matrix has not changed),
  #then the cachesolve should retrieve the inverse from the cache.
  inv<-x$gettranspose()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-inverse(data,...)
  x$settranspose(inv)
  inv
}
