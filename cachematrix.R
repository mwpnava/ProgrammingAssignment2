## These two functions cache the inverse of a matrix.
## By: Wendy Navarrete
## August, 2015

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mtx<-NULL
  set<-function(y){
    x<<-y
    mtx<<-NULL
  }
  get<-function() x
  setinverse<-function(inversemtx) mtx <<- inversemtx
  getinverse <-function() mtx
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mtx<-x$getinverse()
  if(!is.null(mtx)){
    message("Getting cached data")
    return(mtx)
  }
  data<-x$get()
  mtx<-solve(data)
  x$setinverse(mtx)
  mtx
}
