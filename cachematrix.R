## Matrix inversion is a costly computation and hence it makes logical
## sense to create a cache of inverse matrix in case of repeated
## computations.
## We are writing two pair of functions:-
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has
##already been calculated (and the matrix has not changed), then 
## the cachesolve will retrieve the inverse from the cache.
##
##  setmatrix      set the value of a matrix
##  getmatrix      get the value of a matrix
##  setinverse     set the inverse value of the matrix
##  getinverse     get the inverse value of the matrix



## Writing function for making matrix and computing its inverse

makeCacheMatrix<-function(x=matrix()){
 
# initial value is NULL since nothing in cache, else hold cache value  
  Cachevalue<-NULL
  setmatrix<-function(y){
    x<<-y
    
# since the matrix is assigned a new value, flush the cache    
    Cachevalue<<-NULL
  }
  
# returns the stored matrix

getmatrix<-function(){
  x
}

# cache the given argument

setinverse<-function(solve) {
  Cachevalue<<-solve
}

# get the cached value

getinverse<-function(){
  Cachevalue
}

# return a list with each item of the list is a function
list(setmatrix=setmatrix,getmatrix=getmatrix,
     setinverse=setinverse,
     getinverse=getinverse)
}
## Creating cache function to either calculate inverse or call from
## cache if matrix is not changed and inverse is already available.


cachesolve<-function(y,...){
# fetch cache value 
  Cachevalue<-y$getinverse()
  
# if a cache value exist return it

  if(!is.null(Cachevalue)){
    message('getting cached data')
    return(Cachevalue)
    
  }
  
# else to get the matrix, compute inverse and store  in cache
   
  
  
  data<-y$getmatrix()
  Cachevalue<-solve(data)
  y$setinverse(Cachevalue)
  
# return the inverse
  Cachevalue
}