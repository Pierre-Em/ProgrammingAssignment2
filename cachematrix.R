## The two functions create a cache storing the inverse of a matrix and returning
## it without using the function solve if called for a second time

## The function makeCacheMatrix creates a list comprising the matrix and the cache
## value. The cache value contains the inverse of the matrix if already computed
## otherwise it takes the value NULL

makeCacheMatrix <- function(x = matrix()) {
  Inv_m<-NULL
  set<-function(y){
    x<<-y
    Inv_m<<-NULL
  }
  get<-function() x
  get_Inv<-function() Inv_m
  set_Inv<-function(Inv) Inv_m<<-Inv
  list(set=set, get=get, get_Inv=get_Inv, set_Inv=set_Inv)
}


## The function cacheSolve returns the inverse of a matrix. If this has already
## been computed it extracts the cache value otherwise it uses the solve function 
## to inverse the matrix

cacheSolve <- function(x, ...) {

  Inv_m<-x$get_Inv()
  if (!is.null(Inv_m)){
    message("retrieving the cache inverse:")
    return(Inv_m)
  }
  mat<-x$get()
  Inv_m<-solve(mat,...)
  x$set_Inv(Inv_m)
  Inv_m
}
