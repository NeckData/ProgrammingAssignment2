## functions for creating a matrix and  
## computing the inverse if not stored in cache

## makeCacheMatrix takes an invertible matrix as an argument
##- set function internally is used in case we want to change the 
## the value of matrix x.In such a case,the inverse gets back to Null
## -get thgets the value of x
##-setInv sets the value of inverse and stores it in cache
##-getInv gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInv <- function(Inv){
    m <<- Inv
  }
  getInv<-function () m
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


## cacheSolve computes the inverse of the matrix x which takes
##as an argument.It  gets the inverse from cache
## if exists there ,otherwise it takes the value of the matrix through function get and
## calculates the inverse It also stores the value in cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getInv
  
  if (!is.null(m)) {
    
    message('Taken from cache')
    return (m)
  }
  
  data <- x$get()
  
  m <- solve(data,...)
  
  x$setInv(m)
  
  m #return inverted matrix
}
