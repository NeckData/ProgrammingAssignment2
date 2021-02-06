## functions for creating a matrix and  
## computing the inverse if not stored in cache

## makeCacheMatrix creates a 'special' matrix

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


## cacheSolve computes the inverse or gets it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getInv
  
  if (!is.null(m)) {
    
    message('Taken from cache')
    return (m)
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setInv(m)
  
  m
}
