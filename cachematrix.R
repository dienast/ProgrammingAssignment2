makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  
  #set inverse
  setinversematrix<-function(solve) m<<- solve
  #get inverse
  getinversematrix<-function() m
  
  list(set=set, get=get,
       setinversematrix=setinversematrix,
       getinversematrix=getinversematrix)
}

cacheSolve <- function (x, ...) {

  m <- x$getinversematrix() # get inverse
  # is cache available?
  if(!is.null(m)){ 
    
    # acuarately compare two matrices source: https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html 
    matequal <- function(a, b)
      is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
    
    # has the matrix changed or not?
    if(matequal(x$setinversematrix(),x$getinversematrix())) { 
      
      return(m)
    
    }
    else {
     
      y <- x$getinversematrix() 
      x$setinversematrix(y) 
      m <- solve(y, ...) 
      x$setinversematrix(m) 
    
    return(m) 
    }
  }
}  
