## Put comments here that give an overall description of what your
## functions do

## Save de value matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL   # sets the value of m to NULL 
  set<-function(y){
    x<<-y
    m<<-NULL    # sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,    # creates a list to house the four functions
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){      # Need to compare matrix to what was there before
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()       # run the getmatrix function to get the value of the input matrix
  m<-solve(matrix, ...)   # compute the value of the inverse of the input matrix
  x$setmatrix(m)          # run the setinverse function on the inverse to cache the inverse
  m     # return the inverse
}

