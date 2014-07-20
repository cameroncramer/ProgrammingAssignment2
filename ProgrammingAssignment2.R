makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){							#Sets the value of the matrix
  x<<-y
  m<<-NULL
}
get<-function() x							#Gets the value of the matrix
setmatrix<-function(solve) m<<- solve		#Sets the value of the inverse matrix
getmatrix<-function() m						#Gets the value of the inverse matrix	
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()						#Checks if matrix inverse already calculated
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()							#Calculates matrix inverse if not done already 
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m										#Returns the matrix inverse