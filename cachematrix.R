## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x  #Function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%%x  #Function to obtain inverse of the matrix
  }
  list(set = set, get = get,setinv = setinv, getinv = getinv)


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  inv<-x$getinv()
  if(!isnull(inv)){
    message("Getting cached data") #checking if inverese is NULL
    return(inv) #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)  #Calculates inverse value
  x$setinv(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}

