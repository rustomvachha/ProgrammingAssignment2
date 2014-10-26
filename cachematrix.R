##ASSIGNMENT 2 
## makeCacheMatrix takes as input an invertible matrix 
##  the variable i is superassigned and is the inverse of the input matrix

makeCacheMatrix <- function(x) {
  if(!is.matrix(x)) stop("x must be a matrix")
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x  #funtion that simply returns value of'x' (input to makeCacheMatrix)

##function setinverse simply takes an argument passed to the funciton, say  'inverse', and assigns it to the variable 'i' using the superassigned operator  <<
  setinverse <- function(inverse) i <<- inverse     
  
#function getinverse() simply returns the value of the superassigned variable 'i'
  getinverse <- function() i                   
  
#list of set, get, setmean and getmean. This makes it easy and allows you to directly address in the form
#  x$set(), x$get(), x$setinverse(), x$getiinverse()
  list(set = set, get = get,               
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## pass an invertible matrix as an argument to makeCacheMatrix and assign this function to 'amatrix'
amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))


## cacheSolve computes the inverse of a matrix. 
## if the inverse is already in cache (via superassignment operation <<) it will return that value without recalculating the inverse
cacheSolve <- function(x) {
  i<-x$getinverse()
  if (!is.null(i)) {
    messsage("getting cached data")
    return (i)
  }
  data<-x$get()

  ## obtain the inverse of i is not already assigned
  i <- solve(data)
  x <- x$setinverse(i)
  ## retun the inverse of input 'x' 
  i
}
