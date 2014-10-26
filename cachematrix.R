##ASSIGNMENT 2 
makeCacheMatrix <- function(x) {
  if(!is.matrix(x)) stop("x must be a matrix")
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x  #funtion that simply returns value of'x' (input to makeCacheMatrix)
  setinverse <- function(inverse) i <<- inverse     #function that takes an argument 'inverse' and assigns it to variable 'i'
  
  getinverse <- function() i                   #function that simply returns the value of super-assigned variable 'i' 
  
  list(set = set, get = get,               #list of set, get, setmean and getmean. This makes it easy and alllws you to directly address in the form 
                                           #   amatrix$setmean(), amatrix$getmean(), amatrix$set(), amatrix$get()
       setinverse = setinverse,
       getinverse = getinverse)
  
}

amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

cacheSolve <- function(x) {
  i<-x$getinverse()
  if (!is.null(i)) {
    messsage("getting cached data")
    return (i)
  }
  data<-x$get()
  i <- solve(data)
  x <- x$setinverse(i)
  i
}
