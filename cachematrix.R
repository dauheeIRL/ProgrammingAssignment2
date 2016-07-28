## This R script is submitted for peer review for "Programming Assignment 2" for R Programming module in Coursera
## Create date: 28/07/2016

## this function requires an invertible matrix to be passed in
## It returns a list containing properties to get/set a matrix, also
## to get/set the inverse matrix this function must be used in conjunction with cacheSolve
## Note the use of the <<- operator to work on data/vectors outside the scope of the 
## functions immediate environment (through lexical scoping)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL

  setfun <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getfun <- function() x
  setcache <- function(matx) m <<- matx
  getcache <- function() m
  
  list(set = setfun, get = getfun,
       setcache = setcache,
       getcache = getcache)
  
}

## function that works in conjunction with makeCacheMatrix function to get/set the inverse
## of a matrix. When the inverse has been requested for the first time it is stored. for each
## subsequent request for the inverse, a cached matrix is retrieved
cacheSolve <- function(x, ...) {
 
  m <- x$getcache()
  
  if(!is.null(m)) {
    message("returning cached matrix")
    return(invisible(m))
  }
  
  data <- x$get()
  message("calculating matrix inverse for first time")
  m <- solve(data, ...)
  x$setcache(m)
  
  invisible(m)
  
}

# example below for testing this this code
test<-function(){
  
  x<-matrix(c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0) , ncol=5, byrow=TRUE)
  y<-matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
  
  myCache <- makeCacheMatrix(x)
  
  cacheSolve(myCache)
  cacheSolve(myCache)
  cacheSolve(myCache)
  cacheSolve(myCache)
  
}
