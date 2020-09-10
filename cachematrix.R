## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve matrix are two functions to get the inverse of a matrix and store the result 
## so that we can retrieve it and save computation time

## Write a short comment describing this function

## makeCacheMatrix has a matrix as an input and a list as an output. chaceSolve will subset this list

makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL
  set<-function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function() x
  setinversa<-function(inv) inversa<<-inv
  getinversa<-function() inversa 
  list(set=set, get=get,
       setinversa=setinversa,
       getinversa=getinversa)
}

## Cachesolve receives the output of makecachematrix and outputs the inverse of an invertible matrix

## When the matrix has been previously inverted, cacheSolve skips the inverting process and returns the previous result
## When the matrix has not been inverted, cacheSolve computes the inverse of the matrix

cacheSolve <- function(x, ...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
}


# Testing makeCachematrix function

## Creating and solving an invertible matrix

#mimatriz<-matrix(c(4,2,7,6),2,2)

#is.matrix(mimatriz)

#solve(mimatriz)

## Using function to get matrix and its inverse

#test1<-makeCacheMatrix(mimatriz)

#test1$get()
#test1$getinversa()

#test2<- cacheSolve(test1)

#test2

## The function is able to retrieve the inverse 

