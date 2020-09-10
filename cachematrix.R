## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve matrix are two functions to get the inverse of a matrix and store the result 
## so that we can retrieve it and save computation time

## Write a short comment describing this function

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

test4<-makeCacheMatrix(mimatriz)

test4$get()
test4$getinversa()

test5<- cacheSolve(test4)

test5

# Testing makeCachematrix function

## Creating and solving example matrix

mimatriz<-matrix(c(4,2,7,6),2,2)

is.matrix(mimatriz)

solve(mimatriz)

## Using function to get matrix and its inverse

test<-cacheSolve(mimatriz)

test$get()
  
test$getinversa()

## The function is able to retrieve the inverse 

# Testing cacheSolve function

## We use the same example

test2<-cacheSolve(matrix(c(4,2,7,6),2,2))


cacheSolve1 <- function(x, ...) {
  inversa <- x[[getinversa()]]
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
}

test3<-cacheSolve1(matrix(c(4,2,7,6),2,2))


# Using Makevector

rm(list = ls())


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

# Testing cachemean and cmakevector functions

mivector<-c(1:100)

makeVector(mivector)
cachemean(mivector)

# Error produced in cachemean function: $ operator is invalid for atomic vectors

cachemean1 <- function(x, ...) {
  m <- getmean(x)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- get(x)
  m <- mean(data, ...)
}

cachemean1(mivector)

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL
  set<-function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function() x
  setinversa<-function(inv) inversa<<-inv
  getinversa<-function(){
    solve(x)
  }
  list(set=set, get=get,
       setinversa=setinversa,
       getinversa=getinversa)
}