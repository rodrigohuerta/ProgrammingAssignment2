## Put comments here that give an overall description of what your
## functions do

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

mimatriz<-matrix(c(4,2,7,6),2,2)

print(mimatriz)

solve(mimatriz)

makeCacheMatrix(mimatriz)

solve(mimatriz)

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


