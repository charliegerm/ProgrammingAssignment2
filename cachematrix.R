## Put comments here that give an overall description of what your
## functions do

## Accept and input matrix, determine the inverse, and create a cache containing that inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
  cacheMatrix <- NULL
  
  set <- function(mySet) {
    inputMatrix <<- externalMatrix
    cacheMatrix <<- NULL
  }
  
  get <- function() cacheMatrix
  setInverse <- function(inverse) cacheMatrix <<- externalMatrix
  getInverse <- function() cacheMatrix
  
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
  
}

## Accept an input matrix, determine if its inverse is contained in a local cache. If so, return the cache copy. 
## If cache copy is dirty, refresh the cache and return the newly created inverse matrix

cacheSolve <- function(inputMatrix, ...) {
  cacheMatrix <- inputMatrix$getInverse
  if (!is.null(cacheMatrix)) {
    return(cacheMatrix)
    
  }
  
  myMatrix <- inputMatrix$get()
  cacheMatrix <- solve(myMatrix, ...)
  inputMatrix <- setInverse(cacheMatrix)
  
  cacheMatrix
  
}
