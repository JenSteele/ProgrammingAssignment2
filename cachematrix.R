## My functions create an environment that will return the inverse of a specified matrix 
## if it has been found before, otherwise it will caclulate the inverse of the matrix

## This function creates a vector of {set,get,setinverse,getinverse} that each perform 
## a function on the matrix specified in the next function (cacheSolve())

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y  #Here x is specified to be equal to y within the set() function
    m <<- NULL  #Here m is specified to be equal to NULL within the set() function
  }
  get <- function() x    #prints the matrix
  setinverse <- function(solve) m <<- solve  #finds the inverse of the matrix
  getinverse <- function() m    #prints the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve is designed to find the inverse of the matrix.  If the inverse has been found
## previously (!is.null(m)) the previously found inverse is returned.  If the inverse has 
## not been found previously the inverse is calculated using the 'solve()' function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()   #m is defined as the inverse of the matrix called
  if(!is.null(m)) {   #if m is not NULL (i.e. if the inverse has already been found) the 
    #value of the inverse is simply printed from the memory (with the message)
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m   #otherwise m is calculated as the inverse of the matrix
}
## Return a matrix that is the inverse of 'x'
