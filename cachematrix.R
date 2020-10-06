## R Programming - Week 3 Assignment

#First we create the function which creates the matrix and sets the 's' value to _NULL_
#Next we create the function which sets the matrix and adds it to the global environment and resets the 's' value to NULL in the global environment.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {   
      x <<- y            
      s <<- NULL         
    } 

    #We then use the 'get' function to retrieve [x] (the matrix) from the local environment. 
    #Then we create two more functions 'setinverse' and 'getinverse' as anonymous arguments 
    
    get <- function() x    
    setinverse <- function(matrix) s <<- matrix
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  } 

# The cacheSolve function is defined within the matrix object created by 'makeCacheMatrix'
# this function checks if the inverted matrix exists within the object and either returns it or calculates it and returns it.

cacheSolve <- function(x, ...) {
    
    s <- x$getinverse() #retrieves the matrix from the object. 
    if(!is.null(s)) {  #if matrix is inverted, retrieves inverted matrix with the message.
      message("getting inverted matrix")
      return(s)
    }
    data <- x$get() #if not, it gets the matrix 
    s <- solve(data, ...) #and solves the matrix. 
    x$setinverse(s) #and returns the solved matrix. 
    s
}
