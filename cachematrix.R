## The following two functions are used to create a matrix and calculate its inverse. 
# If the matrix isn't changed then its inverse value will be called via cache memory (through lexical scoping)


## The makeCacheMatrix function assigns the values passed to it into a matrix. 
# It also initiates a variable 'i' as a 'NULL' object that may be used to cache the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL # flushes the inverse value i and sets it to Null
      
      # Use the following 'set' method to assign values to the matrix 
      set <- function(y) {
          x <<- y
          i <<- NULL
      }
      
      get <- function() x
      # The following setinverse functions ensures that a 'solve' method was called...
      # to calculate the matrix inverse.
      setinverse <- function(solve) i <<- solve # store the result (inverse) of 'solve(x)' in 'i'. 
      getinverse <- function() i # retrieves the stored inverse, or errors if that doesn't exist
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'. 
# If the inverse value already exists within the makeCacheMatrix$i, it just returns that via Lexical Scoping. 
# Other wise it calculates the inverse using the 'solve' function.
# Note: the cacheSolve function assumes the makeCacheMatrix$set method will be used to create matrices.
# Therefore, it doesn't check for a change in the matrix again.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        # if the inverse exists in cache, print a message and pull the inverse value.
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        # Otherwise, calculate and return the inverse.
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}



## Use case example: 
# x <- matrix(1:4,2,2)
# mat <- makeCacheMatrix(x)
# cacheSolve(mat)
# cacheSolve(mat)
# 
# x2 <- x+3
# mat$set(x2)
# cacheSolve(mat)
# cacheSolve(mat)
