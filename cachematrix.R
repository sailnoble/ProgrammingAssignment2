makeCacheMatrix <- function(x = matrix()) { ##x is initialized as an empty matrix function arguement
AI <- NULL ## AI is set to NULL initializing it as an object to be used later
set <- function (y) {
  x  <<- y ## assigns the input arguement to the x object in the parent environment
  AI <<- NULL ## assigns the NULL value in the parent environment to AI, which clears the prior cache
}
get <- function () x ## since x is not defined within get (), R retrieves it from the parent environment
setinverse <- function(inverse) AI <<- inverse ## this assigns the input arguement inverse to the value of AI in the parent environment
getinverse <- function() AI ## defines getinverse 
list(set = set, ## gives the name 'set'to the set() function defined above
     get = get, ## gives the name 'get'to the get() function defined above
     setinverse = setinverse, ## gives the name 'setinverse'to the setinverse() function defined above
     getinverse = getinverse) ## gives the name 'getinverse'to the getinverse() function defined above 
## together the list assigns each of the functions as an element within a list and returns to the parent environment
}

cacheSolve <- function(x, ...) { ## initializes the arguement x and introduces an ellipsis that allows the caller to pass additional arguements into the function
  AI <- x$getinverse () ## retreives the inverse of the object passed in as the arguement
  if(!is.null(AI)) { ## checks to see if the result is NULL
    message ("getting cached data") ## if it is NULL this message is returned 
    return (AI)
  }
  data <- x$get() ## gets the matrix from the input object
  AI <- solve (data, ...) ## calculates the inverse of the matrix
  x$setinverse (AI) ## uses the setinverse function to set the inverse of AI
  AI ## returns the value to the parent environment by printing
 
}
