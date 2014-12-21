##The makeCacheMatrix and cacheSolve function work together to cache potentially time-consuming computations.
##In this case we want to caculate the inverse of a matrix.
##This code will cache the value of the inverse so it doesn't have to be recomputed when needed.
##This is based on lexical scoping in R.
##We use the <<- operator to assign a value to an object in an environment that is different from the current environment.

## The function makeCacheMatrix will create a special "matrix" object, actually a list, that can cache the original matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL #the value of s is null as a default
        set <- function(y) { #function for setting the value of the matrix
                x <<- y #notice the <<- operator 
                s <<- NULL
        }
        get <- function() x #function for getting the value of the matrix
        setinverse <- function(solve) s <<- solve #function for setting the value of the inverse
        getinverse <- function() s #function for getting the value of the inverse
        list(set = set, get = get, #a list containg the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}

##The function cacheSolve will return a matrix, which is the inverse of "x".
#It will call he new matrix returned by the makeCacheMatrix function, but it first checks if the inverse has already been calculated.
##If so, it just gets the inverse from the cache without calculating it again.
##If the input is new, it calculates the inverse of the data and sets the inverse in the cache via the setinverse function. 

cacheSolve <- function(x=matrix(), ...) {
    s<-x$getinverse() #gets the inverse of the matrix "x" 
    if(!is.null(s)){ #checks if the inverse has already been calculated, if yes:
      message("getting cached data")
      return(s) #this returns the inverse 
    }
    # if it has not been calculated:
    matrix<-x$get() #uses the get function to get the value of the initial matrix
    s<-solve(matrix, ...) #calculates the value of the initial matrix
    x$setinverse(s) #uses the setinverse function to cache te inverse
    s #returns the inverse
}

