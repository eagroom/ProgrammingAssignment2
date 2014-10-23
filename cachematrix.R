## This file contains two functions: makeCacheMatrix and cacheSolve.  
## These functions allow for the inverse of a matrix to be calculated 
## once and cached for later reuse. 
## An example of how to use these functions would be:
##      a<-makeCacheMatrix()
##      a$set(matrix(1:4,2,2)) # call the set function  of makeCacheMatrix to set the matrix
##      cacheSolve(a) # pass the instance of the  makeCacheMatrix function, 
                      # with the matrix set, into cacheSolve (via the variable a)
                      #cacheSolve will return the inverted matrix



## makeCacheMatrix expects a matrix to be passed in. It expects that the 
## matrix is invertible. It returns a list of functions that can be 
## used to set and get a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        set <- function(y) {
                x <<- y
                invertedMatrix <<- NULL
        }
        get <- function() x
        setInvertedMatrix <- function(im) invertedMatrix <<- im
        getInvertedMatrix <- function() invertedMatrix
        list(set = set, get = get,
             setInvertedMatrix = setInvertedMatrix,
             getInvertedMatrix = getInvertedMatrix)
}


## cacheSolve expects a instance of the function makeCacheMatrix to be 
## passed in with a matrix set.  It returns the inverse of the matrix.  
## The function calculates the inverse of the matrix passed in and then stores 
## it in a variable to be retrieved the next time the inverse of this matrix is 
## asked for so that it doesn't have to be recalculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        invertedMatrix <- x$getInvertedMatrix()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        data <- x$get()
        invertedMatrix <- solve(data)
        x$setInvertedMatrix(invertedMatrix)
        invertedMatrix
}

