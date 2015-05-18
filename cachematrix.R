# This file contains two functions: makeCacheMatrix and cacheSolve. These
# functions compute the inverse of an invertible matrix and store the result. If
# the inverse is needed again, the stored value is returned instead of computing
# the inverse again.

# The function makeCacheMatrix initializes four functions for the input matrix.
# The set function (re)sets the matrix values and initializes the inverse to
# NULL. The get function returns the matrix. The setinverse function stores the
# inverse. The getinverse returns the value of the stored inverse. 
# When called on an input matrix, makeCacheMatrix returns a list containing the four
# functions as applied to that matrix. This list can be used as input for
# cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function (y) x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# The cacheSolve function gets the input matrix using the get function as
# defined above. It then computes the inverse of the input matrix and stores it 
# using setinverse, as defined above. It first checks if this has been done
# before, using getinverse as defined above, and in that case returns the stored
# value and a message.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


