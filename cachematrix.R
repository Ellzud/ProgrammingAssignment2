## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function gets gets a matrix a creates a list of functions
##to set the value of the matrix, get the value of the matrix,
## set the value of the inverse of the matrix and get the value
##of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##This function calculates the inverse of the matrix created
##in the function above. First looks if the inverse has been already calculated 
##to get the result from cache. If not, then calculates the inverse of the matrix
##using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
