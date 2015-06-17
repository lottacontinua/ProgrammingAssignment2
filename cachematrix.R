### Assignment 2 ###

# Acknowledgement: Please note that the code is heavily inspired by the example given

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## more precisely it generates a list that contains for functions that can be called by another function
# set: changes the matrix stored in the main function
# getdata: retreives the matrix fed into the main function
# setinv: stores the inverse in the cache
# getinv: retreives the inverse if it is already in cache or it is NULL
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL      # I am assuming that this is helpful if you feed in a new matrix 
                # as a second argument to cacheSolve, however until the end I did not understand 
                # properly why this is really necessary or how we would specify an if condition
                # to first check if the new matrix is different from the old one
                # i.e. I'd be thankful for some feedback on that. 
        }
        getdata <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, 
             getdata = getdata, 
             setinv = setinv, 
             getinv = getinv)
        
}

# cacheSolve:  retrieves the inverse from the cache if the inverse has already been calculated (and the original matrix has not changed), 
# If this is the correct inverse is not yet stored in cache it computes the inverse and stores it in the cache
cacheSolve <- function(x, ...) {
        x$set()
        m <- x$getinv()
        if(!is.null(m)) {
                message("Wait a minute, I have it in cache")
                return(m)
        } else {
                data <- x$getdata()
                m <- solve(data, ...)
                x$setinv(m)
                m
        }
}