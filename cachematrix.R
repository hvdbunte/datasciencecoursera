## In the makeCacheMatrix function the previous matrix is stored.
## In the cacheSolve function the inverse of a matrix is given.

## The makeCacheMatrix function stores the matrix in the set function.
## and it returns the matrix with the get and the getmatrix functions
## The setmatrix function is used in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}

## Return a matrix that is the inverse of the matrix in
## the makeCacheMatrix function. If the current matrix is 
## the same as the previous then the cached answer is shown,
## as stored in the setmatrix function.

cacheSolve <- function(x, ...) {

        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

