## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## Setting the matrix
        set <- function(matrix){
          x <<- matrix
          i <<- NULL
        }
        ## Getting the matrix
        get <- function() x
        ## Setting the inverse of the matrix
        setInverse <- function(inverse) i <<- inverse
        ## Getting the inverse of the matrix
        getInverse <- function() i
        ## Return
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## See if the inverse has already been calculated or not
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        ## Getting the matrix
        data <- x$get()
        ## Calculating the inverse
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}

## A pair of functions that cache the inverse of a matrix