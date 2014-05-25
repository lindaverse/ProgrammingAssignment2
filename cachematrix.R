## Functions to calculate the invese of a matrix and cache the result. 

## Function takes a matrix and returns in a list the functions needed for caching inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
             getInverse = getInverse)
}


## Calculates and caches the inverse of a Matrix. 

cacheSolve <- function(x, ...) {
        inverseMatrix <-  x$getInverse()
        if(!is.null(inverseMatrix)) {
                message("Getting cached Matrix.")
                return(inverseMatrix)
        }
        matrix <- x$getMatrix()
        inverseMatrix <- solve(matrix, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
