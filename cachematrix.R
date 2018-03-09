## Put comments here that give an overall description of what your
## functions do

## An overloaded Matrix that will cache both a target matrix and a resulting inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix = NULL
        set <- function(newMatrix){
                x <<- newMatrix
                invMatrix <<- NULL
        }
        get <- function() x
        setInversedMatrix <- function(inversedMatrix) invMatrix <<- inversedMatrix
        getInversedMatrix <- function() invMatrix
        list(set = set, get = get, setInversedMatrix = setInversedMatrix, getInversedMatrix = getInversedMatrix)
}


## Returns either a previously cached inversed matrix or creates a new inversed matrix then caches before returning

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInversedMatrix()
        if(!is.null(invMatrix)){
                message("Getting cached inversed matrix.")
                return(invMatrix)
        }
        plainMatrix <- x$get()
        invMatrix <- solve(plainMatrix)
        x$setInversedMatrix(invMatrix)
        invMatrix
}
