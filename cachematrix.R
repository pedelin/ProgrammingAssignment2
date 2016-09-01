## Caching the inverse of a matrix

## Creating a list of funcitons

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInv = setInv,
             getInv = getInv)
}


## Function computing inverse of Matrix if not cached, then cahced inverse is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInv(m)
        m
        
}

