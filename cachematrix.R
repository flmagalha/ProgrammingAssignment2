## makeCacheMatrix is a function that creates a list of 4 other functions in each position of the list. The argument is a matrix. It caches 
## the inverse matrix ('im') of the matrix inputed ('x'). It also caches the inputed matrix 'x'. The inverse matrix 
## is calculated by another function named cacheSolve.

## makeCacheMatrix
## --The first function of the list is named 'set'. It caches the input, defined to be a matrix, and defines/caches 
## the inverse matrix ('im') as NULL.
## --The second function is named 'get'. It just returns the inputed matrix 'x'. 
## --The third function is named 'setinv'. It caches the inverse matrix ('inv') into 'im', but doesn't calculate 
## the inverse matrix. 
## --The fourth function is named 'getinv'. It just returns the inverse matrix 'im' already cached.
## --You can call each function of makeCacheMatrix by subsetting the list using the name of the function 
## in the list (4 functions).

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinv <- function(inv) im <<- inv
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##cacheSolve
## --This function verifies if there is any inverse matrix cached.If there is, then it returns a message 
## explaining that the returned matrix was obtained from cache. If there isn't, then it calculates the inverse matrix 
## of 'x', cached by makeCacheMatrix.
## --Thereafter, it caches the inverse matrix 'im' obtained using the third function of makeCacheMatrix.    

cacheSolve <- function(x, ...) {
        im <- x$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}
