## These funtions are designed to save calculation time for repeted calculations
## to find the inverse of a matrix. The function assumes that the inverse for the
## given matrix exists.

## This function takes in a matrix and creates a new "object" with get, set
## getinverse, setinverse methods and stores them in a list. These are all
## separetely callable with the $ notation. Also m is set to Null for the
## initiation of the "object".

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This is the function where the inverse of the matrix is calculated with solve()
## function, stored in the m and is checked if m is null or contains a solution
## already. If yes then no calculation is repeated and the stored result in m is
## returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
