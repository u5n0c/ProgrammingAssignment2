## The functions cache the inverse of a matrix for that the operations be faster
 
##It creates an special matrix that set the value of the inverse, 
##get the value of the inverse, set the value of the inverse and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        setMat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMat <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(setMat = setMat, getMat = getMat,
             setInv = setInv,
             getInv = getInv)
}


## It checks if the matrix inverse has been calculated before, for taking the matrix inverse of the cache.
## If the inverse hasn't been calculated previously, it calculate the inverse in the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMat()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
