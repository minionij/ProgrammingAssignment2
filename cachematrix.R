## makeCacheMatrix function makes a list of functions which are used to cache a inverse of a matrix
## using cacheSolve we first determine if the inverse of the particular matrix has been calculated before is not inverse is calculated using solve
## if it has been it returns the value from the cache



makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs
}
matr=makeCacheMatrix(matrix(2:5,nrow=2,ncol = 2)) 
cacheSolve(matr)
