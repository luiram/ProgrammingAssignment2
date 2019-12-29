
# The following functions are used to create a special object that stores a matrix and caches 
# its inverse. The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:

# set the value of the matrix

# get the value of the matrix

# set the value of the inverse

# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## TESTING

# Below we call the function with a matrix, compute the inverse, retrieve the inverse from 
# the cache list, change the call matrix to the inverse, compute the inverse on that and return 
# the original function:

B <- matrix(c(2,4,-3,-7),2,2)
B

B1 <- makeCacheMatrix(B)
cacheSolve(B1)

cacheSolve(B1)
