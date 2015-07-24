###########################################################
# Assignment: Caching the Inverse of a Matrix
###########################################################

# Matrix inversion is usually a costly computation and there
# may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also 
# alternatives to matrix inversion that we will not discuss
# here).

# Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.

# Write the following functions:

#   makeCacheMatrix: This function creates a special "matrix"
#                    object that can cache its inverse.
#   cacheSolve: This function computes the inverse of the
#               special "matrix" returned by makeCacheMatrix
#               above. If the inverse has already been
#               calculated (and the matrix has not changed),
#               then the cachesolve should retrieve the 
#               inverse from the cache.

# Computing the inverse of a square matrix can be done with
# the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is 
# always invertible.

makeCacheMatrix <- function(X = matrix()) {
    # Initializing
    invers <- NULL
    
    set <- function(Y) {
        X <<- Y
        invers <<- NULL
    }
    
    get <- function() X
    setinvers <- function(inverse) invers <<- inverse
    getinvers <- function() invers
    
    list(set = set, get = get, setinvers = setinvers,
         getinvers = getinvers)
}


cacheSolve <- function(X, ...) {
    
    invers <- X$getinvers()
    
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    
    data <- X$get()
    inverse <- solve(data)
    invers <- X$setinvers(inverse)
    invers
}

###########################################################
#Experiment to try if it works
###########################################################

X <- matrix(rpois(25,3),nrow=5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

Z <- matrix(rpois(25,2), nrow = 5, ncol = 5)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

Y <- matrix(rpois(24,7), nrow = 3, ncol = 3)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)
