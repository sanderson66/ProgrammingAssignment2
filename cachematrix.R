
##  Two functions provided below facilitate caching a matrix and it's inverse


##  MakeCacheMatrix: This function returns a vector of operations
##    set()         Set the cached matrix
##    get()         get the cached matrix
##    setInverse()  set the inverse matrix
##    getInverse()  get the inverse matrix
##
##  Usage:
##  >m <- matrix(c(1,3, 5, 7), nrow = 2, ncol = 2, byrow = TRUE)
##  >f$set(m)
##  >f$set(solve(m))
##  >f$getInverse()
makeCacheMatrix <- function(x = matrix()) {
    ## storage for cached matrix
    cachedMatrixInverse <- NULL
    cachedMatrix<-x

    set <- function(y) {
        cachedMatrix <<- y
        ## throw away any inverse caclulated on a previous
        cachedMatrixInverse <<- NULL
    }
    #define atomic operations
    get <- function() cachedMatrix
    setinverse <- function(MatrixInverse)cachedMatrixInverse <<- MatrixInverse
    getinverse <- function() cachedMatrixInverse

    # return a vector of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  cacheSolve: This function computes the inverse of the "matrix"
##  using the the makeCacheMatrix above. If the inverse has already been
##  calculated (and the matrix has not changed), then the cachesolve should
##  retrieve the inverse from the cache rather than recalcualting
##
##  Usage:
##  >m <- matrix(c(1,3, 5, 7), nrow = 2, ncol = 2, byrow = TRUE)
##  >cm=makeCacheMatrix()
##  >cacheSolve(cm)

cacheSolve <- function(x, ...) {

    ##  See the cached matrix is alrady cached
    inverseData <- x$getinverse()
    if(!is.null(inverseData)) {
        message("getting cached data")
        return(inverseData)
    }

    # we need to caclulate the inverse and cache it
    data <- x$get()
    inverseData <- solve(data, ...)
    x$setinverse(inverseData)

    ## Return the cached inverse matrix
    inverseData
}


##test
# > m1 <- matrix(c(1,3, 5, 7), nrow = 2, ncol = 2, byrow = TRUE)
# > cm<-makeCacheMatrix()
# > cm$get()
# [,1]
# [1,]   NA
# > cm$set(m1)
# > cm$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    5    7
# > cacheSolve(cm)
# [,1]   [,2]
# [1,] -0.875  0.375
# [2,]  0.625 -0.125
# > cacheSolve(cm)
# getting cached data
# [,1]   [,2]
# [1,] -0.875  0.375
# [2,]  0.625 -0.125
# > m2 <- matrix(c(2,4, 6, 8), nrow = 2, ncol = 2, byrow = TRUE)
# > cm$set(m2)
# > cacheSolve(cm)
# [,1]  [,2]
# [1,] -1.00  0.50
# [2,]  0.75 -0.25
# >
#

