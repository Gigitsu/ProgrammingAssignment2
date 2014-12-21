## Matrix inversion is usually a costly computation
## and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly


## The first function creates a special "matrix"
## which is a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverted matrix
##      4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <-function() x

    setInvertedMatrix <- function(invertedMatrix) im <<- invertedMatrix
    getInvertedMatrix <- function() im

    ## True if has an inverted matrix cached
    isCached <- function() !is.null(im)

    list(set = set, get = get,
         isCached = isCached,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function.
## However, it first checks to see if the inverted matrix
## has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it calculates the inverted matrix and stores it in the cache
## via the setmean function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Checks if the special matrix has a cached velue
    if(!x$isCached()) {
        #if not creates the inverted matrix and stores it
        x$setInvertedMatrix( solve(x$get(), ...) )
    } else {
        message("getting cached data")
    }

    x$getInvertedMatrix()
}
