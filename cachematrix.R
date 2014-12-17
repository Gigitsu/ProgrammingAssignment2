## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<-NULL
    }
    get <-function() x

    setInvertedMatrix <- function(invertedMatrix) im <<- invertedMatrix
    getInvertedMatrix <- function() im

    isCached <- function() !is.null(im)

    list(set = set, get = get,
         isCached = isCached,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!x$isCached()) {
        x$setInvertedMatrix(solve(x, ...))
    } else {
        message("getting cached data")
    }

    x$getInvertedMatrix()
}
