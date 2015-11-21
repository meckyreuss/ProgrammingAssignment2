# 1.  `makeCacheMatrix`: This function creates a special "matrix" object
# that can cache its inverse.
# 2.  `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` retrieves the inverse from the cache.


## makeCacheMatrix creates a special "matrix" object with a list of 3 functions
## 1. getCachedInverseMatrix which returns the already calculated inverse matrix
## 2. getMatrix which returns the matrix fed into makeCacheMatrix 
## 3. setCachedInverseMatrix which sets the inverse matrix to the variable im
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        getCachedInverseMatrix <- function() im
        getMatrix <- function() x
        setCachedInverseMatrix <- function(inv_matrix) im <<- inv_matrix
        list(getCachedInverseMatrix = getCachedInverseMatrix, 
             setCachedInverseMatrix = setCachedInverseMatrix,
             getMatrix = getMatrix)
}


## cacheSolve checks if an inverse matrix has already been calculated by 'looking' into
## the 'special' matrix object created by makeCacheMatrix. If true, it returns the stored
## inverse matrix. Otherwise it calculates the inverse matrix and stores it.
cacheSolve <- function(x, ...) {
        im <- x$getCachedInverseMatrix()
        if (!is.null(im)){
                message("retrieving cached inverse matrix")
                return(im)
        }
        message("calculating inverse matrix")
        data <- x$getMatrix()
        im <- solve(data)
        x$setCachedInverseMatrix(im)
        im
}