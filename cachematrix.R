## Coursera R Programming Week 3 Assignment, 05.21.2020, EM
## Compute the inverse of a square matrix using the functions makeCacheMatrix() and cacheSolve()
## Create a special matrix object with makeCacheMatrix() by passing a n x n, invertible matrix as its argument
## Pass this special object as an argument to cacheSolve to compute the inverse or return cached inverse if already computed

## Function that creates a special "matrix" object than can cache its inverse.
## Assume matrix, x, is a square, n x n, invertible matrix.
## Create a special matrix object with makeCacheMatrix(),
## by passing a n x n, invertible matrix as its argument:
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Compute the inverse of of the special "matrix" returned by makeCacheMatrix(),
## If the inverse has already been calculated, return the cached value.
## Pass this special object returned from makeCacheMatrix() as an argument to cacheSolve(),
## to compute the inverse or return the cached inverse if already computed:
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## Check function with below matrix (remove first set of ## on each line below):
## my_matrix <- matrix(1:4, 2, 2)  ##create an invertible n x n matrix
## store_my_matrix <- makeCacheMatrix(my_matrix)  ## Use makeCacheMatrix function to create an object that caches the matrix and functions from mackeCacheMatrix() in a list
## cacheSolve(store_my_matrix) ## Compute the inverse of the special "matrix" returned by makeCacheMatrix()
## cacheSolve(store_my_matrix) ## Rerun and see that the inverse has already been calculated and it returns the cached value




