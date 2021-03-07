## This script receives an invertible matrix, generates an inverted matrix and stores
## this cached result, so that whenever we need to recover again
## this inverted matrix, we don't need to perform the inversion calculations again
## because we will already have these results in cache. It is important to note that this phase
## recalculating the matrix will only be avoided if the original matrix does not change

## If necessary I use this matrix for tests:
## mat_test_01 <- matrix(c(1,0,0,2,1,0,3,4,1), nrow = 3, ncol=3)

## My first function (makeCacheMatrix ()) receives an invertible matrix and
## generates an inverted matrix from the original matrix. This function also
## generates 4 lists that are used by my second function (cacheSolve ()).
makeCacheMatrix <- function(x = matrix()) {
    mat_i <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        mat_i <<- NULL
    }
    
    setsolve <- function(solve) mat_i <<- solve
    getsolve <- function() mat_i
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
## for tests, I create tis object for use in the second function:
## mat_cached <- makeCacheMatrix(mat_test_01)


## This second function, receives the object created through the makeCacheMatrix () function,
## containing your lists and verifies that the original matrix has not changed,
## in this case, it will not perform the inversion again and will only return the matrix
## inverted already cached, otherwise, that is, if your matrix
## original has been changed, the cacheSolve () function will execute
## again the inversion of this new matrix.
cacheSolve <- function(x, ...) {
    mat_i <- x$getsolve()
    if(!is.null(mat_i)) {
        message("getting cached data")
        return(mat_i)
    }
    data <- x$get()
    mat_i <- solve(data, ...)
    x$setsolve(mat_i)
    mat_i
}
## For the last test, I use this comando bellow:
## cacheSolve(mat_cached)

## Thanks for your time :)
## Keep in touch: Instagram: @charbavito