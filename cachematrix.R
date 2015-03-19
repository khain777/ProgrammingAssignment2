## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## set: A new matrix that replaces the original matrix. This function sets the value of m to NULL,
## In this way, cacheSolvw will compute the inverse of the new special matrix
## get: Returns the original matrix of makeCacheMatrix
## setsolve: This function saves the result of solve (the inverse matrix) in m
## getsolve: Returns the inverse of the original matrix (m)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## If the object isn't a numeric square matrix, then the cacheSolve returns a NA value
## Additionaly, if the determinat (impossible to invert) of the matrix is 0, the cacheSolve returns NA


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getsolve())){
                message("getting cached data")
                return(x$getsolve())
        }
        
        
        if(!is.matrix(x$get()) || ncol(x$get()) != nrow(x$get()) || !is.numeric(x$get())){
                message("The object must be a numeric square matrix")
                return(NA)
        }
        
        if(det(x$get()) == 0){
                message("Determinant: 0. It's impossible to invert")
                return(NA) 
        }
        
        x$setsolve(solve(x$get(), ...))
        
        x$getsolve()
}
