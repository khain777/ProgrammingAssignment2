## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getsolve())){
                message("getting cached data")
                return(x$getsolve())
        }else if(!is.matrix(x$get())){
                message("The object must be a matrix")
                return(NA);
        }else if(ncol(x$get()) != nrow(x$get())){
                message("The matrix must be square")
                return(NA);
        }else if(!is.numeric(x$get())){
                message("The matrix must be numeric")
                return(NA);
        }
        
        x$setsolve(solve(x$get(), ...))
        
        x$getsolve()
}
