# Description of functions:-
# 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


###############################################
###############################################
## This function creates a special "matrix" object that can cache its inverse. 
# And the special "matrix" object contains the following member functions:-
# 1) getInverse :- returns the caches inverse of the matrix
# 2) setInverse :- Sets the inverse of the matrix and returns the value setted
# 3) setMatrix :- Sets the matrix, if it is not same, and returns the matrix setted
# 4) getMatrix :- retruns the current matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    getInverse <- function() inv
    setInverse <- function(inverse) inv <<- inverse
    getMatrix <- function() x
    ## Set the matrix inverse as null as well
    setMatrix <- function(y) {        
        if (!identical(x, y)) {
            inv <<- NULL
            x <<- y
        }
        x
    }
    list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
    
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache Otherwise the inverse is calculate
# and cached in makeCacheMatrix for future use.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (is.null(inv)) {
        print("Matrix inverse is not found in cache, computing inverse and save it in the cache")
        inv <- solve(x$getMatrix())
        x$setInverse(inv)
    } else {
        print("Matrix inverse is found in cache, returning it from cache")  
    }    
    ## Return a matrix that is the inverse of 'x'
    inv
}

################################################
#####MAIN CODE END HERE AND TESTING STARTS######
################################################
## For testing purpose of the functions computing correctly the matrix inverse or not
mat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2)
cached.mat <- makeCacheMatrix(mat)
print(cacheSolve(cached.mat))
print(cacheSolve(cached.mat))

## Verify if the matrix is changed, the new inverse is computed and returned
cached.mat$setMatrix(matrix(c(1,2, 11,5), nrow = 2, ncol = 2))
print(cacheSolve(cached.mat))
print(cacheSolve(cached.mat)) 

##Verify if the matrix is not changed and should not compute the inverse of the matrix
cached.mat$setMatrix(matrix(c(1,2, 11,5), nrow = 2, ncol = 2))
print(cacheSolve(cached.mat))
print(cacheSolve(cached.mat))             
