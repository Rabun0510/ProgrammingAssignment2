## The two functions makeCacheMatrix and cacheSolve uses the computer's cache to 
## store the answer to a computationally intensive matrix operation, namely the 
## inverse of the matrix. This will ensure that the inverse of the matrix is only
## calculated once and thereafter recalled from cache when used in the rest of the 
## code, give that the matrix did not change since the last time the inverse was 
## calculated.

## The makeCacheMatrix function returns a "matrix object" which is actually a list
## of functions. These functions are set, get, setinv and getinv. They are used to
## set the matrix data (stored in cache), get the matrix data, set the inverse of
## the matrix (stored in cache) and get the inverse of the matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
        
    inv <<- NULL                    ## Initialize the inv variable to NULL when "matrix object" is created
    
    set <- function (y) {           ## The set function receives as argument the matrix data to be used in the "matrix object".
        x <<- y                     ## It stores this data in the x variable using the computer's cache.
        inv <<- NULL                ## The inv variable is set to NULL to ensure that the inverse will be recalculated when a 
    }                               ## change is made to the matrix data.
    
    get <- function () x            ## The get function simply retrieves the matrix data from cache and returns it.
        
    setinv <- function(inverse) inv <<- inverse         ## The setinv function receives as argument the inverse of the matrix. 
                                                        ## It is stored in the computer's cahce using the inv variable.
       
    getinv <- function() inv         ## The getinv function retrieves the inverse of the matrix from the computer's cache and returns it.
        
    list(set = set, get = get, setinv = setinv, getinv = getinv)        ## A list of all four functions is returned. Each list element 
                                                                        ## has the same name as the function it stores.
}


## The cacheSolve function returns the inverse of the matrix contained in the "matrix object" 'x' as created by 
## the makeCaheMatrix function. If the inverse of the matrix has already been computed, it is retrieved from the 
## computer's cache via the "matrix object".

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {                    ## This if statement check whether the inverse was previously calculated 
        message("getting inv from cache")   ## and returns the inverse matrix from cache if so.
        return(inv)
    }
    
    data <- x$get()                         ## If the inverse has not been calculated yet (or it the matrix has changed 
    inv <- solve(data)                      ## since the last call to this function), the inverse is calculated and 
    x$setinv(inv)                           ## stored in cache via the "matrix object".
    inv
}
