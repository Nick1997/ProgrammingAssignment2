## GOAL: TO CACHE INVERSE OF MATRIX
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## cacheSolve()  

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set_matrix <- function(y)
    {
        x <<- y
        inverse_matrix <- NULL
    }
    get_matrix <- function() x
    set_inverse <- function(inverse) inverse_matrix <<- inverse
    get_inverse <- function() inverse_matrix
    list(set_matrix=set_matrix, get_matrix=get_matrix, set_inverse=set_inverse,
         get_inverse=get_inverse)
}


## cacheSolve() first checks if inverse is already cached, If yes then
## it fetches from the cache and displays the inverse, Or else the inverse
## is calculated which is set to the cache for future use.

cacheSolve <- function(x, ...) {
    
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix))
    {
        message("Getting cached matrix ")
        return(inverse_matrix)
    }
    original_matrix <- x$get_matrix()
    inverse_matrix <- solve(original_matrix)
    x$set_inverse(inverse_matrix)
    
    ## Return a matrix that is the inverse of 'x'
    return(inverse_matrix)
}