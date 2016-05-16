## My solution to the assignment 2.

## Creates a matrix object (list) with access functions.

makeCacheMatrix <- function(mx=matrix())
{
    minv <- NULL
    set <- function(my){
        mx <<- my
        minv <<- NULL
    }
    
    get <- function() mx
    setSolve <- function(minv_new) minv <<- minv_new
    getSolve <- function() minv
    
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
    
}


## Returns a matrix that is inverse of mx.

cacheSolve <- function(mx,...)
{
    minv <- mx$getSolve()

    if (!is.null(minv))
    {
        message("getting cached matrix inverse.")
        return(minv)
    }
    minv <- solve(mx$get(),...)
    mx$setSolve(minv)    
    minv    
}
