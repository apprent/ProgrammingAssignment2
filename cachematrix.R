## Make cache matrix and solve its inverse matrix

## Make cache matrix and save its inverse matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        
        ## Set NULL to inverse matrix (im)
        im <- NULL
        
        ## Set cached matrix and reset cached im
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ## Output matrix
        get <- function() x
        
        ## Set cached inverse matrix to inputim
        setim <- function(inputim) im <<- inputim
        
        ## Output inverse matrix
        getim <- function() im
        
        ## Make the list
        list(set = set, get = get,
             setim = setim,
             getim = getim)

}


## Check if inverse matrix exist otherwise solve it

cacheSolve <- function(x, ...) {
        
        ## Get cached inverse matrix
        im <- x$getim()
        
        ## Use cached im if not NULL
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        ## Otherwise fetch the matrix
        data <- x$get()
        ## solve im
        im <- solve(data, ...)
        ## and put it in cache
        x$setim(im)
        
        ## Output im
        im
}
