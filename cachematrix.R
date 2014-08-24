## Given the example code originating from 'makeVector' change the types
# from numeric to matrix; change value names where approps. 
# Set/Get the value of the matrix; then use solve()  to get the inverse
# Finally get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Given the original 'cacheman' function; use the new vectors from above
# 'getmatrix'/'setmatrix'; check to see if the value is good (not NULL) and
# retrieve 'm'; else solve for the inverse and then return m with that value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        
        matrix <- x$get()
        
        m <- solve(matrix, ...)
        x$setmatrix(m)
        
        m
}
