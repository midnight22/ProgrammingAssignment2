## These functions cache the inverse of a matrix. If the inverse of the matrix has been calculated, its value is returned; otherwise the inverse is calculated and printed as well as stored in the cache.

## makeCacheMatrix creates a special "matrix" object in the form of a list containing a function to (1) set the value of the matrix, (2)get the value
##of the matrix, (3) set the value of the inverse matrix, and (4) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #make m null
        set <- function(y) {
                x <<- y
                m <<- NULL
        } #defines function that assigns y to x and make m Null again
        get <- function() x # function that prints value of x
        setinverse <- function(inverse) m <<- inverse #function that assigns input (mean) to m in makeCacheMatrix
        getinverse <- function() m # function that prints value of m
        list(set = set, get = get,  
             setinverse = setinverse,
             getinverse = getinverse) #returns list
}


## cacheSolve computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #set value of m to value of 'getinverse' from previous function
        if(!is.null(m)) { #if m is not null  (i.e. inverse has been calculated) return value of m 
                message("getting cached data")
                return(m)
        }
        data <- x$get() #if inverse has not been calculated, get value of matrix and assign to 'data'
        m <- solve(data, ...) #calcutes inverse of 'data'
        x$setinverse(m) #stores inverse as 'setinverse'
        m #print inverted matrix
}
