# Matrix inversion is computationally intensive. One approach to mitigate the issue is to store the precomputed
# inverse and check if the operation has been done earlier each time before computing the inverse.

# the function makeCacheMatrix generates a list of Getters and setters that would get 
# and set the values of the  matrix and its inverses respectively

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invx <<- inverse
        getinverse <- function() invx
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function cacheSolve calcuates the inverse of the matrix under the assumption that the matrix 
# given as input is always non-singular.
# the function first checks if the inverse is already calculated. If it is, then it retrieves the cached 
# inverse (indicated by the message )

cacheSolve <- function(x, ...) {
        invx <- x$getinverse()
        if(!is.null(invx)) {
                print("Retreiving cached inverse.")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setinverse(invx)
        invx
}

# Example result:
# > x = matrix(c(4,0,0,4),nrow=2,ncol=2)
# > m = makeCacheMatrix(x)
# >  m$get()
#      [,1] [,2]
# [1,]    4    0
# [2,]    0    4
# Solving the Inverse in the first run
# cacheSolve(m)
#      [,1] [,2]
# [1,] 0.25 0.00
# [2,] 0.00 0.25
# Retrieving from the cache in the second run
# > cacheSolve(m)
# [1] "Retreiving cached inverse."
#      [,1] [,2]
# [1,] 0.25 0.00
# [2,] 0.00 0.25
# > 
