# makeCacheMatrix creates a list containing the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(mat1 = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat1 <<- mat2
        inv <<- NULL
    }
    get <- function() mat1
    setinverse <- function(inverse) 
    inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The second function computes the inverse of the matrix and sets the value in the cache using
# the setinverse function.

cacheSolve <- function(mat1, ...) {
    inv <- mat1$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse of the matrix")
        return(inv)
    }
    data <- mat1$get()
    inv <- solve(data)
    mat1$setinverse(inv)
    inv
}
# Excuting the two functions in R returns the following results
> mat1 = rbind(c(1,0,5), c(2,1,6),c(3,4,0))
> CacheMat = makeCacheMatrix(x)
> CacheMat$get()
     [,1] [,2] [,3]
[1,]    1    0    5
[2,]    2    1    6
[3,]    3    4    0
> cacheSolve(CacheMat)
     [,1] [,2] [,3]
[1,]  -24   20   -5
[2,]   18  -15    4
[3,]    5   -4    1
