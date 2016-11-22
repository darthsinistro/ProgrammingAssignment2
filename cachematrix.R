## This function is very similiar to the original script.
## Instead of numeric, we have a matrix input to the function.

## The makeCacheMatrix function takes a Matrix and returns an object
## containing x (and it's inputted value), inv, and the 4 functions 
## in its first iteration.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMatrix) inv <<- invMatrix
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This is pretty straightforward. It calls the getinv() function
## and checks it the value is NULL or not i.e. whether the inverse has
## already been calculated and stored in the cache. If it has, then 
## it fetches the inverse and prints that. Else, it gets the 
## matrix inputted in makeCacheMatrix, calculates it's inverse,
## calls on setinv and prints the inverse matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        print("Getting the inverse from the Cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
