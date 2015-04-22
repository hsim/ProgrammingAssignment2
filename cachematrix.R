## cachematrix.R

## This is a program which calculates the inverse of a matrix and caches the result. 
## It contains two functions: makeCacheMatrix and cacheSolve.



## makeCacheMatrix function

## This is a constructor function which creates the set, get, setinverse and getinverse
## functions.

## As per lexical scoping rules, the values of free variables are looked up in the
## defining environment.  Therefore, free variables in the set, get, setinverse 
## and getinverse functions are looked up in the makeCacheMatrix function.

## This allows variables assigned in the makeCacheMatrix function to be "shared" 
## by the set, get, setinverse and getinverse functions.  In this case, we "share" 
## two variables: x (the input matrix) and i (the cached inverse matrix).

makeCacheMatrix <- function(x = matrix()) {                                     
## x is the input matrix, i is the cached inverse matrix
## The default value of x is given by the function argument x = matrix().
## The default value of i is NULL since the inverse has yet to be calculated.
    i <- NULL

## The set function:
## 1. Accepts y, a new input matrix.  This replace x, the old input matrix stored 
## in makeCacheMatrix.
## 2. Resets i to NULL, since i contains the inverse matrix of x (instead of y).  
    set <- function(y) {
         x <<- y
         i <<- NULL
    }    

## The get function returns x, the input matrix.
    get <- function() x

## The setinverse function accepts inverse, which is a newly calculated inverse 
## matrix.  This replaces i, the cached inverse matrix stored in makeCacheMatrix.
    setinverse <- function(inverse) i <<- inverse

## The getinverse function returns i, the cached inverse matrix.
    getinverse <- function() i

## Returns a list of the constructed functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve function

## This function checks whether there is a cached inverse matrix.  If so, it returns
## the cached inverse matrix.  If not, it calculates and returns a new inverse matrix.
## It also caches this new inverse matrix for future use.

cacheSolve <- function(x) {
## x is the list of constructed functions (set, get, setinverse, getinverse)

## Checks whether there is already a cached inverse matrix 
    i <- x$getinverse()

## If so, returns the cached inverse matrix and exits the function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

## If not, calculates a new inverse matrix
## 1. Retrieves the input matrix
    data <- x$get()
## 2. Uses the solve function to compute the inverse of a square matrix
    i <- solve(data)
## 3. Caches this result for future use
    x$setinverse(i)
## 4. Returns the newly calculated inverse matrix
    i
}
