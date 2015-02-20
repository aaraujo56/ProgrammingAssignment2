## INVERSE MATRIX CACHING ASSIGNMENT

## Functions derived from *makeVector* and *cachemean* examples in assignment 2, 
## Coursera R Programming

## This pair of functions sets up the cache for an inverse matrix calculation
## and a function to set and use the cached inverse matrix. Since an inverse
## matrix calculation is a processing intensive calculation, there are
## efficiency gains in pulling a cached result if there are no changes to the
## source matrix

## makeCacheMatrix creates a list of functions which use their local environment
## to store the cached inverse matrix, as well as create the functions to get 
## and set both the source matrix and the resulting inverse matrix.

## x is the source matrix from which the inverse will be calculated

makeCacheMatrix <- function(x = matrix()) {
    ## declare the caching variable for inverse matrix value inside 
    ## top function environment and assign it a NULL value
    invm <- NULL
    
    ## setter function
    set <- function(y) {
        ## set the new source matrix in parent function environment
        x <<- y
        ## clear cached value of inverse matrix in parent function environment
        ## to NULL when setting new source matrix
        invm <<- NULL
    }
    ## getter function: get current source matrix stored in the function
    ## environment
    get <- function() x
    
    ## setter function: sets new inverse matrix and assigns to caching variable
    ## in parent function environment
    setinverse <- function(newivm) invm <<- newivm
    
    ## getter function: gets cached inverse matrix
    getinverse <- function() invm
    
    ## return list with all the functions that can be used to set and get
    ## source matrix and cached inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is the worker function that returns the cached inverse matrix for
## a particular matrix. It only runs the solve function to calculate the 
## inverse the first round trip to the cached variable

## Requires x, a list created by makeCacheMatrix which contains all functions
## to access the cached inverse matrix

cacheSolve <- function(x, ...) {
    ## invm is declared and an attempt is made to get the inverse matrix
    invm <- x$getinverse()
    
    ## if there is a cached inverse matrix, use it to return the result
    ## and stop the function from going further
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    ## otherwise get the source matrix in the list from x
    data <- x$get()
    
    ## run the solve function to compute the inverse of data, passing 
    ## along any other variables that came along with the cacheSolve call
    invm <- solve(data, ...)
    
    ## save the resulting inverse matrix to the cache using the setter function
    x$setinverse(invm)
    
    ## Return a matrix that is the inverse of x
    invm
}
