## MakeCacheMatrix first declares a null variable to store the inverse matrix
## following the format of the example, the function then defines functions to
## set a matrix, output the currently cached matrix, find the inverse of the matrix
## and cache the matrix inverse.

## returns a list of functions used to calculcate and cache a matrix inverse,
## using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    
    InvMat <- NULL
    
    set <- function(y) {
        x <<- y
        InvMat <<- NULL
    }
    
    getMat<- function() x
    setInv<- function(Inv) InvMat <<- Inv
    getInv<- function() InvMat
    
    list(set = set, getMat = getMat,
         setInv = setInv,
         getInv = getInv)

}


## if the inverse is not already found in the makeCacheMatrix object, the function
## uses the solve function to find an inverse, and cache it

cacheSolve <- function(x, ...) {
    
    InvMat<-x$getInv()
    
    if(!is.null(InvMat)) {
        message("Inverse found")
        return(InvMat)
    }

    Mat<- x$getMat()
    InvMat<-solve(Mat,...)
    x$setInv(InvMat)
    
    InvMat
        ## Return a matrix that is the inverse of 'x'
}
