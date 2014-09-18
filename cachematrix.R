## These functions calculate the inverse of a matrix and cache 
## the results for future reference

## makeCacheMatrix creates a list with 4 functions to get and set a matrix
## and to get and set the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        #initialize ix with empty matrix
        imx     <- matrix(nrow=0, ncol=0)
        #function to set the value of the matrix
        setmx   <- function(y) {
                        x   <<- y
                        imx <<- matrix(nrow=0, ncol=0)
        }
        #function to get the value of the matrix
        getmx   <- function() x
        #function to set the value of the inverse matrix by using solve 
        setimx  <- function(solve) imx <<- solve
        #function to get the value of the inverse matrix         
        getimx  <- function() imx
        #output the functions in a list
        list(setmx  = setmx, 
             getmx  = getmx,
             setimx = setimx,
             getimx = getimx)
} #end of makeCacheMatrix


## cacheSolve calculates the inverse matrix of the vector which is created 
## by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        #see if there is cached version of the inverse matrix
        imx  <- x$getimx()
        #check whether the inverse matrix is cached by checking if its empty
        if(!(nrow(imx)==0 && ncol(imx)==0)) {
                message("getting cached inverse matrix")
                return(imx)
        }
        #calculate the inverse matrix (if its not cached)
        #get the matrix
        mtx <- x$getmx()
        #calculate the inverse
        imx <- solve(mtx, ...)
        #put the inverse in cache
        x$setimx(imx)
        #output the inverse matrix
        imx
} #end of cacheSolve

## sample code to test the results
#mx      <- matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#vmx     <- makeCacheMatrix(mx)
## first calculation which should just return the inverse
#cacheSolve(vmx)
## second calculation with should return the inverse from the cache
#cacheSolve(vmx)

