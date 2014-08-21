## Put comments here that give an overall description of what your
## functions do

## There are two functions here. 
## 1. The first function, makeCacheMatrix, creates a special kind of matrix(CacheMatrix). The one which can cache its own inverse (once computed (AND PROVIDED)). This kind of a matrix
## also contains helper functions to set the data (YES, you can dynamically change its data. At which point the cached inverse is invalidated), get the data, set the inverse and get the inverse.
## 2. The Second function, cacheSolve, takes as input, a CacheMatrix. And sees if its inverse is already computed, if so it just returns the inverse. On the other hand, if the inverse 
## is not there, it computes the inverse, and does two additional things. 
## a. Stores the inverse back in the CacheMatrix
## b. Returns the inverse to the caller



## Write a short comment describing this function
## This is the function, 1, described above. It takes as input a standard matrix, and returns the interface (i.e., set of functions) that can be invoked on it. These being
## set(x) -> To reset the data content of the CachedMatrix, to the one that is provided. At which point the inverse is also invalidated(set to NULL). 
## get() -> returns the dataContent of the CacheMatrix Object
## getInv() -> returns the STORED inverse. Note that the inverse is NOT computed. It just returns whatever is stored. And this would be null, till the time someone calls the setInv with a non
## non NULL input. This function should ONLY be called by the cacheSolve function described below. End users should NOT be calling this to get inverse. 
## setInv(inv) -> Stores the given inverse, within itself
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    
    set <- function(y){
        inv<<-NULL
        x<<-y
    }
    get <- function(){
        x
    }
    setInv <- function(computedInverse){
        inv <<- computedInverse        
    }
    getInv <- function(){
        inv
    }
    # return the list of functions created. This could be called as the Interface of a CacheMatrix.
    list(set=set, get=get, setInv=setInv, getInv= getInv)
    
}


## Write a short comment describing this function
## This is the function, 2, described at the top. This is the function called for getting the computed Inverse, and end users should be calling this.
## This takes a CacheMatrix as input. And looks up its inverse. And sees if its inverse is already computed, if so it just returns the inverse. On the other hand, if the inverse 
## is not computed, it computes the inverse, and does two additional things. 
## a. Stores the inverse back in the Cache Matrix
## b. Returns the inverse to the caller

cacheSolve <- function(x, ...) {
        ## Note that, here x is a CacheMatrix!
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInv()
        if (!is.null(inv)){
            message("Getting Inverse from Cache")
            return(inv)
        }
        
        mat<-x$get()
        message("Inverse Not in Cache. Computing the Inverse")
        inv<-solve(mat, ...)
        ## It is assumed that the Matrix provided is always INVERTIBLE
        x$setInv(inv)
        inv
}

## I just used the below function to test the code. You could uncomment this and run it, and look at the messages. This also includes the test case where the contents of a CacheMatrix are reset.

# test <- function(){
#     m1<-matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, byrow = T)
#     m2<-solve(m1)
#     
#     cm1<-makeCacheMatrix(m1)
#     cacheSolve(cm1)
#     cacheSolve(cm1)
#     
#     cm2<-makeCacheMatrix(m2)
#     cacheSolve(cm1)
#     cacheSolve(cm2)
#     cacheSolve(cm1)
#     cacheSolve(cm2)
#     cacheSolve(cm1)
#     cacheSolve(cm2)
#     #This should invalidate the inverse
#     cm1$set(m2)    
#     cacheSolve(cm1)
#     cacheSolve(cm1)
#     
# }
