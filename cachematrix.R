## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

   myInvMat<-NULL;    
   mySetMat<-function(x)
        {
        myMat<<-x
        myInvMat<<-NULL
        }
    
    myGetMat<-function()x
    
    mySetInv<-function(inv) myInvMat<<-inv
    myGetInv<-function() myInvMat
    
    myList<-list(set=mySetMat,get=myGetMat,setInv=mySetInv,getInv=myGetInv)
    myList

}

##
## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {

    myinvMat2<-x$getInv()
    
    myinvMat3<-solve(x$get())
    
    if(!is.null(myinvMat2) && (identical(myinvMat3,myinvMat2)))
    {
        message("getting cached data")
        ## Return cached inverse of 'x'
        return(myinvMat2)
    }
    
    myMat2<-x$get()
    myInvMat2<-solve(myMat2)
    x$setInv(myInvMat2)
    ## Return computed inverse of 'x'    
    myInvMat2    
}

