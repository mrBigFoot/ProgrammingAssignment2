
## ------------------------------------------------------------------
##
##                    Coursera  May 2015
##                       R Course
##             Peer Reviewed Programming 2 assignment

## ------------------------------------------------------------------
##
##                       -Overview-
##
## ------------------------------------------------------------------

##      -Two functions to demonstrate lexical scoping in R-

## 1/                  makeCacheMatix(x) 

## Returns a named list to implement caching
## This list exposes functions to both set and access the
## target matrix and its inverse.

## 2/                   cacheSolve(x,...)

## cacheSolve(x,...) inspects the global environment to see if the inverse
## can simply be cached. Otherwise inverse is computed. 


## usage: closure<-makeCacheMatrix(yourSquareMatrix)
##        cacheSolve(closure) first call will compute inverse and cache it.
##        cacheSolve(closure) second call will report usage of cache

## ------------------------------------------------------------------

## Function makeCacheMatrix(x)
 
## This function returns a named list referring to four 
## internally defined functions which in effect
## are returned as parameters. (functions are first class citizen in R)

makeCacheMatrix <- function(x = matrix()) {
  
  mtx_cached<-NULL  ## initialize cached matrix variable to null
  
  set<-function(y){ ## assigns a new matrix y to x, in the global environment 
    ## for subsequent inversion. This function will be called via the return
    ## named list and so x will be taken from theglobal environment due to 
    ## lexical scoping.
    x<<-y
    mtx_cached<-NULL ## re-initialize to null
  }
  
  get<-function()x
  
  ## Note that <<- operator assigns inverse to mtx_cached and stores it in
  ## the global environment. Any subsequent call will retrieve the cached 
  ## inverse matrix
  
  setinverse<-function(inverse) mtx_cached<<-inverse
  
  getinverse<-function() mtx_cached  ## simply retrieves cached inverse matrix
  
  ## named list data structure to expose matrix caching functions for return to caller
  list(set = set, get = get,   
       setinverse = setinverse,
       getinverse = getinverse)

}

## ----------------------------------------------------------------

## Function cacheSolve

## Take named list argument x. If inverse already cached then return that
## otherwise calculate its inverse and store it back in the global environment

## Argument x takes a named list populated with functions from makeCacheMatrix 
## (see usage above)


cacheSolve <- function(x, ...) { 
                                 
  ## Argument x assigned to a named list returned by a call to makeCacheMatrix
  ## for inversion
  
  invmtx<-x$getinverse() ## assign inverse matrix(maybe null) to invmtx
  
  if(!is.null(invmtx)){ ## matrix inverse has already been cached   
    message("getting cached data")
    return(invmtx)
  }
  
  ## otherwise calculate and cache the inverse
  
  rawMatrix<-x$get()  ## retrieve raw matrix for inversion
  
  j<-solve(rawMatrix) ## compute inverse of the matrix
  
  x$setinverse(j)     ## cache the inverse
  
  j ## report the inverted matrix
}

## ----------------------------------------------------------------
