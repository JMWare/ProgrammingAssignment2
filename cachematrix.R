## The function makeCacheMatrix(x) takes a square matrix as its argument 
## and caches it in the global environment using the superassignment 
## operator <<- and returns a list of functions : get(), set(), getinv() 
## and setinv() to be passed as the argument to the function cacheSolve(). 

## The function cacheSolve(x) uses the functions returned by makeCacheMatrix()
## to calculate the inverse matrix using function solve(x) and caches 
## the result. If run a second time with the same input, the cached
## version of the inverse matrix is retrieved.

## Repeated calls to makeCacheMatrix(x) and cacheSolve(x) create distinct 
## objects & function lists in the global environment so multiple matrices
## and their inverses can exist simultaneously. (see example output.)

## makeCacheMatrix() is a simple adaptation from the makevector(x) function 
## provided in the assignment instructions.

## cacheSolve() is adapted from the function cachemean() provided in the 
## assignment instructions by substituting function solve() for mean().

## --------------------------------------------------------------

makeCacheMatrix <- function(x = matrix())
{	m <- NULL
	set <- function(y)
	{	x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(matrix) m <<- matrix
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


cacheSolve <- function(x, ...) 
{	m <- x$getinv()
	if(is.null(m) != TRUE)
	{	message("Getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}

## --------------------------------------------------------------

## EXAMPLE OUTPUT

## Running the above code gave the following output with 3 x 3 
## and 4 x 4 matrices, (copied and pasted from the R console) :
## Also tested with matrices up to 12 x 12


## > source("cachematrix.R")

## > M1<-matrix(runif(9,1,5),3,3)
## > M2<-matrix(runif(16,1,5),4,4)

## > M1
##          [,1]     [,2]     [,3]
## [1,] 1.422114 1.002056 1.753871
## [2,] 4.214930 2.289924 2.994016
## [3,] 2.545084 3.332325 2.448888

## > M2
##          [,1]     [,2]     [,3]     [,4]
## [1,] 3.566596 3.258568 3.679607 4.201350
## [2,] 1.796351 4.400722 2.218202 1.786272
## [3,] 4.667157 4.670657 2.412075 3.162723
## [4,] 4.888050 1.511477 4.570044 2.798614

## > R1<-makeCacheMatrix(M1)
## > R2<-makeCacheMatrix(M2)

## > cacheSolve(R1)
##            [,1]       [,2]       [,3]
## [1,] -0.7956616  0.6174325 -0.1850288
## [2,] -0.4920220 -0.1786719  0.5708263
## [3,]  1.4964352 -0.3985583 -0.1761063

## > cacheSolve(R2)
##             [,1]       [,2]        [,3]        [,4]
## [1,] -0.26424570 -0.2396584  0.36621911  0.13579366
## [2,] -0.10889333  0.2738216  0.05633737 -0.07496581
## [3,] -0.03494294  0.3693145 -0.41666572  0.28761020
## [4,]  0.57740254 -0.3323774  0.01033745 -0.30902739

## > cacheSolve(R1)
## Getting cached data
##            [,1]       [,2]       [,3]
## [1,] -0.7956616  0.6174325 -0.1850288
## [2,] -0.4920220 -0.1786719  0.5708263
## [3,]  1.4964352 -0.3985583 -0.1761063
## 
## > cacheSolve(R2)
## Getting cached data
##             [,1]       [,2]        [,3]        [,4]
## [1,] -0.26424570 -0.2396584  0.36621911  0.13579366
## [2,] -0.10889333  0.2738216  0.05633737 -0.07496581
## [3,] -0.03494294  0.3693145 -0.41666572  0.28761020
## [4,]  0.57740254 -0.3323774  0.01033745 -0.30902739
## > 

## These are the functions returned by R1<-makeCacheMatrix(M1):

## > R1
## $set
## function (y) 
## {
##     x <<- y
##     m <<- NULL
## }
## <environment: 0x01eccce8>
## 
## $get
## function () 
## x
## <environment: 0x01eccce8>
## 
## $setinv
## function (matrix) 
## m <<- matrix
## <environment: 0x01eccce8>
## 
## $getinv
## function () 
## m
## <environment: 0x01eccce8>
## 
## >

 

