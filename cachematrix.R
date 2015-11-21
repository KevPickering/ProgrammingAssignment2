## This pair of functions enables us to apply the function solve() to a square matrix
## and store the result in memory. If we repeat it again to the same square matrix
## then instead of re-calculating the result, we return the previously stored result
## from memory. Thus saving time and improving performance when using these functions
## with a long list of ordered matrices to which we need to apply the solve() function.
## Emphasis is on the 'ordered' set of matrices in order to see performance gain.
## 

## This function, makeCacheMatrix, returns a list of 4 elements, each of which is a function.
## Function variables:
## x 		- matrix, passed as an argument
## solcach	- free variable, initially set to NULL, stores subsequent cached results
## get 		- function() with no arguments, returns the matrix variable x
## set  	- function(newmat), replaces the current matrix x with argument newmat
## getsolve - function() with no arguments,returns current result in solcache
## setsolve	- function(newsolve), replaces the current cached result in solcache with
##			  argument newsolve
##
## Returns list of 4 elements:
## get		- variable get
## set		- variable set
## getsolve	- variable getsolve
## setsolve	- variable setsolve
##
## Example:
## z <- makeCacheMatrix()
## x <- matrix(1:4, 2, 2)
## z$set(x)
## z$get()
## z$setsolve(x)
## z$getsolve()

makeCacheMatrix <- function(x = matrix()) {
	solcache <- NULL
	set <- function(newmat) {
		x <<- newmat
		solcache <<- NULL
	}
	get <- function() {
		x
	}
	setsolve <- function(newsolve) {
		solcache <<- newsolve
	}
	getsolve <- function() {
		solcache
	}
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function cacheSolve returns result of solve() function, but the returned
## value is a cached value if previously calculated.
## Function variables:
## x			- list, passed as an argument. Object passed is result of call to 
##				  function makeCacheMatrix
## cachedsol	- free variable, set to cached solve result
## newmatrix	- free variable, set to matrix passed in this functions argument x
## newsolve		- free variable, set to result of applying function solve() to
##				  matrix passed in this functions argument x
##
## Returns either value in cachedsol or value in newsolve
##
## Example:
## z <- makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(z)
## z$set(matrix(5:8, 2, 2))
## cachesolve(z)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	cachedsol <- x$getsolve()
	if(!is.null(cachedsol)) {
		message("retrieving cached solve result")
		return(cachedsol)
	}
	## this part only runs if cachedsol is NULL
	newmatrix <- x$get()  ## returns matrix in this functions argument x
	newsolve <- solve(newmatrix, ...)  ## returns matrix which is the inverse of matrix newdata
	x$setsolve(newsolve = newsolve)  ## sets new result in this functions argument x
	newsolve  ## return result of solve function
}
