##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set=set,get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
	m<-x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data,...)
	x$setinverse(m)	
	m        	
}


#Output

#x<-matrix(1:4,nrow=2,ncol=2)
#m=makeCacheMatrix(x)
#m$get()
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#cacheSolve(m)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# getting cached data

#cacheSolve(m)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
