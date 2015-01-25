#Assignment #1 - Part 1
#https://class.coursera.org/rprog-010/human_grading

## --------------------------------------------------------------------------------
## Method to test
## Run the following commands to test
## set.seed(1110201)
## r = rnorm(1000000)
## mat1 = matrix(r, nrow=1000, ncol=1000)
## test(mat1)
## 
## try to run test(mat1) 4-5 times , you would start seeing the difference and 
## value of caching 
## --------------------------------------------------------------------------------
## Probably can be enhanced to Runit, my bad no time :-(
## http://www.johnmyleswhite.com/notebook/2010/08/17/unit-testing-in-r-the-bare-minimum/

test = function(matrix){
        ## @matrix: an invertible matrix

		## NOTE: makeCacheMatrix(x) has to be called prior to cacheSolve(x).
        
        temp = makeCacheMatrix(matrix)
        
        start.time = Sys.time()
        cacheSolve(temp)
        end.time = Sys.time()
        duration = end.time - start.time
        print(duration)
        
        start.time = Sys.time()
        cacheSolve(temp)
        end.time = Sys.time()
        duration = end.time - start.time
        print(duration)
}


## Write a short comment describing this function

## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'inversion' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setInversion() to assign computed inverse matrix (of x) to inversion;
## 4. Provides function getInversion() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
 	## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
	
	inversion = NULL
    
        # A setter function, use this to set a matrix to object created by makeCacheMatrix function
        setMatrix = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inversion <<- NULL
        }
        
        getMatrix = function() x # return the input matrix

        setInversion = function(inverse) inversion <<- inverse 
        
        getInversion = function() inversion # return the inversed matrix

      # x$setMatrix - (newmatrix) to change matrix
      # x$getMatrix - to get the setted matrix
      #
      # x$setInversion -  to set the inversed matrix
      # x$getInversion -  to get the inversed matrix

      list(setMatrix=setMatrix, getMatrix=getMatrix, setInversion=setInversion, getInversion=getInversion)

}


## --------------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  
## It first checks if the in-verse matrix has been found;
## if yes, returns the result and quits. 
## If not, the inverse of x is calculated, saved to cached, and returned.
##
## NOTE: argument x for this function must be cached, i.e. a list returned from
##       calling makeCacheMatrix(x).
## --------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
	## @x: output of makeCacheMatrix()
    ## return: inverse of the original matrix input to makeCacheMatrix()
       	## Return a matrix that is the inverse of 'x'
		
		inversion = x$getInversion()

	# if the inverse has already been calculated
        if (!is.null(inversion)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inversion)
        }
		else{
				message("No cached data found. Calculating inverse matrix...")
			
				# otherwise, calculates the inverse 
		        matrix.data = x$getMatrix()
		        inversion = solve(matrix.data, ...)

				# sets the value of the inverse in the cache via the setinv function.
		        x$setInversion(inversion)

		        message("Inversion Done.")

		        return(inversion)			
		}	
	
}

