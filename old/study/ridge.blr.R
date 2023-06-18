###########################################################################
##  The following function uses Newton's method to fit weighted ridge
##  penalized Binomial logistic regression
## 
##  Arguments
##  X,  the n row by p column model matrix with ones in its first column.
##  y,  the n element vector of responses (observed sample proportions of success,
##      one for each of the n cases).
##  n.list,  the n element vector where the ith element is the index for the ith case's
##           Binomial experiment
##  lam,  the non-negative tuning parameter value
##  m,  the p element vector where the jth element is the non-negative penalty weight
##      for the jth regression coefficient estimate
##  tol, convergence tolerance
##  maxit, the maximum number of iterations allowed
##  quiet, should the function stay quiet?
##  b.start, optional 0th iterate for the regression coefficient estimates
##
##  The function returns a list with two elements
##    b,  the vector of regression coefficient estimates (the final iterate)
##    total.iterations, the number of Newton steps taken
#############################################################################
ridge.blr=function(X, y, n.list, lam, m=NULL, tol=1e-7, maxit=100, quiet=F, b.start=NULL)
{
  p=ncol(X)
  ## create vector of penalty weights
  ## if unspecified
  if(is.null(m))
    m=c(0, rep(1, p-1))
  
  ## create useful variables  
  lam.m=lam*m
  lam.M=diag(lam.m)
  X.t.n.list.y=crossprod(X, n.list*y)
  
  ## If unspecified, make the 0th iterate
  ## for b the zero vector
  if(is.null(b.start)) 
    b=rep(0,p)
  
  ## initialize iteration counter
  k=0
  add=tol+1
  while( (k <= maxit) & (sum(abs(add)) > tol))
  { 
    k=k+1
    pi.t=ilogit(as.numeric(X%*%b))
    W=diag(n.list*pi.t*(1-pi.t))
    minusGrad=X.t.n.list.y-crossprod(X, n.list*pi.t) - lam.m*b
    Hess=crossprod(X,W%*%X)+lam.M
    add=qr.solve(Hess, minusGrad)
    b=b+add
    if(!quiet) cat("k=", k, "b=", b, "\n")
  }
  b=as.numeric(b)
  return(list(b=b, total.iterations=k))
}