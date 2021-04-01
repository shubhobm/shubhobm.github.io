bsearch=function(dg, a0, b0, L=1e-7, quiet=FALSE, ...)
{
  ## compute the midpoint
  mm=mean(c(a0,b0))
  k=0
  while(b0-a0 > L)
  {
    k=k+1
    ## get the derivative at the midpoint
    dgm=dg(mm, ...)
    if(dgm < 0)  
    {
      ## function is decreasing at mm
      ## new interval is [mm, b0]
      a0=mm
    } else if (dgm > 0)
    {
      ## function is increasing at mm
      ## new interval is [a0, mm]
      b0=mm
    } else
    {
      ## mm is a stationary point
      b0=mm
      a0=mm
    }
    if(!quiet) cat("k=", k, " [a_k, b_k]=[", a0, ",", b0, "]\n", sep="")
    mm=mean(c(a0,b0))
  }
  return(mm)
}