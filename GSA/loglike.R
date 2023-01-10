loglik=function(m,o){
  if(length(m)!=length(o)){
     print("Inequal number of modeled and observed values, cannot proceed")
     return()
   }
  
  res=log(m)-log(o)
  sigma=sqrt(mean(res^2))
  n=length(m)
  #lk=-n*log(sigma)-(1/(2*sigma^2))*sum(res^2)
  lk=-n*log(sigma)-(1/(2*sigma^2))*sum(res^2)
  return(lk)
  
}




#test
# m=c(4,3,4,3)
# o=c(4, 3.1, 5, 4)
# loglik(m, o)
# 
# logLik(lm(m~o),REML = T )
 
