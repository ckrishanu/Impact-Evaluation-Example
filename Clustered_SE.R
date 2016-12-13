# ------------------------------------------------------------------------------------
##################################### Meta ##########################################
# Clustered SE
# Collaborators: Krishanu Chakraborty
# 27th November 2016
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Based on Slawa Rokicki's blog : R for public health 
# (http://rforpublichealth.blogspot.in/2014/10/easy-clustered-standard-errors-in-r.html)
# Version : 1.0.0
# Last edited by : Krishanu Chakraborty
# ------------------------------------------------------------------------------------

get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}

super.cluster.fun<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  
  coef<-coeftest(model, vcovCL)
  w<-waldtest(model, vcov = vcovCL, test = "F")
  ci<-get_confint(model, vcovCL)
  
  return(list(coef, w, ci))
}