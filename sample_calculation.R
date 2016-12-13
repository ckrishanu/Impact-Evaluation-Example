# ------------------------------------------------------------------------------------
##################################### Meta ##########################################
# Power Calculations and simulations
# Collaborators: Krishanu Chakraborty
# 27th November 2016
# Based on Slawa Rokicki's blog : R for public health 
# Original exercise in STATA written by John Tebes and Rohit Naimpally
# Version : 1.0.0
# Last edited by : Krishanu Chakraborty
# ------------------------------------------------------------------------------------

sampsi.mean <- function (muA, muB, kappa = 1, sd, alpha = 0.05, beta = 0.20)
{
  (nB <<- (1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
  nA <<- kappa*nB
  
  # The following steps are to align the output as the sampsi command shows in STATA
  
  return_list_name <<- c("Alpha Two sided", "Power", "mA", "mB","sd1", "sd2", "nA", "nB")
  return_list_val <<-c(alpha, (1- beta), muA, muB, sd, sd, ceiling(nA),ceiling(nB)) 
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val)) 
  return(return_list)
}



sampsi.means<-function(m1, m2, sd1, sd2=NA, ratio=1, power=.90, alpha=.05, two.sided=TRUE, one.sample=FALSE){
  
  effect.size<-abs(m2-m1)
  sd2<-ifelse(!is.na(sd2), sd2, sd1)
  
  z.pow<-qt(1-power, df=Inf, lower.tail=FALSE)
  z.alph<-ifelse(two.sided==TRUE, qt(alpha/2, df=Inf, lower.tail=FALSE), qt(alpha, df=Inf, lower.tail=FALSE))
  ct<-(z.pow+z.alph)
  
  n1<-(sd1^2+(sd2^2)/ratio)*(ct)^2/(effect.size^2)
  n<-(ct*sd1/effect.size)^2
  
  if(one.sample==FALSE){
    col1<-c("alpha", "power", "m1", "m2", "sd1", "sd2", "effect size", "n2/n1", "n1", "n2")
    col2<-c(alpha,  power, m1, m2, sd1, sd2, effect.size, ratio, ceiling(n1), ceiling(n1*ratio))
  }
  else{
    col1<-c("alpha", "power", "null", "alternative", "n")
    col2<-c(alpha, power, m1, m2, ceiling(n))
  }
  ret<-as.data.frame(cbind(col1, col2))
  ret$col2<-as.numeric(as.character(ret$col2))
  colnames(ret)<-c("Assumptions", "Value")
  
  description<-paste(ifelse(one.sample==FALSE, "Two-sample", "One-sample"), ifelse(two.sided==TRUE, "two-sided", "one-sided"), "test of means")
  
  retlist<-list(description, ret)
  
  return(retlist)
}

samp.clus<-function(sampsi.object, rho, num.clus=NA, obs.clus=NA){
  
  if(is.na(num.clus)&is.na(obs.clus)) print("Either num.clus or obs.clus must be identified")
  else{
    
    so<-sampsi.object[[2]]
    n1<-as.numeric(so[so$Assumptions=="n1",2])
    n2<-as.numeric(so[so$Assumptions=="n2",2])
    
    if(!is.na(obs.clus)){
      deff<-1+(obs.clus-1)*rho
      n1.clus<-n1*deff
      n2.clus<-n2*deff
      num.clus<-ceiling((n1.clus+n2.clus)/obs.clus)
    }
    else if(!is.na(num.clus)){
      
      tot<-(n1*(1-rho)+n2*(1-rho))/(1-(n1*rho/num.clus) - (n2*rho/num.clus))
      if(tot<=0) stop("Number of clusters is too small")
      else{
        obs.clus<-ceiling(tot/num.clus)
        deff<-1+(obs.clus-1)*rho
        n1.clus<-n1*deff
        n2.clus<-n2*deff
      }
    }
    
    col1<-c("n1 uncorrected", "n2 uncorrected", "ICC", "Avg obs/cluster", "Min num clusters", "n1 corrected", "n2 corrected")
    col2<-c(n1, n2, rho, obs.clus, num.clus, ceiling(n1.clus), ceiling(n2.clus))
    ret<-as.data.frame(cbind(col1, col2))
    colnames(ret)<-c("Assumptions", "Value")
    return(ret)
  }
}

