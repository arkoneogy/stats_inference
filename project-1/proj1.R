# relevant functions ---------------------
dataprep = function (lambda, samps, sims)
{
  x= matrix(data= rexp(samps*sims, lambda), nrow= sims, ncol= samps)
  mns= apply(x,1,mean)
  sds= apply(x,1,sd)
  return(list(mns,sds))
}

comparePlots = function(x, lambda, inpStr)
{
  print(paste('Plotting 1000 samples from an exponential distribution with lambda=', lambda))
  hist(x= rexp(1000, lambda))
  
  print(paste("Plotting distribution of ", inpStr))
  hist(x, prob= T, ylim= c(0,0.6))
  lines(density(x), col= 'blue')  
  
  print(paste("Plotting qq-plot of ", inpStr))
  qqnorm(x)
#   abline(0,1)
}

# MAIN ------------------------

lambda= 0.2

dt= dataprep(lambda, samps= 40, sims= 1000)
mns= dt[[1]]; sds= dt[[2]]

print(paste("Mean of the sample distribution of means is ", round(mean(mns),3) , 
            "while the theoretical mean is ", (1/lambda) )) 
print(paste("Mean of the sample distribution of variance is ", (round(mean(sds),3))^2 , 
            "while the theoretical variance is ", (1/lambda)^2 )) 

pdf(file= 'sample_means.pdf', )
comparePlots(mns, lambda, inpStr= 'sample means')
dev.off()

pdf(file= 'sample_stdevs.pdf')
comparePlots(sds, lambda, inpStr= 'sample standard deviations')
dev.off()