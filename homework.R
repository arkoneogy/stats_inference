
# h4q1
x1= c(140,138,150,148,135)
x2= c(132,135,151,146,130)
t.test(x1-x2,alternative = 'two.sided',mu=0)

# h4q2
1100 + c(-1,1)*qt(0.975, df= 8)*(30/sqrt(9))

# h4q3
pbinom(3-1, 4, 0.5, lower.tail = F)

#h4q4
exp= (1/100)*1787
obs= 10
ppois(obs, exp, lower.tail = T)

# h4q5
md1= -3
md2= 1
sd1= 1.5
sd2= 1.8
n= 9
sp= sqrt((sd1^2+sd2^2)/2)
md1 - md2 + c(-1,1)*qnorm(0.995,mean=0,sd=sp,lower.tail=F)*(sp/sqrt(n))

#h4q7
power.t.test(delta= 0.01, sd = 0.04, sig.level = 0.05, n = 100, type = "one.sample", alt= "one.sided")

# h4q8
power.t.test(delta= 0.01, sd = 0.04, sig.level = 0.05, power= 0.9, type = "one.sample", alt= "one.sided")
