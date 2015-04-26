# libraries ------------------------------
library(data.table)
library(datasets)
library(ggplot2)

# functions ------------------------------
performAllTests = function(mydt)
{
  sups= unique(mydt$supp)
  dss= unique(mydt$dose)
  
  # all dose tests
  for (sup in sups) {
    for (ds in dss) {
      # setup the test data
      dss.rel= dss[dss != ds]
      dss.test= max(dss.rel)
      dss.ref= min(dss.rel)
      print(paste('Testing dose', dss.test, 'vs', dss.ref, 'keeping supplement fixed as', sup))
      
      test.dt= mydt[supp == sup & dose %in% dss.rel]
      test.dt[, dose:= as.factor(dose)]
      
      # run the test
      test.result = t.test(len ~ I(relevel(dose, 2)), paired= F, data= test.dt)
      printConclusions(test.result, dss.ref, dss.test, type= 'dose')   
    }
  }
  
  # all supplement tests
  for (ds in dss) {
    print(paste('Testing supplment OJ vs VC keeping dose fixed at', ds))
    test.result = t.test(len ~ supp, paired= F, data= mydt[dose == ds])
    printConclusions(test.result, 'VC', 'OJ', 'supplement')
  }
}

printConclusions = function (result, refLevel, testLevel, type, sigLevel= 0.05)
{
  if (result$p.value < sigLevel){
    print (paste('Evidence to reject H0 at', sigLevel, 'level of significance!'))
    if (result$statistic > 0 ){
      print(paste('Treatment of', type, testLevel, 'produces BETTER results than', type, refLevel))
    } else {
      print(paste('Treatment of', type, testLevel, 'produces WORSE results than', type, refLevel))
    }
  } else {
    print (paste('No clear evidence favoring H1 at', sigLevel, 'level of significance'))
  }
}

# MAIN -----------------------------------

mydt= data.table(ToothGrowth)

# lets look at the data
pdf('toothGrowth.pdf')

coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")

boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose") 

boxplot(len~dose*supp, data=ToothGrowth, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose") 

dev.off()

performAllTests(mydt)
