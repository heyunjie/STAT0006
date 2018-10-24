####Exercise 1
## two sampe t-test
data1 <- data.frame("A"=c(11.9,14.6,11.4,9.7,4.2,9.2,8.5,11.2,11.5,14.3),"B"=c(12.4,15.7,13.1,11.8,7.4,9.7,8.9,12.2,13.8,13.5))
#H0: mean(A) = mean(B)  H1: mean(A)!= mean(B)
t.test(data1$A-data1$B,alternative = c("two.sided"),paired=FALSE,mu=0,var.equal = TRUE)
# t = -3.29, p = 0.00931

# 95 percent confidence interval: [-2.0239554 -0.3760446]
# 95% of the 95% confidence intervals contain the real value of mean(A) - mean(B)

## model buidling
treatment<-c(rep("A", 10), rep("B", 10))
measurement<-c(data1$A, data1$B)
lm(measurement~treatment)


###Exercise 2
data2 <- data.frame("dose"=1:10,"weight gain"=c(722,1185,1250,1163,1247,1446,1601,1507,1672,1868))
plot(x=data2$dose,y=data2$weight.gain,xlab="dose",ylab="weight gain")
abline(lm(data2$weight.gain~data2$dose))
mymodel1 <- lm(data2$weight.gain~data2$dose)
summary(mymodel1)
mymodel # y = 815.4 + 100.1dose
mymodel2 <- lm(data2$weight.gain~1)
summary(mymodel2)

## tree data analysis
tree <- read.csv("trees.csv")
mymodel3 <- lm(tree$height~tree$volume+tree$diameter4+tree$diameter16)
summary(mymodel3)

