install.packages("dplyr")
library(dplyr) 
library(lattice)
data <- read.delim ("Stat100_Spring2019_Survey02.dat",sep = " ")
n<-100
x <- rnorm(100)
freq<-table(data[,29])
freq
pie(freq,col = rainbow(length(freq)),main = "Grade VS Learning")
hist(data[,29],col = "orange",main = "Grade VS Learning",xlab = "")
sample_means<-rnorm(100)
###########################################################
#The study for male and female

df<- select(filter(data,GPA/11.2679<1),c('Grade_vs_Learning'))################GPA < 1
ndf<- select(filter(data,between(GPA/11.2679,1,2)),c('Grade_vs_Learning'))###############GPA < 2
df1<- select(filter(data,between(GPA/11.2679,2,3)),c('Grade_vs_Learning'))################GPA < 3
df2<- select(filter(data,between(GPA/11.2679,3,4.3)),c('Grade_vs_Learning'))################GPA <= 4.3
plot(density(df$Grade_vs_Learning),main = "GPA and Choices")
lines(density(ndf$Grade_vs_Learning), col = "red")
lines(density(df1$Grade_vs_Learning), col = "orange")
lines(density(df2$Grade_vs_Learning), col = "green")
legend("topright", c("< 1", "< 2","< 3","< 4.3"),
       col =c("black","red","orange","green"), lty=1)
################################################################
###########################################################
#The study for GPAs
library(dplyr) 
df<- select(filter(data,Gender_ID==0),c('Grade_vs_Learning'))################Male
ndf<- select(filter(data,Gender_ID==1),c('Grade_vs_Learning'))###############Female
plot(density(df$Grade_vs_Learning),main = "Male Vs Female Choices")
lines(density(ndf$Grade_vs_Learning), col = "red")
legend("topright", c("Male", "Female"),
       col =c("black","red"), lty=1)
t.test(x =  data$Grade_vs_Learning,y = data$Gender_ID)#hypothesis test between choice and Gender
t.test(x = data$GPA,y = data$Grade_vs_Learning)#hypothesis test between choice and GPAs
################################################################

for(i in 1:n){
       sample_means[i] = mean(sample( data[,29],50))
       x[i] = mean(sample( data[,20],50))
}#take 100 means for different random samples of size 50
sample_means
hist(sample_means,main = "",xlab = "Sample Means",col = "darkgreen")#Histogram for the sample distribution
mean(sample_means)#mean of means and i will use that value in hypothesis
sd(sample_means)#Estimated population standard deviation

densityplot(sample_means,main = "",xlab = "Sample Means",col = "violet")#Histogram for the sample distribution
xyplot(data$Grade_vs_Learning~data$Social_Media,data = data, ylab = "Grade Vs Learning",
       xlab = "Social Media", pch=19,col="red",scales = "free",type = c("p", "smooth"))

t.test(sample_means)
t.test(x = data$Grade_vs_Learning,mu=7,alternative = "less")#hypotheses check for mean 7.5

