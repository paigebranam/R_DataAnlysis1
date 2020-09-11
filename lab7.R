#Load in data

dat <- read.csv("/Users/PaigeBranam/Downloads/yawn.csv")

obs <-table(dat)

obs


mosaicplot (obs, xlab="Eyes", ylab="Behavior", main="", col=c("blue","red"))

#Correlation between eyes being visible and yawning 

#Test assocaition between eyes and yawning 

#calculating expected values : 
#first get col, row, and grand totals 
noyawn <-sum(obs[,1]) #this will call the entire first column in the table 
yawn<- sum(obs[,2])
covered <- sum(obs[1,]) #this will call the entire first row of the table 
visible <- sum(obs[2,])

grand <- sum(obs)  #grand total 
                         
 #calculate expected values 
exp_NoYC <- (noyawn*covered)/grand 
exp_NoYV <- (noyawn*visible)/grand 
exp_YaC <- (yawn*covered)/grand 
exp_YaV <- (yawn*visible)/grand 

                         
#putting all these expected values in a table 
#first copy our observed table 
exp <- obs 
#now we will fill (cell by cell) our expected values 
exp[1,1] <-exp_NoYC
exp[2,1] <-exp_NoYV
exp[1,2] <-exp_YaC
exp[2,2] <-exp_YaV
      
#Now we can see if cochrans rule's are violated (based on expected values)
                         #Lecture 15 -nothing below 5 
chisq<-sum(((obs-exp)^2)/exp) 
                     
Degrees of Freedom (#Rows-1 * #Columns-1)
Df <- (2-1)*(2-1)                        
#Df = 1 

p= 1-pchisq(chisq, Df)

p= .1205

#####

chisq.test(obs, correct=FALSE)
                           
#Matches 

##Exercise 2 


bdata <- read.csv("~/Downloads/bats - Sheet1 (1).csv", row.names=1)
                           
fisher.test(bdata)
obs <- c(6,3,9,5)
control<-6
test<-3
notfed<-9
fed<-5
grand <-sum(bdata)

exp_CF <- (control*fed)/grand 
exp_CN <- (control*notfed)/grand 
exp_TF <- (test*fed)/grand 
exp_TN <- (test*notfed)/grand 

exp <- c(2,14,3.86,1.07,1.93)
chisq<-sum(((obs-exp)^2)/exp)
