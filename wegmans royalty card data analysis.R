##Set the working directory (can do using menus)
#setwd("~/Dropbox/Marketing Research/Cases/National Ins")
#setwd("C:/Users/mitch.lovett/Dropbox/Marketing Research/Cases/National Ins")
#Need to ajust the setwd() filename (surrounded by "") to match your working directory

setwd("D:/Winter Quarter/Marketing Research/Week 4_Regression, Interactions & Segmentation/Yogurt Case")
#install and call the ggplot2 library, which contains plotting functions we will use
install.packages("ggplot2", dependencies=T)
library(ggplot2)

install.packages("tm",dependencies=T)
install.packages("wordcloud", dependencies=T)
library(wordcloud)
library(tm)



#call the library for getting data from other programs
library(foreign)
filnm = "Wegmans Survey"; #this is the name of the file
spssDataLab <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=TRUE,trim_values=TRUE)
spssData <- read.spss(paste(filnm,".sav",sep=""),to.data.frame=TRUE,use.value.labels=FALSE); #turning values into numbers, but be sure getting right values to numbers!

##0) Summarize variables and validate
summary(spssDataLab); #a quick look, but must examine carefully
summary(spssData)
str(spssData)
##1) Test whether sample matches with population for gender and length of use
##check gender:
popSEX = c(.87,.13); #true population values
sex <- spssData$Q33[spssData$Q33!=1&spssData$Q33!=4]
prop.table(table(sex))
cbind(popSEX,prop.table(table(sex))); #creating table as matrix(contingency table)
chisq.test(table(sex),p=popSEX)
# The sample differs from the population
w = popSEX/prop.table(table(sex))
w

pie <- data.frame(value = c(0.87,0.13),
                 Group = c("Female","Male"))
ggplot(pie, aes(x = 1, weight = value, fill = Group)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y")+theme(axis.text.x = element_blank()) 

##2) calculate average importatnt ratings
grep("Question6Allnatural", colnames(spssData)) #47:59
impMeans = colMeans(spssData[,47:59],na.rm=TRUE)
impMeans
spssData[ ,47]

##3)
grep("Question17WhendecidingwhatGreekYogurttopurchasewhatisthe.1reason",colnames(spssDataLab))#89:92
spssDataLab[ ,89:92]
library(dplyr)
Capital <- mutate_all(spssData[ ,89:92], funs(toupper))
write.table(Capital,file="MYFILENAME.txt",row.names=F, col.names=F, sep="\t")


##4)
A<-spssData[spssData[4]=="Yes", ]
grep("Question24Allnatural",colnames(spssData))#106
grep("Question24Price",colnames(spssData))#110
grep("Question24Taste",colnames(spssData))#113
grep("Question30Allnatural",colnames(spssData))#132 
grep("Question30Price",colnames(spssData))#138
grep("Question30Taste",colnames(spssData))#140

cat(paste("******","Allnatural","*****"),fill=TRUE)
t.test(spssData[spssData[4]=="Yes",106], spssData[spssData[4]=="Yes",132])

cat(paste("******","Price","*****"),fill=TRUE)
t.test(spssData[spssData[4]=="Yes",110],spssData[spssData[4]=="Yes",138])

cat(paste("******","Taste","*****"),fill=TRUE)
t.test(spssData[spssData[4]=="Yes",113],spssData[spssData[4]=="Yes",140])

impVars = c("Question6Allnatural","Question6Organic","Question6rbSTfree","Question6Price")
impMeans = colMeans(cooking[,impVars],na.rm=TRUE)
impMeans
imV=c("Question24Allnatural","Question24Price","Question24Price")
colMeans(A[ ,imV],na.rm=TRUE)
imVr=c("Question30Allnatural","Question30Price","Question30Price")
colMeans(A[ ,imVr],na.rm=TRUE)



##5)
summary(spssDataLab)
grep("Question12DoyouuseGreekYogurtforcooking", colnames(spssData))#76
grep("Question6Allnatural",colnames(spssData))#47
grep("Question6Organic",colnames(spssData))#53
grep("Question6rbSTfree",colnames(spssData))#56
grep("Question6Price",colnames(spssData))#54

?cat()
#cooking&snack,Allnatural
summary(spssData[ ,95])

notcooking<-subset(spssData,spssData[ ,76]!="Yes")
cooking<-subset(spssData,spssData[ ,76]=="Yes")
notcooking[1,56]

cat(paste("******","Allnatural","*****"),fill=TRUE)
chisq.test(spssDataLab[[76]],spssDataLab[[47]])
t.test(spssData$Question6Allnatural[spssData[76]=="Yes"],spssData$Question6Allnatural[spssData[76]!="Yes"])
#cooking&snack,Organic
cat(paste("******","Organic","*****"),fill=TRUE)
chisq.test(spssDataLab[[76]],spssDataLab[[53]])
t.test(spssData$Question6Organic[spssData[76]=="Yes"],spssData$Question6Organic[spssData[76]!="Yes"])
#cooking&snack,rbSTfree
cat(paste("******","rbSTfree","*****"),fill=TRUE)
chisq.test(spssDataLab[[76]],spssDataLab[[56]])
t.test(spssData$Question6rbS[spssData[76]=="Yes"],spssData$Question6rbS[spssData[76]!="Yes"])
spssData[ ,76]
?subset()
cooking
notcooking
cooking
#cooking&snack,price
cat(paste("******","pricemeans","*****"),fill=TRUE)
impVars = c("Question6Allnatural","Question6Organic","Question6rbSTfree","Question6Price")
impMeans = colMeans(cooking[,impVars],na.rm=TRUE)
impMeans
ImpMeans = colMeans(snacks[,impVars],na.rm=TRUE)
ImpMeans

impMeans11 = colMeans(notcooking[,impVars],na.rm=TRUE)
impMeans11
