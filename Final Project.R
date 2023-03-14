library(ggplot2)
library(RColorBrewer)
library(rpart)
library(rpart.plot)

set <- read.csv("C:/Users/foreh/Downloads/diabetes.csv")




#creating chart of percentage of diabetic/non-diabetic people
table(set$Diabetes_binary)

nond <- (218334/253680) *100
posd <- (35346/253680) *100

p <- c(nond, posd)

colors <- brewer.pal(3, "Set2") 
pie(p, labels = c("non-diabetic (86.07%)", "diabetic (13.93%)"), border ="white", col = colors, main = "Diabetes")



t1<- rpart(Diabetes_binary ~ BMI + HighChol + GenHlth + HighBP, data =set, cp=0.001)
rpart.plot(t1,box.palette = "pink")
printcp(t1)


risk <- subset(set,BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1)
diabetes <- subset(set, Diabetes_binary==1)
nondiabetic <- subset(set, Diabetes_binary ==0)
#P(BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1)

d <- nrow(risk)/nrow(set)

#P(BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1 | diabetic)
dirsk <- subset(diabetes, BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1 )

p1 <- nrow(dirsk)/nrow(diabetes)


#P(BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1 | non-diabetic)
nond1 <- subset(nondiabetic, BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1 )

p2 <- nrow(nond1)/nrow(nondiabetic)

#P(diabetic | BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1)
(p1 *posd)/d


#P(non-diabetic | BMI >= 30 & GenHlth == 5 & HighBP == 1 & HighChol == 1)
(p2 *nond)/d


#distribution of BMI greater than or equal to 30
BMIO <- subset(set, BMI>= 30) #BMI greater than or equal to 30 is considered obese

BMIOK <- subset(set, BMI <30)

obese <- nrow(BMI)/nrow(set) *100

ok <- nrow(BMIOK)/nrow(set) * 100

a <- c(obese,ok)

pie(a, labels = c("obese (34.63%)", "not obese (65.37%)"), border ="white", col = colors, main = "Distribution of BMI")




#distribution of Blood Pressure 
HBP <- subset(set, HighBP == 1)

LBP <- subset(set, HighBP == 0)

hibp <- nrow(HBP)/nrow(set) *100

lobp <- nrow(LBP)/nrow(set) * 100

b <- c(hibp,lobp)

pie(b, labels = c("High Blood Pressure (42.9%)", "Low Blood Pressure (57.1%)"), border ="white", col = colors, main = "Distribution of Blood Pressure")


#distribution of Blood Pressure 
HCh <- subset(set, HighChol == 1)

LCh <- subset(set, HighChol == 0)

hcho <- nrow(HCh)/nrow(set) *100

loch <- nrow(LCh)/nrow(set) * 100

b <- c(hcho,loch)

pie(b, labels = c("High Cholesterol (42.41%)", "Low Cholesterol (57.59%)"), border ="white", col = colors, main = "Distribution of Cholesterol")



#distribution of General Health 
GHlth <- subset(set, GenHlth< 5) #1 = excellent 2 = very good 3 = good 4 = fair 5 = poor

BHlth <- subset(set, GenHlth == 5) 

hlth <- nrow(GHlth)/nrow(set) *100

nhlth <- nrow(BHlth)/nrow(set) * 100

a <- c(hlth,nhlth)

pie(a, labels = c("Healthy (95.24%)", "Poor Health (4.76%)"), border ="white", col = colors, main = "Distribution of Health")

























