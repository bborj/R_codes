# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

#Importing the data
mydata<-import("/Users/borja/Desktop/Curso R/Curso Machine Learning/Datasets/opsd_germany_daily.csv", format = "csv")

#Head
head(mydata)

#Tail
tail(mydata)

#View
view(mydata)

#Structure
str(mydata)

row.names(mydata)
#Accesing a specific row
mydata["802",] 
mydata["2006-01-03",] #Date is not the index, so it doesnt show anything

#Summary
summary(mydata)
str(mydata$Date)

#Create year month and day columns
year <- as.numeric(format(mydata$Date,'%Y'))
month <- as.numeric(format(mydata$Date,'%m'))
day <- as.numeric(format(mydata$Date,'%d'))

#Add my new columns to my dataframe

mydata <- cbind(mydata, year,month,day)
mydata <- transform(
  mydata,
  Date=as.factor(Date)
)
#First 3 rows
mydata[1:3,]

#Plotting the consumption over the years
#Using plot()
#It doesnt give useful information
plot(mydata$year,mydata$Consumption,type="l",xlab = "Year",ylab = "Consumption")
#Using limit
plot(mydata$year,mydata$Consumption,type="l",xlab = "Year",ylab = "Consumption",xlim = c(2006,2018),ylim = c(800,1700))

#Multiple plots in a window
par(mfrow=c(1,1))

#Different ways
plot(mydata[,2],xlab = "year",ylab = "Consumption")
plot(mydata[,2],xlab = "year",ylab = "Consumption",type="l",col="blue")
plot(mydata[,2],xlab = "year",ylab = "Consumption",type="l",lwd = 2,xlim=c(0,2018))
plot(mydata[,2],xlab = "year",ylab = "Consumption",type="l",lwd = 2,xlim=c(2006,2018))
plot(mydata[,2],xlab = "year",ylab = "Consumption",type="l",lwd = 2,xlim=c(2006,2018),ylim = c(900,2000),main="Consumption over the years")

#Taking log values and difference of logs
#Better
plot(10*diff(log(mydata[,2])),xlab = "year",ylab="Consumption",type="l",lwd="2",
     ylim=c(-5,5),main="Consumption Graph",col="orange")

#using ggplot
install.packages("ggplot2")
library(ggplot2)

#Similar to our first plot
ggplot(mydata,type ="o")+geom_line(aes(x=year,y=Consumption))

#Another option (still better with plot, it choses better frames, every year)
ggplot(data = mydata,aes(x=year,y=Consumption,group=1)) + geom_line(linetype="dashed")+geom_point()

#Plotting considering solar and time
#Wind column
min(mydata[,3],na.rm=T)
max(mydata[,3],na.rm=T)

#Consumption
min(mydata[,2],na.rm=T)
max(mydata[,2],na.rm=T)

#Solar
min(mydata[,4],na.rm=T)
max(mydata[,4],na.rm=T)

#Wind+Solar
min(mydata[,5],na.rm=T)
max(mydata[,5],na.rm=T)

#For multiple plots
par(mfrow=c(3,1))

#Si las fechas no estan en factores, no funciona correctamente
plot1 <- plot(10*diff(log(mydata[,2])),xlab = "year",ylab="Consumption",type="l",lwd="2",
              ylim=c(-5,5),main="Consumption Graph",col="orange")
plot2 <- plot(mydata[,1],mydata[,2],xlab="Days",ylab="Daily Totals (Gwh)",type="l",
              lwd = 2, main = "Consumption",ylim=c(840,1750))
plot3 <- plot(mydata[,1],mydata[,4])

par(mfrow=c(1,1))
str(mydata)

#Plotting in a single year
x <- as.Date(mydata$Date)

#we modify the dates 
moddate <- as.Date(x,format = "%m/%d/%Y")
mydata <- cbind(moddate,mydata)

#We create a subset with the info of a particular year
mydata2 <- subset(mydata,subset = mydata$moddate > "2007-01-01" & mydata$moddate < "2007-12-31")
head(mydata2)

plot4 <- plot(mydata2[,1],mydata2[,3],xlab = "year",ylab="Daily Consumptions (GWh)",col = "orange",type="l",
             main = "Consumption in 2007" )
#Boxplot
quantile(mydata2$Consumption,probs=c(0,0.25,0.5,0.75,1))
boxplot(mydata2$Consumption,main="Consumption",ylim=c(600,1800))

#Yearly
boxplot(mydata$Consumption ~ mydata$year,main="Consumption over the years",xlab="Years",ylab="Consumption",
        ylim = c(600,1800))
#Monthly
boxplot(mydata2$Consumption ~ mydata2$month,main="Consumption over the months",xlab="Months",ylab="Consumption (GWh)",
        ylim = c(600,1800))
