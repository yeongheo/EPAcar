#Load necessary packages
library("xlsx")
library(stringr)
library(ggplot2)
#setworking directory
setwd("~/Desktop/BSAD 341")
#load in data
data.2016 = read.xlsx("SmartWay2016.xlsx", sheetIndex = 1, header=TRUE)
data.2017 = read.xlsx("SmartWay2017.xlsx",sheetIndex = 1, header = T)
#start data renaming the data and organinzing it by converting the necessary variables into numeric
trainingdata = data.2016
trainingdata$Displ=as.numeric(as.character(trainingdata$Displ))
trainingdata$Cyl=as.numeric(as.character(trainingdata$Cyl))
trainingdata$Greenhouse.Gas.Score = as.numeric(as.character(trainingdata$Greenhouse.Gas.Score))
#split city mpg into Electric and Gas
citympg=  str_split_fixed(trainingdata$City.MPG, "/", 2)
citympg = as.data.frame(citympg)
trainingdata$City.MPG.G = citympg$V1
trainingdata$City.MPG.G = as.numeric(as.character(trainingdata$City.MPG.G))
mean(trainingdata$City.MPG.G,na.rm = T)
trainingdata$City.MPG.E = citympg$V2
trainingdata$City.MPG.E = as.numeric(as.character(trainingdata$City.MPG.E))
mean(trainingdata$City.MPG.E,na.rm = T)
#split highway mpg into Electric and Gas
hwympg=  str_split_fixed(trainingdata$Hwy.MPG, "/", 2)
hwympg = as.data.frame(hwympg)
trainingdata$Hwy.MPG.G = hwympg$V1
trainingdata$Hwy.MPG.G = as.numeric(as.character(trainingdata$Hwy.MPG.G))
mean(trainingdata$Hwy.MPG.G,na.rm = T)
trainingdata$Hwy.MPG.E = hwympg$V2
trainingdata$Hwy.MPG.E = as.numeric(as.character(trainingdata$Hwy.MPG.E))
mean(trainingdata$Hwy.MPG.E,na.rm = T)
#split combined mpg into Electric and Gas
cmbmpg=  str_split_fixed(trainingdata$Cmb.MPG, "/", 2)
cmbmpg = as.data.frame(cmbmpg)
trainingdata$Cmb.MPG.G = cmbmpg$V1
trainingdata$Cmb.MPG.G = as.numeric(as.character(trainingdata$Cmb.MPG.G))
mean(trainingdata$Cmb.MPG.G,na.rm = T)
trainingdata$Cmb.MPG.E = cmbmpg$V2
trainingdata$Cmb.MPG.E = as.numeric(as.character(trainingdata$Cmb.MPG.E))
mean(trainingdata$Cmb.MPG.E,na.rm = T)
#histogram showing the combined mpg, shows that there are 2 distinct groups
#we later discover these groups to be electric and gasoline
hist(trainingdata$Cmb.MPG.G)
#Split the Make and Model(model.solo) of the Car
brand=  str_split_fixed(trainingdata$Model, " ", 2)
brand = as.data.frame(brand)
trainingdata$Make = as.factor(brand$V1)
trainingdata$Model.solo = as.factor(brand$V1)
#Rename Gasoline/Electric cars as Hybrid and Ethanol/Gas cars into Ethanol
trainingdata$Fuel = as.character(trainingdata$Fuel)
trainingdata$Fuel = ifelse(trainingdata$Fuel == 'Gasoline/Electricity','Hybrid',
                           ifelse(trainingdata$Fuel == 'Ethanol/Gas','Ethanol',trainingdata$Fuel))
trainingdata$Fuel = as.factor(trainingdata$Fuel)
#Plot various categorical variables by the combined mpg
qplot(trainingdata$Fuel,trainingdata$Cmb.MPG.G)
#Fill the empty electric mpg.g values with the electric values to prevent missing values
trainingdata$City.MPG.E = ifelse(trainingdata$Fuel == 'Electricity',trainingdata$City.MPG.G,trainingdata$City.MPG.E)
trainingdata$Hwy.MPG.E = ifelse(trainingdata$Fuel == 'Electricity',trainingdata$Hwy.MPG.G,trainingdata$Hwy.MPG.E)
trainingdata$Cmb.MPG.E = ifelse(trainingdata$Fuel == 'Electricity',trainingdata$Cmb.MPG.G,trainingdata$Cmb.MPG.E)
#Fill the empty gasoline mpg.e values with the gasoline values to prevent missing values
trainingdata$City.MPG.E = ifelse(trainingdata$Fuel == 'Hybrid',trainingdata$City.MPG.E,trainingdata$City.MPG.G)
trainingdata$Hwy.MPG.E = ifelse(trainingdata$Fuel == 'Hybrid',trainingdata$Hwy.MPG.E,trainingdata$Hwy.MPG.G)
trainingdata$Cmb.MPG.E = ifelse(trainingdata$Fuel == 'Hybrid',trainingdata$Cmb.MPG.E,trainingdata$Cmb.MPG.G)
#average the mpg.g and mpg.e values so we can use electric and gasoline in the same sample
trainingdata$mean.City = (trainingdata$City.MPG.G+trainingdata$City.MPG.E)/2
trainingdata$mean.Hwy = (trainingdata$Hwy.MPG.G+trainingdata$Hwy.MPG.E)/2
trainingdata$mean.Cmb = (trainingdata$Cmb.MPG.G+trainingdata$Cmb.MPG.E)/2
#more graphs for visual understadning
qplot(trainingdata$Smog.Rating,trainingdata$mean.Cmb)
#split the class of the car into the class and size
class=  str_split_fixed(trainingdata$Veh.Class, " ", 2)
cor(as.numeric(as.character(trainingdata$Displ)),as.numeric(as.character(trainingdata$Cyl)))
class = as.data.frame(class)
trainingdata$Size = class$V1
trainingdata$Type = class$V2
trainingdata$Country = ifelse(trainingdata$Make== "ACURA" | trainingdata$Make=="NISSAN"|
                                trainingdata$Make=="TOYOTA"|trainingdata$Make=="HONDA"|
                                trainingdata$Make=="INFINTI" | trainingdata$Make=="LEXUS"|
                                trainingdata$Make=="SCION"|trainingdata$Make=="MAZDA"|
                                trainingdata$Make=="MITSUBISH" | trainingdata$Make=="SUBARU",
                              "JAPAN",ifelse(trainingdata$Make=="AUDI"|trainingdata$Make=="BMW"|
                                trainingdata$Make=="PORCHE"|trainingdata$Make=="VOLKSWAGEN"
                                |trainingdata$Make=="MERCEDES-BENZ","Germany",
                                ifelse(trainingdata$Make=="CHEVROLET"|trainingdata$Make=="CHRYSLER"
                                |trainingdata$Make=="DODGE"|trainingdata$Make=="FORD"|
                                  trainingdata$Make=="LINCOLN"| trainingdata$Make=="TESLA" | 
                                  trainingdata$Make=="CADILLAC","USA",ifelse(trainingdata$Make=="KIA"|
                                 trainingdata$Make=="HYUNDAI","KOREA","OTHER"))))
summary(trainingdata)
train.electric = subset(trainingdata,trainingdata$Electric == 1 | trainingdata$Hybrid==1)
summary(train.electric)
Electric = lm(Cmb.MPG.E~Greenhouse.Gas.Score+Type+Size
              +Hybrid+Country,data = train.electric)
summary(Electric) 
plot(log(train.gas$Cmb.MPG.G),resid(Gas))
#abline(a, col="blue", lwd=2)
#train.electric$Greenhouse.Gas.Score2 = train.electric$Greenhouse.Gas.Score^2
#train.electric$Greenhouse.Gas.Score3 =train.electric$Greenhouse.Gas.Score^3
#a = lm(Cmb.MPG.E~(Greenhouse.Gas.Score),data = train.electric)
#summary(a)

train.gas= subset(trainingdata, trainingdata$Gasoline == 1 | trainingdata$Diesel == 1 | 
                    trainingdata$Hydrogen == 1 | trainingdata$Ethanol == 1 )
summary(train.gas)
#Gas = lm(Cmb.MPG.G~ Displ + Cyl + Smog.Rating + Drive + Sales.Area +Veh.Class 
#         + SmartWay + Fuel+Country, data = train.gas)
Gas = lm(Cmb.MPG.G~ Displ+ Cyl + Smog.Rating + Drive + Sales.Area + Veh.Class 
         + SmartWay + Fuel+Country+, data = train.gas)

summary(Gas)
plot(train.gas$Cmb.MPG.G, resid(Gas))

Avg = lm(mean.Cmb ~ Displ + Cyl + Smog.Rating + Drive + Sales.Area + Veh.Class + SmartWay + Fuel
          + Country, data = trainingdata)
summary(trainingdata)
