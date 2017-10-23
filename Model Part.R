cor(trainingdata$Cmb.MPG.G,trainingdata$Displ)
trainingdata$Cmb.MPG=as.numeric(as.character(trainingdata$Cmb.MPG))

train.gas= subset(trainingdata, trainingdata$Gasoline == 1 | trainingdata$Diesel == 1 | 
                    trainingdata$Hydrogen == 1 | trainingdata$Ethanol == 1 )

cor(as.numeric(train.gas$Cyl),as.numeric(train.gas$Displ))

trainingdata$Electric= ifelse(trainingdata$Fuel =="Electricity",1,0)
trainingdata$Gasoline= ifelse(trainingdata$Fuel =="Gasoline",1,0)
trainingdata$Ethanol= ifelse(trainingdata$Fuel =="Ethanol",1,0)
trainingdata$Hydrogen= ifelse(trainingdata$Fuel =="Hydrogen",1,0)
trainingdata$Hybrid= ifelse(trainingdata$Fuel =="Hybrid",1,0)
trainingdata$Japan= ifelse(trainingdata$Country =="JAPAN",1,0)
trainingdata$Germany= ifelse(trainingdata$Country =="Germany",1,0)
trainingdata$Other= ifelse(trainingdata$Country =="OTHER",1,0)
trainingdata$USA= ifelse(trainingdata$Country =="USA",1,0)
trainingdata$Korea= ifelse(trainingdata$Country =="KOREA",1,0)
summary(trainingdata)
trainingdata$SCar= ifelse(trainingdata$Veh.Class =="small car",1,0)
trainingdata$MCar= ifelse(trainingdata$Veh.Class =="midsize car",1,0)
trainingdata$StSUV= ifelse(trainingdata$Veh.Class =="standard SUV",1,0)
trainingdata$SmSUV= ifelse(trainingdata$Veh.Class =="small SUV",1,0)
trainingdata$SWagon= ifelse(trainingdata$Veh.Class =="station wagon",1,0)
trainingdata$LCar= ifelse(trainingdata$Veh.Class =="large car",1,0)
df=trainingdata[,c(2,3,5,7,12,16,17,18,19,20,21,22,23,28,36,35,34,33,32,41,40,39,38,37,47,46,45,44,43,42)]
df$SmartWay=as.numeric(df$SmartWay)  #yes=2 elite =1
df$Drive=as.numeric(df$Drive)  #2WD=1,4WD=2
df$Sales.Area=as.numeric(df$Sales.Area)   #CA=1,FA=2

df.g=subset(df,df$Gasoline==1|df$Hybrid==1|df$Ethanol==1)
cor(df.g)
mpg.ghousegas=lm(Cmb.MPG.G~Greenhouse.Gas.Score,data = df.g)
summary(mpg.ghousegas)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghousegas))
df.g$Greenhouse.Gas.Score2=df.g$Greenhouse.Gas.Score^2
mpg.ghousegas2=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Greenhouse.Gas.Score2,data = df.g)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghousegas2))
summary(mpg.ghousegas2)
df.g$Greenhouse.Gas.Score3=df.g$Greenhouse.Gas.Score^3
mpg.ghousegas3=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Greenhouse.Gas.Score2+Greenhouse.Gas.Score3,data = df.g)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghousegas3))
summary(mpg.ghousegas3)

#Add Hybrid/Gasoline
mpg.ghg.engine=lm(Cmb.MPG.G~Greenhouse.Gas.Score+Hybrid+Gasoline,data = df.g)
summary(mpg.ghg.engine)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghg.engine))
abline(mpg.ghg.engine, col="Blue", lwd=2)

mpg.displ=lm(Cmb.MPG.G~Displ,data = df.g)
summary(mpg.displ)
df.g$Displ2=df.g$Displ^2
mpg.displ2=lm(Cmb.MPG.G~Displ+Displ2,data = df.g)
summary(mpg.displ2)
df.g$Displ3=df.g$Displ^3
mpg.displ3=lm(Cmb.MPG.G~Displ+Displ2+Displ3,data = df.g)
summary(mpg.displ3)
plot(df.g$Cmb.MPG.G,residuals(mpg.displ2))
mpg.displ=lm(Cmb.MPG.G~Displ,data = df.g)
summary(mpg.displ)

mpg.Cyl=lm(Cmb.MPG.G~Cyl,data = df.g)
summary(mpg.Cyl)
df.g$Cyl2=df.g$Cyl^2
mpg.Cyl2=lm(Cmb.MPG.G~Cyl+Cyl2,data = df.g)
summary(mpg.Cyl2)
df.g$Cyl3=df.g$Cyl^3
mpg.Cyl3=lm(Cmb.MPG.G~Cyl+Cyl2+Cyl3,data = df.g)
summary(mpg.Cyl3)
plot(df.g$Cmb.MPG.G,residuals(mpg.Cyl))
mpg.Cyl.Dipl=lm(Cmb.MPG.G~Cyl+Displ,data = df.g)
summary(mpg.Cyl.Dipl)
cor(df.g)

#Add Cyl
mpg.ghg.engine.cyl=lm(Cmb.MPG.G~Greenhouse.Gas.Score+Hybrid+Gasoline
                  +Cyl,data = df.g)
summary(mpg.ghg.engine.cyl)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghg.engine.cyl))
#Add SmartWay
mpg.ghg.engine.cyl.smart=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline
                      +Cyl+SmartWay,data = df.g)
summary(mpg.ghg.engine.cyl.smart)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghg.engine.cyl.smart))
#Add Smog.Rating
mpg.Smog.Rating=lm(Cmb.MPG.G~Smog.Rating,data = df.g)
summary(mpg.Smog.Rating)
df.g$Smog.Rating2=df.g$Smog.Rating^2
mpg.Smog.Rating2=lm(Cmb.MPG.G~Smog.Rating+Smog.Rating2,data = df.g)
summary(mpg.Smog.Rating2)
df.g$Smog.Rating3=df.g$Smog.Rating^3
mpg.Smog.Rating3=lm(Cmb.MPG.G~Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
summary(mpg.Smog.Rating3)
plot(df.g$Cmb.MPG.G,residuals(mpg.Smog.Rating3))
#View(df.g)

mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline
                            +SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
plot(df.g$Cmb.MPG.G,residuals(mpg.ghg.engine.cyl.smart.SR))
plot(df.g$Cmb.MPG.G,residuals(mpg.ghg.engine.cyl.smart))
summary(mpg.ghg.engine.cyl.smart)
summary(mpg.ghg.engine.cyl.smart.SR)
sum(residuals(mpg.ghg.engine.cyl.smart.SR)^2)
sum(residuals(mpg.ghg.engine.cyl.smart)^2)

#Add Germany
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score+Hybrid+Gasoline+Germany
                               +Cyl+SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart))
summary(mpg.ghg.engine.cyl.smart.SR)
sum(residuals(mpg.ghg.engine.cyl.smart.SR)^2)
#Add USA
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline+Germany+USA
                               +Cyl+SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
summary(mpg.ghg.engine.cyl.smart.SR)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart.SR))

cor(df.g)
#Add SCar

mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score+Hybrid+Gasoline+Germany+SCar
                               +USA+Cyl+SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart.SR))
summary(mpg.ghg.engine.cyl.smart.SR)
sum(residuals(mpg.ghg.engine.cyl.smart.SR)^2)
cor(df.g)
#Substract USA 
#Add Other
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score+Hybrid+Gasoline+Germany+SCar+Other
                               +SCar+Cyl+SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
summary(mpg.ghg.engine.cyl.smart.SR)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart.SR))
sum(residuals(mpg.ghg.engine.cyl.smart.SR)^2)
#Other is not significant 
sum(residuals(mpg.ghg.engine.cyl.smart)^2)
sum(residuals(mpg.ghg.engine.cyl)^2)
cor(df.g)

#Add SWagon, No change
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Greenhouse.Gas.Score2+Greenhouse.Gas.Score3+Hybrid+Gasoline+Germany+SCar+Other
                               +Cyl+Cyl2+Cyl3+SmartWay+Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.g)
summary(mpg.ghg.engine.cyl.smart.SR)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart.SR))
sum(residuals(mpg.ghg.engine.cyl.smart.SR)^2)
cor(df.g)

#Add Ethanol
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline+Germany
                               +Cyl+Smog.Rating+Smog.Rating2,data = df.ga)
summary(mpg.ghg.engine.cyl.smart.SR)
plot(df.g$Cmb.MPG.G, residuals(mpg.ghg.engine.cyl.smart.SR))
abline(mpg.ghg.engine.cyl.smart.SR, col="blue",lwd=2)

#Ethanol does not matter
cor(df.ga)
df.ga=df.g[,-c(8,9,10,11,34,33)]
#Add

#Electronic

train.gas= subset(trainingdata, trainingdata$Electric == 1 | trainingdata$Hybrid== 1 ) 
df.e=subset(df,df$Electric == 1 | df$Hybrid == 1)                 
cor(df.e)

#GreenHouseGas
mpg.ghousegas=lm(Cmb.MPG.E~Greenhouse.Gas.Score,data = df.e)
summary(mpg.ghousegas)
plot(df.e$Cmb.MPG.E,residuals(mpg.ghousegas))
df.e$Greenhouse.Gas.Score2=df.e$Greenhouse.Gas.Score^2
mpg.ghousegas2=lm(Cmb.MPG.E~Greenhouse.Gas.Score +Greenhouse.Gas.Score2,data = df.e)
plot(df.e$Cmb.MPG.E,residuals(mpg.ghousegas2))
summary(mpg.ghousegas2)
#df.e$Greenhouse.Gas.Score3=df.e$Greenhouse.Gas.Score^3
#mpg.ghousegas3=lm(Cmb.MPG.E~Greenhouse.Gas.Score +Greenhouse.Gas.Score2+Greenhouse.Gas.Score3,data = df.e)
#plot(df.e$Cmb.MPG.E,residuals(mpg.ghousegas3))
#summary(mpg.ghousegas3)

cor(df.e)
#SmogRating
mpg.Smog.Rating=lm(Cmb.MPG.E~Smog.Rating,data = df.e)
summary(mpg.Smog.Rating)
df.e$Smog.Rating2=df.e$Smog.Rating^2
mpg.Smog.Rating2=lm(Cmb.MPG.E~Smog.Rating+Smog.Rating2,data = df.e)
summary(mpg.Smog.Rating2)
df.e$Smog.Rating3=df.e$Smog.Rating^3
mpg.Smog.Rating3=lm(Cmb.MPG.E~Smog.Rating+Smog.Rating2+Smog.Rating3,data = df.e)
summary(mpg.Smog.Rating3)
plot(df.e$Cmb.MPG.E,residuals(mpg.Smog.Rating3))

mpg.ghg.SR=lm(Cmb.MPG.E~Greenhouse.Gas.Score
              +Smog.Rating,data = df.e)
summary(mpg.ghg.SR)
plot(df.e$Cmb.MPG.E,residuals(mpg.ghg.SR))

#Add SmartWay, not significant
mpg.ghg.SR=lm(Cmb.MPG.E~Greenhouse.Gas.Score
              +Smog.Rating+SmartWay,data = df.e)
summary(mpg.ghg.SR)
plot(df.e$Cmb.MPG.E,residuals(mpg.ghg.SR))

#Final
mpg.ghg.SR=lm(formula = Cmb.MPG.E ~ Greenhouse.Gas.Score  + Greenhouse.Gas.Score2+
                Electric  + Japan  + SCar, data = df.e)
  #lm(Cmb.MPG.E~Electric+USA+Japan+MCar+SCar+SmartWay+StSUV,data = df.e)

summary(mpg.ghg.SR)
plot(df.e$Cmb.MPG.E,residuals(mpg.ghg.SR))
df.ea=df.e[,-c(8,9,10,11,34,33)]
cor(df.ea)

###### Testing Data ########

testdata=read.xlsx(SmartWay2017.xlsx, sheetIndex = 1, header = TRUE)
testdata= data.2017 
testdata$Displ=as.numeric(as.character(testdata$Displ))
testdata$Cyl=as.numeric(as.character(testdata$Cyl))
testdata$Greenhouse.Gas.Score = as.numeric(as.character(testdata$Greenhouse.Gas.Score))
#split city mpg into Electric and Gas
citympg=  str_split_fixed(testdata$City.MPG, "/", 2)
citympg = as.data.frame(citympg)
testdata$City.MPG.G = citympg$V1
testdata$City.MPG.G = as.numeric(as.character(testdata$City.MPG.G))
mean(testdata$City.MPG.G,na.rm = T)
testdata$City.MPG.E = citympg$V2
testdata$City.MPG.E = as.numeric(as.character(testdata$City.MPG.E))
mean(testdata$City.MPG.E,na.rm = T)
#split highway mpg into Electric and Gas
hwympg=  str_split_fixed(testdata$Hwy.MPG, "/", 2)
hwympg = as.data.frame(hwympg)
testdata$Hwy.MPG.G = hwympg$V1
testdata$Hwy.MPG.G = as.numeric(as.character(testdata$Hwy.MPG.G))
mean(testdata$Hwy.MPG.G,na.rm = T)
testdata$Hwy.MPG.E = hwympg$V2
testdata$Hwy.MPG.E = as.numeric(as.character(testdata$Hwy.MPG.E))
mean(testdata$Hwy.MPG.E,na.rm = T)
#split combined mpg into Electric and Gas
cmbmpg=  str_split_fixed(testdata$Cmb.MPG, "/", 2)
cmbmpg = as.data.frame(cmbmpg)
testdata$Cmb.MPG.G = cmbmpg$V1
testdata$Cmb.MPG.G = as.numeric(as.character(testdata$Cmb.MPG.G))
mean(testdata$Cmb.MPG.G,na.rm = T)
testdata$Cmb.MPG.E = cmbmpg$V2
testdata$Cmb.MPG.E = as.numeric(as.character(testdata$Cmb.MPG.E))
mean(testdata$Cmb.MPG.E,na.rm = T)
#histogram showing the combined mpg, shows that there are 2 distinct groups
#we later discover these groups to be electric and gasoline
hist(testdata$Cmb.MPG.G)
#Split the Make and Model(model.solo) of the Car
brand=  str_split_fixed(testdata$Model, " ", 2)
brand = as.data.frame(brand)
testdata$Make = as.factor(brand$V1)
testdata$Model.solo = as.factor(brand$V1)
#Rename Gasoline/Electric cars as Hybrid and Ethanol/Gas cars into Ethanol
testdata$Fuel = as.character(testdata$Fuel)
testdata$Fuel = ifelse(testdata$Fuel == 'Gasoline/Electricity','Hybrid',
                           ifelse(testdata$Fuel == 'Ethanol/Gas','Ethanol',testdata$Fuel))
testdata$Fuel = as.factor(testdata$Fuel)
#Plot various categorical variables by the combined mpg
qplot(testdata$Fuel,testdata$Cmb.MPG.G)
#Fill the empty electric mpg.g values with the electric values to prevent missing values
testdata$City.MPG.E = ifelse(testdata$Fuel == 'Electricity',testdata$City.MPG.G,testdata$City.MPG.E)
testdata$Hwy.MPG.E = ifelse(testdata$Fuel == 'Electricity',testdata$Hwy.MPG.G,testdata$Hwy.MPG.E)
testdata$Cmb.MPG.E = ifelse(testdata$Fuel == 'Electricity',testdata$Cmb.MPG.G,testdata$Cmb.MPG.E)
#Fill the empty gasoline mpg.e values with the gasoline values to prevent missing values
testdata$City.MPG.E = ifelse(testdata$Fuel == 'Hybrid',testdata$City.MPG.E,testdata$City.MPG.G)
testdata$Hwy.MPG.E = ifelse(testdata$Fuel == 'Hybrid',testdata$Hwy.MPG.E,testdata$Hwy.MPG.G)
testdata$Cmb.MPG.E = ifelse(testdata$Fuel == 'Hybrid',testdata$Cmb.MPG.E,testdata$Cmb.MPG.G)
#average the mpg.g and mpg.e values so we can use electric and gasoline in the same sample
testdata$mean.City = (testdata$City.MPG.G+testdata$City.MPG.E)/2
testdata$mean.Hwy = (testdata$Hwy.MPG.G+testdata$Hwy.MPG.E)/2
testdata$mean.Cmb = (testdata$Cmb.MPG.G+testdata$Cmb.MPG.E)/2
#more graphs for visual understadning
qplot(testdata$Smog.Rating,testdata$mean.Cmb)
#split the class of the car into the class and size
class=  str_split_fixed(testdata$Veh.Class, " ", 2)
cor(as.numeric(as.character(testdata$Displ)),as.numeric(as.character(testdata$Cyl)))
class = as.data.frame(class)
testdata$Size = class$V1
testdata$Type = class$V2
testdata$Country = ifelse(testdata$Make== "ACURA" | testdata$Make=="NISSAN"|
                                testdata$Make=="TOYOTA"|testdata$Make=="HONDA"|
                                testdata$Make=="INFINTI" | testdata$Make=="LEXUS"|
                                testdata$Make=="SCION"|testdata$Make=="MAZDA"|
                                testdata$Make=="MITSUBISH" | testdata$Make=="SUBARU",
                              "JAPAN",ifelse(testdata$Make=="AUDI"|testdata$Make=="BMW"|
                                               testdata$Make=="PORCHE"|testdata$Make=="VOLKSWAGEN"
                                             |testdata$Make=="MERCEDES-BENZ","Germany",
                                             ifelse(testdata$Make=="CHEVROLET"|testdata$Make=="CHRYSLER"
                                                    |testdata$Make=="DODGE"|testdata$Make=="FORD"|
                                                      testdata$Make=="LINCOLN"| testdata$Make=="TESLA" | 
                                                      testdata$Make=="CADILLAC","USA",ifelse(testdata$Make=="KIA"|
                                                                                                   testdata$Make=="HYUNDAI","KOREA","OTHER"))))
testdata$Electric= ifelse(testdata$Fuel =="Electricity",1,0)
testdata$Gasoline= ifelse(testdata$Fuel =="Gasoline",1,0)
testdata$Ethanol= ifelse(testdata$Fuel =="Ethanol",1,0)
testdata$Hydrogen= ifelse(testdata$Fuel =="Hydrogen",1,0)
testdata$Hybrid= ifelse(testdata$Fuel =="Hybrid",1,0)
testdata$Japan= ifelse(testdata$Country =="JAPAN",1,0)
testdata$Germany= ifelse(testdata$Country =="Germany",1,0)
testdata$Other= ifelse(testdata$Country =="OTHER",1,0)
testdata$USA= ifelse(testdata$Country =="USA",1,0)
testdata$Korea= ifelse(testdata$Country =="KOREA",1,0)
summary(testdata)
testdata$SCar= ifelse(testdata$Veh.Class =="small car",1,0)
testdata$MCar= ifelse(testdata$Veh.Class =="midsize car",1,0)
testdata$StSUV= ifelse(testdata$Veh.Class =="standard SUV",1,0)
testdata$SmSUV= ifelse(testdata$Veh.Class =="small SUV",1,0)
testdata$SWagon= ifelse(testdata$Veh.Class =="station wagon",1,0)
testdata$LCar= ifelse(testdata$Veh.Class =="large car",1,0)
trainingdata$Cmb.MPG=as.numeric(as.character(testdata$Cmb.MPG))
df.test=testdata[,c(2,3,5,7,12,16,17,18,19,20,21,22,23,28,36,35,34,33,32,41,40,39,38,37,47,46,45,44,43,42)]
df.test$SmartWay=as.numeric(df$SmartWay)  #yes=2 elite =1
df.test$Drive=as.numeric(df$Drive)  #2WD=1,4WD=2
df.test$Sales.Area=as.numeric(df$Sales.Area)
View(df.test)
df.test.g=subset(df.test,df.test$Gasoline==1|df.test$Hybrid==1|df.test$Ethanol==1)
coef(mpg.ghg.engine.cyl.smart.SR)[2]
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline+Germany
                               +Cyl+Smog.Rating+Smog.Rating2,data = df.ga)
#Linear
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline+Germany
                               +Cyl+Smog.Rating,data = df.ga)
rating_predict.gas=(coef(mpg.ghg.engine.cyl.smart.SR)[1]+
                      coef(mpg.ghg.engine.cyl.smart.SR)[2]*df.test.g$Greenhouse.Gas.Score +
                      coef(mpg.ghg.engine.cyl.smart.SR)[3]*df.test.g$Hybrid+
                      coef(mpg.ghg.engine.cyl.smart.SR)[4]*df.test.g$Gasoline+
                      coef(mpg.ghg.engine.cyl.smart.SR)[5]*df.test.g$Germany+
                      coef(mpg.ghg.engine.cyl.smart.SR)[6]*df.test.g$Cyl+
                      coef(mpg.ghg.engine.cyl.smart.SR)[7]*df.test.g$Smog.Rating)
plot(df.test.g$Cmb.MPG.G ,rating_predict.gas)     
error1.g=sum((df.g$Cmb.MPG.G -rating_predict.gas)^2/length(rating_predict.gas))
rmse1.g=sqrt(error1.g)
#Non-linear
mpg.ghg.engine.cyl.smart.SR=lm(Cmb.MPG.G~Greenhouse.Gas.Score +Hybrid+Gasoline+Germany
                               +Cyl+Smog.Rating+Smog.Rating2,data = df.ga)
rating_predict.gas=(coef(mpg.ghg.engine.cyl.smart.SR)[1]+
                      coef(mpg.ghg.engine.cyl.smart.SR)[2]*df.test.g$Greenhouse.Gas.Score +
                      coef(mpg.ghg.engine.cyl.smart.SR)[3]*df.test.g$Hybrid+
                      coef(mpg.ghg.engine.cyl.smart.SR)[4]*df.test.g$Gasoline+
                      coef(mpg.ghg.engine.cyl.smart.SR)[5]*df.test.g$Germany+
                      coef(mpg.ghg.engine.cyl.smart.SR)[6]*df.test.g$Cyl+
                      coef(mpg.ghg.engine.cyl.smart.SR)[7]*df.test.g$Smog.Rating+
                      coef(mpg.ghg.engine.cyl.smart.SR)[8]+((df.test.g$Smog.Rating)^2))
plot(df.test.g$Cmb.MPG.G ,rating_predict.gas)
error2.g=sum((df.g$Cmb.MPG.G -rating_predict.gas)^2/length(rating_predict.gas))
rmse2.g=sqrt(error2.g)
df.test.e=subset(df.test,df.test$Electric == 1 | df.test$Hybrid == 1)   

#Linear E
mpg.ghg.SR=lm(formula = Cmb.MPG.E ~ Greenhouse.Gas.Score  +
                Electric  + Japan  + SCar, data = df.e)
rating_predict.gas2=(coef(mpg.ghg.SR)[1]+coef(mpg.ghg.SR)[2]*df.test.e$Greenhouse.Gas.Score
                     +coef(mpg.ghg.SR)[3]*(df.test.e$Electric)*
                       +coef(mpg.ghg.SR)[4]*(df.test.e$Japan)
                     +coef(mpg.ghg.SR)[5]*(df.test.e$SCar))
plot(df.test.e$Cmb.MPG.E,rating_predict.gas2)
error1=sum((df.e$Cmb.MPG.E -rating_predict.gas2)^2/length(rating_predict.gas2))
rmse1=sqrt(error1)
#Non-Linear E
mpg.ghg.SR=lm(formula = Cmb.MPG.E ~ Greenhouse.Gas.Score  + Greenhouse.Gas.Score2+
                Electric  + Japan  + SCar, data = df.e)

rating_predict.gas2=(coef(mpg.ghg.SR)[1]+coef(mpg.ghg.SR)[2]*df.test.e$Greenhouse.Gas.Score
                     +coef(mpg.ghg.SR)[3]*((df.test.e$Greenhouse.Gas.Score)^2)
                     +coef(mpg.ghg.SR)[4]*(df.test.e$Electric)*
                     +coef(mpg.ghg.SR)[5]*(df.test.e$Japan)
                     +coef(mpg.ghg.SR)[6]*(df.test.e$SCar))
plot(df.test.e$Cmb.MPG.E,rating_predict.gas2)
error2=sum((df.e$Cmb.MPG.E -rating_predict.gas2)^2/length(rating_predict.gas2))
rmse2=sqrt(error2)
