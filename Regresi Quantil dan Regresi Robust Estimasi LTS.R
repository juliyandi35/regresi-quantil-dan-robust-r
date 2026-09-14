#Regresi Robust 
#Import Data
install.packages("readxl")
library(readxl)
Data<-read_excel("Data Robust Quantil.xlsx")
View(Data)
names(Data)

#Ringkasan Data
summary(Data)

#Estimasi Model dengan Regresi OLS
fit.ols<-lm(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data)
summary(fit.ols)

#Uji Asumsi Klasik
#Uji Normalitas
shapiro.test(fit.ols$residuals)

#Uji homoskedastisitas
install.packages("lmtest")
library(lmtest)
bptest(fit.ols,studentize = FALSE, data=data)

#Uji Autokorelasi
dwtest(fit.ols)

#Uji Multikolinieritas
install.packages("car")
library(car)
vif(fit.ols)

#Pemodelan outlier dengan Plot
par(mfrow=c(2,2))
plot(fit.ols)

#Memperjelas Plot
install.packages("olsrr")
library(olsrr)
B<-ols_plot_cooksd_bar(fit.ols)
summary(B)
#Regresi Robust
#Estimasi LTS
install.packages("MASS")
library(MASS)
Est_LTS<-lqs(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, Data, method="lts", model=TRUE, x.ret=FALSE, y.ret=FALSE, contrasts=NULL)
Est_LTS
Est_LTS$coefficients
Est_LTS$fitted.values
Est_LTS$residuals
ltsreg(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, Data, method="lts", model=TRUE, x.ret=FALSE, y.ret=FALSE, contrasts=NULL)
install.packages("robustbase")
library(robustbase)
LTS<-ltsReg(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data)
summary(LTS)
#Regresi Quantil
install.packages("quantreg")
library(quantreg)
Quantreg25<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = 0.25)
summary(Quantreg25)

Quantreg50<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = 0.5)
summary(Quantreg50)

Quantreg75<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = 0.75)
summary(Quantreg75)

Quantreg90<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = 0.90)
summary(Quantreg90)

#Regresi Quantil Simultan
Quantreg2590<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = c(0.25,0.90))
summary(Quantreg2590)

#ANOVA Test
anova(Quantreg25,Quantreg90)

#Ploting Data
AllQuantreg<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan,tau = seq(0.25,0.90,by=0.05), data=Data)
Quantreg.plot<-summary(AllQuantreg)
plot(Quantreg.plot)

summary.
#Mengecek Standard Error
install.packages("plotrix")
library(plotrix)

A<-data.frame(Est_LTS$coefficients)
std.error(A)
B<-data.frame(AllQuantreg$coefficients)
std.error(B)
C<-fit.ols$coefficients
std.error(C)
#Melihat nilai korelasi antar variabel
Data$Provinsi<-as.numeric(Data$Provinsi)
cor(Data)

TtestRobust<-t.test(A)
TtestRobust$p.value

TtestQuantil<-t.test(B)
TtestQuantil$p.value


summary(AllQuantreg, se="nid")

Quantreg85<-rq(DBDCase~TenKes+FasKes+Miskin+RS+Murni+Kepadatan, data=Data,tau = 0.85)
summary(Quantreg85,se="nid")
