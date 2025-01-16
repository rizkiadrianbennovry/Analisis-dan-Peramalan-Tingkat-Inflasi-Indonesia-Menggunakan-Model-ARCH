library(readxl)
library(forecast)
library(TTR)
library(imputeTS)
library(tseries)
library(ggplot2)
library(dplyr)
library(graphics)
library(TSA)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggfortify)
library(cowplot)
library(lmtest)
library(stats)
library(MASS)
library(fpp2)
library(FinTS)
library(car)
library(nortest)
library(fGarch)
library(rugarch)
library(aTSA)
library(dynlm)
library(psych)
library(moments)
library(kableExtra)

#Deklarasi Data
Data_Inflasi <- read_excel("C:/Users/ASUS/Downloads/Data Inflasi BI.xlsx")
Data<-as.numeric(Data_Inflasi$'Tingkat Inflasi (%)')
Data <- rev(Data)
data<-ts(Data, start=c(2020,1), frequency = 12)
data
kableExtra::kable(head(Data_Inflasi) ,caption = 'Data Inflasi Indonesia (%) 2020-2023')


#statistika deskriptif
summary(data)
describe(data)
z <- data
boxplot(z, xlab="Tingkat Inflasi Indonesia (%) 2020-2023")

#Time Series Plot
ts.plot(data.ts, main="Tingkat Inflasi (%)", xlab="Waktu", ylab="Jumlah ", type='o')

# Plot ini mengalami penurunan ketika kondisi perekonomian mulai membaik. 
# Pola data harga penutupan emas yang terbentuk dari data tidak stasioner yang cenderung siklis dan tren positif.
data.ts<-ts(data, start= 1, end=48) 
data.ts
#splitting data
data.train <- subset(data.ts,start=1,end=36) #Data 3 Tahun pertama
data.test <- subset(data.ts,start=37,end=48) #Data 1 tahun terakhir
ts.plot(data.train, xlab="Periode", ylab="Inflasi (%)", lty=1)
title("Tingkat Inflasi Indonesia (%)")
points(data.train)


#plot acf dan pacf
acf(data.train)
pacf(data.train)
adf.test(data.train)
#data tidak stasioner karena p-value>5%

# If there are negative values or zero, adjust the data.train data
if(any(data.train <= 0)){
  data.train <- data.train + abs(min(data.train)) + 1
}

# Box-Cox transformation
lambda_seq <- seq(-2, 2, by = 0.1)
boxcox_model <- boxcox(lm(data.train ~ 1), lambda = lambda_seq)

# uji boxcox untuk tes stasioneritas ragam
best_lambda <- boxcox_model$x[which.max(boxcox_model$y)]
print(paste("Best Lambda for Box-Cox: ", best_lambda))

data.dif1 <- diff(data.train,difference=1)
plot.ts(data.dif1, lty=1, xlab="Waktu", ylab="Data Diff Ordo 1")
points(data.dif1)

adf.test(data.dif1)
#Setelah dilakukan differencing satu kali d=1, 
#pola data penupang pesawat sudah stasioner berdasarkan times series plot di atas.


#plot acf
ggAcf(data.dif1,col="black",lwd=2)+labs( x="Lag",y = "ACF",
                                             title="Plot ACF Tingkat Inflasi",
                                             subtitle = "(Januari 2020-Desember 2023)")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 11L,
                                 face = "plain",
                                 hjust = 0.5)
  )
#terpotong di lag ke 0, 3

#plot pacf
ggPacf(data.dif1,col="black",lwd=2)+labs( x="Lag",y = "PACF",
                                              title="Plot ACF Tingkat Inflasi",
                                              subtitle = "(Januari 2020-Desember 2023)",col="white")+
  theme_bw()+
  theme(
    plot.title = element_text(size = 14L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 11L,
                                 face = "plain",
                                 hjust = 0.5)
  )
#terpotong di lag ke 0, 3


#Signifikansi Model
model1 <- Arima(data.dif1,order = c(0,1,0),method ="ML")
model2 <- Arima(data.dif1,order = c(0,1,3),method ="ML")
model3 <- Arima(data.dif1,order = c(3,1,0),method ="ML")
model4 <- Arima(data.dif1,order = c(3,1,3),method ="ML")

Model <- c("ARIMA (0,1,0)","ARIMA (0,1,3)","ARIMA (3,1,0)","ARIMA (3,1,3)")
AIC <- c(model1$aic,model2$aic,model3$aic,model4$aic)
BIC <- c(model1$bic,model2$bic,model3$bic,model4$bic)
Akurasi <- data.frame(Model,AIC,BIC)
kableExtra::kable(Akurasi)
paste("Model yang terbaik adalah model",Akurasi$Model[which.min(Akurasi[,"AIC"])])

#Uji Diagnostik
#Diagnostik Model: Eksploratif
#Analisis sisaan dapat dilihat secara eksploratif menggunakan Q-Q plot, residuals plot, ACF dan PACF plot.
sisaan <- model3$residuals
par(mfrow=c(2,2))
qqnorm(sisaan)
qqline(sisaan, col = "red", lwd =1)
plot(c(1:length(sisaan)),sisaan)
acf(sisaan)
pacf(sisaan)

#Berdasarkan hasil eksplorasi menggunakan Q-Q plot, 
#terlihat bahwa sisaan berdistribusi mengikuti garis normal, 
#sehingga dapat dikatakan bahwa sisaan menyebar normal. 
#Kemudian, plot sisaan yang diperoleh menunjukkan bahwa sisaan memiliki pola acak dan tersebar di sekitar nilai nol. 
#Sedangkan pada plot ACF dan PACF, nilai awal amatan tidak melewati garis signifikan, 
#atau dapat dikatakan bahwa sisaan saling bebas

#Diagnostik Model: Uji Formal
#1. Sisaan Menyebar Normal
jarque.bera.test(sisaan)
shapiro.test(sisaan)
#Berdasarkan Jarque-Bera test, diperoleh p-value (1.565e-10) < α (0.05). 
#Selain itu, hasil Shapiro-Wilk test, diperoleh p-value (0.000156) < α (0.05) maka tolak H0. 
#Artinya, tidak cukup bukti untuk menyatakan bahwa sisaan menyebar normal pada taraf nyata 5%. 
#Namun, asumsi normalitas tidak wajib terpenuhi.

#2. Sisaan Saling Bebas
#Uji formal ini dilakukan dengan LJung-Box test.
Box.test(sisaan, type = "Ljung")
#Berdasarkan LJung-Box test, diperoleh p-value (0.9711) > α (0.05), maka tak tolak H0. Artinya, cukup bukti untuk menyatakan bahwa sisaan antara lag saling bebas atau dapat dikatakan tidak ada autokorelasi antara sisaan lag pada taraf nyata 5%.

#3. Nilai Tengah Sisaan Sama dengan Nol
t.test(sisaan, mu = 0, conf.level = 0.95)

#Forecasting ARIMA
ramalan <- forecast::forecast(Arima(data.train, order=c(3, 1, 0),method="ML",include.drift = TRUE), h=12) 
data.ramalan <- ramalan$mean
plot(ramalan,lwd=2)
testing
data.ramalan

perbandingan.temp<-matrix(data=c(data.test[1:5], data.ramalan[1:5]), nrow = 5, ncol = 2)
colnames(perbandingan.temp)<-c("Aktual","Hasil Forecast")
head(perbandingan.temp)

accuracy(data.test,data.ramalan)

# Plot hasil peramalan
plot(data.ramalan, lwd=2, main="Hasil Peramalan", col="red", ylab="Tingkat Inflasi (%)", xlab="Tahun")

# Membandingkan data aktual dengan peramalan
model <- Arima(data.train, order=c(3, 1, 0), method="ML", include.drift = TRUE)

# Model ARIMA (2,1,1) menggunakan Data Awal Y
model.diag = stats::arima(data.dif1, order = c(3,1,0), method="ML")
model.diag
sisaan = checkresiduals(model.diag)


# ARCH
library(dynlm)
data.mean <- dynlm(data.dif1~1)
summary(data.mean)

# Evaluasi perilaku volatilitas - variance model
ehatsq <- ts(resid(data.mean)^2)
data.ARCH <- dynlm(ehatsq~L(ehatsq))
summary(data.ARCH)

# Evaluasi perilaku volatilitas - ARCH Test
library(FinTS)
dataArchTest <- ArchTest(data.dif1, lags=1, demean=TRUE)
dataArchTest

library(FinTS)
dataArchTest2 <- ArchTest(data.dif1, lags=2, demean=TRUE)
dataArchTest2

# Evaluasi perilaku volatilitas – model ragam
library(tseries)
data.arch <- garch(data.dif1,c(0,1))

spricearch <- summary(data.arch)
spricearch

hhat <- ts(2*data.arch$fitted.values[-1,1]^2)
plot.ts(hhat)
ggplot(aes(y = hhat, x = date)) + geom_line(col = '#ff9933') + ylab('Conditional Variance') + xlab('Date')

