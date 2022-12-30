library(lubridate)
library(tidyverse)
library(psych)
library(forecast)
library(fUnitRoots)
library(forecast)
library(rugarch)


#load data
data=read.csv("/Users/baeknarim/2022_2/시계열분석/project/Nasdaq 100 Historical Data.csv")
vol=read.csv("/Users/baeknarim/2022_2/시계열분석/project/NASDAQ 100 Volatility Historical Data-3.csv")


data$Date = mdy(data$Date)
vol$Date = mdy(vol$Date)
result=inner_join(data, vol, by="Date")
head(result)
result2=result[,c(1,2,8)]
names(result2)=c("Date", "Pt","Vt")
result2$Pt= as.numeric(gsub(",","",result2$Pt))

result2 = result2[order(result2$Date),]
attach(result2)
write.csv(result2,"/Users/baeknarim/2022_2/시계열분석/project/NASDAQ100_data", row.names=FALSE)

#A1, 기초통계분석
###1 일별가격,로그수익율,변동성지수를 구해서 시도표그리기
#Pt 시도표
plot(Pt, type="l",xlab="t", ylab="Pt")
#로그수익률 시도표 
return =diff(log(result2$Pt))
plot(return, type="l",xlab="t", ylab="rt")

#변동성지수 시도표 
plot(Vt, type="l",xlab="t", ylab="Vt")

###2.Pt, Rt, Vt의 기본통계량 표를 작성하라 -대표값, 산포, 비대칭성, 첨도, 정규성 등등 이들 통계량값을 통해 자료의 특징을 간단하게 기술하라
#Pt
describe(Pt)

#Rt
describe(return)
#Vt
describe(Vt)


###3. Pt, return, Vt의 표본자기상관함수를 lag 을 250 까지 그리기, 각 계열의 자기상관의 특징을 간단하게 설명하라.
acf(Pt, lag=250)

acf(return,lag=250) 

acf(Vt, lag=250)

#A2. 모형추정, 검진 및 예측
###Pt, return, Vt각에 대해 정상성 여부를 ADF 검정

#Pt
aic=c()
for(p in 1:20){
  ar.fit=Arima(Pt,order=c(p,0,0))
  aic[p] = ar.fit$aic
}
plot(aic)
which.min(aic) #19

adfTest(Pt, type="ct", lags=18)
#p-value > 0.05 H0  do not rejected at 5% 
#Pt는 확률적 추세이다.단위근 계열로 차분이 필요하다. 


#return
aic=c()
for(p in 1:20){
  ar.fit=Arima(return,order=c(p,0,0))
  aic[p] = ar.fit$aic
}
plot(aic)
which.min(aic) #9

adfTest(return, type="c", lags=8)
#p-value < 0.05 H0  rejected at 5% 
#Rt는 확정적 추세이다.  차분이 필요없다  


#Vt
aic=c()
for(p in 1:20){
  ar.fit=Arima(Vt,order=c(p,0,0))
  aic[p] = ar.fit$aic
}
plot(aic)
which.min(aic) #13

adfTest(Vt, type="c", lags=12)
#p-value < 0.05 H0  rejected at 5% 
#Vt는 확정적 추세이다. 차분이 필요없다  

###Pt, return, Vt각에 대해 ARIMA 모델을 BIC 기준으로 identify 추정치의 표준오차도 명시
#Pt - arima(9,1,0)
bic=c()
for(p in 1:20){
  arima.fit = Arima(Pt, order = c(p-1,1,0)); bic[p] = arima.fit$bic}
which.min(bic) #10

arima.fit = Arima(Pt, order = c(9,1,0))
arima.fit
#표준오차 :118.4314

#return -  #arima(9,0,0) 
bic=c()
for(p in 1:20){
  arima.fit = Arima(return, order = c(p-1,0,0)); bic[p] = arima.fit$bic}
which.min(bic) #10

arima.fit = Arima(return, order = c(9,0,0))
arima.fit
#표준오차 0.01286857

#Vt - #arima(2,0,0) 
bic=c()
for(p in 1:20){
  arima.fit = Arima(Vt, order = c(p-1,0,0)); bic[p] = arima.fit$bic}
which.min(bic) #3

arima.fit = Arima(Vt, order = c(2,0,0))
arima.fit
#표준오차 1.672423

aic=c()
for(p in 1:20){
  arima.fit = Arima(Vt, order = c(p-1,0,0)); aic[p] = arima.fit$aic}
which.min(aic) #14

arima.fit = Arima(Vt, order = c(13,0,0))
arima.fit
#표준오차 1.656502

###위에서 추정한 모형의 타당성을 검진하라 
#Pt - arima(9,1,0)
arima.fit = Arima(Pt, order = c(9,1,0))
L=c();Q=c();p=c()
for(i in 1:4){
  L[i]=6*i
  Q[i]=Box.test(arima.fit$residual, lag=L[i],type="Ljung-Box")$statistic
  df=L[i]
  p[i]=1-pchisq(Q[i],df)}
Portmanteau=cbind(L,Q,p)
Portmanteau
#6,12,18이 기각되지 않기 때문에 arima(9,1,0) 적절해 보인다 
acf(arima.fit$residual)

#return -  #arima(9,0,0) 
arima.fit = Arima(return, order = c(9,0,0))
L=c();Q=c();p=c()
for(i in 1:4){
  L[i]=6*i
  Q[i]=Box.test(arima.fit$residual, lag=L[i],type="Ljung-Box")$statistic
  df=L[i]
  p[i]=1-pchisq(Q[i],df)}
Portmanteau=cbind(L,Q,p)
Portmanteau
#모두 기각되지 않기 때문에 arima(9,1,0) 적절해 보인다 
acf(arima.fit$residual)

#Vt - #arima(2,0,0) 
#bic 기준 2
arima.fit = Arima(Vt, order = c(2,0,0))
L=c();Q=c();p=c()
for(i in 1:4){
  L[i]=6*i
  Q[i]=Box.test(arima.fit$residual, lag=L[i],type="Ljung-Box")$statistic
  df=L[i]
  p[i]=1-pchisq(Q[i],df)}
Portmanteau=cbind(L,Q,p)
Portmanteau
acf(arima.fit$residual)
#모두 기각되지 않는다. 그래서 p를 bic기준이 아닌, aic기준 선택된 13을 이용한다 

#13 - aic 기준 
arima.fit = Arima(Vt, order = c(13,0,0))
L=c();Q=c();p=c()
for(i in 1:4){
  L[i]=6*i
  Q[i]=Box.test(arima.fit$residual, lag=L[i],type="Ljung-Box")$statistic
  df=L[i]
  p[i]=1-pchisq(Q[i],df)}
Portmanteau=cbind(L,Q,p)
Portmanteau
#모두 기각되지 않으므로 Vt모형은 ARIMA(13,0,0)이 적절해 보인다. 
acf(arima.fit$residual)

###Pt, return, Vt각에 각각에 대해 데이터의 마지막 시점 T 에서 일주일간 미래 T+1, T+2, ..T+5에서의 미래값을 예측하고 95% 예측구간을 그리기 이그림은 데이터 시점 T-22, T-21, ..., T, T+1, ..., T+5 만 포함되게 한다
#Pt - arima(9,1,0)
arima.fit = Arima(Pt, order = c(9,1,0))
ar.hat = forecast(arima.fit, h=5)
plot(ar.hat, xlim=c(2741,2768))

#return - arima(9,0,0) 
arima.fit = Arima(return, order = c(9,0,0))
ar.hat = forecast(arima.fit, h=5)
plot(ar.hat, xlim=c(2740,2767))

#Vt - arima(13,0,0)
arima.fit = Arima(Vt, order = c(13,0,0))
ar.hat = forecast(arima.fit, h=5)
plot(ar.hat, xlim=c(2741,2768))

#A3. out-of-sample 예측력 비교
#return 에 대해 AIC order AR 모형과 BIC order AR 모형의 예측력을 비교하라

###1. 전체데이터중 앞부분 85% 를 이용하여 AIC order AR 모형과 BIC order AR 모형을 추정하라
#return 
n=length(return); m=round(0.15*n)
train=return[1:n]

#bic 기준 arima(9,0,0)
bic=c()
for(p in 1:20){
  arima.fit = Arima(train, order = c(p-1,0,0)); bic[p] = arima.fit$bic}
which.min(bic)  #10

plot(bic)
arima.fit = Arima(train, order = c(9,0,0))
arima.fit


#aic 기준 arima(9,0,0)
aic=c()
for(p in 1:20){
  arima.fit = Arima(train, order = c(p-1,0,0)); aic[p] = arima.fit$aic}
which.min(aic)  #10
plot(aic)

arima.fit = Arima(train, order = c(9,0,0))
arima.fit

###2.각모형의 최근 15% 데이터의 1-step, 2-step, 3-step, 4-step, 5-step 예측치의 RMSE, MAE, MAPE 비교표를 작성하라
#위 1번에서 AIC BIC기준이 동일하게 AR(9)가 나왔다. 그렇기 때문에 BIC AIC plot을 그려본 결과 그다음으로 낮은 AR(10)을 비교모델로 선정하고, RMSE, MAE, MAPE기준으로 비교해보겠다. 
n = length(return) #2763
m = round(0.15*n) #414
y = return

#1단계 예측
#AR(9)
e.aic_step1 = c()
for(k in 1:m){
  N = n-k
  ar1 = Arima(y[1:N], order = c(9,0,0)) #1~2349 데이터로 fitting
  y.hat_aic_step1 = forecast(ar1, h=1)$mean[1]  #2349~ 부터 예측
  e.aic_step1[k] = y[(N+1)] - y.hat_aic_step1
}

#2단계 예측
e.aic_step2 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(9,0,0)) 
  y[2349] = forecast(ar, h=2)$mean[1] 
  
  y.hat = forecast(ar1, h=2)$mean[2] 
  
  e.aic_step2[1] = return[2349] - y[2349]
  e.aic_step2[m+2-k] = y[(N+2)] - y.hat
}


#3단계 예측
e.aic_step3 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(9,0,0))
  y[2349] = forecast(ar, h=3)$mean[1] 
  y[2350] = forecast(ar, h=3)$mean[2] 
  
  y.hat = forecast(ar1, h=3)$mean[3] 
  
  e.aic_step3[1] = return[2349] - y[2349]
  e.aic_step3[2] = return[2350] - y[2350]
  e.aic_step3[m+3-k] = y[(N+3)] - y.hat
}


#4단계 예측
e.aic_step4 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(9,0,0))
  y[2349] = forecast(ar, h=4)$mean[1] 
  y[2350] = forecast(ar, h=4)$mean[2] 
  y[2351] = forecast(ar, h=4)$mean[3] 
  
  y.hat = forecast(ar1, h=4)$mean[4] 
  
  e.aic_step4[1] = return[2349] - y[2349]
  e.aic_step4[2] = return[2350] - y[2359]
  e.aic_step4[3] = return[2351] - y[2351]
  
  e.aic_step4[m+4-k] = y[(N+4)] - y.hat
}


#5단계 예측
e.aic_step5 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(9,0,0))
  y[2349] = forecast(ar, h=5)$mean[1] 
  y[2350] = forecast(ar, h=5)$mean[2] 
  y[2351] = forecast(ar, h=5)$mean[3] 
  y[2352] = forecast(ar, h=5)$mean[4] 
  
  y.hat = forecast(ar1, h=5)$mean[5] 
  
  e.aic_step5[1] = return[2349] - y[2349]
  e.aic_step5[2] = return[2350] - y[2350]
  e.aic_step5[3] = return[2351] - y[2351]
  e.aic_step5[4] = return[2352] - y[2352]
  
  e.aic_step5[m+5-k] = y[(N+5)] - y.hat
}

#AIC order model
RMSE<- c(sd(e.aic_step1),sd(e.aic_step2[1:m]),sd(e.aic_step3[1:m]),sd(e.aic_step4[1:m]),sd(e.aic_step5[1:m]))

MAE<- c(mean(abs(e.aic_step1)),mean(abs(e.aic_step2[1:m])),mean(abs(e.aic_step3[1:m])),mean(abs(e.aic_step4[1:m])),mean(abs(e.aic_step5[1:m])))

y.test = return[2350:2763]
MAPE<-c(100*mean(abs(e.aic_step1/(y.test+1))),
        100*mean(abs(e.aic_step2[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step3[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step4[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step5[1:m]/(y.test+1)))
)

result_aic<- data.frame(RMSE = RMSE, MAE = MAE, MAPE = MAPE)
result_aic



#AR(10)
#1단계 예측
e.aic_step1 = c()
for(k in 1:m){
  N = n-k
  ar1 = Arima(y[1:N], order = c(10,0,0)) #1~2349 데이터로 fitting
  y.hat_aic_step1 = forecast(ar1, h=1)$mean[1]  #2349~ 부터 예측
  e.aic_step1[k] = y[(N+1)] - y.hat_aic_step1
}

#2단계 예측
e.aic_step2 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(10,0,0)) 
  y[2349] = forecast(ar, h=2)$mean[1] 
  
  y.hat = forecast(ar1, h=2)$mean[2] 
  
  e.aic_step2[1] = return[2349] - y[2349]
  e.aic_step2[m+2-k] = y[(N+2)] - y.hat
}


#3단계 예측
e.aic_step3 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(10,0,0))
  y[2349] = forecast(ar, h=3)$mean[1] 
  y[2350] = forecast(ar, h=3)$mean[2] 
  
  y.hat = forecast(ar1, h=3)$mean[3] 
  
  e.aic_step3[1] = return[2349] - y[2349]
  e.aic_step3[2] = return[2350] - y[2350]
  e.aic_step3[m+3-k] = y[(N+3)] - y.hat
}


#4단계 예측
e.aic_step4 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(10,0,0))
  y[2349] = forecast(ar, h=4)$mean[1] 
  y[2350] = forecast(ar, h=4)$mean[2] 
  y[2351] = forecast(ar, h=4)$mean[3] 
  
  y.hat = forecast(ar1, h=4)$mean[4] 
  
  e.aic_step4[1] = return[2349] - y[2349]
  e.aic_step4[2] = return[2350] - y[2359]
  e.aic_step4[3] = return[2351] - y[2351]
  
  e.aic_step4[m+4-k] = y[(N+4)] - y.hat
}


#5단계 예측
e.aic_step5 = c()
for(k in m:1){
  N = n-k
  ar = Arima(y[1:N], order = c(10,0,0))
  y[2349] = forecast(ar, h=5)$mean[1] 
  y[2350] = forecast(ar, h=5)$mean[2] 
  y[2351] = forecast(ar, h=5)$mean[3] 
  y[2352] = forecast(ar, h=5)$mean[4] 
  
  y.hat = forecast(ar1, h=5)$mean[5] 
  
  e.aic_step5[1] = return[2349] - y[2349]
  e.aic_step5[2] = return[2350] - y[2350]
  e.aic_step5[3] = return[2351] - y[2351]
  e.aic_step5[4] = return[2352] - y[2352]
  
  e.aic_step5[m+5-k] = y[(N+5)] - y.hat
}

#AIC order model
RMSE<- c(sd(e.aic_step1),sd(e.aic_step2[1:m]),sd(e.aic_step3[1:m]),sd(e.aic_step4[1:m]),sd(e.aic_step5[1:m]))

MAE<- c(mean(abs(e.aic_step1)),mean(abs(e.aic_step2[1:m])),mean(abs(e.aic_step3[1:m])),mean(abs(e.aic_step4[1:m])),mean(abs(e.aic_step5[1:m])))

y.test = return[2350:2763]
MAPE<-c(100*mean(abs(e.aic_step1/(y.test+1))),
        100*mean(abs(e.aic_step2[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step3[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step4[1:m]/(y.test+1))),
        100*mean(abs(e.aic_step5[1:m]/(y.test+1)))
)

result_aic<- data.frame(RMSE = RMSE, MAE = MAE, MAPE = MAPE)
result_aic

###3. 비교 결과를 간단히 설명하라.

#A5.  Rt에 대한 GARCH모형추정및 Pt, Rt의 5%VaR
### rt에 대해 AR(0)+GARCH(1,1) 과 AR(0)+GJR-GARCH(1,1) 모형을 추정하고 추정식을 써라.정규분포와 표준화 t-분포 사용하라. 총 4개의 모형을 추정
r.t = return
#AR(0)+GARCH(1,1) 
#정규분포 
spec.garch = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="norm") 
a.t = r.t - mean(r.t)
garch.fit = ugarchfit(data = a.t, spec=spec.garch)
garch.fit

#t분포 
spec.garch.t = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std") 
a.t = r.t - mean(r.t)
garch.fit.t = ugarchfit(data = a.t, spec=spec.garch.t)
garch.fit.t

#AR(0)+GJR-GARCH(1,1) 모형
#정규분포
gjr.garch = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="norm") 

a.t=r.t-mean(r.t)
gjr.garch.fit=ugarchfit(data=a.t, spec=gjr.garch)
gjr.garch.fit

#t분포
gjr.garch.t = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                         distribution.model="std") 

a.t=r.t-mean(r.t)
gjr.garch.fit.t=ugarchfit(data=a.t, spec=gjr.garch.t)
gjr.garch.fit.t


### AR(0)+GARCH(1,1) 모형으로부터 계산된 sigma hat 시도표를 그려보고 이를 rt, IVt의 시도표와 비교 설명하여라
r.t = return
v.t = Vt

#AR(0)+GARCH(1,1) 
#정규분포 
spec.garch = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="norm") 
a.t = r.t - mean(r.t)
garch.fit = ugarchfit(data = a.t, spec=spec.garch)

#sigma hat 시도표 
sigma.fit=garch.fit@fit$sigma
plot(sigma.fit, type="l",xlab="t", ylab="rt")

#t분포 
spec.garch.t = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std") 
a.t = r.t - mean(r.t)
garch.fit.t = ugarchfit(data = a.t, spec=spec.garch.t)
garch.fit.t
#sigma hat 시도표 
sigma.fit=garch.fit.t@fit$sigma
plot(sigma.fit, type="l",xlab="t", ylab="rt")

#rt
plot(r.t, type="l",xlab="t", ylab="rt")

#IVt 시도표 (Vt)
#변동성지수 시도표 
plot(v.t, type="l",xlab="t", ylab="Vt")

### r_t+1, .. r_t+5의 조건부 표준편차의 예측치를 구하시오 또한 이를 시도표 그림으로 나타내라 이그림은 데이터 시점 t = T-22, T-21, ..., T, T+1,..., T+5 에서의 sigma만 포함되게 한다 
sigma.hat5=ugarchforecast(garch.fit, n.ahead=5)@forecast$sigmaFor
sigma.hat5

#plot 
plot(c(sigma.fit[2741:2763] , sigma.hat5), type="l",xlab="t", ylab="sigma_hat")


### 위1의 각4개의 모형을 이용하여 r_t+1의 1%, 5% VaR 구하고 표로 작성하라. 이중 어느 모형의 VaR 더 좋은지 어떻게 평가할지를 설명하라 또 P_t+1의 1% 5% VAR 구하고 표를 작성하라 
#내일 log return의 1% 5% VAR
#AR(0)+GARCH(1,1) 
#정규분포 
ar.fit = Arima(r.t, order = c(0,0,0))

spec.garch = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                        mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                        distribution.model="norm") 
a.t = r.t - mean(r.t)
garch.fit = ugarchfit(data = a.t, spec=spec.garch)
sigma.hat=ugarchforecast(garch.fit, n.ahead=1)@forecast$sigmaFor
mu= ar.fit$coef[1]

# 5% value at risk of return
garch_nor_rt_5=mu + qnorm(0.05) * sigma.hat 
# 1% value at risk of return
garch_nor_rt_1=mu + qnorm(0.01) * sigma.hat

# Pt 5% value at risk of return
log.Pt_1 = mu + qnorm(0.05) * sigma.hat + log(Pt[n]) #7.688656 
garch_nor_pt_5=exp(log.Pt_1)*exp(sigma.hat^2/2)

# Pt 1% value at risk of return
log.Pt_1 = mu + qnorm(0.01) * sigma.hat + log(Pt[n]) #7.688656 
garch_nor_pt_1=exp(log.Pt_1)*exp(sigma.hat^2/2)


#AR(0)+GARCH(1,1) 
#t분포 
ar.fit = Arima(r.t, order = c(0,0,0))

spec.garch.t = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                          mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                          distribution.model="std") 
a.t = r.t - mean(r.t)
garch.fit.t = ugarchfit(data = a.t, spec=spec.garch.t)
sigma.hat=ugarchforecast(garch.fit.t, n.ahead=1)@forecast$sigmaFor
mu= ar.fit$coef[1]

# 5% value at risk of return
garch_t_rt_5=mu + qt(0.05,df=6) * sigma.hat 

# 1% value at risk of return
garch_t_rt_1=mu + qt(0.01,df=6) * sigma.hat

# Pt 5% value at risk of return
log.Pt_1 = mu + qt(0.05,df=6) * sigma.hat + log(Pt[n]) #7.688656 
garch_t_pt_5=exp(log.Pt_1)*exp(sigma.hat^2/2)

# Pt 1% value at risk of return
log.Pt_1 = mu + qt(0.01,df=6) * sigma.hat + log(Pt[n]) #7.688656 
garch_t_pt_1=exp(log.Pt_1)*exp(sigma.hat^2/2)


#AR(0)+GJR-GARCH(1,1) 모형
#정규분포
ar.fit = Arima(r.t, order = c(0,0,0))
gjr.garch = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                       mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                       distribution.model="norm") 

a.t=r.t-mean(r.t)
gjr.garch.fit=ugarchfit(data=a.t, spec=gjr.garch)

sigma.hat=ugarchforecast(gjr.garch.fit, n.ahead=1)@forecast$sigmaFor
mu= ar.fit$coef[1]

# 5% value at risk of return
GJR_nor_rt_5=mu + qnorm(0.05) * sigma.hat 

# 1% value at risk of return
GJR_nor_rt_1=mu + qnorm(0.01) * sigma.hat

# Pt 5% value at risk of return
log.Pt_1 = mu + qnorm(0.05) * sigma.hat + log(Pt[n]) #7.688656 
GJR_nor_pt_5=exp(log.Pt_1)*exp(sigma.hat^2/2)

# Pt 1% value at risk of return
log.Pt_1 = mu + qnorm(0.01) * sigma.hat + log(Pt[n]) #7.688656 
GJR_nor_pt_1=exp(log.Pt_1)*exp(sigma.hat^2/2)


#AR(0)+GJR-GARCH(1,1) 모형
#t분포
ar.fit = Arima(r.t, order = c(0,0,0))
gjr.garch.t = ugarchspec(variance.model=list(model="gjrGARCH",garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(0,0), include.mean=FALSE),
                         distribution.model="std") 

a.t=r.t-mean(r.t)
gjr.garch.fit.t=ugarchfit(data=a.t, spec=gjr.garch.t)
sigma.hat=ugarchforecast(gjr.garch.t, n.ahead=1)@forecast$sigmaFor
mu= ar.fit$coef[1]

# 5% value at risk of return
GJR_t_rt_5=mu + qt(0.05,df=7) * sigma.hat 

# 1% value at risk of return
GJR_t_rt_1=mu + qt(0.01,df=7) * sigma.hat

# Pt 5% value at risk of return
log.Pt_1 = mu + qt(0.05,df=7) * sigma.hat + log(Pt[n]) #7.688656 
GJR_t_pt_5=exp(log.Pt_1)*exp(sigma.hat^2/2)

# Pt 1% value at risk of return
log.Pt_1 = mu + qt(0.01,df=7) * sigma.hat + log(Pt[n]) #7.688656 
GJR_t_pt_1=exp(log.Pt_1)*exp(sigma.hat^2/2)

#result dataframe 
var_rt=data.frame(var5=c(garch_nor_rt_5,garch_t_rt_5,GJR_nor_rt_5,GJR_t_rt_5),
                  var1=c(garch_nor_rt_1,garch_t_rt_1,GJR_nor_rt_1,GJR_t_rt_1))


#현재가 11994.26
var_pt=data.frame(var5=c(garch_nor_pt_5,garch_t_pt_5,GJR_nor_pt_5,GJR_t_pt_5),
                  var1=c(garch_nor_pt_1,garch_t_pt_1,GJR_nor_pt_1,GJR_t_pt_1))

var_rt;var_pt


#AR6 GBM
###위A1-1의데이터를이용하여 mu, sigma를추정(단위 명시)
#Pt: 11년치 data 
r_bar<-mean(return) #일별 평균 로그수익률 
s_r<-sd(return) #일별 로그수익률 표준편차 

sigma.hat=sd(return) * sqrt(250) # 연 표준편차
mu.hat=mean(return) * 250 + sigma^2/2 # 연 평균

mu.hat;sigma.hat # per year

###ps = lnPs라 할 때 ps가 만족하는 모형을 적어라. 모수값도 적어야함
mu.hat-sigma.hat^2/2
sigma.hat
P0=tail(Pt,1)
log(P0)

###마지막 시점 T (대응되는 s 값은 T/250)서 조건부로 2년후의 미래값 P_s+2 의 조건부 분포를 구하고 평균 분산을 구하시오.
#lnP3 ~N(9.374736+ 0.1467989*2,  0.04331835*2)
log(P0)
mu.hat2 = 9.374736+ 0.1467989*2 
var.hat2 =0.04331835*2 

exp(mu.hat2+var.hat2/2)
exp(2*mu.hat2+var.hat2)*(exp(var.hat2)-1)


###마지막 시점 T (대응되는 s 값은 T/250)서조건부로 향후 2년간의 미래수익률 r_s+2 = lnP_s+2 - lnP_s의 조건부 분포를 구하고 평균 분산을 구하시오.
#r~ N(mu.hat - sig.hat^2/2, sig.hat^2/2)
mu.hat - sigma.hat^2/2
sigma.hat^2/2

###이자산의 마지막가격 P_t 를쓰시오.또만기6개월,행사가 P_t인 Europeancall option 의 공정한 현재가를 구하시오.
P0=tail(Pt,1) #현재가 
K=11786.8 #행사가 
T=6/12
r=mu.hat
vol=sigma.hat
h1=(log(P0/K))+(r+vol^2/2)*T/(vol*sqrt(T))
h2=(log(P0/K))+(r-vol^2/2)*T/(vol*sqrt(T))
Ct =P0*pnorm(h1) - K*exp(-r*T)*pnorm(h2)
Ct


#AR7 Simulation for GARCH and GJR-GARCH

###위 A5-1에서 추정된 AR(0)+GARCH(1,1) 과 AR(0)+GJR-GARCH(1,1) 모수를 이용하 여 추정된 모형을 참모형으로 간주하고 r_t, t=1..n,n=1000 simulate 하여라. 그 후 각 모형별로 skewness 를 계산하여라.
#AR(0)+GARCH(1,1)
#garch.fit
#omega=0.000005, alpha=0.144727, beta1=0.823771

sigma=c();epsilon=c();a=c();
n = 1000;set.seed(1); epsilon=rnorm(n);
alpha_0=0.000005; alpha_1=0.144727; beta_1=0.823771
sigma[1]=sqrt(alpha_0/(1-alpha_1-beta_1)) #uncond. variance 
a[1] = sigma[1]*epsilon[1]
for (t in 2:n){
  sigma[t]= sqrt(alpha_0 + alpha_1*(a[t-1])^2+beta_1*(sigma[t-1])^2); a[t]=sigma[t]*epsilon[t]}

skew(a) 

#AR(0)+GARCH(1,1) + t분포  추정
#omega=0.000003, alpha=0.147735 , beta1=0.841785, shape(df)=5.640680
sigma=c();epsilon=c();a=c();
alpha_0=0.000003; alpha_1=0.147735; beta_1=0.841785; shape=5.640680
n = 1000;set.seed(1); epsilon=rt(n,df=shape);
sigma[1]=sqrt(alpha_0/(1-alpha_1-beta_1)) #uncond. variance 
a[1] = sigma[1]*epsilon[1]
for (t in 2:n){
  sigma[t]= sqrt(alpha_0 + alpha_1*(a[t-1])^2+beta_1*(sigma[t-1])^2); a[t]=sigma[t]*epsilon[t]}

skew(a)



#AR(0)+GJR-GARCH(1,1) 모형 + normal 분포 
#gjr.garch.fit
#omega=0.000005 , alpha=0.018093, beta1=0.853590 , gamma1= 0.184960 
sigma=c();epsilon=c();a=c();
alpha_0=0.000005; alpha_1=0.018093; beta_1=0.853590;gamma1=0.184960
n = 1000;set.seed(1); epsilon=rnorm(n);
sigma[1]=sqrt(alpha_0/(1-alpha_1-gamma1/2-beta_1)) #uncond. variance 
a[1] = sigma[1]*epsilon[1]
for (t in 2:n){
  sigma[t]= sqrt(alpha_0 + alpha_1*(a[t-1])^2 + gamma1*ifelse(a[t-1]<0,1,0)*a[t-1]^2+beta_1*(sigma[t-1])^2)
  a[t]=sigma[t]*epsilon[t]}

skew(a)

#AR(0)+GJR-GARCH(1,1)  + t분포  추정
#gjr.garch.fit.t
#omega=0.000004 , alpha=0.000000 , beta1=0.849232 , gamma1=0.257918, shape=5.927278
sigma=c();epsilon=c();a=c();
alpha_0=0.000004; alpha_1=0.000000; beta_1=0.849232;gamma1=0.257918;shape=5.927278

n = 1000;set.seed(1); epsilon=rt(n,df=shape);
sigma[1]=sqrt(alpha_0/(1-alpha_1-gamma1/2-beta_1))  #uncond. variance 
a[1] = sigma[1]*epsilon[1]
for (t in 2:n){
  sigma[t]= sqrt(alpha_0 + alpha_1*(a[t-1])^2 + gamma1*ifelse(a[t-1]<0,1,0)*a[t-1]^2+beta_1*(sigma[t-1])^2)
  a[t]=sigma[t]*epsilon[t]}

skew(a)


###2. 위 1을 500 번 반복하여 skewness 의 평균을 구하여라.
#AR(0)+GARCH(1,1)
#garch.fit
n = 1000;m=500;set.seed(1); 
sigma=matrix(nrow=n,ncol=m);epsilon=c();a=matrix(nrow=n,ncol=m);
alpha_0=0.000005; alpha_1=0.144727; beta_1=0.823771
for(i in 1:m){
  epsilon=rnorm(n)
  sigma[1,i]=sqrt(alpha_0/(1-alpha_1-beta_1)) #uncond. variance 
  a[1,i] = sigma[1,i]*epsilon[1]
  for (t in 2:n){
    sigma[t,i]= sqrt(alpha_0 + alpha_1*(a[t-1,i])^2+beta_1*(sigma[t-1,i])^2)
    a[t,i]=sigma[t,i]*epsilon[t]}
}
mean(apply(a,2,skew)) #-0.007062047

#AR(0)+GARCH(1,1) + t분포  추정
#garch.fit.t
n = 1000;m=500;set.seed(1); 
sigma=matrix(nrow=n,ncol=m);epsilon=c();a=matrix(nrow=n,ncol=m);
alpha_0=0.000003; alpha_1=0.147735; beta_1=0.841785; shape=5.640680
for(i in 1:m){
  epsilon=rt(n,df=shape)
  sigma[1,i]=sqrt(alpha_0/(1-alpha_1-beta_1))  #uncond. variance 
  a[1,i] = sigma[1,i]*epsilon[1]
  for (t in 2:n){
    sigma[t,i]= sqrt(alpha_0 + alpha_1*(a[t-1,i])^2+beta_1*(sigma[t-1,i])^2)
    a[t,i]=sigma[t,i]*epsilon[t]}
}
mean(apply(a,2,skew)) 


#AR(0)+GJR-GARCH(1,1) 모형 + normal 분포 
#gjr.garch.fit
n = 1000;m=500;set.seed(1); 
sigma=matrix(nrow=n,ncol=m);epsilon=c();a=matrix(nrow=n,ncol=m);
alpha_0=0.000005; alpha_1=0.018093; beta_1=0.853590;gamma1=0.184960
for(i in 1:m){
  epsilon=rnorm(n)
  sigma[1,i]=sqrt(alpha_0/(1-alpha_1-gamma1/2-beta_1)) #uncond. variance 
  a[1,i] = sigma[1,i]*epsilon[1]
  for (t in 2:n){
    sigma[t,i]= sqrt(alpha_0 + alpha_1*(a[t-1,i])^2+beta_1*(sigma[t-1,i])^2)
    a[t,i]=sigma[t,i]*epsilon[t]}
}
mean(apply(a,2,skew)) 



#AR(0)+GJR-GARCH(1,1)  + t분포  추정
#gjr.garch.fit.t
#omega=0.000004 , alpha=0.000000 , beta1=0.849232 , gamma1=0.257918, shape=5.927278
n = 1000;m=500;set.seed(1); 
sigma=matrix(nrow=n,ncol=m);epsilon=c();a=matrix(nrow=n,ncol=m);
alpha_0=0.000004; alpha_1=0.000000; beta_1=0.849232;gamma1=0.257918;shape=5.927278
for(i in 1:m){
  epsilon=rt(n,df=shape)
  sigma[1,i]=sqrt(alpha_0/(1-alpha_1-gamma1/2-beta_1)) #uncond. variance 
  a[1,i] = sigma[1,i]*epsilon[1]
  for (t in 2:n){
    sigma[t,i]= sqrt(alpha_0 + alpha_1*(a[t-1,i])^2+beta_1*(sigma[t-1,i])^2)
    a[t,i]=sigma[t,i]*epsilon[t]}
}
mean(apply(a,2,skew)) 




