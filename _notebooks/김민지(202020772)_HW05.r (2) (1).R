library(fpp2)
library(astsa)
library(ggplot2)
library(gridExtra)
library(forecast)
library(fUnitRoots)  # library for function adfTest
library(tseries)
library(lmtest) 
library(data.table)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr) 
library(TTR)


##2번 sheep 모형

#2-(1)


tmp.sheep<-data.table(
  sheep
)
tmp.sheep

acf(tmp.sheep)
pacf(tmp.sheep)

ggtsdisplay(tmp.sheep,                                                                                                             
            theme=theme_bw(), 
            smooth=T)
###############

z <- sheep
dt <- data.table(diff_sheep)

z

diff_sheep<-diff(sheep) # 차분


diffsheep<-data.table(diff_sheep)
  

ggtsdisplay(diff_sheep, 
            theme=theme_bw(), 
            smooth=T)

acf(diffsheep)


t.test(diff_sheep)  # H0 : mu=0

fit2 <- arima(diffsheep, order=c(3,0,0))
summary(fit2)


diffsheep[, resid := resid(fit2)]


p2 <- ggAcf(diffsheep$resid) + 
  theme_bw() + ylim(-1,1) +
  theme(plot.title = element_blank())

p3 <- ggPacf(diffsheep$resid) + 
  theme_bw() +ylim(-1,1) +
  theme(plot.title = element_blank())


grid.arrange (p2, p3, nrow = 2,
              layout_matrix = rbind(c(1,1),
                                    c(2,2)))



checkresiduals(fit2, theme=theme_bw())


# 잔차의 포트맨토 검정 
Box.test(diffsheep$resid, lag=6, type = "Ljung-Box")
Box.test(diffsheep$resid, lag=12, type = "Ljung-Box")
Box.test(diffsheep$resid, lag=18, type = "Ljung-Box")

## 정규성검정
shapiro.test(diffsheep$resid)  ##H0 : normal distribution
jarque.bera.test(diffsheep$resid)  ##JB test H0: normal

p1 <- ggplot(diffsheep, aes(resid)) + 
  geom_histogram(aes(y=..density..),
                 bins = 10,
                 fill='steelblue', col= 'grey') +
  ggtitle('Histogram of residual') + 
  xlab("")+ylab('')+
  geom_density()+
  stat_function(fun = dnorm, 
                args = list(mean = mean(diffsheep$resid), sd = sd(diffsheep$resid)),
                col = 'red')+
  theme_bw()

p2 <- ggplot(diffsheep, aes(sample =resid)) + 
  geom_qq() +
  geom_qq_line() +
  xlab("")+ylab('')+
  theme_bw()

grid.arrange(p1, p2, nrow = 1)

#2-(3)

ggtsdisplay(diff_sheep, 
            theme=theme_bw(), 
            smooth=T)
acf(diff_sheep)


sarima.for(sheep, 25, 3,0,0)

fit <- arima(sheep, order=c(3,0,0), method='ML')
mean(z)

summary(fit)
forecast_fit <- forecast(fit, 25)
forecast_fit


#3
hsales
plot(hsales, type='l')


#원래 데이터 
ggtsdisplay(hsales, 
            theme=theme_bw(), 
            smooth=T)


plot(log(hsales))
plot(sqrt(hsales))
plot(BoxCox(hsales,lambda= BoxCox.lambda(hsales)))


dt <- data.table(t = 1:length(hsales),
                 z = hsales)

dt[, lnz := log(z)]
dt[, sqrtz := sqrt(z)]
dt[, boxcoxz := BoxCox(z,lambda= BoxCox.lambda(z))]

# 세 변환 비교
melt.dt <- melt(dt, id=1)
ggplot(melt.dt, aes(t, value)) + 
  geom_line(col='steelblue') +
  xlab("")+ylab('')+ 
  facet_wrap(variable~.,nrow=2, scales = "free_y")+
  theme_bw()

bptest(lm(z~t, dt)) #H0 : 등분산이다 
bptest(lm(lnz~t, dt))
bptest(lm(sqrtz~t, dt))
bptest(lm(boxcoxz~t, dt))

log_hsales<- log(hsales)

ggtsdisplay(log_hsales, 
            theme=theme_bw(), 
            smooth=T)

#계절성분없이 차분한 것

diffloghsales <- diff(log_hsales)

ggtsdisplay(diffloghsales,
            theme = theme_bw(),
            smooth=T)

#로그변환 한 것을 계절 차분한 것
lag12_log_hsales <- diff(log_hsales, lag=12)

ggtsdisplay(lag12_log_hsales,
            theme = theme_bw(),
            smooth=T)

#단위근 검정
# nc는 상수항도 없고 추세도 없는 것이고 
# c는 상수항은 잇는데 추세는 없고 ct 는 상수항도 있고 추세ㅗㄷ 잇음 
adfTest(lag12_log_hsales, lags = 0, type = "nc")
adfTest(lag12_log_hsales, lags = 1, type = "nc")
adfTest(lag12_log_hsales, lags = , type = "c")

log1_lag12_log_hsales <- diff(lag12_log_hsales)

ggtsdisplay(log1_lag12_log_hsales,
            lag.max=60,
            theme = theme_bw(),
            smooth=T)


# 3-(5)
fit_arima1 = arima(log(hsales), order = c(1,0,0), 
             seasonal = list(order = c(1,1,0), 
                             period = 12))
fit_arima1


fit_arima2 = arima(log(hsales), order = c(0,1,0), 
                   seasonal = list(order = c(1,1,0), 
                                   period = 12))
fit_arima2

fit_arima1$aic
fit_arima2$aic

fit_arima1$sigma2
fit_arima2$sigma2


checkresiduals(fit_arima1, theme=theme_bw())
checkresiduals(fit_arima2, theme=theme_bw())


#3-(8)
forecast_fit_hsales <- forecast(fit_arima1, 25)
forecast_fit_hsales

sarima_fit <- sarima.for(log(hsales), 3,1,1,0)
summary(sarima_fit)
hsales



####4번
ukcars
plot(ukcars)

ggtsdisplay(ukcars, 
            theme=theme_bw(), 
            smooth=T)



dt_ukcars <- data.table(t = 1:length(ukcars),
                 z = ukcars)

dt_ukcars[, lnz := log(z)]
dt_ukcars[, sqrtz := sqrt(z)]
dt_ukcars[, boxcoxz := BoxCox(z,lambda= BoxCox.lambda(z))]

# 세 변환 비교
melt.dt_ukcars <- melt(dt_ukcars, id=1)
ggplot(melt.dt_ukcars, aes(t, value)) + 
  geom_line(col='steelblue') +
  xlab("")+ylab('')+ 
  facet_wrap(variable~.,nrow=2, scales = "free_y")+
  theme_bw()


#bp test
bptest(lm(z~t, dt_ukcars)) #H0 : 등분산이다 
bptest(lm(lnz~t, dt_ukcars))
bptest(lm(sqrtz~t, dt_ukcars))
bptest(lm(boxcoxz~t, dt_ukcars))


####
ukcars
df = as.data.frame(ukcars)


train= df[c(1:108),]
test = df[c(109:113),]

ukcars_train= ts(train);
plot.ts(ukcars_train)

#4- 4 이동평균
ukcarsSMA3 <- SMA(ukcars_train,n=5)
plot.ts(ukcars_train)
lines(ukcarsSMA3, col='red', lty=2)
lines( SMA(kingstimeseries,n=7), col='blue', lty=2)


##  4- 5  단순지수 평활
fit01 <- ses(train, 
             alpha = 0.9,
             initial = 'simple',
             h = 8)  ##Exponential smoothing forecasts
ls(fit01)
summary(fit01)




#4-6
ggtsdisplay(train, 
            theme=theme_bw(), 
            smooth=T)
sarima_fit <- sarima.for(train, 8,2,0,0)
sarima_fit

sarima_fit2= arima(train, order = c(0,0,0), 
                   seasonal = list(order = c(2,0,0), 
                                   period = 4))
summary(sarima_fit2)

#4-7 이동평균법 mse , mae , mape
tmp.dat <- data.table(train = train,
                      t = 1:length(train))
tmp.dat[, sma3 := SMA(ukcars_train,n=3)]
tmp.dat[, sma10 := SMA(ukcars_train,n=10)]



mean((tmp.dat$train- tmp.dat$sma3)^2, na.rm=T) ##MSE
mean(abs(tmp.dat$train- tmp.dat$sma3), na.rm=T) ##MAE 
mean(abs((tmp.dat$train- tmp.dat$sma3)/tmp.dat$train), na.rm=T)*100 ##mape

# 4-7  단순지수 평활 summary 값
summary(fit01)



