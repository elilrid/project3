library(sqldf)
library(forecast)
library(MASS)
library(xlsx)
library(fma)
library(lubridate)
library(tseries)

##### Data Read #####
data = read.table("example.txt",header=TRUE,sep=",")

##### Examine the Data #####
data$UygunFormatGün <- as.Date(data$Gün,"%d-%m-%Y")
query = "select Gün ,Mağaza,Lokasyon,Kod,SatıcıÜrünAdı,ANAGRUP,ALTGRUP,URUNCESIDI, SUM(SatışMiktarı) as Miktar, UygunFormatGün from data where ANAGRUP = 'anagrup1' and ALTGRUP ='alt-grup-3' group by UygunFormatGün order by UygunFormatGün"
result <- sqldf(query)
plot(result$Miktar)
write.xlsx(result, file = "test.excelfile.xlsx",
        sheetName = "TestSheet", row.names = FALSE)
result$Aylık <- as.Date(cut(result$UygunFormatGün, breaks = "months"))
query = "select Gün ,Mağaza,Lokasyon,Kod,SatıcıÜrünAdı,ANAGRUP,ALTGRUP,URUNCESIDI, SUM(Miktar) as Miktar, UygunFormatGün, Aylık from result group by Aylık order by Aylık"
#result <-sqldf(query)

##### Time Series #####
inds <- seq(as.Date(min(result$UygunFormatGün)), as.Date(max(result$UygunFormatGün)), by = "day")
result.timeseries <- ts(result$Miktar,start = c(2016, as.numeric(format(inds[1], "%j"))), frequency = 365)
x = time(result.timeseries)
y = result.timeseries

##### Check Seosanality #####
ets(y)
fit <- tbats(y)
seasonal <- !is.null(fit$seasonal)
seasonal
fit1 <- ets(y)
fit2 <- ets(y,model="ANN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
#P value
1-pchisq(deviance,df)

##### Auto-Correlation #####
Acf(result.timeseries)
Pacf(result.timeseries)

##### Stationary Test #####
Box.test(result.timeseries,lag=20,type="Ljung-Box")
adf.test(result.timeseries,alternative = "stationary")
kpss.test(result.timeseries)
##### Fitting #####

##### Linear Regression #####
relation <- lm(y~x)
a <- data.frame(y)
result$prediction <-  predict(relation,a)

##### Chi Square #####
tbl = table(result$Miktar, result$prediction)
chisq.test(tbl)

##### Plot the result #####
png(file = "GünlereBağlıMikar_Altgrup3.jpg")
plot(result.timeseries,type = "o", col = "red", xlab = "ID", ylab = "Miktar", main = "Alt Grup 3")
dev.off()
