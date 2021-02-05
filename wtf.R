library(dLagM)
library(forecast)
library(TSA)
library(dynlm)
library(tseries)
library(lmtest)
library(car)
library(caTools)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2) 
library(ggthemes) 
library(scales) 
library(grid)
library(gridExtra) 
library(corrplot)
library(ggrepel) 
library(RColorBrewer)
library(data.table) 
library(dplyr) 
library(readr)
library(tibble) 
library(tidyr)
library(lazyeval) 
library(broom) 
library(stringr) 
library(purrr) 
library(forcats) 
library(lubridate) 
library(forecast) 
#library(prophet) 
#install.packages("caTools")

#Loading data set
train_1 <- read.csv("H:/R/web-traffic-time-series-forecasting/train_1.csv", header = TRUE, check.names = FALSE)
head(train_1)

#missing value count
(sum(is.na(train_1))/(ncol(train_1)*nrow(train_1)))*100

#Replace misisng values with 0
train_1[is.na(train_1)] <- 0

#separate data for all dates in dataset
traffic.dates <- train_1 %>% select(-1)

#1. converting Page names in rows to columns
#2. Filtering the page name based on 3 articles 
traffic.page <- train_1 %>% select(Page) %>% rownames_to_column()
article.mediawiki <- traffic.page %>% filter(str_detect(Page, "mediawiki"))
article.wikimedia <- traffic.page %>% filter(str_detect(Page, "wikimedia"))
article.wikipedia <- traffic.page %>% filter(str_detect(Page, "wikipedia")) %>% 
  filter(!str_detect(Page, "wikimedia")) %>%
  filter(!str_detect(Page, "mediawiki"))

#Separating the locale, acess and the agent as per the wikipedia article
article.wikipedia <- article.wikipedia %>%
  separate(Page, into = c("art.loc", "accs.agnt"), sep = ".wikipedia.org_") %>%
  separate(accs.agnt, into = c("access", "agent"), sep = "_") %>%
  separate(art.loc, into = c("article", "locale"), sep = -3) %>%
  mutate(locale = str_sub(locale,2,3))

#Separating the locale, acess and the agent as per the wikimedia article
article.wikimedia <- article.wikimedia %>%
  separate(Page, into = c("article", "accs.agnt"), sep = "_commons.wikimedia.org_") %>%
  separate(accs.agnt, into = c("access", "agent"), sep = "_") %>%
  add_column(locale = "wikmed")

#Separating the locale, acess and the agent as per the mediawiki article
article.mediawiki <- article.mediawiki %>%
  separate(Page, into = c("article", "accs.agnt"), sep = "_www.mediawiki.org_") %>%
  separate(accs.agnt, into = c("access", "agent"), sep = "_") %>%
  add_column(locale = "medwik")

#Combining all the article and corresponding compoennts into a single dataframe
traffic.page <- article.wikipedia %>%
  full_join(article.wikimedia, by = c("rowname", "article", "locale", "access", "agent")) %>%
  full_join(article.mediawiki, by = c("rowname", "article", "locale", "access", "agent"))

rndm.data<-as.data.frame(sample_n(traffic.page, size = 3))
rndm.data<- rndm.data %>% select(1)

#traffic.page %>% filter(str_detect(article, "The_Beatle")) %>%
#  filter(access == "all-access") %>%
#  filter(agent == "all-agents")

#get the time series extract 
for (row in 1:nrow(rndm.data)) {
  vec <<- rndm.data %>% slice(row) %>% unlist() %>% unname() 
  print(paste("Row number is ",vec))}


str(get.ts)
#Get the Time series for the first row number
get.ts <- 
  traffic.dates %>%
    filter_((interp(~x == row_number(), .values = list(x = vec)))) %>%
    rownames_to_column %>% 
    gather(dates, value, -rowname) %>% 
    spread(rowname, value) %>%
    mutate(dates = ymd(dates),
           views = as.integer(`1`)) %>%
    select(-`1`)

#TEST OF TIME SERIES FORMATION FOR ROWNUM1
ronum.ts<-get.ts

#plot the extracted time series
  art <- traffic.page %>% filter(rowname == vec) %>% .$article
  loc <- traffic.page %>% filter(rowname == vec) %>% .$locale
  acc <- traffic.page %>% filter(rowname == vec) %>% .$access
  ag <- traffic.page %>% filter(rowname == vec) %>% .$agent
  
  ronum.ts %>%
    ggplot(aes(dates, views)) +
    geom_line() +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = str_c(art, " - ", loc, " - ", acc))

#Time series conversion  
  vec.ts <- ts(ronum.ts$views, start = decimal_date(as.Date("2015-07-01")),  frequency = 365.25)

#Structure of time series converted vector
str(vec.ts)

#Plot of Time Series converted series
mytsDF <- data.frame(data = vec.ts, date = ronum.ts$dates)
ggplot(mytsDF, aes(date, data)) + geom_line() +
  scale_x_date(date_labels = "%d-%m-%Y", date_breaks = "3 months") + 
  xlab("") + ylab("y") + ggtitle("Time Series Plot")

#Stationarity test
adf.test(vec.ts) #Null Hypo: Data is non-stationary(p- val <0.05 so reject null hypo and data is stationary)
PP.test(vec.ts)

#ACF & PACF plot to check for trend & stationarity
acf(vec.ts, main="sample ACF for daily views on the page From July 2015",lag.max = 60) 
pacf(vec.ts, main ="Sample PACF for aily views on the page from July 2015", lag.max = 60)

#Creating dataframe that stores corresponding models MASE, AIC,BIC,AICc values for final model selection
mod.web.eval.df <<- data.frame(Model=character(),MASE=numeric(), BIC= numeric(),AICC=numeric(),AIC=numeric())

#Now Checking if the time series has zero or negative elements or not
bool.val<- 0 %in% vec.ts
class(bool.val)
if(bool.val)
{
  print("O or negative value is present for rownum : ",row)
  vec.ts.zero <- vec.ts+50
  
  #DlagM Models cannot be applied as we do not have predictor series
  #Dynamic Linear Model cannot be applied as we do not have any intervention point & frequency > 1
  #1. Simple Exponential Smoothing
  tryCatch({ 
    fit1 <-ses(vec.ts, alpha=0.1, initial="simple", h=10) 
    summary(fit1)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpExpSmoo",MASE=accuracy(fit1)[6], BIC = fit1$model$bic, AICC = fit1$model$aicc, AIC = fit1$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit2 <-ses(vec.ts, alpha=0.1, initial="optimal", h=10) 
    summary(fit2)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalExpSmoo",MASE=accuracy(fit2)[6], BIC = fit2$model$bic, AICC = fit2$model$aicc, AIC = fit2$model$aic))
    
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit3 <-ses(vec.ts, h=10) 
    summary(fit3)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalExpSmoo",MASE=accuracy(fit3)[6], BIC = fit3$model$bic, AICC = fit3$model$aicc, AIC = fit3$model$aic))
    
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  
  #2. Holt's Linear Method
  tryCatch({ 
    fit4 <-holt(vec.ts, alpha=0.8, beta=0.1, initial="optimal", h=10)
    summary(fit4)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalHoltLinear",MASE=accuracy(fit4)[6], BIC = fit4$model$bic, AICC = fit4$model$aicc, AIC = fit4$model$aic))
    },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit5 <-holt(vec.ts, alpha=0.8, beta=0.1, initial="simple", h=10) 
    summary(fit5)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleHoltLinear",MASE=accuracy(fit5)[6], BIC = fit5$model$bic, AICC = fit5$model$aicc, AIC = fit5$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit6 <-holt(vec.ts, alpha=0.1, beta=0.1, initial="simple", h=10) 
    summary(fit6)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleHoltLinear2",MASE=accuracy(fit6)[6], BIC = fit6$model$bic, AICC = fit6$model$aicc, AIC = fit6$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit7 <-holt(vec.ts, alpha=0.1, beta=0.1, initial="optimal", h=10)
    summary(fit7)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalHoltLinear",MASE=accuracy(fit7)[6], BIC = fit7$model$bic, AICC = fit7$model$aicc, AIC = fit7$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit8 <-holt(vec.ts, alpha=0.1, beta=0.8, initial="simple", exponential=TRUE, h=10)
    summary(fit8)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleExpHoltLinear",MASE=accuracy(fit8)[6], BIC = fit8$model$bic, AICC = fit8$model$aicc, AIC = fit8$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit9 <-holt(vec.ts, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=10) 
    summary(fit9)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleDampHoltLinear",MASE=accuracy(fit9)[6], BIC = fit9$model$bic, AICC = fit9$model$aicc, AIC = fit9$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  
  #We cant apply Holt Winters Method as it has no periodicity and is a daily baisis data with frequency of 365.25
  
  #State Space Models For Additive Data of time series
    error.type = c("N","A","M") 
    trend.type = c("N","A","M") 
    seas.type = c("N","A","M") 
    damped.val = c(TRUE, FALSE) 
    methd.type = expand.grid(error.type,trend.type,seas.type,damped.val) 
    for(i in 1:nrow(methd.type))
    { 
      ets.config = paste(methd.type[i,1],methd.type[i,2],methd.type[i,3],sep = "") 
      tryCatch({ 
        if(ets.config == "NNN") { 
          ets.model.sel = ets(vec.ts, damped = methd.type[i,4], ic=c("aicc", "aic", "bic")) 
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
        else if(methd.type[i,2] != "N") { 
          ets.model.sel = ets(vec.ts, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic")) 
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
        else {
          ets.model.sel = ets(vec.ts, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic"))
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
      },error=function(e){ 
        print(paste(methd.type[i,],e,sep=":")) 
      }) } 
    
    #State Space Models For Multiplicative Data of time series
    error.type = c("N","A","M") 
    trend.type = c("N","A","M") 
    seas.type = c("N","A","M") 
    damped.val = c(TRUE, FALSE) 
    methd.type = expand.grid(error.type,trend.type,seas.type,damped.val) 
    for(i in 1:nrow(methd.type))
    { 
      ets.config = paste(methd.type[i,1],methd.type[i,2],methd.type[i,3],sep = "") 
      tryCatch({ 
        if(ets.config == "NNN") { 
          ets.model.sel = ets(vec.ts.zero, damped = methd.type[i,4], ic=c("aicc", "aic", "bic")) 
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
        else if(methd.type[i,2] != "N") { 
          ets.model.sel = ets(vec.ts.zero, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic")) 
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
        else {
          ets.model.sel = ets(vec.ts.zero, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic"))
          mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
      },error=function(e){ 
        print(paste(methd.type[i,],e,sep=":")) 
      }) } 

  #Autofit ETS model for additive
    tryCatch({ 
      fit.auto.add = ets((vec.ts))
      summary(fit.auto.add)
      checkresiduals(fit.auto.add)
      mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=fit.auto.add$method, MASE=accuracy(fit.auto.add)[6], BIC = fit.auto.add$bic,AICC = fit.auto.add$aicc, AIC = fit.auto.add$aic)) 
    },error=function(e){ 
      print(paste(row,e,sep=":")) 
    })
    
    #Autofit ETS model for multiplicative
    tryCatch({ 
      fit.auto.mult = ets((vec.ts.zero))
      summary(fit.auto.mult)
      checkresiduals(fit.auto.mult)
      mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=fit.auto.mult$method, MASE=accuracy(fit.auto.mult)[6], BIC = fit.auto.mult$bic,AICC = fit.auto.mult$aicc, AIC = fit.auto.mult$aic)) 
    },error=function(e){ 
      print(paste(row,e,sep=":")) 
    })
    
}else{
  print("Non zero values present for rownum : ",row)
 
  tryCatch({ 
    fit1 <-ses(vec.ts, alpha=0.1, initial="simple", h=10) 
    summary(fit1)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpExpSmoo",MASE=accuracy(fit1)[6], BIC = fit1$model$bic, AICC = fit1$model$aicc, AIC = fit1$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit2 <-ses(vec.ts, alpha=0.1, initial="optimal", h=10) 
    summary(fit2)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalExpSmoo",MASE=accuracy(fit2)[6], BIC = fit2$model$bic, AICC = fit2$model$aicc, AIC = fit2$model$aic))
    
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit3 <-ses(vec.ts, h=10) 
    summary(fit3)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalExpSmoo",MASE=accuracy(fit3)[6], BIC = fit3$model$bic, AICC = fit3$model$aicc, AIC = fit3$model$aic))
    
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  
  #2. Holt's Linear Method
  tryCatch({ 
    fit4 <-holt(vec.ts, alpha=0.8, beta=0.1, initial="optimal", h=10)
    summary(fit4)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalHoltLinear",MASE=accuracy(fit4)[6], BIC = fit4$model$bic, AICC = fit4$model$aicc, AIC = fit4$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit5 <-holt(vec.ts, alpha=0.8, beta=0.1, initial="simple", h=10) 
    summary(fit5)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleHoltLinear",MASE=accuracy(fit5)[6], BIC = fit5$model$bic, AICC = fit5$model$aicc, AIC = fit5$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit6 <-holt(vec.ts, alpha=0.1, beta=0.1, initial="simple", h=10) 
    summary(fit6)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleHoltLinear2",MASE=accuracy(fit6)[6], BIC = fit6$model$bic, AICC = fit6$model$aicc, AIC = fit6$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit7 <-holt(vec.ts, alpha=0.1, beta=0.1, initial="optimal", h=10)
    summary(fit7)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="OptimalHoltLinear",MASE=accuracy(fit7)[6], BIC = fit7$model$bic, AICC = fit7$model$aicc, AIC = fit7$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit8 <-holt(vec.ts, alpha=0.1, beta=0.8, initial="simple", exponential=TRUE, h=10)
    summary(fit8)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleExpHoltLinear",MASE=accuracy(fit8)[6], BIC = fit8$model$bic, AICC = fit8$model$aicc, AIC = fit8$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  tryCatch({ 
    fit9 <-holt(vec.ts, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=10) 
    summary(fit9)
    mod.web.eval.df <- rbind(mod.web.eval.df,cbind(Model="SimpleDampHoltLinear",MASE=accuracy(fit9)[6], BIC = fit9$model$bic, AICC = fit9$model$aicc, AIC = fit9$model$aic))
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  
  #State Space Models For  time series
  error.type = c("N","A","M") 
  trend.type = c("N","A","M") 
  seas.type = c("N","A","M") 
  damped.val = c(TRUE, FALSE) 
  methd.type = expand.grid(error.type,trend.type,seas.type,damped.val) 
  for(i in 1:nrow(methd.type))
  { 
    ets.config = paste(methd.type[i,1],methd.type[i,2],methd.type[i,3],sep = "") 
    tryCatch({ 
      if(ets.config == "NNN") { 
        ets.model.sel = ets(vec.ts, damped = methd.type[i,4], ic=c("aicc", "aic", "bic")) 
        mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
      else if(methd.type[i,2] != "N") { 
        ets.model.sel = ets(vec.ts, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic")) 
        mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
      else {
        ets.model.sel = ets(vec.ts, model=ets.config, damped=methd.type[i,4], ic=c("aicc", "aic", "bic"))
        mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=ets.model.sel$method, MASE=accuracy(ets.model.sel)[6], BIC = ets.model.sel$bic,AICC = ets.model.sel$aicc, AIC = ets.model.sel$aic)) } 
    },error=function(e){ 
      print(paste(methd.type[i,],e,sep=":")) 
    }) }
  
  #Autofit ETS model for additive
  tryCatch({ 
    fit.auto.add = ets((vec.ts))
    summary(fit.auto.add)
    checkresiduals(fit.auto.add)
    mod.web.eval.df = rbind(mod.web.eval.df, cbind(Model=fit.auto.add$method, MASE=accuracy(fit.auto.add)[6], BIC = fit.auto.add$bic,AICC = fit.auto.add$aicc, AIC = fit.auto.add$aic)) 
  },error=function(e){ 
    print(paste(row,e,sep=":")) 
  })
  
  
}




rownr <- 41557

pageviews <- extract_ts(rownr) %>% rename(y = views,ds = dates)

pred_len <- 60

pred_range <- c(nrow(pageviews)-pred_len+1, nrow(pageviews))

pre_views <- pageviews %>% head(nrow(pageviews)-pred_len)

post_views <- pageviews %>% tail(pred_len)

proph <- prophet(pre_views, changepoint.prior.scale=0.5, yearly.seasonality=TRUE)

future <- make_future_dataframe(proph, periods = pred_len)

fcast <- predict(proph, future)

plot(proph, fcast)



prophet_plot_components(proph, fcast)


fcast %>%
  
  as.tibble() %>%
  
  mutate(ds = date(ds)) %>%
  
  ggplot(aes(ds, yhat)) + 
  
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
  
  geom_line(colour = "blue") +
  
  geom_line(data = pre_views, aes(ds, y), colour = "black") +
  
  geom_line(data = post_views, aes(ds, y), colour = "grey50")



  
