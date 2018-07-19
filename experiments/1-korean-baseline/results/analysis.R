library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

#setwd("~/Documents/git/spanish_adjectives/experiments/3-order-preference-expanded2/Submiterator-master")
setwd("~/git/korean_scope/experiments/1-korean-baseline/Submiterator-master")

#### first run of experiment
num_round_dirs = 5
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/korean-baseline.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("vi.",df1$workerid)

d = subset(df1, select=c("workerid","order","item","scramble","scope", "type", "response", "school","lived","family","language","level","gender","age","describe","years","assess","classes","college","education"))

# re-factorize
d[] <- lapply( d, factor) 

t <- d

# only look at "both8" for lived
t = t[t$lived=="both8",]

# only look at "español" as the native language
t = t[t$language!="English"&t$language!="english"&!is.na(t$language)&t$language!=""&t$language!="gbhj",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

# t = d[d$language=="Espanol"|d$language=="espanol"|d$language=="espanol "|
#         d$language==" Español"|d$language=="Española"|d$language=="spanish"|d$language=="Castellano"|
#         d$language=="Español, de España"|d$language=="SPANISH"|d$language=="castellano"|
#         d$language=="Español, Catalan"|d$language=="espanol, vasco"|d$language=="Español e italiano"|
#         d$language=="ESPAÑOL E ITALIANO",]

t$response = as.numeric(as.character(t$response))

#summary(t) 

length(unique(t$workerid))# 48 indicated "spanish" as native language

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$scramble)

agr = aggregate(response~order*scope*scramble,data=f,FUN=mean)
