library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

#setwd("~/Documents/git/korean-scope/experiments/2-english-baseline/Submiterator-master/")
setwd("~/git/korean_scope/experiments/2-english-baseline/Submiterator-master/")

#### first run of experiment
num_round_dirs = 10
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/english-baseline.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("e.",df1$workerid)

d = subset(df1, select=c("workerid","order","item","subexperiment","scope", "type", "response", "language","gender","age","asses","education"))

# re-factorize
d[] <- lapply( d, factor) 

t <- d

# only look at "español" as the native language
t = t[t$language!="Turkish"&t$language!="Vietnamese"&!is.na(t$language)&t$language!=""&t$language!="Spanish"&t$language!="Emg;osj",]

t$response = as.numeric(as.character(t$response))

#summary(t) 

length(unique(t$workerid))# 85 indicated "English" as native language

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$subexperiment)

agr = aggregate(response~order*scope*subexperiment,data=f,FUN=mean)
agr
