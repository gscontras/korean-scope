library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

#setwd("~/Documents/git/korean-scope/experiments/3-korean-baseline2/Submiterator-master/")
setwd("~/git/korean_scope/experiments/3-korean-baseline2/Submiterator-master")

#### first run of experiment
num_round_dirs = 20
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/korean-baseline2.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("v2.",df1$workerid)

d = subset(df1, select=c("workerid","order","item","scramble","scope", "type", "response", "school","lived","family","language","level","gender","age","describe","years","assess","classes","college","education"))

# re-factorize
d[] <- lapply( d, factor) 

t <- d

# only look at "both8" for lived
t = t[t$lived=="both8",]

# must have provided a native langauge
t = t[t$language!="",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

t$response = as.numeric(as.character(t$response))

#summary(t) 

length(unique(t$workerid))# n=11

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$scramble)

agr = aggregate(response~order*scope*scramble,data=f,FUN=mean)
agr