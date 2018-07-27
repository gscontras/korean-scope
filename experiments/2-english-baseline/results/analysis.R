library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
library(scales)
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

length(unique(t$workerid))# 86 indicated "English" as native language

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$subexperiment)

agr = aggregate(response~order*scope*subexperiment,data=f,FUN=mean)
agr


source("../results/helpers.R")

## violin plot
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
d_raw <- f
d_raw$subexpt <- factor(d_raw$subexperiment,levels=c("plain","one","there","thereone")) 
d_raw$order <- factor(d_raw$order,levels=c("everya","aevery"))
d_raw$scope <- factor(d_raw$scope,levels=c("surface","inverse"))
ee_violin <- ggplot(d_raw, aes(x=factor(order,labels=c("every > a/one","a/one > every")),y=response,fill=scope)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  geom_violin()+
  stat_summary(fun.data=data_summary,color="red",shape=18,size=1.1,position=position_dodge(width=0.9))+
  ylab("rating (out of 1)\n")+
  #scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7),oob = rescale_none)+
  xlab("\n surface configuration") +
  labs(fill="interpretation")+
  theme_bw() +
  scale_fill_manual(values=c("dimgray", "gray"))+
  theme(panel.background = element_rect(fill = 'white',color ='black')) +
  facet_wrap( ~ subexpt)
ee_violin
#ggsave("../results/english-baseline-violin.png",width=7.5,height=4)



