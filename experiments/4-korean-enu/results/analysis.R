library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

#setwd("~/Documents/git/korean-scope/experiments/4-korean-enu/Submiterator-master/")
setwd("~/git/korean_scope/experiments/4-korean-enu/Submiterator-master")

#### first run of experiment
num_round_dirs = 5
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../../4-korean-enu/Submiterator-master/round', i, '/korean-enu.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("v1.",df1$workerid)

d = subset(df1, select=c("workerid","order","quantifier", "item","scramble","scope", "type", "response", "school","lived","family","language","level","gender","age","describe","years","assess","classes","college","education"))

# re-factorize
d[] <- lapply( d, factor) 

length(unique(d$workerid))# n=225

t <- d

# only look at "both8" for lived
t = t[t$lived=="both8",]

# must have provided a native langauge
t = t[t$language=="한국어"|t$language=="한국말"|t$language=="korean"|t$language=="Korean"|t$language=="한국어"|t$language=="KOREAN",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

t$response = as.numeric(as.character(t$response))

#summary(t) 

length(unique(t$workerid))# n=9
unique(t$language)

length(unique(t[t$scramble!="scrambled",]$workerid))# n=4
length(unique(t[t$scramble=="scrambled",]$workerid))# n=5

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$scramble,f$quantifier)

agr = aggregate(response~order*scope*scramble*quantifier,data=f,FUN=mean)
agr


## violin plot
source("../results/helpers.R")
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
d_raw <- f
d_raw$order <- factor(d_raw$order,levels=c("everya","aevery"))
d_raw$scope <- factor(d_raw$scope,levels=c("surface","inverse"))
kk_violin <- ggplot(d_raw, aes(x=factor(order,labels=c("every > a/one","a/one > every")),y=response,color=scope)) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  #geom_bar(stat="identity",position=position_dodge()) +
  #geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=context, width=0.1),position=position_dodge(width=0.9))+
  #geom_violin()+
  stat_summary(fun.data=data_summary,shape=18,size=1.1,position=position_dodge(width=0.9))+
  ylab("rating (out of 1)\n")+
  #scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7),oob = rescale_none)+
  xlab("\n surface configuration") +
  labs(fill="interpretation")+
  theme_bw() +
  scale_fill_manual(values=c("dimgray", "gray"))+
  theme(panel.background = element_rect(fill = 'white',color ='black')) +
  facet_grid(scramble ~ quantifier)
kk_violin
#ggsave("../results/korean-baseline-enu.png",width=6,height=4)


#############################
### HERITAGE SPEAKER ANALYSIS
#############################

h <- d

# only look at "both8" for lived
h = h[h$lived!="both8"&h$lived!="over8",]

# describe self as learning korean first, now using english
h = h[h$describe=="KorEng",]

### only one heritage speaker so far

# must list Korean as a first language
#h = h[h$language!=""&h$language!="English"&h$language!="ENGLISH"&h$language!="yes"&h$language!="가 어렸을 때",]
