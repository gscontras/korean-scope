library(ggplot2)
library(lme4)
library(hydroGOF)
library(dplyr)
library(lmerTest)
#library(tidyr)

#setwd("~/Documents/git/korean-scope/experiments/3-korean-baseline2/Submiterator-master/")
setwd("~/git/korean_scope/experiments/3-korean-baseline2/Submiterator-master")

#### first run of experiment
num_round_dirs = 5
df1 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    '../../1-korean-baseline/Submiterator-master/round', i, '/korean-baseline.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df1$workerid = paste("v1.",df1$workerid)

#### second run of experiment
num_round_dirs = 20
df2 = do.call(rbind, lapply(1:num_round_dirs, function(i) {
  return (read.csv(paste(
    'round', i, '/korean-baseline2.csv', sep=''),stringsAsFactors=FALSE) %>% 
      mutate(workerid = (workerid + (i-1)*9)))}))
df2$workerid = paste("v2.",df2$workerid)

df = rbind(df1,df2)

d = subset(df, select=c("workerid","order","item","scramble","scope", "type", "response", "school","lived","family","language","level","gender","age","describe","years","assess","classes","college","education"))

# re-factorize
d[] <- lapply( d, factor) 

length(unique(d$workerid))# n=225

###########################
### NATIVE SPEAKER ANALYSIS
###########################

t <- d

# only look at "both8" for lived
t = t[t$lived=="both8",]

# must have provided a native langauge
t = t[t$language!=""&t$language!="esañol"&t$language!="English, Korean"&t$language!="korean, english"&t$language!="Korean, English"&t$language!="한국어 영어",]

# no self-described L2 speakers
t = t[t$describe!="L2",]

t$response = as.numeric(as.character(t$response))

#summary(t) 

length(unique(t$workerid))# n=23
unique(t$language)

length(unique(t[t$scramble!="scrambled",]$workerid))# n=14
length(unique(t[t$scramble=="scrambled",]$workerid))# n=9

## eventually want to filter by fillers?

f = t[t$type=="test",]

table(f$order,f$scope,f$scramble)

agr = aggregate(response~order*scope*scramble,data=f,FUN=mean)
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
kk_violin <- ggplot(d_raw, aes(x=factor(order,labels=c("every > a/one","a/one > every")),y=response,fill=scope)) +
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
  facet_wrap( ~ scramble)
kk_violin
#ggsave("../results/korean-baseline-violin.png",width=6,height=2)

native_korean = d_raw

#############################
### HERITAGE SPEAKER ANALYSIS
#############################

h <- d

# only look at "both8" for lived
h = h[h$lived!="both8"&h$lived!="over8",]

# describe self as learning korean first, now using english
h = h[h$describe=="KorEng",]

# must list Korean as a first language
h = h[h$language!=""&h$language!="English"&h$language!="ENGLISH"&h$language!="yes"&h$language!="가 어렸을 때",]

unique(h$language)
length(unique(h$workerid)) # 12

length(unique(h[h$scramble!="scrambled",]$workerid))# n=7
length(unique(h[h$scramble=="scrambled",]$workerid))# n=5

h$response = as.numeric(as.character(h$response))

## eventually want to filter by fillers?

hf = h[h$type=="test",]

table(h$order,h$scope,h$scramble)

agr = aggregate(response~order*scope*scramble,data=hf,FUN=mean)
agr


## violin plot
source("../results/helpers.R")
data_summary <- function(x) {
  m <- mean(x)
  ymin = m-as.numeric(ci.low(x))
  ymax = m+as.numeric(ci.high(x))
  return(c(y=m,ymin=ymin,ymax=ymax))
}
d_raw <- hf
d_raw$order <- factor(d_raw$order,levels=c("everya","aevery"))
d_raw$scope <- factor(d_raw$scope,levels=c("surface","inverse"))
hkk_violin <- ggplot(d_raw, aes(x=factor(order,labels=c("every > a/one","a/one > every")),y=response,fill=scope)) +
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
  facet_wrap( ~ scramble)
hkk_violin
#ggsave("../results/heritage-korean-violin.png",width=6,height=2)

heritage_korean = d_raw


######################
### Tübingen plots ###
######################

heritage_korean$subexpt = "Heritage Korean"
heritage_korean$subexperiment = "Heritage Korean"
native_korean$subexpt = "Native Korean"
native_korean$subexperiment = "Native Korean"
english$subexpt = "English"
english$scramble = "default"

hk =subset(heritage_korean,select=c("workerid","subexperiment","response","subexpt","order","scope","scramble"))
nk =subset(native_korean,select=c("workerid","subexperiment","response","subexpt","order","scope","scramble"))
e  =subset(english,select=c("workerid","subexperiment","response","subexpt","order","scope","scramble"))

d = rbind(hk,nk,e)

### only default order

default = d[d$scramble=="default"&d$order=="aevery"&d$subexperiment!="one"&d$subexperiment!="there"&d$subexperiment!="thereone",]

d_s = bootsSummary(data=default, measurevar="response", groupvars=c("order","scope","subexpt"))
d_s$subexpt <- factor(d_s$subexpt,levels=c("English","Native Korean","Heritage Korean")) 
d_s$subexpt <- factor(d_s$subexpt,labels=c("Native English\n(n=86)","Native Korean\n(n=23)","Heritage Korean\n(n=12)")) 

plot <- ggplot(d_s, aes(x=subexpt,y=response,fill=factor(scope,labels=c("surface","inverse")))) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=subexpt, width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  #scale_y_continuous(limits=c(1,7),oob = rescale_none)+
  geom_text(aes(y=(bootsci_low-.06),label=round(response,1)),position=position_dodge(width=0.9))+
  #scale_y_continuous(limits=c(1,7),rescale_none)+
  #scale_y_continuous(limits=c(1,5),oob = rescale_none)+
  #ylim(0,7)+
  xlab("\n experiment") +
  labs(fill="interpretation")+
  theme_bw() +
  scale_fill_manual(values=c("darkgrey", "lightgrey"))+
  theme(panel.background = element_rect(fill = 'white',color ='black')) 
#facet_wrap( ~ subexpt)
plot

#ggsave("../results/tuebingen_scope_plot.pdf",width=6,height=4)


### default and scramble

ds = d[d$order=="aevery"&d$subexperiment!="one"&d$subexperiment!="there"&d$subexperiment!="thereone",]
ds_s = bootsSummary(data=ds, measurevar="response", groupvars=c("order","scope","subexpt","scramble"))
ds_s$subexpt <- factor(ds_s$subexpt,levels=c("English","Native Korean","Heritage Korean")) 
ds_s$subexpt <- factor(ds_s$subexpt,labels=c("Native English\n(n=86)","Native Korean\n(n=23)","Heritage Korean\n(n=12)")) 

plot <- ggplot(ds_s, aes(x=subexpt,y=response,color=scramble,fill=factor(scope,labels=c("surface","inverse")))) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=subexpt, width=0.1),position=position_dodge(width=0.9))+
  ylab("rating\n")+
  #scale_y_continuous(limits=c(1,7),oob = rescale_none)+
  geom_text(aes(y=(bootsci_low-.065),label=round(response,1)),position=position_dodge(width=0.9))+
  #scale_y_continuous(limits=c(1,7),rescale_none)+
  #scale_y_continuous(limits=c(1,5),oob = rescale_none)+
  #ylim(0,7)+
  xlab("\n experiment") +
  labs(fill="interpretation",color="order")+
  theme_bw() +
  scale_fill_manual(values=c("darkgrey", "lightgrey"))+
  scale_color_manual(values=c("black", "blue"))+
  theme(panel.background = element_rect(fill = 'white',color ='black')) 
#facet_wrap( ~ subexpt)
plot

#ggsave("../results/tuebingen_scope_plot_scramble.pdf",width=6,height=4)

