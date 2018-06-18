setwd("~/Dropbox/MashaLab/Quantifier\ Scope/HERENGextendedNEW/")
bootsSummary <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                         conf.interval=.95, .drop=TRUE, n_boots_samps=1000) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     bootsci_high = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["97.5%"]],
                     bootsci_low = quantile( #doesn't play nice with na.rm
                       replicate(n_boots_samps, mean(sample(xx[[col]], replace = TRUE))),
                       c(0.025, 0.975))[["2.5%"]]
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  return(datac)
}
library(ggplot2)


# where your experiment is hosted
experigen.sourceURL = "web.stanford.edu.scontras.HERENGextendedNEW.web"
# this information comes from your settings.js file
experigen.experimentName = "HeritageEnglishExtended"
experigen.database = "http://db.phonologist.org/"


# check for usage of the experiment (number of page views per participant)
experigen.users  =  paste(experigen.database, "users.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL, sep="")
read.csv(experigen.users, sep="\t")

# read the experimental results from the server
experigen.url  =  paste(experigen.database, "makecsv.cgi?experimentName=", experigen.experimentName, "&sourceurl=", experigen.sourceURL, sep="")
exp  = read.csv(experigen.url, sep="\t")

#demographic data
meta = read.csv(paste(experigen.url, "&file=demographics.csv", sep=""), sep="\t")
meta$time = as.POSIXct(strptime(as.character(meta$time), "%a %b %d %H:%M:%S %Y"))



#write.csv(exp,"HERENGextended.csv")

head(exp)
head(meta)


# Add demographic info
demo <- subset(meta, select = -c(X_,destination,experimentName,time))

## add demo info to d
exp$age = demo$age[match(exp$userCode,demo$userCode)]
exp$born = demo$born[match(exp$userCode,demo$userCode)]
exp$first = demo$first[match(exp$userCode,demo$userCode)]
exp$home = demo$home[match(exp$userCode,demo$userCode)]
exp$live = demo$live[match(exp$userCode,demo$userCode)]

d <- exp

## only test trials

d = d[d$condition!='filler',]
d = d[d$condition!='',]
d = d[d$condition!='practice',]

## condition info

meta = read.csv("~/Dropbox/MashaLab/Quantifier\ Scope/HERENGextendedNEW/meta.csv",header=T)

d$itemcond = paste(d$item,d$condition)
meta$itemcond = paste(meta$item,meta$condition)

d$experiment = meta$experiment[match(d$itemcond,meta$itemcond)]
d$ORDER = meta$ORDER[match(d$itemcond,meta$itemcond)]
d$INVERSE = meta$INVERSE[match(d$itemcond,meta$itemcond)]
d[d$experiment=="there" | d$experiment=="thereone",]$ORDER = "OE"

aggregate(response1~experiment*ORDER*INVERSE,data=d,mean)
aggregate(response1~experiment*ORDER*INVERSE,data=d,var)




## Some plots

rating <- d

rating$measure = "rating"

rating$inverse_name = "surf"
rating[rating$INVERSE==1,]$inverse_name = "inv"


rating$condition_name = paste(rating$experiment, rating$ORDER, rating$inverse_name, sep=" ")

table(rating$condition_name,rating$userCode)

rating_s = bootsSummary(data=rating, measurevar="response1", groupvars=c("ORDER","INVERSE","experiment"))

rating_plot <- ggplot(rating_s, aes(x=ORDER,y=response1,fill=factor(INVERSE,labels=c("SURFACE","INVERSE")))) +
  #  geom_bar(alpha=1/2,stat="identity",position=position_dodge()) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=bootsci_low, ymax=bootsci_high, x=ORDER, width=0.1),position=position_dodge(width=0.9))+
  ylab("rating \n")+
  xlab("\n order")+coord_cartesian(ylim=c(1,7))
rating_plot <- rating_plot  +facet_grid(~ experiment)+ labs(fill="scope")+ggtitle("Heritage English ratings (1-7)")
#+ theme_blackDisplay()
rating_plot

ggsave(filename='Englishrating.png',plot=rating_plot,width=7.15, height=2.15)
