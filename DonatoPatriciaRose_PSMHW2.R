## Load data and packages
library(readr)
library(tidyverse)
data <- read_csv("C:/Users/pdonato/Desktop/Classroom Lectures/PSM/psm_example.csv")

#---------------------------------- BEFORE MATCHING ----------------------------------#
## 2-sample test
## Ho: prop(death | treat) = prop(death | control)
## Ha: prop(death | treat) != prop(death | control)
## no sig difference
prop.test(n = c(nrow(data %>% filter(trt == 0)), nrow(data %>% filter(trt == 1))),
          x = c(sum(data %>% filter(trt == 0) %>% select(death)), sum(data %>% filter(trt == 1) %>% select(death))))

## Perform EDA by group
## Age, risk, and severity might be confounders
## a) plots
ggplot(data=data,aes(x=as.factor(trt),y=age)) + geom_boxplot() ## older people in trt group
ggplot(data=data,aes(x=as.factor(trt),y=risk)) + geom_boxplot() ## higher risk in trt group
ggplot(data=data,aes(x=as.factor(trt),y=severity)) + geom_boxplot() ## a little higher severity in trt group
ggplot(data=data,aes(x=as.factor(male), y=..count..)) + geom_bar(aes(fill=as.factor(trt)),position="fill") ## all are males; 32% are in trt group
## b) summary table
data %>%
  group_by(trt) %>%
  summarise(N=n(), prp_score=mean(ps), age=mean(age), risk=mean(risk), severity=mean(severity), death=mean(death))

#---------------------------------- PERFORMING PSM ----------------------------------#
## Perform PSM
ps_model <- glm(trt~age+risk+severity, data=data, family="binomial")
data$ps <- predict(ps_model,type="response")

## Check for overlap
## Obs in ctrl group are usually a lot more than in the trt group
ggplot(data=data,aes(x=ps,y=..count..)) + geom_histogram() + facet_wrap(~as.factor(trt), nrow=2)

#*********************************** ASSIGNMENT 2 ***********************************#
## Perform nearest neighbor matching
nearest <- function(data,group) {
  data$id <- seq.int(nrow(data)) # add unique id
  
  trt <- data %>% # get prop score and id of trt group
    filter(eval(parse(text=paste(group,"== 1")))) %>%
    select(id,ps)
  trt <- trt[sample(nrow(trt)),] # shuffle rows of trt group
  ctrl <- data %>% # get prop score and id of trt group
    filter(eval(parse(text=paste(group,"== 0")))) %>%
    select(id,ps)
  
  match_ref <- data.frame()
  for(i in 1:nrow(trt)) {
    trt_ps=trt$ps[i] # gets the first obs in trt group to be matched
    ctrl_ps=ctrl$ps
    match <- ctrl[which(abs(ctrl_ps-trt_ps)==min(abs(ctrl_ps-trt_ps))),][sample(length(which(abs(ctrl_ps-trt_ps)==min(abs(ctrl_ps-trt_ps)))),1),] # finds the obs in ctrl group where the distance is the minimum
    match_ref <- rbind(match_ref,cbind(trt[i,],match)) 
    
    ctrl <- setdiff(ctrl,match) # remove obs from the ctrl group once it has been matched
  }
  colnames(match_ref) <- c("id.trt","ps.trt","id.ctrl","ps.ctrl")
  
  match_df <- data %>% # outputs the orig df with all vars containing only the matched obs
    filter(id %in% c(match_ref$id.ctrl,match_ref$id.trt))
  
  assign("match_ref",match_ref,envir=.GlobalEnv)
  assign("match_df",match_df,envir=.GlobalEnv)
}

## Redo prop test
prop.test(n = c(nrow(match_df %>% filter(trt == 0)), nrow(data %>% filter(trt == 1))),
          x = c(sum(match_df %>% filter(trt == 0) %>% select(death)), sum(data %>% filter(trt == 1) %>% select(death))))
nearest(data=data,group="trt")

## Check for the datasets generated
match_ref
match_df
#**********************************************************************************#

#---------------------------------- AFTER MATCHING ----------------------------------#
## Verify summary table
match_df %>%
  group_by(trt) %>%
  summarise(N=n(), prp_score=mean(ps), age=mean(age), risk=mean(risk), severity=mean(severity), death=mean(death))

## Redo prop test
prop.test(n = c(nrow(match_df %>% filter(trt == 0)), nrow(data %>% filter(trt == 1))),
          x = c(sum(match_df %>% filter(trt == 0) %>% select(death)), sum(data %>% filter(trt == 1) %>% select(death))))
