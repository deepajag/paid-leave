##Load necessary packages
library(readstata13)
library(ggplot2)
library(sandwich)
library(lmtest)
library(splines)
library(plm)
library(gridExtra)
library(dplyr)
library(multiwayvcov)
library(survey)

source('world_bank_cleaning v2.R')
set.seed(1234)

##Restrict data according to 1958 birth year and 1980 calendar year
ds = read.dta13('share_jobepisodes_condensed1.dta')
ds = ds[ds$yrbirth <= 1958 & ds$year >= 1990,]
theme_set(theme_bw(base_size=18))

##Import weight data
cs.weights = read.dta13('sharew3_rel5-0-0_gv_weights.dta')
long.weights = read.dta13('sharewX_rel5-0-0_gv_longitudinal_weights_w2w3.dta')

#####Recode age variable
ds$age_cat[ds$age < 40] <- 0
ds$age_cat[ds$age >= 40 & ds$age < 50] <- 1
ds$age_cat[ds$age >= 50 & ds$age < 60] <- 2
ds$age_cat[ds$age >= 60 & ds$age < 70] <- 3
ds$age_cat[ds$age >= 70] <- 4
ds$age_cat = factor(ds$age_cat, 
                    levels = c(0,1,2,3,4), 
                    labels = c('less40','40.49','50.59','60.69','70+'))


##Data set up for control selection
get.data.controls= function(type, controls,policy.year, gender){
  ds.sample$pre.policy = rep(0, nrow(ds.sample))
  #Code pre-policy years as 1
  ds.sample$pre.policy[ds.sample$year <= policy.year] <- 1 
  restricted.sample = ds.sample[ds.sample$yrbirth <= 1958 & ds.sample$year >= 1980,]  
  #Code treated country as 1
  restricted.sample$treated = ifelse(restricted.sample$country==type, 1, 0) 
  
  restricted.sample$include.control = rep(0,nrow(restricted.sample))
  #Code candidate controls as 1
  restricted.sample$include.control[restricted.sample$country %in% controls] <- 1 
  #New data restricting to candidate controls + treated country + gender + pre-policy years
  rs = restricted.sample[restricted.sample$gender==gender & 
                           (restricted.sample$include.control==1 | 
                              restricted.sample$treated==1) & 
                           restricted.sample$pre.policy==1,]
  
  return(rs)
}

##Data set up for primary analysis
get.data= function(type, controls,policy.year, gender){
    ds.sample$policy.introduced = rep(0, nrow(ds.sample))
    ds.sample$policy.introduced[ds.sample$year > policy.year] <- 1 
    restricted.sample = ds.sample[ds.sample$yrbirth <= 1958,]
    restricted.sample$treated = ifelse(restricted.sample$country==type, 1, 0) 
    
    restricted.sample$include.control = rep(0,nrow(restricted.sample))
    restricted.sample$include.control[restricted.sample$country %in% controls] <- 1
    restricted.sample$interaction = restricted.sample$policy.introduced*restricted.sample$treated
    restricted.sample$year2= restricted.sample$year*restricted.sample$year
    rs = restricted.sample[(restricted.sample$include.control==1 | 
                          restricted.sample$treated==1) &
                          restricted.sample$gender==gender,]
    rs$country = as.character(rs$country);
    rs$year = as.integer(rs$year)
    wb_final$country = as.character(wb_final$country);
    wb_final$year = as.integer(wb_final$year);
    wb_final$gender = as.character(wb_final$gender); rs$gender = as.character(rs$gender)
    rs = left_join(rs, wb_final, by = c('country','year','gender'))
    
    rs$country = as.factor(rs$country)
    rs$country = relevel(rs$country,type)
    rs$eligible = rep(NA,nrow(rs))
    rs$eligible <- ifelse(rs$age < rs$ret_age, 0, 1)
    return(rs)
  }


###Function for clustered SEs
cluster.se = function(model=m,cluster= rs[,'mergeid'] ){
  vcovCL <- cluster.vcov(model, cluster)
  coef <- coeftest(model, vcovCL)
  return(coef)
}


##Final control Countries
belg.male = c('Switzerland','France','Spain')
belg.female = c('Italy','France','Greece')
den.male = c('Greece')
den.female = c('Austria')


##Function to generate sample description tables
sample.description = function(rs,policy.year){
  sample.size = rs %>% group_by(country,year) %>% summarise(count = n()) %>%
    group_by(country) %>% summarise(n = mean(count))
  
  age = rs %>% filter(year=='1990') %>% 
    group_by(country) %>% 
    summarise(mean.age= mean(age),sd.age = sd(age)) 
  
  work.pre.policy = rs %>% filter(year<policy.year) %>% 
    group_by(country,year) %>% 
    summarise(count = n(),working = sum(working)) %>%
    mutate(prop = working/count) %>% 
    group_by(country) %>% 
    summarise(mean.pre.policy=mean(prop),sd.pre.policy=sd(prop))
  
  work.post.policy = rs %>% filter(year>policy.year) %>% 
    group_by(country,year) %>% 
    summarise(count = n(),working = sum(working)) %>%
    mutate(prop = working/count) %>% 
    group_by(country) %>% 
    summarise(mean.post.policy=mean(prop),sd.post.policy=sd(prop))
  
  sample = data.frame(cbind(sample.size,age,work.pre.policy,work.post.policy))
  sample$change = round(sample$mean.post.policy - 
                          sample$mean.pre.policy,2)
  
  sample$age = paste(round(sample$mean.age,2),
                     "(", round(sample$sd.age,2),")", 
                     sep="")
  sample$pre.policy = paste(round(sample$mean.pre.policy ,2),
                            "(", round(sample$sd.pre.policy,2),")", 
                            sep="")
  sample$post.policy =paste(round(sample$mean.post.policy ,2),
                            "(", round(sample$sd.post.policy ,2),")", 
                            sep="")
  
  sample = sample %>% select(country,n,age,pre.policy,post.policy,change)
  return(sample)
}
