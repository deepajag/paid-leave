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

##Set theme for ggplot graphics
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


##Final control Countries
belg.male = c('Switzerland','France','Spain')
belg.female = c('Italy','France','Greece')
den.male = c('Greece')
den.female = c('Austria')


##Data set up for primary analysis
get.data= function(type, controls,policy.year, gender){
    ##Create dummy variables for pre/post policy period and control versus treated status
    ds$policy.introduced = ifelse(ds$year > policy.year, 1, 0)
    ds$treated = ifelse(ds$country==type, 1, 0) 
    ds$include.control <- ifelse(ds$country %in% controls, 1, 0)

    ##Create interaction terms
    ds$interaction = ds$policy.introduced*ds$treated
    ds$year2= ds$year*ds$year
    
    ##Place gender and year of birth restrictions on the sample
    rs = ds[(ds$include.control==1 | ds$treated==1) &
             ds$gender==gender &
             ds$yrbirth <= 1958,]
    
    ##Modify variables to correct class, add retirement eligibility, and merge the World Bank data
    rs$eligible <- ifelse(rs$age < rs$ret_age, 0, 1)
    rs$country = as.character(rs$country);
    rs$year = as.integer(rs$year)
    wb_final$country = as.character(wb_final$country);
    wb_final$year = as.integer(wb_final$year);
    wb_final$gender = as.character(wb_final$gender); rs$gender = as.character(rs$gender)
    rs = left_join(rs, wb_final, by = c('country','year','gender'))
    
    ##Change variable class again and specific reference factor level as treated country
    rs$country = as.factor(rs$country)
    rs$country = relevel(rs$country,type)
    
 
    return(rs)
  }


###Function for clustered SEs
cluster.se = function(model=m,cluster= rs[,'mergeid'] ){
  vcovCL <- cluster.vcov(model, cluster)
  coef <- coeftest(model, vcovCL)
  return(coef)
}




##Function to generate sample description tables
sample.description = function(rs,policy.year){
  ##Sample sizes
  sample.size = rs %>% group_by(country,year) %>% summarise(count = n()) %>%
    group_by(country) %>% summarise(n = mean(count))
  
  ##Mean age in 1990
  age = rs %>% filter(year=='1990') %>% 
    group_by(country) %>% 
    summarise(mean.age= mean(age),sd.age = sd(age)) 
  
  ##Pre-policy working proportions
  work.pre.policy = rs %>% filter(year<policy.year) %>% 
    group_by(country,year) %>% 
    summarise(count = n(),working = sum(working)) %>%
    mutate(prop = working/count) %>% 
    group_by(country) %>% 
    summarise(mean.pre.policy=mean(prop),sd.pre.policy=sd(prop))
  
  ##Post-policy working proportions
  work.post.policy = rs %>% filter(year>policy.year) %>% 
    group_by(country,year) %>% 
    summarise(count = n(),working = sum(working)) %>%
    mutate(prop = working/count) %>% 
    group_by(country) %>% 
    summarise(mean.post.policy=mean(prop),sd.post.policy=sd(prop))
  
  ##Organize information into descriptives table
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

##Function for primary analysis modelling
get.results.table = function(rs){
  #Primary models
  #model 1 - country fixed effects and SE clustered at mergeID level 
  ##Cluster.se function for clustering SEs available in the data script
  m1 = lm(working ~ policy.introduced + treated + interaction + country +
            bs(year, degree=2), data = rs)
  cluster.se1 = cluster.se(model = m1)
  
  #model2 - model 1 plus 10 year age category dummies 
  m2 = lm(working ~ policy.introduced + treated +  
            interaction +  country + bs(year, degree=2) + age_cat, data =rs)
  cluster.se2 = cluster.se(model = m2)
  
  #Extract all results into an empty matrix
  results.mat = matrix(,1,2,); colnames(results.mat) = c('base','adjusted')
  se = cluster.se1['interaction','Std. Error']
  estimate = round(cluster.se1,4)['interaction','Estimate']
  results.mat[1,1] = paste(estimate, '[',
                           paste(round(estimate - qnorm(0.975)*se,2),
                                 round(estimate + qnorm(0.975)*se,2),sep=';'),
                           ']',sep="")
  estimate2 = round(cluster.se2,4)['interaction','Estimate']
  se2 = cluster.se2['interaction','Std. Error']
  results.mat[1,2] = paste(estimate2, '[',
                           paste(round(estimate2 - qnorm(0.975)*se2,2),
                                 round(estimate2 + qnorm(0.975)*se2,2),sep=';'), ']',sep="")
  
  #Export results
  type = unique(rs[rs$treated==1,'country'])
  gender = rs$gender[1]
  write.table(results, file=paste(type,gender,'.txt',sep=""),sep=",")
  
  return(results.mat)
  
}

##########################MAIN SENSITIVITY ANALYSIS###########################
##Function to estimate lead and lag effects
lead.lag = function(rs,year,type,gender,controls){
  ##Original year
  policy.year.original = year
  m = lm(working ~ policy.introduced + treated +  
           interaction +  country + bs(year, degree=2) + age_cat  ,
         data = rs)
    cluster.se.original = cluster.se(m = m )
  
  ##Reset and move policy year to original+2
  rs$policy.introduced <- NA
  policy.year2 = policy.year.original + 2
  rs$policy.introduced = ifelse(rs$year > policy.year2,1,0)
  rs$interaction = rs$policy.introduced*rs$treated
  m_lag = lm(working ~ policy.introduced + treated +  
               interaction +  country + bs(year, degree=2) + age_cat,
             data = rs)
  cluster.se.plus2 = cluster.se(m = m_lag )
  
  ##Reset and move policy year to original-2
  rs$policy.introduced <- NA
  policy.year3 = policy.year.original - 2
  rs$policy.introduced = ifelse(rs$year > policy.year3 , 1,0)
  rs$interaction = rs$policy.introduced*rs$treated
  m_lead = lm(working ~ policy.introduced + treated +  
                interaction +  country + bs(year, degree=2) + age_cat,
              data = rs)
  cluster.se.minus2 = cluster.se(m = m_lead)
  
  ##Generate matrix of results and plots
  results.mat = matrix(,3,2,); 
  colnames(results.mat) = c('coefficient','se')
  rownames(results.mat) = c('lead','lag','original')
  results.mat2 = matrix(,1,3,)
  
  estimateplus2 = round(cluster.se.plus2 ,4)['interaction','Estimate']; 
  seplus2 = round(cluster.se.plus2 ,4)['interaction','Std. Error']
  results.mat[1,1] = estimateplus2
  results.mat[1,2] = seplus2
  results.mat2[,1] <- paste(estimateplus2 , '[',
                            paste(round(estimateplus2  - qnorm(0.975)*seplus2,3),
                                  round(estimateplus2  + qnorm(0.975)*seplus2,3),sep=';'),
                            ']',sep="")
  
  estimateminus2 = round(cluster.se.minus2, 4)['interaction','Estimate']
  seminus2 = round(cluster.se.minus2, 4)['interaction','Std. Error']
  results.mat[2,1] = estimateminus2 
  results.mat[2,2] = seminus2
  results.mat2[,2] <- paste(estimateminus2 , '[',
                            paste(round(estimateminus2  - qnorm(0.975)*seminus2,3),
                                  round(estimateminus2  + qnorm(0.975)*seminus2,3),sep=';'),
                            ']',sep="")
  
  original = round(cluster.se.original,4)['interaction','Estimate']
  se.original = round(cluster.se.original,4)['interaction','Std. Error']
  results.mat[3,1] = original
  results.mat[3,2] = se.original
  results.mat2[,3] <- paste(original , '[',
                            paste(round(original  - qnorm(0.975)*se.original,3),
                                  round(original  + qnorm(0.975)*se.original,3),sep=';'),
                            ']',sep="")
  
  model.names = c('3.t-plus2', '1.t-minus2', '2.Original')
  results = data.frame(results.mat)
  
  
  ggplot(results , aes(x=model.names, y=coefficient)) + 
    geom_errorbar(aes(ymin=coefficient-(1.96*se), ymax=coefficient+(1.96*se)), width=.2) +
    geom_line(size = 2) + geom_hline(yintercept = 0, color = 'darkgrey') +
    geom_point() + coord_flip() + 
    ylim(c(-0.15,0.15)) + theme(axis.text = element_text(size = 14)) +
    ggtitle(paste(type, 'Results', '(', gender, ')', sep=" ")) +
    theme(plot.caption = element_text(hjust=0.3, vjust=-0.1,size=15,family="serif" ),
          axis.title = element_text(face="bold" )) +
    labs(y='Estimate',x='Model',  
         caption = paste('Controls=', toString(controls),sep=" "))
  
  ##Create lead lag folder in working directory
  dir.create('lead_lag')
  ggsave(filename = paste(type,gender,'SA', '.tiff', sep=""), 
         path = paste(getwd(),'/lead_lag', sep=""))
  
  ##Explort all results to working directory
  write.table(results, file=paste(type,gender,'SA','.txt',sep=""),sep=",")
  
  return(results.mat2)
  
  
}

######################SUPPLEMENTAL SENSITIVITY ANALYSIS########################

####Alternate Controls####
###Control countries ordered by p-value indicating difference in trend in pre-policy period
#relative to treated country
belg.male2 = c('France','Spain','Switzerland', 'Germany','Austria','
               Czech Republic','Greece','Italy','Poland')
den.male2 = c('Greece','Czech Republic','Spain','Switzerland','France',
              'Germany',  'Austria','Italy', 'Poland')
belg.female2 = c('France','Italy','Greece','Germany','Switzerland','Austria',
                 'Spain', 'Czech Republic', 'Poland')
den.female2 = c('Austria','Germany','France','Czech Republic','Greece',
                'Italy','Switzerland','Poland','Spain')

###Labels for plots
#For Belgium female
model.names1 = c('1) France','2) 1+Italy','3) 2+Greece (MAIN)', 
                 '4) 3+Germany','5) 4+Switzerland', '6) 5+Austria','7) 6+Spain',
                 '8) 7+Czech Republic','9) 8+Poland')

#For Belgium male
model.names2 = c('1) France','2) 1+Spain','3) 2+Switzerland (MAIN)',
                 '4) 3+Germany','5) 4+Austria','6)  5+Czech Republic',
                 '7) 6+Greece','8) 7+Italy','9) 8+Poland')

#For Denmark male
model.names3 = c('1) Greece (MAIN)','2) 1+Czech Republic','3) 2+Spain',
                 '4) 3+Switzerland','5) 4+France','6)  5+Germany',
                 '7) 6+Austria','8) 7+Italy','9) 8+Poland')

#For Denmark female
model.names4 = c('1) Austria (MAIN)','2) 1+Germany','3) 2+France',
                 '4) 3+Czech Republic','5) 4+Greece','6)  5+Italy',
                 '7) 6+Switzerland','8) 7+Poland','9) 8+Spain')

model.names = list(model.names1,model.names2,model.names3,model.names4)
control.plots = vector('list',4)

##Function to see estimate across diferent controls
alternate.controls = function(type,controls,policy.year,gender,model.names.x){
  
  rs = get.data(type, controls, policy.year, gender=gender)
  x <- controls
  l <- paste(x)
  
  ##Matrix to store results
  results.mat = matrix(,9,2,); colnames(results.mat) = c('coefficient','se')
  
  ##Loop to generate results for each combination of controls
  for (j in 1:(length(belg.male2))){
    rs2 = rs[rs$country %in% c(type,l[1:j]), ]   
    
    m = lm(working ~ policy.introduced + treated +  
             interaction +  country + bs(year, degree=2) + age_cat,
           data = rs2)
    m.se = cluster.se(model = m, cluster = rs2[,'mergeid'])
    
    results.mat[j,1] = round(m.se ,4)['interaction','Estimate']
    results.mat[j,2] = round(m.se ,4)['interaction','Std. Error']
  }
  
  results = data.frame(results.mat)
  
  ##Numbers to create gradient marking quality on the plot
  results$numbers = seq(0.1,0.9,by=0.1)
  results$numbers = as.numeric(results$numbers)
  results$model.names.x = model.names.x
  
  ##Final Plot
  control.plot = 
    ggplot(results , aes(x=model.names.x, y=coefficient, color = numbers)) + 
    scale_color_continuous(low='black', high='lightgrey',
                           name='Quality of \n control group',
                           breaks = c(0.2),
                           labels=c('Best')) +
    geom_errorbar(aes(ymin=coefficient-(qnorm(0.975)*se), 
                      ymax=coefficient+(qnorm(0.975)*se)), width=.2) +
    geom_line(size = 2) + 
    geom_hline(yintercept = 0, color = 'darkgrey') +
    geom_point(size=2) + coord_flip() + 
    ylim(c(-0.10,0.10)) + 
    theme(axis.text = element_text(size = 10)) +
    ggtitle(paste(type, 'Results', '(', gender, ')', sep=" ")) +
    labs(y='Estimate',x='Model')
  
  return(control.plot)
  
}


##Function to re-estimate results using sampling weights
weighted.analysis = function(rs){
  
  ##Join cross sectional and longitudinal weights
  cs.weights  = cs.weights %>% select(mergeid, cciw_w3)
  long.weights  = long.weights %>% select(mergeid, cliw_c)
  rs = left_join(rs, cs.weights)
  rs = left_join(rs, long.weights)
  
  ##Re-run adjusted model with different types of weights and apply standarde error clustering function
  m.longweight = lm(working ~ policy.introduced + treated +  
                      interaction +  country + bs(year, degree=2) + 
                      age_cat, data =rs, weights = cliw_c)
  cluster.se.long = cluster.se(m.longweight)
  
  m.csweights = lm(working ~ policy.introduced + treated +  
                     interaction +  country + bs(year, degree=2) + 
                     age_cat, data =rs, weights = cciw_w3)
  cluster.se.cs = cluster.se(m.csweights )
  
  
  m.primary = lm(working ~ policy.introduced + treated +  
                   interaction +  country + bs(year, degree=2) + 
                   age_cat, data =rs)
  cluster.se.1 = cluster.se(m.primary)
  
  ##Store results in a matrix to return
  results.mat = matrix(,3,1,); rownames(results.mat) = c('primary','long.weights','cs.weights')
  colnames(results.mat) = 'Estimate[95% CI]'
  
  estimate = round(cluster.se.1,4)['interaction','Estimate']
  se = cluster.se.1['interaction','Std. Error']
  results.mat[1,] = paste(estimate, '[',
                          paste(round(estimate - qnorm(0.975)*se,3),
                                round(estimate + qnorm(0.975)*se,3),sep=';'),
                          ']',sep="")
  estimate2 = round(cluster.se.long ,4)['interaction','Estimate']
  se2 = cluster.se.long['interaction','Std. Error']
  results.mat[2,] = paste(estimate2, '[',
                          paste(round(estimate - qnorm(0.975)*se2,3),
                                round(estimate + qnorm(0.975)*se2,3),sep=';'), ']',sep="")
  estimate2 = round(cluster.se.cs,4)['interaction','Estimate']
  se2 = cluster.se.cs['interaction','Std. Error']
  results.mat[3,] = paste(estimate2, '[',
                          paste(round(estimate - qnorm(0.975)*se2,3),
                                round(estimate + qnorm(0.975)*se2,3),sep=';'), ']',sep="")
  
  results.mat = noquote(results.mat)
  
  return(results.mat)
}
