source('data_script.R')
RStudio.version()

###Loop the functions over all treatment-control groups###
policy.year =  c(1998,1998,2002,2002) 
type = c('Belgium','Belgium','Denmark','Denmark') 
gender = c('Female','Male','Female','Male')
controls = list(belg.female,belg.male, den.female,den.male) 

###Total sample sizes before/after restrictions
total.elig = ds %>% filter(year==1990) %>% count()
total.elig.gender = ds %>% filter(year==1990) %>% group_by(gender) %>% count()
total.by = ds %>% filter(year==1990 & yrbirth <= 1958) %>% count()
total.by.gender = ds %>% filter(year==1990 & yrbirth > 1958) %>% group_by(gender) %>% count()
total.all.country = ds %>% filter(year==1990 & yrbirth <= 1958) %>% group_by(country) %>% count()
total.elig.country = ds %>% 
  filter(year==1990 & yrbirth <= 1958 & 
           country != 'Netherlands' & 
           country != 'Ireland' & 
           country != 'Sweden') %>% count()
total.elig.country.gender = ds %>% 
  filter(year==1990 & yrbirth <= 1958 & 
           country != 'Netherlands' & 
           country != 'Ireland' & 
           country != 'Sweden') %>% 
  group_by(gender) %>% count()

###Table of descriptives
samples.list = vector('list',4)
for (i in 1:4) {
  ds.sample = ds
  rs = get.data(type = type[i], controls = controls[[i]], policy.year[i], gender=gender[i])
  samples.list[[i]] <- sample.description(rs,policy.year = policy.year[i])
}

table1 = do.call(rbind,samples.list)
write.csv(table1,file="table1.txt")

##Primary analysis
all.results = matrix(,4,2,); colnames(all.results)=c('base','adjusted');
row.names(all.results) = c('belg.female','belg.male','den.female','den.male')
for(i in 1:nrow(all.results )){
  rs = get.data(type[i], controls[[i]], policy.year[i], gender=gender[i])
  all.results[i,] <- get.results.table(rs)
}

all.results <- noquote(all.results)
all.results

write.csv(all.results, file='all-results-final.txt')


##########The function loads from data_script.R but pasted below for easy viewing###############
# get.results.table = function(rs){
# #Primary models
# #model 1 - country fixed effects and SE clustered at mergeID level 
# ##Cluster.se function for clustering SEs available in the data script
# 
# m1 = lm(working ~ policy.introduced + treated + interaction + country +
#           bs(year, degree=2), data = rs)
# cluster.se1 = cluster.se(model = m1)
# 
# #model2 - model 1 plus 10 year age category dummies 
# m2 = lm(working ~ policy.introduced + treated +  
#            interaction +  country + bs(year, degree=2) + age_cat, data =rs)
# cluster.se2 = cluster.se(model = m2)
# 
# #Extract all results with confidence intervals into an empty matrix
# results.mat = matrix(,1,2,); colnames(results.mat) = c('base','adjusted')
# se = cluster.se1['interaction','Std. Error']
# estimate = round(cluster.se1,4)['interaction','Estimate']
# results.mat[1,1] = paste(estimate, '[',
#                          paste(round(estimate - qnorm(0.975)*se,3),
#                                round(estimate + qnorm(0.975)*se,3),sep=';'),
#                          ']',sep="")
# estimate2 = round(cluster.se2,4)['interaction','Estimate']
# se2 = cluster.se2['interaction','Std. Error']
# results.mat[1,2] = paste(estimate2, '[',
#       paste(round(estimate2 - qnorm(0.975)*se2,3),
#             round(estimate2 + qnorm(0.975)*se2,3),sep=';'), ']',sep="")
# 
# #Export results
# type = unique(rs[rs$treated==1,'country'])
# gender = rs$gender[1]
# write.table(results, file=paste(type,gender,'.txt',sep=""),sep=",")
# 
# return(results.mat)
# 
# }


