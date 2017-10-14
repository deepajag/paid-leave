source('data_script.R')

policy.year = c(1998,1998,2002,2002)
type = c('Belgium','Belgium','Denmark','Denmark')
gender = c('Female','Male','Male','Female')
controls = list(belg.female,belg.male,den.male,den.female)

lead.lag.results <- matrix(,4,3,)
colnames(lead.lag.results) <- c('lag','lead','original')
rownames(lead.lag.results) <- c('belg.female','belg.male','den.male','den.female')


for ( i in 1:length(type)){
type2  = type[i]
controls2 = controls[[i]]
policy.year2 = policy.year[i]
gender2 = gender[i]

rs = get.data(type = type2 ,
         controls = controls2 ,
         policy.year = policy.year2,
         gender = gender2)

results.mat = lead.lag(rs,
         year = policy.year2,
         type = type2,
         gender = gender2,
         controls = controls2)

lead.lag.results[i,] <- results.mat
}

lead.lag.results = noquote(lead.lag.results)
lead.lag.results 
write.table(lead.lag.results, file=paste(getwd(),'/lead_lag/lead-lag-results.txt', sep=""))

