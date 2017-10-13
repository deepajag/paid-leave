source('data_script.R')

policy.year = c(1998,1998,2002,2002)
type = c('Belgium','Belgium','Denmark','Denmark')
gender = c('Female','Male','Male','Female')
controls = list(belg.female,belg.male,den.male,den.female)

lead.lag.results <- matrix(,4,3,)
colnames(lead.lag.results) <- c('lag','lead','original')
rownames(lead.lag.results) <- c('belg.female','belg.male','den.male','belg.female')


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
i
lead.lag.results = noquote(lead.lag.results)
lead.lag.results 
write.table(lead.lag.results, file=paste(getwd(),'/lead_lag/lead-lag-results.txt', sep=""))

##########The function loads from data_script.R but pasted below for easy viewing###############
# ##Function to estimate lead and lag effects
# lead.lag = function(rs,year,type,gender,controls){
#   ##Original year
#   policy.year.original = year
#   m = lm(working ~ policy.introduced + treated +  
#            interaction +  country + bs(year, degree=2) + age_cat  ,
#          data = rs)
#   cluster.se.original = cluster.se(m = m )
#   
#   ##Reset and move policy year to original+2
#   rs$policy.introduced <- NA
#   policy.year2 = policy.year.original + 2
#   rs$policy.introduced = ifelse(rs$year > policy.year2,1,0)
#   rs$interaction = rs$policy.introduced*rs$treated
#   m_lag = lm(working ~ policy.introduced + treated +  
#                interaction +  country + bs(year, degree=2) + age_cat,
#              data = rs)
#   cluster.se.plus2 = cluster.se(m = m_lag )
#   
#   ##Reset and move policy year to original-2
#   rs$policy.introduced <- NA
#   policy.year3 = policy.year.original - 2
#   rs$policy.introduced = ifelse(rs$year > policy.year3 , 1,0)
#   rs$interaction = rs$policy.introduced*rs$treated
#   m_lead = lm(working ~ policy.introduced + treated +  
#                 interaction +  country + bs(year, degree=2) + age_cat,
#               data = rs)
#   cluster.se.minus2 = cluster.se(m = m_lead)
#   
#   ##Generate matrix of results and plots
#   results.mat = matrix(,3,2,); 
#   colnames(results.mat) = c('coefficient','se')
#   rownames(results.mat) = c('lead','lag','original')
#   results.mat2 = matrix(,1,3,)
#   
#   estimateplus2 = round(cluster.se.plus2 ,4)['interaction','Estimate']; 
#   seplus2 = round(cluster.se.plus2 ,4)['interaction','Std. Error']
#   results.mat[1,1] = estimateplus2
#   results.mat[1,2] = seplus2
#   results.mat2[,1] <- paste(estimateplus2 , '[',
#                             paste(round(estimateplus2  - qnorm(0.975)*seplus2,3),
#                                   round(estimateplus2  + qnorm(0.975)*seplus2,3),sep=';'),
#                             ']',sep="")
#   
#   estimateminus2 = round(cluster.se.minus2, 4)['interaction','Estimate']
#   seminus2 = round(cluster.se.minus2, 4)['interaction','Std. Error']
#   results.mat[2,1] = estimateminus2 
#   results.mat[2,2] = seminus2
#   results.mat2[,2] <- paste(estimateminus2 , '[',
#                             paste(round(estimateminus2  - qnorm(0.975)*seminus2,3),
#                                   round(estimateminus2  + qnorm(0.975)*seminus2,3),sep=';'),
#                             ']',sep="")
#   
#   original = round(cluster.se.original,4)['interaction','Estimate']
#   se.original = round(cluster.se.original,4)['interaction','Std. Error']
#   results.mat[3,1] = original
#   results.mat[3,2] = se.original
#   results.mat2[,3] <- paste(original , '[',
#                             paste(round(original  - qnorm(0.975)*se.original,3),
#                                   round(original  + qnorm(0.975)*se.original,3),sep=';'),
#                             ']',sep="")
#   
#   model.names = c('3.t-plus2', '1.t-minus2', '2.Original')
#   results = data.frame(results.mat)
#   
#   
#   ggplot(results , aes(x=model.names, y=coefficient)) + 
#     geom_errorbar(aes(ymin=coefficient-(1.96*se), ymax=coefficient+(1.96*se)), width=.2) +
#     geom_line(size = 2) + geom_hline(yintercept = 0, color = 'darkgrey') +
#     geom_point() + coord_flip() + 
#     ylim(c(-0.15,0.15)) + theme(axis.text = element_text(size = 14)) +
#     ggtitle(paste(type, 'Results', '(', gender, ')', sep=" ")) +
#     theme(plot.caption = element_text(hjust=0.3, vjust=-0.1,size=15,family="serif" ),
#           axis.title = element_text(face="bold" )) +
#     labs(y='Estimate',x='Model',  
#          caption = paste('Controls=', toString(controls),sep=" "))
#   
#   ##Export plots to lead lag folder in working directory
#   ggsave(filename = paste(type,gender,'SA', '.tiff', sep=""), 
#          path = paste(getwd(),'/lead_lag', sep=""))
#   
#   ##Explort all results to working directory
#   write.table(results, file=paste(type,gender,'SA','.txt',sep=""),sep=",")
#   
#   return(results.mat2)
#   
#   
# }