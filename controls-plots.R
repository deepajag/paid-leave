load('paid-leave-data.Rdata')

library('sjPlot')


###Loop the functions over all treatment-control groups###
policy.year =  c(1998,1998,2002,2002) 
type = c('Belgium','Belgium','Denmark','Denmark') 
gender = c('Female','Male','Female','Male')
controls = list(belg.female,belg.male, den.female,den.male) 

##Plot marginal effects
plots.list = vector('list',4)
for (i in 1:4) {
  ds.sample = ds
  rs = get.data(type = type[i], controls = controls[[i]], policy.year[i], gender=gender[i])
  x <- sample.description(rs,policy.year = policy.year[i])
  x$treated = ifelse(x$country %in% type[i], 1, 0)
  x$country = paste(x$country,", ",gender[i],sep="" )
  samples.list[[i]] <- x
}

rs = get.data(type = 'Belgium', controls = belg.female, policy.year = 1998, gender='Female')
rs1 = rs %>% filter(year <= policy.year)

m1 = lm(working ~ year*treated + age_cat, data=rs1)
summary(m1)
plot_model(m1, type="eff", terms=c("year", "treated"))
