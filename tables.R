##xx
source('data_script.R')
source('modellin_script.R')
source('sensitivity analysis.R')

theme_set(theme_bw(base_size=10))
policy.year = c(1998,1998,2002,2002) ##N 2001 #D 2002 #B 1998
type = c('Belgium','Belgium','Denmark','Denmark') #'Denmark' '#Belgium
gender = c('Female','Male','Female','Male')
controls = list(belg.female,belg.male,den.female,den.male) 

#####Table 1 Sample description#########
samples.list = vector('list',4)
for (i in 1:4) {
ds.sample = ds
rs = get.data(type = type[i], controls = controls[[i]], policy.year[i], gender=gender[i])
samples.list[[i]] <- sample.description(rs,policy.year = policy.year[i])
}

table1 = do.call(rbind,samples.list)
write.csv(table1,file="table1.txt")

#######Figures 1a and 1b####### - DONE IN STATA new-controls-do-file.do
policy.year = 2002
type='Denmark'
gender='Male'
random.controls = den.male
rs = get.data.controls(type=type,policy.year = policy.year, gender=gender,controls=random.controls)
rs$country = droplevels.factor(rs$country)
mod1 = lm(working ~ country*year + age_cat, data = rs)  
mm = model.matrix(mod1)[,]
rs$work.predict1 = mm %*% mod1$coefficients
ggplot(rs, aes(year, work.predict1, group=age_cat,color = factor(treated))) + 
    geom_line(size=1.5)
  stat_smooth(method="gam", formula=y~s(x,k=8, bs="cs"),se=FALSE,size=1.5) +
  labs(x='Year',y='Mean Proportion Working',
       caption = paste('Controls=', toString(random.controls),sep=" "),
       title = paste('Treated Group:',type,gender,sep=" ")) +
  scale_color_grey(name = 'Treated \n (Passed Policy)') +
  theme(plot.caption = element_text(hjust=0.3, vjust=-0.1,size=11,family="serif" ),
        axis.title = element_text(face="bold" ))

##Table 3 Results


