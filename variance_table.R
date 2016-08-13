data = read.csv('tags_by_features_data.csv')
data.table = read.csv('tags_by_features.csv')

l = unlist(list(as.character(data.table$tag)))


##################
## variances #####
##################

norm = function(X){
  (X - min(X)) / diff(range(X))
}
  
df = data.frame()

n = length(l)
for (i in 1:n) {
  tag = l[i]
  tmp = subset(data, data[tag] >= 0.8)
  tmp = tmp[,c(4:57)]
  tmp = apply(tmp,2,norm)
  row = apply(tmp,2,var)
  df = rbind(df,row)
}

colnames(df) = colnames(data[,c(4:57)])
df$tag = l
variances = df


################
## r-squareds ##
################


df = data.frame()
features = colnames(data[,c(4:57)])

m = length(features) 

for (i in 1:n) {
  tag = l[i]
  tmp = subset(data, data[tag] >= 0.8)
  tmp[,c(4:57)] = apply(tmp[,c(4:57)],2,norm)
  row = NULL
  for (j in 1:m) {
    feature = features[j]
    f <- as.formula(paste(feature, "~ IG_city_name"))
    R2 = summary(do.call("lm", args = list(f,data=as.name("tmp"))))$r.squared
    row = append(row,R2)
  }
  df = rbind(df,row)
}

colnames(df) = colnames(data[,c(4:57)])
df$tag = l
rsq = df

library(reshape2)
library(ggplot2)

v.m = melt(variances,id.vars='tag')
r.m = melt(rsq,id.vars='tag')

colnames(v.m) = c('tag','feature','value')
colnames(r.m) = c('tag','feature','value')

base.plot = ggplot(v.m,aes(feature,tag,color=value)) + geom_point(data=r.m,aes(size=value))
base.labs = base.plot +
  labs(title='For Photos With Subject Y, How Much Variance in Feature X and How Much of the Variance Accounted for By City?')

base.theme = theme(panel.background = element_rect(fill = "white"),
                   panel.grid.major = element_line(color = "gray75", size = 0.25),
                   panel.grid.minor = element_line(color = "gray75", size = 0.25),
                   plot.background = element_rect(fill = "white"),
                   axis.text.x = element_text(color='gray50', size = 10, angle = 50, hjust = 1.1),
                   axis.text.y = element_text(color='gray50', size = 10),
                   text = element_text(face='plain', color='black'),
                   legend.key = element_rect(fill = 'white'),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   plot.title = element_text(face="plain", vjust=2, size = 12),
                   #axis.ticks = element_blank()
                   legend.position = 'none')

base = base.labs + base.theme
base


write.csv(variances,"./var_table.csv")
write.csv(rsq,"./var_table_rsq.csv")
