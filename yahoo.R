d = read.table("120K_Instagram_images_data.txt",sep="\t",header=T)
str(d)
library(ggplot2)

berlin = d[d$city=='berlin',]
tokyo = d[d$city=='tokyo',]
bangkok = d[d$city=='bangkok',]
moscow = d[d$city=='moscow',]
sao = d[d$city=='sao_paulo',]
ny = d[d$city=='ny',]

rev(sort(table(berlin$username)))[1:5]
rev(sort(table(bangkok$username)))[1:5]
rev(sort(table(sao$username)))[1:5]
rev(sort(table(ny$username)))[1:5]
rev(sort(table(moscow$username)))[1:5]
rev(sort(table(tokyo$username)))[1:5]

table(d$city)
