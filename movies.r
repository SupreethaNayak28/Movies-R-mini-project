pdata=read.csv("PF.csv")
print(pdata)
str(pdata)
plot(pdata)
library(ggplot2)
ggplot(pdata,aes(Revenue.INR.,Budget.INR))+
  geom_point()
ggplot(pdata,aes(Revenue.INR.,Budget.INR))+
  geom_line()
ggplot(pdata,aes(Revenue.INR.,Budget.INR))+
geom_bar(stat="identity")+
  labs(title="Movies")

pdata
head(pdata)
pdata$Genre
?table
table(pdata$Genre)
barplot(xtabs(~pdata$Genre),main="genres",xlab="Genre",col=c("red","orange","yellow","green","blue","purple","pink","maroon","violet","pink","darkgreen","darkblue","maroon","yellow"))
pie(xtabs(~pdata$Genre),main="genres",xlab="Genre",col=c("red","orange","yellow","green","blue","purple","pink","maroon","violet","pink","darkgreen","darkblue","maroon","yellow"))

quantile(pdata$Units)
quantile(pdata$Units,probs=seq(from=0.1,to=1.0,by=0.1))

Remake<-pdata$Whether.Remake
Franchise=pdata$Whether.Franchise
print(Remake)
print(Franchise)

install.packages("ggplot2")

library(ggplot2)
ggplot()+theme_bw()
geom_bar(aes(x="",y=Franchise,fill=Remake),stat="identity",color="red")+coord_polar("y",start=0)
 
head(pdata)
plot(pdata$Revenue.INR.,pdata$Budget.INR.)

print(pdata)

library(ggplot2)
library(dplyr)
pdata$Genre=factor(as.character(pdata$Genre))
data
data=pdata %>% count(Genre) %>%
  mutate(percentage =n/sum(n)*100,
         pos_pie=round(cumsum(percentage)-0.5*percentage,2))
head(data)

ggplot()+geom_col(data=data,mapping=aes(x=,y=percentage))
ggplot(data=data)+
  geom_col(mapping = aes(x="",y=percentage,fill=Genre))+
  coord_polar(theta="y")+
  geom_text(aes(x="",y=pos_pie,label=scales::percent(percentage,scale=1)))

data=pdata %>% count(Genre) %>%
  arrange(desc(Genre)) %>%
  mutate(percentage =n/sum(n)*100,
         pos_pie=round(cumsum(percentage)-0.45*percentage,100))
library(ggplot2)
ggplot(data=data)+
  geom_col(mapping = aes(x="",y=percentage,fill=Genre))+
  coord_polar(theta="y")+
  geom_text(aes(x="",y=pos_pie,label=scales::percent(percentage,scale=1)),cex=1.8)

count=table(pdata$Revenue.INR.)
barplot(count)
barplot(count,main="Revenue vs Budget",xlab="Revenue",ylab="Budget",legend=rownnames(count),col=c("red","pink","blue"))

percents <- round(pdata/sum(pdata)*100)

# Add percentage values as legends
legend(x = 1, y = 1, legend = paste(percents, "%"), cex =0.8,bty="n")

library(plotrix)

x=data.frame(pdata$Number.of.Screens)
print(x)

labels=c(2,10,30,550)
pie(x,labels,main="No. of screens")

library(plotly)
values <- c(Genre)
print(values)
labels <- c("A", "B", "C", "D")
plot_ly(labels = labels, values = values, type = "pie", hole = 0.4, sort = FALSE, direction = "clockwise") %>%
  layout(title = "3D Pie Chart", font = list(size=12))

library(ggplot2)
pdata <- data.frame(pdata$Genre, labels=c("thriller","suspense"))
ggplot(data, aes(x = "", y = values, fill = labels)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  ggtitle("3DPieChart")

pdata$Whether.Franchise=factor(as.character(pdata$Whether.Franchise))
data

data=pdata %>% count(Whether.Franchise) %>%
  arrange(desc(Whether.Franchise))%>%
  mutate(percentage =n/sum(n)*100,
         pos_pie=round(cumsum(percentage)-0.45*percentage,100))
library(ggplot2)
ggplot(data=data)+
  geom_col(mapping = aes(x="",y=percentage,fill=Whether.Franchise)) +
  coord_polar(theta="y")+
  geom_text(aes(x="",y=pos_pie,label=scales::percent(percentage,scale=1)),cex=1.8)

pdata
head(pdata)
pdata$Whether.Franchise
?table
table(pdata$Whether.Remake)

pie(xtabs(~pdata$Whether.Remake),main="Remake vs Franchise  ",xlab="",col=c("green","pink"))
  