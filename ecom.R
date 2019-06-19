read<-read_excel("C:/Users/AKHIL/Desktop/New folder/ECommerce.xlsx")
View(read)
library(readxl)
ls("package:readxl")


View(read)
plot(read$Sales,read$Profit,main='Sales Vs Profit',xlab='Sales',ylab='Profit')
plot(read$`Product Category`,read$`Shipping Cost`,main ="Product category vs Shipping cost",xlab="Product Category",ylab="Shipping Cost",xlim=c(0,5000),cex=(30))

hist(read$Sales)
hist(read$Sales,main='histogram for the Sales',ylab='frequency of Sales',xlab='Sales',col='green')


boxplot(read$Sales,main='Sales summary')
boxplot(read$Profit~read$Sales,main='Sales Vs Profit',xlab='Sales',ylab='Profit')
boxplot(read$Profit~read$`Shipping Cost`,main='Shipping Cost Vs Profit',xlab='Shipping Cost',ylab='Profit')


counts<-table(read$`Product Category`)
counts
barplot(counts,xlab= "Category",ylab="Products",main=" categories Vs No of Products",legend=rownames(counts),col=c('RED','BLUE','GREEN','Yellow','Violet'))

counts<-table(read$Region)
counts
barplot(counts,xlab= "Region",ylab=" No Of Orders",main=" Region Vs Number Of Orders")


library(dplyr)
z1<-filter(read,Segment=="Corporate")
View(z1)
prf1<-round(sum(z1$Profit))
prf1

z2<-filter(read,Segment=="Consumer")
View(z2)
prf2<-round(sum(z2$Profit))
prf2


z3<-filter(read,Segment=="Home Office")
View(z3)
prf3<-round(sum(z3$Profit))
prf3



zz<-c(prf1,prf2,prf3)
Segment<-c('Consumer','Home Office','Corporate')
read_Pie<-pie(zz,labels=Segment,main='Distribution')
read_Pie

sum_1<-sum(zz)
perc_1<-round((zz/sum_1)*100)
perc_1
perc_count_label<-paste(Segment,zz,'&',perc_1,'%')
perc_count_label
Quantity_Pie<-pie(zz,labels=perc_count_label,main='Quantity Distribution')




read1<-read[,c(4,8,9,10,11,12)]
View(read1)

read_matt<-as.matrix((read1))
read_matt 
heatmap(read_matt,col=heat.colors(256),Rowv=NA,COlv=NA,scale='column')

read2<-read[c(1:50),c(4,8,9,10,11,12)]
View(read2)
read_matt<-as.matrix((read2))
read_matt 
heatmap(read_matt,col=heat.colors(256),Rowv=NA,COlv=NA,scale='column')


library(dplyr)
s1<-filter(read,`Product Category`=="Auto & Accessories")
View(s1)
sa1<-round(sum(s1$Sales))
sa1

s2<-filter(read,`Product Category`=="Electronic")
View(s2)
sa2<-round(sum(s2$Sales))
sa2


s3<-filter(read,`Product Category`=="Fashion")
View(s3)
sa3<-round(sum(s3$Sales))
sa3

s4<-filter(read,`Product Category`=="Home & Furniture")
View(s4)
sa4<-round(sum(s4$Sales))
sa4



ss<-c(sa1,sa2,sa3,sa4)
cat<-c('Auto & Accessories','Electronic','Fashion','Home & Furniture')
read_Pie<-pie(ss,labels=cat,main='Distribution')
read_Pie

sum_1<-sum(ss)
perc_1<-round((ss/sum_1)*100)
perc_1
perc_count_label<-paste(cat,ss,'&',perc_1,'%')
perc_count_label
Quantity_Pie<-pie(ss,labels=perc_count_label,main='Quantity Distribution')


















