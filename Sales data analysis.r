rm(list=ls())

data_1a = read.csv('assignment4.1a.csv',header=TRUE)
data_1b = read.csv('assignment4.1b.csv',header=TRUE)
data_pd = read.csv('PromotionDates.csv',header=TRUE) ##I have changed fifth and sixth promo dates' types in excel.

head(data_1a)
head(data_1b)
head(data_pd)

#substituting primary data
subs1a=data_1a

nr1=nrow(data_1a)
nr2=nrow(data_1b)

days2a=data.frame(as.Date(sort(unique(subs1a[,1])),"%Y-%m-%d"))
days_back=days2a-7
# days3a=data.frame(c(days2a,days_back))
# days3a[,3]=0
# colnames(days3a) <- c("Date","a_week_before","sum_of_sales")
# beneficial_days_back=data.frame(days_back[8:212,])
# colnames(beneficial_days_back) <- c("Date")
colnames(days2a) <- c("Date")
number_of_days1a=length(unique(subs1a[,1]))
# there are sales in every days between 01-01-2015/31-07-2015, because there are 212 days 
# between these days and number_of_days1a = 212


#Creating string array of promo dates
promo_date=c()
for(i in 1:6){
  for(j in 2:3){
    promo_date=c(promo_date,toString(data_pd[i,j]))
  }
}


#matching dates into date elements
DATE1 <- as.Date(promo_date[1],"%d/%m/%Y")
DATE2 <- as.Date(promo_date[2],"%d/%m/%Y")
DATE3 <- as.Date(promo_date[3],"%d/%m/%Y")
DATE4 <- as.Date(promo_date[4],"%d/%m/%Y")
DATE5 <- as.Date(promo_date[5],"%d/%m/%Y")
DATE6 <- as.Date(promo_date[6],"%d/%m/%Y")
DATE7 <- as.Date(promo_date[7],"%d/%m/%Y")
DATE8 <- as.Date(promo_date[8],"%d/%m/%Y")
DATE9 <- as.Date(promo_date[9],"%d/%m/%Y")
DATE10 <- as.Date(promo_date[10],"%d/%m/%Y")
DATE11 <- as.Date(promo_date[11],"%d/%m/%Y")
DATE12 <- as.Date(promo_date[12],"%d/%m/%Y")

#using date elements, the data with promo are marked
indexno1=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE1 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE2)))
indexno2=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE3 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE4)))
indexno3=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE5 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE6)))
indexno4=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE7 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE8)))
indexno5=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE9 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE10)))
indexno6=as.numeric(rownames(subset(subs1a, as.Date(subs1a$Date,"%Y-%m-%d")>= DATE11 & as.Date(subs1a$Date,"%Y-%m-%d")<= DATE12)))


#putting all marked rows into an array
indices=c(indexno1,indexno2,indexno3,indexno4,indexno5,indexno6)

#placing the info of promo into data and creating promo and nonpromo data
subs1a[,5]=0
subs1a[indices,5]=1
colnames(subs1a) <- c("Date", "StoreCode", "ProductCode","SalesQuantity","PromoInfo")
head(subs1a)
nonpromo=subset(subs1a,subs1a[,5]<1)
head(nonpromo)
promo=subset(subs1a,subs1a[,5]>0)
head(promo)

summary(nonpromo)
summary(promo)

#####Determining the proportion of sales of promos to nonpromos by PRODUCTS#####
mean_product_nonpromo=aggregate(SalesQuantity~ProductCode, data=nonpromo, FUN=function(x) c(mean=mean(x)))
r_mean_product_nonpromo=round(mean_product_nonpromo[,1:2],2)
head(r_mean_product_nonpromo)
rn_np=nrow(r_mean_product_nonpromo)
# head(r_mean_product_nonpromo)
# tail(r_mean_product_nonpromo)
# summary(r_mean_product_nonpromo)

mean_product_promo=aggregate(SalesQuantity~ProductCode, data=promo, FUN=function(x) c(mean=mean(x)))
r_mean_product_promo=round(mean_product_promo,2)
# head(r_mean_product_promo)
# tail(r_mean_product_promo)
# summary(r_mean_product_promo)

RateOfSalesbyProd=merge(r_mean_product_nonpromo,r_mean_product_promo,by="ProductCode")
RateOfSalesbyProd[,4]=round(RateOfSalesbyProd[,3]/RateOfSalesbyProd[,2],2)
colnames(RateOfSalesbyProd) <- c("ProductCode", "SalesQuantity.nonpromo", "SalesQuantity.promo", "RateOfSales")
head(RateOfSalesbyProd)

sum_product_nonpromo=aggregate(SalesQuantity~ProductCode, data=nonpromo, FUN=function(x) c(sum=sum(x)))
fin_RateOfSalesbyProd=merge(RateOfSalesbyProd,sum_product_nonpromo,by="ProductCode")
head(fin_RateOfSalesbyProd)





# Determining the proportion of sales of promos to nonpromos by STORES with respect to nonpromo data
mean_stores_nonpromo=aggregate(SalesQuantity~StoreCode, data=nonpromo, FUN=function(x) c(mean=mean(x)))
r_mean_stores_nonpromo=round(mean_stores_nonpromo,2)
rn_np_str=nrow(r_mean_stores_nonpromo)
# head(r_mean_stores_nonpromo)
# tail(r_mean_stores_nonpromo)
# summary(r_mean_stores_nonpromo)

mean_stores_promo=aggregate(SalesQuantity~StoreCode, data=promo, FUN=function(x) c(mean=mean(x)))
r_mean_stores_promo=round(mean_stores_promo,2)
# head(r_mean_stores_promo)
# tail(r_mean_stores_promo)
# summary(r_mean_stores_promo)

RateOfSalesbyStores=merge(r_mean_stores_nonpromo,r_mean_stores_promo,by="StoreCode")
RateOfSalesbyStores[,4]=round(RateOfSalesbyStores[,3]/RateOfSalesbyStores[,2],2)
colnames(RateOfSalesbyStores) <- c("StoreCode", "SalesQuantity.nonpromo", "SalesQuantity.promo", "RateOfSales")
head(RateOfSalesbyStores)

sum_store_nonpromo=aggregate(SalesQuantity~StoreCode, data=nonpromo, FUN=function(x) c(sum=sum(x)))
fin_RateOfSalesbyStores=merge(RateOfSalesbyStores,sum_store_nonpromo,by="StoreCode")
head(fin_RateOfSalesbyProd)





# Examining data in order to decide the types of products
hist(RateOfSalesbyProd$SalesQuantity.nonpromo, breaks=20,	main="Histogram with breaks=20",xlab="Means of items")
# We observe skewness, so I will apply log() transformation.
aa=RateOfSalesbyProd$SalesQuantity.nonpromo
head(aa)
max(aa)
min(aa)
for(i in 1:length(aa)){
  if(aa[i]>=-min(aa)){
    aa[i]=aa[i]-min(aa)+0.2
  }
  else {aa[i]=aa[i]-min(aa)+0.2}
}
min(aa)
max(aa)
# After applying log() transformation
plot(density(log(aa)),main="log() plot")

# The control whether it has changed to normal or not
install.packages("nortest")
library(nortest)
ad.test(log(aa))
# p_value is too small. It means the normalization with log() transformation did not happen. 
# so, I used different method for detection of fast/medium and slow products

# I have used change point detection methodology, to decide the types of items
qnt_prod=  quantile(RateOfSalesbyProd$SalesQuantity.nonpromo, probs = seq(0, 1, 0.05))
qnt_prod
summary(RateOfSalesbyProd)
plot(RateOfSalesbyProd$SalesQuantity.nonpromo)
arr_prod=data.frame()
for(i in 1:19){
  arr_prod[i,1]=round(qnt_prod[i+2]/qnt_prod[i+1],2)
}
arr_prod
first_breaking_point=qnt_prod[11][[1]]
second_breaking_point=qnt_prod[18][[1]]
# As we can see step 10 and 17 are breaking points. Therefore, I have determined the data smaller than 
# 10th point as slow, bigger than 17th as fast and other are medium.

# a.	What are your criteria for separating Fast, Medium and Slow items? Why?
#Determining the type of PRODUCTS with respect to nonpromo data
type_of_prod=data.frame(r_mean_product_nonpromo[,1])
head(type_of_prod)

for(i in 1:rn_np){
  if(r_mean_product_nonpromo[i,2]<=first_breaking_point){
    type_of_prod[i,2]="slow"
  }
  else if(r_mean_product_nonpromo[i,2]>=second_breaking_point){
    type_of_prod[i,2]="fast"
  }
  else
    type_of_prod[i,2]="medium"
}
colnames(type_of_prod) <- c("ProductCode", "TypeOfProduct")
# head(type_of_prod)
table_of_types=table(type_of_prod[2])
# shows the number of products in each type
table_of_types

fnl_prod <- merge(RateOfSalesbyProd, type_of_prod,by="ProductCode")
colnames(fnl_prod) <- c( "ProductCode", "SalesQuantity.nonpromo", "SalesQuantity.promo", "RateOfSales_product", "TypeOfProduct")
head(fnl_prod)


# Examining data in order to decide the types of stores
hist(RateOfSalesbyStores$SalesQuantity.nonpromo, breaks=20,	main="Histogram with breaks=20", xlab="Means of stores")
plot(density(RateOfSalesbyStores$SalesQuantity.nonpromo))

# We observe lighter skewness than products' skewness, again I will apply log() transformation.
bb=RateOfSalesbyStores$SalesQuantity.nonpromo
head(bb)
max(bb)
min(bb)
# After applying log() transformation
plot(density(log(bb)), main="log() plot of stores")

# The control whether it has changed to normal or not
# install.packages("nortest")
# library(nortest)
ad.test(log(bb))
ad.test(bb)
# p_value increases important amount, but it is still smaller than 0.05. 
# It means the normalization with log() transformation did not happen. 
# so, I tried different method for detection of fast/medium and slow products

# I have used change point detection methodology, to decide the types of items
qnt_store=  quantile(RateOfSalesbyStores$SalesQuantity.nonpromo, probs = seq(0, 1, 0.05))
qnt_store
head(RateOfSalesbyStores)
summary(RateOfSalesbyStores)
plot(RateOfSalesbyStores$SalesQuantity.nonpromo)
arr_store=data.frame()
for(i in 1:19){
  arr_store[i,1]=round(qnt_store[i+2]/qnt_store[i+1],2)
}
arr_store
# As we can see there does not seem any breaking points, so I have applied another method.
# In new method, I have used control chart with the interval [stores_mean-stddev_stores_mean,stores_mean+stddev_stores_mean]
# If the mean of a store is bigger than "stores_mean+stddev_stores_mean", then it is fast.
# If the mean of a store is smaller than "stores_mean-stddev_stores_mean", then it is slow.
# Medium, otherwise.

# b.	What are your criteria for separating Fast, Medium and Slow Stores? Why?
#####Determining the type of STORES with respect to nonpromo data
type_of_stores=data.frame(r_mean_stores_nonpromo[,1])
stddev_stores_mean=sd(RateOfSalesbyStores$SalesQuantity.nonpromo)
stores_mean=mean(RateOfSalesbyStores$SalesQuantity.nonpromo)

for(i in 1:rn_np_str){
  if(r_mean_stores_nonpromo[i,2]<stores_mean-stddev_stores_mean){
    type_of_stores[i,2]="slow"
  }
  else if(r_mean_stores_nonpromo[i,2]>stores_mean+stddev_stores_mean){
    type_of_stores[i,2]="fast"
  }
  else
    type_of_stores[i,2]="medium"
}
colnames(type_of_stores) <- c("StoreCode","TypeOfStores")
# head(type_of_stores)
table_of_types_stores=table(type_of_stores[2])
# shows the number of stores in each type
table_of_types_stores


fnl_stores <- merge(RateOfSalesbyStores, type_of_stores,by="StoreCode")
colnames(fnl_stores) <- c( "StoreCode", "SalesQuantity.nonpromo", "SalesQuantity.promo", "RateOfSales_stores", "TypeOfStores")
head(fnl_stores)


# c.	Which items experienced the biggest sale increase during promotions?
ndx <- order(fnl_prod[,4])[1:nrow(fnl_prod)]
aaa=fnl_prod[ndx[291:300],]
biggest_increase_products=aaa[,1]
# products 68 269 271  61 270 229  22  55 291 192 231
# 9 slow, 1 medium

# d.	Are there stores that have higher promotion reaction?
ndx <- order(fnl_stores[,4])[1:nrow(fnl_stores)]
length(ndx)
aaa=fnl_prod[ndx[325:336],]
aaa1=aaa[complete.cases(aaa),]
biggest_increase_stores=aaa1[,1]
# stores 269 271  61 270 229  22  55 291 192 231
# 6 medium, 3 slow, 1 fast

subs2a <- merge(subs1a, fnl_prod[,c(1,5)],by="ProductCode")
subs3a <- merge(subs2a, fnl_stores[,c(1,5)],by="StoreCode")

fnl_product_fast=subset(subs3a,subs3a[,6]=="fast")
fnl_product_medium=subset(subs3a,subs3a[,6]=="medium")
fnl_product_slow=subset(subs3a,subs3a[,6]=="slow")




fnl_stores_fast=subset(subs3a,subs3a[,7]=="fast")
fnl_stores_medium=subset(subs3a,subs3a[,7]=="medium")
fnl_stores_slow=subset(subs3a,subs3a[,7]=="slow")

a=subs1a[,c(1,5)]
b=unique(a)

#fast product's sums by dates
date_fast_product_nonpromo=aggregate(SalesQuantity~Date, data=fnl_product_fast, FUN=function(x) c(sum=sum(x)))
r_date_fast_product_nonpromo=date_fast_product_nonpromo
r_date_fast_product_nonpromo[,2]=round(date_fast_product_nonpromo[,2],2)
r_date_fast_product_nonpromo <- merge(r_date_fast_product_nonpromo, b,by="Date")
# head(r_date_fast_product_nonpromo)
# nrow(r_date_fast_product_nonpromo)
new=r_date_fast_product_nonpromo
new[8:212,4]=0
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.fast_product=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.fast_product)
change_of_sales_fast_product=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_fast_product=change_of_sales_fast_product[1,2]/change_of_sales_fast_product[2,2]
rate_of_change_of_sales_fast_product #  4.069437


#medium product's sums by dates
date_medium_product_nonpromo=aggregate(SalesQuantity~Date, data=fnl_product_medium, FUN=function(x) c(sum=sum(x)))
r_date_medium_product_nonpromo=date_medium_product_nonpromo
r_date_medium_product_nonpromo[,2]=round(date_medium_product_nonpromo[,2],2)
r_date_medium_product_nonpromo <- merge(r_date_medium_product_nonpromo, b,by="Date")
# head(r_date_medium_product_nonpromo)
new=r_date_medium_product_nonpromo
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.medium_product=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.medium_product)
change_of_sales_medium_product=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_medium_product=change_of_sales_medium_product[1,2]/change_of_sales_medium_product[2,2]
rate_of_change_of_sales_medium_product #  4.359555


#slow product's sums by dates
date_slow_product_nonpromo=aggregate(SalesQuantity~Date, data=fnl_product_slow, FUN=function(x) c(sum=sum(x)))
r_date_slow_product_nonpromo=date_slow_product_nonpromo
r_date_slow_product_nonpromo[,2]=round(date_slow_product_nonpromo[,2],2)
r_date_slow_product_nonpromo <- merge(r_date_slow_product_nonpromo, b,by="Date")
# head(r_date_slow_product_nonpromo)
new=r_date_slow_product_nonpromo
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.slow_product=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.slow_product)
change_of_sales_slow_product=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_slow_product=change_of_sales_slow_product[1,2]/change_of_sales_slow_product[2,2]
rate_of_change_of_sales_slow_product  #  4.598088

#f.	Is there any significant difference between promotion impacts of the Fast versus Slow items?
a1=subset(fnl_product_fast,fnl_product_fast[,5]==1)
a2=subset(fnl_product_fast,fnl_product_fast[,5]==0)
code_fast_product_promo=aggregate(SalesQuantity~ProductCode, data=a1, FUN=function(x) c(mean=mean(x)))
code_fast_product_nonpromo=aggregate(SalesQuantity~ProductCode, data=a2, FUN=function(x) c(mean=mean(x)))
a12=merge(code_fast_product_promo,code_fast_product_nonpromo,by="ProductCode")
a12[,4]=a12[,2]/a12[,3]


a3=subset(fnl_product_slow,fnl_product_slow[,5]==1)
a4=subset(fnl_product_slow,fnl_product_slow[,5]==0)
code_fast_product_promo=aggregate(SalesQuantity~ProductCode, data=a3, FUN=function(x) c(mean=mean(x)))
code_fast_product_nonpromo=aggregate(SalesQuantity~ProductCode, data=a4, FUN=function(x) c(mean=mean(x)))
a34=merge(code_fast_product_promo,code_fast_product_nonpromo,by="ProductCode")
a34[,4]=a34[,2]/a34[,3]

t.test(a12[,4],a34[,4])
# p-value = 0.6446 true difference in means is equal to 0. Therefore, there is not any significant difference
# between promotion impacts of the Fast versus Slow items


#fast store's sums by dates
date_fast_store_nonpromo=aggregate(SalesQuantity~Date, data=fnl_stores_fast, FUN=function(x) c(sum=sum(x)))
r_date_fast_store_nonpromo=date_fast_store_nonpromo
r_date_fast_store_nonpromo[,2]=round(date_fast_store_nonpromo[,2],2)
r_date_fast_store_nonpromo <- merge(r_date_fast_store_nonpromo, b,by="Date")
# head(r_date_fast_store_nonpromo)
new=r_date_fast_store_nonpromo
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.fast_product=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.fast_product)
change_of_sales_fast_store=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_fast_store=change_of_sales_fast_store[1,2]/change_of_sales_fast_store[2,2]
rate_of_change_of_sales_fast_store # 4.157513

#medium store's sums by dates
date_medium_store_nonpromo=aggregate(SalesQuantity~Date, data=fnl_stores_medium, FUN=function(x) c(sum=sum(x)))
r_date_medium_store_nonpromo=date_medium_store_nonpromo
r_date_medium_store_nonpromo[,2]=round(date_medium_store_nonpromo[,2],2)
r_date_medium_store_nonpromo <- merge(r_date_medium_store_nonpromo, b,by="Date")
# head(r_date_medium_store_nonpromo)
new=r_date_medium_store_nonpromo
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.medium_product=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.medium_product)
change_of_sales_medium_store=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_medium_store=change_of_sales_medium_store[1,2]/change_of_sales_medium_store[2,2]
rate_of_change_of_sales_medium_store # 4.202121

#slow store's sums by dates
date_slow_store_nonpromo=aggregate(SalesQuantity~Date, data=fnl_stores_slow, FUN=function(x) c(sum=sum(x)))
r_date_slow_store_nonpromo=date_slow_store_nonpromo
r_date_slow_store_nonpromo[,2]=round(date_slow_store_nonpromo[,2],2)
r_date_slow_store_nonpromo <- merge(r_date_slow_store_nonpromo, b,by="Date")
# head(r_date_slow_store_nonpromo)
new=r_date_slow_store_nonpromo
new[8:212,4]=new[1:205,2]
colnames(new) <- c("Date", "SalesQuantity", "PromoInfo","Sales_a_week_before")
res.slow_store=lm(SalesQuantity~PromoInfo+Sales_a_week_before,new)
summary(res.slow_store)
change_of_sales_slow_store=aggregate(SalesQuantity~PromoInfo, data=new, FUN=function(x) c(sum=sum(x)))
rate_of_change_of_sales_slow_store=change_of_sales_slow_store[1,2]/change_of_sales_slow_store[2,2]
rate_of_change_of_sales_slow_store # 4.323564


# g.	Is there any significant difference between promotion impacts of the Fast versus Slow stores?
a5=subset(fnl_stores_fast,fnl_stores_fast[,5]==1)
a6=subset(fnl_stores_fast,fnl_stores_fast[,5]==0)
code_fast_product_promo=aggregate(SalesQuantity~StoreCode, data=a5, FUN=function(x) c(mean=mean(x)))
code_fast_product_nonpromo=aggregate(SalesQuantity~StoreCode, data=a6, FUN=function(x) c(mean=mean(x)))
a56=merge(code_fast_product_promo,code_fast_product_nonpromo,by="StoreCode")
a56[,4]=a56[,2]/a56[,3]


a7=subset(fnl_stores_slow,fnl_stores_slow[,5]==1)
a8=subset(fnl_stores_slow,fnl_stores_slow[,5]==0)
code_fast_product_promo=aggregate(SalesQuantity~StoreCode, data=a7, FUN=function(x) c(mean=mean(x)))
code_fast_product_nonpromo=aggregate(SalesQuantity~StoreCode, data=a8, FUN=function(x) c(mean=mean(x)))
a78=merge(code_fast_product_promo,code_fast_product_nonpromo,by="StoreCode")
a78[,4]=a78[,2]/a78[,3]

t.test(a56[,4],a78[,4])
# p-value = 0.01301 true difference in means is not equal to 0. Therefore, there is significant difference
# between promotion impacts of the Fast versus Slow stores




# sum(subs1a[,4])
# [1] 4 209 330 
# stddev_prod=sd(fin_RateOfSalesbyProd[,5])
# stddev_prod
# [1] 27271.41
# sum(nonpromo[,4])    
# [1] 3 399 463
# length(unique(nonpromo[,3]))    
# [1] 316
# length(unique(nonpromo[,2]))    
# [1] 340
# sum(promo[,4])   
# length(unique(promo[,3]))    
#  length(unique(promo[,2]))    
# [1] 336
