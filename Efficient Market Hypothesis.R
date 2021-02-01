#导入数据
s1<-read.csv("stock1.csv")
s2<-read.csv("stock2.csv")
s3<-read.csv("stock3.csv")
s<-rbind(s1,s2,s3)
i<-read.csv("index.csv")
d1<-read.csv("disclosure_time1.csv")
d2<-read.csv("disclosure_time2.csv")
d<-rbind(d1,d2)

#处理disclosure time数据
d<-d[-1,]
d<-d[,-c(2,3,4,5,6,7)]
names(d)<-c("id","date")
#d$date<-as.Date(d$date)
install.packages(c("sqldf","gsubfn", "proto", "RSQLite"))
library(RSQLite)
library(gsubfn)
library(proto)
library(sqldf)
da<-sqldf("select * from d where date between '2016-01-01' and '2018-12-31'") #去除2016-2018以外的数据
ID<-sqldf("select id from (select *,count(date) as n from da group by id) where n=12") #选出2016-2018年间有12次披露的股票
pd<-sqldf("select da.id, da.date from da inner join ID on da.id = ID.id") #processed disclosure time 选出2016-2018年间有12次披露的股票和相对应实际披露的时间
pd1<-sqldf("select *,row_number() over(partition by id order by date) as times from pd") 
write.csv(pd1,file="processed_disclosure_time.csv",col.names=T,row.names=F)

#合并股票收盘价和收盘指数数据
names(s)<-c("id","date","sclose")
names(i)<-c("index","date","iclose")
si<-sqldf("select s.id,s.date,s.sclose,i.iclose from s inner join i on s.date=i.date order by s.id")

#合并披露次数和股票大盘数据
a<-sqldf("select si.id,si.date,si.sclose,si.iclose,pd1.times from si left join pd1 on si.date=pd1.date and si.id=pd1.id order by si.id")
write.csv(a,file="processed_return.csv",col.names=T,row.names=F)

install.packages("data.table")
library(data.table)
install.packages("magrittr")
library(magrittr)
#获得某次披露每只股票（-5，1）的累积涨幅
minus_five_to_one<-function(ns,data){
	begin<-ns-5
	end<-ns+1
	sprice<-as.matrix(data$sclose)
	iprice<-as.matrix(data$iclose)
	gain_minus_five_to_one<-((sprice[end]-sprice[begin])/sprice[begin]-(iprice[end]-iprice[begin])/iprice[begin])*100
	cbind(ns,gain_minus_five_to_one)
}

#根据涨幅大小分为10组，获得每组股票id
divided<-function(ns,gain){
	c<-cut(gain,quantile(gain,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)),label=c(1,2,3,4,5,6,7,8,9,10))
	combi<-cbind(ns,c)
	q<-rep(0,10)
	for (i in 1:10) {
		q[i]<-list(combi[,1][combi[,2]==i])		
	}
	q
}

#算出每组(2,6)平均超额收益
two_to_six<-function(ns,data) {
	begin<-ns+2
	end<-ns+6
	sprice<-as.matrix(data$sclose)
	iprice<-as.matrix(data$iclose)
	abnormal_g<-((sprice[end]-sprice[begin])/sprice[begin]-(iprice[end]-iprice[begin])/iprice[begin])*100
	avg<-mean(abnormal_g,na.rm=T)
	avg
}

#算出每组(2,11)平均超额收益
two_to_eleven<-function(ns,data) {
	begin<-ns+2
	end<-ns+11
	sprice<-as.matrix(data$sclose)
	iprice<-as.matrix(data$iclose)
	abnormal_g<-((sprice[end]-sprice[begin])/sprice[begin]-(iprice[end]-iprice[begin])/iprice[begin])*100
	avg<-mean(abnormal_g,na.rm=T)
	avg
}
#算出每组(2,16)平均超额收益
two_to_sixteen<-function(ns,data) {
	begin<-ns+2
	end<-ns+16
	sprice<-as.matrix(data$sclose)
	iprice<-as.matrix(data$iclose)
	abnormal_g<-((sprice[end]-sprice[begin])/sprice[begin]-(iprice[end]-iprice[begin])/iprice[begin])*100
	avg<-mean(abnormal_g,na.rm=T)
	avg
}
#算出每组(2,21)平均超额收益
two_to_twentyone<-function(ns,data) {
	begin<-ns+2
	end<-ns+21
	sprice<-as.matrix(data$sclose)
	iprice<-as.matrix(data$iclose)
	abnormal_g<-((sprice[end]-sprice[begin])/sprice[begin]-(iprice[end]-iprice[begin])/iprice[begin])*100
	avg<-mean(abnormal_g,na.rm=T)
	avg
}

#得出每次披露10组在(2,6)(2,11)(2,16)(2,21)分别的超额收益
calculate<-function(ns,data){
	l<-minus_five_to_one(ns,data)
	q<-divided(l[,1],l[,2])
	abnormal6<-rep(0,10)
	abnormal11<-rep(0,10)
	abnormal16<-rep(0,10)
	abnormal21<-rep(0,10)
	for (i in 1:10) {
		abnormal6[i] <-two_to_six(q[[i]],data)
		abnormal11[i] <-two_to_eleven(q[[i]],data)
		abnormal16[i] <-two_to_sixteen(q[[i]],data)
		abnormal21[i] <-two_to_twentyone(q[[i]],data)
	}
	list(abnormal6,abnormal11,abnormal16,abnormal21)
}

abnormal<-rep(0,12)
a$sclose<-100*a$sclose
a$iclose<-100*a$iclose
#a1<-sqldf("select *,row_number over(order by id) as ns from a")
for (j in 1:12) {
	ns<-which(a$times==j)
	abnormal[j]<-list(calculate(ns,a))
}
write.csv(abnormal,"abnormal_return.csv")
write.csv(abnormal,"abnormal1.csv")
abnormal<-read.csv("abnormal2.csv")
x<-0:11
abnormal1<-t(abnormal[,4*x+2])
abnormal2<-t(abnormal[,4*x+3])
abnormal3<-t(abnormal[,4*x+4])
abnormal4<-t(abnormal[,4*x+5])
write.csv(abnormal1,"(2,6).csv")
write.csv(abnormal2,"(2,11).csv")
write.csv(abnormal3,"(2,16).csv")
write.csv(abnormal4,"(2,21).csv")

s1<-rep(0,12)
for (i in c(1,10)) {
	s1[i]<-var(abnormal1[i,])
}
sum(s1)

s2<-rep(0,12)
for (i in c(1,10)) {
	s2[i]<-var(abnormal2[i,])
}
sum(s2)

s3<-rep(0,12)
for (i in c(1,10)) {
	s3[i]<-var(abnormal3[i,])
}
sum(s3)

s4<-rep(0,12)
for (i in c(1,10)) {
	s4[i]<-var(abnormal4[i,])
}
sum(s4)
for (i in 1:12) {
	s1[i]<-var(abnormal1[i,])
}
sum(s1)

s2<-rep(0,12)
for (i in 1:12) {
	s2[i]<-var(abnormal2[i,])
}
sum(s2)

s3<-rep(0,12)
for (i in 1:12) {
	s3[i]<-var(abnormal3[i,])
}
sum(s3)

s4<-rep(0,12)
for (i in 1:12) {
	s4[i]<-var(abnormal4[i,])
}
sum(s4)
