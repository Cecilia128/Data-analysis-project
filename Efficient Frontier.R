#4_(3)
library(data.table)
library(scales)
library(ggplot2)
df<-data.table(read.csv("4_3.csv"))
names(df)<-c("id","date","mon_r")
x<-df$mon_r[df$id==1]
y<-df$mon_r[df$id==89]

# calculate expected returns for the two assets
er_x <- mean(x)
er_y <- mean(y)

# calculate standard deviation
sd_x <- sd(x)
sd_y <- sd(y)

# calculate covariance
cov_xy <-cov(x,y)

# create 1000 portfolio weights
x_weights<-seq(from=0, to=1, length.out=1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights, wy = 1-x_weights)

#calculate the expected returns and standard derivations for the 1000 possible portfolios
two_assets[, ':='(er_p = wx*er_x + wy*er_y, sd_p = sqrt(wx^2*sd_x^2+wy^2*sd_y^2+2*wx*(1-wx)*cov_xy))]

#delete those points in the lower part
a<-two_assets$er_p[two_assets$sd_p==min(two_assets$sd_p)]
upper<-two_assets[two_assets$er_p>=a,]

# plot
ggplot() + geom_point(data=upper,aes(x=sd_p,y=er_p,color=wx)) +theme_bw() + ggtitle("Efficient Frontier with Two Risky Assets") +
xlab("Volatility") + ylab("Expected Returns") +
scale_y_continuous(label = percent, limits = c(0.01, max(upper$er_p) * 1.1)) +
scale_x_continuous(label = percent, limits = c(0.07, max(upper$sd_p) * 1.1)) +
scale_color_continuous(name = expression(omega[x]), labels = percent)

# solve final equation
A<-matrix(c(upper$er_p[1]^2,upper$er_p[1],1,upper$er_p[2]^2,upper$er_p[2],1,upper$er_p[3]^2,upper$er_p[3],1),nrow=3,byrow=T)
B<-matrix(c(upper$sd_p[1]^2,upper$sd_p[2]^2,upper$sd_p[3]^2),nrow=3)
equation<-solve(A,B)

# 4_(4)
SR<-max((upper$er_p-er_z)/upper$sd_p)
market_p<-upper[(upper$er_p-er_z)/upper$sd_p==SR,c(3,4)]
ggplot() + geom_point(data=upper,aes(x=sd_p,y=er_p,color=wx)) +theme_bw() + ggtitle("Efficient Frontier with Two Risky Assets and Risk-free Asset") +
xlab("Volatility") + ylab("Expected Returns") +
geom_line(data = data.table(sd=c(market_p$sd_p,0),mean=c(market_p$er_p,0.0003)),aes(x = sd, y = mean),size=2)+
geom_point(data = data.table(sd=c(market_p$sd_p,0),mean=c(market_p$er_p,0.0003)), aes(x = sd, y = mean, color = 1))+
scale_y_continuous(label = percent, limits = c(0, max(upper$er_p) * 1.1)) +
scale_x_continuous(label = percent, limits = c(0, max(upper$sd_p) * 1.1)) +
scale_color_continuous(name = expression(omega[x]), labels = percent)


# 4_(5)
df<-data.table(read.csv("4_5.csv"))
names(df)<-c("id","date","mon_r")
x<-df$mon_r[df$id==1]
y<-df$mon_r[df$id==89]
z<-df$mon_r[df$id==333]

# calculate expected returns for the three assets
er_x <- mean(x)
er_y <- mean(y)
er_z <- mean(z)

# calculate standard deviation
sd_x <- sd(x)
sd_y <- sd(y)
sd_z <- sd(z)

# calculate covariance
cov_xy <-cov(x,y)
cov_xz <-cov(x,z)
cov_yz <-cov(z,y)

# create 1000 portfolio weights
x_weights<-seq(from=0, to=1, length.out=1000)

# calculate a data.table that contains the weights for the three assets
three_assets<-data.table(wx=rep(x_weights,each=length(x_weights)),wy=rep(x_weights,length(x_weights)))
three_assets[,wz:=1-wx-wy]

#calculate the expected returns and standard deviations for the 1000^2 possible portfolios
three_assets[,':=' (er_p=wx*er_x+wy*er_y+wz*er_z,sd_p = sqrt(wx^2 * sd_x^2 +wy^2 * sd_y^2 +wz^2 * sd_z^2 +2 * wx * wy * cov_xy +2 * wx * wz * cov_xz +2 * wy * wz * cov_yz))]

# take out cases where we have negative weights
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]

min_var_p<-three_assets[three_assets$sd_p==min(three_assets$sd_p),]