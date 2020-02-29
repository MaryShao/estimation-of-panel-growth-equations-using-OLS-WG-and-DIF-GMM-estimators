library('plm')
#install.packages(gmm)
library('gmm')
library('gmm4')

# Gini
gini<-c(0.97,0.85,0.79,0.71,0.26,0.2,0.06,0.06,0.98,0.92,0.97,0.06,0.84,0.84,0.83,
        0.06,0.06,0.06,0.99,0.06,0.65,0.06,0.06,0.06,0.9,0.59,0.06,0.06,0.56,0.68,0.93)
length(gini)
# 2008-2017
higher_edu <- read.csv('1# of students per100 000 in higher education.csv',header=T)
jsecondary_edu <- read.csv('2# of students per100 000 in junior secondary education.csv',header=T)
ssecondary_edu <- read.csv('3# of students per100 000 in senior secondary education.csv',header=T)
primary_edu <- read.csv('4# of students per100 000 in primary education.csv',header=T)
# 2008-2017
provd<-higher_edu[1]
# secondary enrollment
enroll_edu <- data.frame(provd,higher_edu[2:11]+jsecondary_edu[2:11]+
                           ssecondary_edu[2:11]+primary_edu[2:11])
# 1998-2017
GRP<-read.csv('gross regional product.csv',header=T)
# 1998-2017
GRP_income<-read.csv('Gross regional product by income approach.csv',header=T)
GRP_income<-GRP_income[2:19]
# 2000-2017
NGR<-read.csv('natural growth rate.csv',header=T)
NGR<-NGR[2:19]
# 1998-2017
invest<-read.csv('Total Investment in Fixed Assets in the Whole Country.csv',header=T)
invest<-invest[2:19]
# 1998-2017
imexport<-read.csv('total value of import and export.csv',header=T)
imexport<-imexport[2:19]

Y <-c()
Y_income<-c()
y <-c()
y_income<- c()
N <-c()
inv <-c()
iexp<-c()
dif_ins_Y<-c()
dif_ins_inv<-c()
dif_ins_N<-c()
dif_ins_iexp<-c()

for (i in (1:31)){
  for (j in (2:19)){
    Y<-c(Y,GRP[i,j])
    #Y_income<-c(Y_income,GRP_income[i,j])
  }
}

tao<-2
for (i in (1:31)){
  for (j in (1:18)){
    if (j <= (18-tao)){
      y<-c(y,log(Y[(i-1)*18+j]/Y[(i-1)*18+j+tao]))
      #y_income<-c(y_income,log(Y_income[(i-1)*18+j]/Y_income[(i-1)*18+j+2]))
    }
  }
}

Y <-c()
Y_income<-c()
prov<-c()
year<-c()

for (i in (1:31)){
  n<-2017
  for (j in (1:(18-tao))){
    Y<-c(Y,log(GRP[i,j+tao+1]))
    prov<-c(prov,GRP[i,1])
    year<-c(year,n)
    #dif_ins_Y<-c(dif_ins_Y,log(GRP[i,j+1]))
    #dif_ins_inv<-c(dif_ins_inv,log(invest[i,j+1]))
    #dif_ins_N<-c(dif_ins_N,NGR[i,j+1]+0.05)
    #dif_ins_iexp<-c(dif_ins_iexp,log(imexport[i,j+1]))
    #Y_income<-c(Y_income,log(GRP_income[i,j]))
    N<-c(N,NGR[i,j]+0.05)
    inv<-c(inv,log(invest[i,j]))
    iexp<-c(iexp,log(imexport[i,j]))
    n<-n-1
  }
}

dat<-data.frame(prov,year,y,Y,N,inv,iexp)

#OLS
ols<-lm(y~Y+N+inv+iexp)
summary(ols)
plot(N,y,xlab='Natural growth rate',ylab='GDP per capita')
abline(lm(y~N),col='red')
cor(N,y)
plot(inv,y,xlab='Total physical investment',ylab='GDP per capita')
abline(lm(y~inv),col='red')
cor(inv,y)
plot(iexp,y,xlab='Total value of import and export',ylab='GDP per capita')
abline(lm(y~iexp),col='red')
cor(iexp,y)


p <- plm(y~Y+N+inv+iexp,
         data = dat, model = "pooling")
summary(p)

# within group
wi <- plm(y~Y+N+inv+iexp,
          data = dat, model = "within", effect = "twoways")
summary(wi)

# DIF-GMM
#dif_gmm1 <- gmm(y~Y+N+inv+iexp,~Y+N+inv+iexp)
#summary(dif_gmm1)

dif_gmm2 <-pgmm(y~Y+N+inv+iexp|lag(Y,2:99)+lag(N,2:99)+lag(inv,2:99)+lag(iexp,2:99),
                dat,transformation='d')
summary(dif_gmm2)

dif_gmm3 <-pgmm(y~Y+N+inv+iexp|lag(Y,2:99)+lag(N,2:99),
                dat,transformation='d')
summary(dif_gmm3)

# SYS-GMM

sys_gmm <-pgmm(y~Y+N+inv+iexp|lag(Y,2:99)+lag(N,2:99)+lag(inv,2:99)+lag(iexp,2:99),
                dat,effect = "twoways", model = "onestep", 
               transformation = "ld")
summary(sys_gmm)

sys_gmm2 <-pgmm(y~Y+N+inv+iexp|lag(Y,2:99)+lag(N,2:99),
               dat,effect = "twoways", model = "onestep", 
               transformation = "ld")
summary(sys_gmm2)


#m2<-lm(y_income~Y_income+N+inv+iexp)
#summary(m2)

#m2<-gmm(y~Y+N+inv+iexp, ~ dif_ins_Y+ dif_ins_inv+ dif_ins_N+ dif_ins_iexp)
#summary(m2)
#pgmm(formula=y~Y+N+inv+iexp|lag(y,1),data=dat,transformation='d')


# fixed-effect
ols_y <-predict(ols)
wi_y<-wi$model[[1]] - wi$residuals 
dif_gmm_y<-fitted.values(dif_gmm2) #dif_gmm2
sys_gmm_y<-fitted.values(sys_gmm) #sys_gmm1
n <- length(ols_y)/31
y_avg<-c()
ols_avg<-c()
wi_avg<-c()
dif_gmm_avg<-c()
sys_gmm_avg<-c()
for (i in (1:31)){
  y_i<-y[((i-1)*n+1):(i*n)]
  print((i-1)*n+1)
  print(y_i)
  y_avg<-c(y_avg,mean(y_i))
  ols_i<-ols_y[((i-1)*n+1):(i*n)]
  ols_avg<-c(ols_avg,mean(ols_i))
  wi_i<-wi_y[((i-1)*n+1):(i*n)]
  wi_avg<-c(wi_avg,mean(wi_i))
  dif_gmm_i<-dif_gmm_y[((i-1)*n+1):(i*n)]
  dif_gmm_avg<-c(dif_gmm_avg,mean(dif_gmm_i))
  sys_gmm_i<-sys_gmm_y[((i-1)*n+1):(i*n)]
  sys_gmm_avg<-c(sys_gmm_avg,mean(sys_gmm_i))
}


fix_ols<-lm((y_avg-ols_avg)~gini)
summary(fix_ols)
fix_wi<-lm((y_avg-wi_avg)~gini)
summary(fix_wi)
fix_dif_gmm<-lm((y_avg-dif_gmm_avg)~gini)
summary(fix_dif_gmm)
fix_sys_gmm<-lm((y_avg-sys_gmm_avg)~gini)
summary(fix_sys_gmm)
