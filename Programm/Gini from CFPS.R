#install.packages('gmm')
library(gmm)
install.packages('sas7bdat')
library(sas7bdat)
#install.packages('SASxport')
library(SASxport)
#install.packages('foreign')
library(foreign)
install.packages('RevoScaleR')
library(RevoScaleR)
#install.packages('haven')
#install.packages('ineq')

#read.ssd('cfps2016famecon_201807.sas7bdat')
#read.sas7bdat(file.choose(), debug=FALSE)
#data <- read.table('2016fam.txt',header=T)
#x<-read.sas7bdat("2016fam.sas7bdat", debug=FALSE)

#install.packages("readstata13")
#library(readstata13)
#CGSS <- read.dta13("cgss2015_14.dta", convert.dates = TRUE, convert.factors = TRUE,
#                   missing.type = FALSE,
#                   convert.underscore = FALSE)

##############################

library(haven)
library(ineq)

#################################

est_gini <- read.csv('estimate provincial gini.csv',header=T)
est_gini

# official national data 
# (---- National Bureau of Statistics of China (NBS))

# Using data from CFPS
# Provicial Gini coefficient
# Gini function I write
Gini<-function(income){    
  library("caTools")     
  cum_income <- cumsum(sort(c(income,0)))     
  sum_income <- cum_income[length(cum_income)]     
  xarray     <- seq(0,length(cum_income)-1) / (length(cum_income)-1)     
  yarray     <- cum_income/sum_income     
  B          <- trapz(x=xarray,y=yarray)     
  A          <- 0.5 - B     
  return     (A/(A+B))     
}

# Two ways to calculate Gini coef.
# gini1 <- tapply(dat$fincome1, as.factor(dat$provcd16), Gini)
# gini2 <- tapply(dat$fincome1, as.factor(dat$provcd16), 
#               function(x) ineq(x,type="Gini"))

# 2016
data_2016 <- read_sas("ecfps2016famecon_201807.sas7bdat", "formats_cfps_english.sas7bcat")

## family net income
inc1_2016 <- data_2016$fincome1
weight1_2016 <- data_2016$fswt_rescs16
for (i in (1:length(weight1_2016))){
  if (is.na(weight1_2016[i])){
    weight1_2016[i] <- 1
  }
}
sum(weight1_2016)
proc_2016<-rep(0,sum(weight1_2016))
income_2016<-rep(0,sum(weight1_2016))
for (i in (1:length(inc1_2016))){
  n<-weight1_2016[i]
  fi<-inc1_2016[i]
  pi<-data_2016$provcd16[i]
  for (j in (1:n)){
    income_2016[j]<- fi
    proc_2016[j]<-pi
  }
}
## family wage + operate + transfer(subsidy) + property + else income
inc2_2016 <- data_2016$fwage_1 + data_2016$foperate_1 + data_2016$ftransfer_1 + data_2016$fproperty_1+ data_2016$felse_1

gini1_2016 <- tapply(income_2016, as.factor(proc_2016),
                     function(x) ineq(x,type="Gini"))
gini1_2016 <- tapply(income_2016, as.factor(proc_2016),
gini2_2016 <- tapply(inc2_2016, as.factor(data_2016$provcd16),
                     function(x) ineq(x,type="Gini"))

gini1_2016
gini1_2016


# 2014
data_2014 <- read_sas("ecfps2014famecon_170630.sas7bdat", "formats_cfps_english.sas7bcat")

## family net income
inc1_2014 <- data_2014$fincome1
## family wage + operate + transfer(subsidy) + property + else income
inc2_2014 <- data_2014$fwage_1 + data_2014$foperate_1 + data_2014$ftransfer_1 + data_2014$fproperty_1+ data_2014$felse_1

gini1_2014 <- tapply(inc1_2014, as.factor(data_2014$provcd14),
                     function(x) ineq(x,type="Gini"))
gini2_2014 <- tapply(inc2_2014, as.factor(data_2014$provcd14),
                     function(x) ineq(x,type="Gini"))

gini1_2014
gini2_2014


# 2012
data_2012 <- read_sas("cfps2012family_092015.sas7bdat", "formats_cfps_english.sas7bcat")

## family net income
inc1_2012 <- data_2012$FINCOME1
## family wage + operate + transfer(subsidy) + property + else income
inc2_2012 <- data_2012$WAGE_1 + data_2012$foperate_1 + data_2012$ftransfer_1 + data_2012$fproperty_1+ data_2012$FELSE_1

gini1_2012 <- tapply(inc1_2012, as.factor(data_2012$provcd),
                     function(x) ineq(x,type="Gini"))
gini2_2012 <- tapply(inc2_2012, as.factor(data_2012$provcd),
                     function(x) ineq(x,type="Gini"))

gini1_2012
gini2_2012


# 2011
data_2011 <- read_sas("cfps2011family_102014.sas7bdat", "formats_cfps_english.sas7bcat")

## family net income
inc1_2011 <- data_2011$faminc_net
## family wage + operate + transfer(subsidy) + property + else income
inc2_2011 <- data_2011$FINC + data_2011$NET_AGRI + data_2011$foperate_net + data_2011$WELFARE + data_2011$fproperty+ data_2011$FELSE

gini1_2011 <- tapply(inc1_2011, as.factor(data_2011$PROVCD),
                     function(x) ineq(x,type="Gini"))
gini2_2011 <- tapply(inc2_2011, as.factor(data_2011$PROVCD),
                     function(x) ineq(x,type="Gini"))

gini1_2011
gini2_2011


# 2010
data_2010 <- read_sas("ecfps2010family_nat072016.sas7bdat", "formats_cfps_english.sas7bcat")

## family net income
inc1_2010 <- data_2010$faminc_net
## family wage + operate + transfer(subsidy) + property + else income
inc2_2010 <- data_2010$FINC + data_2010$NET_AGRI + data_2010$foperate_net + data_2010$WELFARE + data_2010$fproperty+ data_2010$FELSE

gini1_2010 <- tapply(inc1_2010, as.factor(data_2010$provcd),
                     function(x) ineq(x,type="Gini"))
gini2_2010 <- tapply(inc2_2010, as.factor(data_2010$provcd),
                     function(x) ineq(x,type="Gini"))

gini1_2010
gini2_2010


# 2009 & 2008
# only have 3 province
data_2009 <- read_sas("cfps_2009_family.sas7bdat", "formats_cfps_english.sas7bcat")


gini<-cbind(gini1_2016,gini2_2016,gini1_2014,gini2_2014,gini1_2012,gini2_2012,gini1_2011,gini2_2011,gini1_2010,gini2_2010)
write.csv(gini,'CFPSgini.csv')



