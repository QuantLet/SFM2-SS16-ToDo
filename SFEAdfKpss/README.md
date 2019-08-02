[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **SFEAdfKpss** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet: SFEAdfKpss

Published in: Statistics of Financial Markets

Description: 'Computes the ADF and KPSS test statistics for 20 biggest companies (by market capitalisation) of FTSE100 and DAX. Estimated ADF and KPSS test with only a constant and a constant plus linear trend. Adds asterisk to indicate significance at 5%-confidence level'

Keywords: ADF-Test, nonstationary, dax, ftse100, random-walk


Author: Joanna Tomanek, Sophie Burgard
Author[Python]: Justin Hellermann


```

### PYTHON Code
```python

import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
from statsmodels.tsa.stattools import kpss
from statsmodels.tsa.stattools import adfuller

#Import Data
data = pd.read_csv('DAXFTSE.csv',sep=';').drop(columns=['Name'])

#ADF function which inputs a dataframe, calculates test statistics and 
#assigns * if result is significant at 5% level, outputs marked values of
#ADF test statistic

def ADF(df,maxlag,regression):
	res=[]
	for col in df.columns:
		test_results=adfuller(df[col],maxlag=maxlag,regression=regression)
		pvalue=test_results[1]
		if pvalue<=0.05:#test decision
			test_stat=str(test_results[0].round(4))+'*'
		else:
			test_stat=str(test_results[0].round(4))
		res.append([test_stat])
	return(pd.DataFrame(res,columns=['ADF_'+str(maxlag)+'_'+str(regression)]))

#KPSS function which inputs a dataframe, calculates test statistics and 
#assigns * if result is significant at 5% level, outputs marked values of
#KPSS test statistic
def KPSS(df,lags,regression):
	res=[]
	for col in df.columns:
		test_results=kpss(df[col],lags=lags,regression=regression)
		pvalue=test_results[1]
		if pvalue<=0.05:#test decision
			test_stat=str(test_results[0].round(4))+'*'
		else:
			test_stat=str(test_results[0])
		res.append([test_stat])
	return(pd.DataFrame(res,columns=['KPSS_'+str(lags)+'_'+str(regression)]))

#ignore warnings from statsmodels package related to the displayed p-value
import warnings
warnings.filterwarnings("ignore")
#

ADF_summary=pd.concat([ADF(data,0,'c'),ADF(data,4,'c'),ADF(data,0,'ct'),ADF(data,4,'ct')],axis=1)
ADF_summary.index=data.columns

KPSS_summary=pd.concat([KPSS(data,8,'c'),KPSS(data,12,'c'),KPSS(data,8,'ct'),KPSS(data,12,'ct')],axis=1)
KPSS_summary.index=data.columns
pd.set_option('display.max_columns',10)
print(ADF_summary)
print(KPSS_summary)

```

automatically created on 2019-08-02

### R Code
```r

setwd("/Users/Sophie/Desktop/ERS/SFEAdfKpss")
# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("tseries", "fUnitRoots", "urca")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

datax = read.csv2("DAXFTSE.csv", stringsAsFactors = FALSE)                 
datax = as.data.frame(sapply(datax[,2:43], as.numeric))  


stocks = as.matrix(c("DAX", "SAP", "SIEMENS", "BAYER", "DEUTSCHE.TELEKOM", "BASF", "VOLKSWAGEN", 
                     "DAIMLER", "ALLIANZ", "BMW", "HENKEL", "FRESENIUS", "CONTINENTAL", 
                     "DEUTSCHE.POST", "ADIDAS", "FRESENIUS.MED.CARE", "MUENCHENER.RUCK.", 
                     "LINDE", "BEIERSDORF", "E.ON", "DEUTSCHE.BANK", "FTSE.100", 
                     "ROYAL.DUTCH.SHELL.B", "UNILEVER", "HSBC", 
                     "BRITISH.AMERICAN.TOBACCO", "GLAXOSMITHKLINE", "SABMILLER", "BP", 
                     "VODAFONE.GROUP", "ASTRAZENECA", "RECKITT.BENCKISER.GROUP", "DIAGEO", 
                     "BT.GROUP", "LLOYDS.BANKING.GROUP", "BHP.BILLITON", "NATIONAL.GRID", 
                     "IMPERIAL.BRANDS", "RIO.TINTO", "PRUDENTIAL", "ROYAL.BANK.OF.SCTL.GP.", 
                     "BARCLAYS"))


x  = log(datax)
st = stocks


result = matrix(NA,ncol = 16,nrow = ncol(x),
         dimnames = list(st,c("ADF.constant.0",
                              "p-val.1","ADF.constant.4","p-val.2", 
                              "ADF.trend.0","p-val.3","ADF.trend.4",
                              "p-val.4" ,"KPSS.constant.8","crit-val.1", 
                              "KPSS.constant.12","crit-val.2","KPSS.trend.8",
                              "crit-val.3","KPSS.trend.12", "crit-val.4"))) 

for(i in 1:ncol(x))		# start calc. table
{
  #******   ADF   **************************
  # constant
  e = adfTest(x[,i],lags = 0, type ="c") 
  result[i,1] = as.numeric(e@test$statistic)
  result[i,2] = as.numeric(e@test$p.value)
  
  f = adfTest(x[,i],lags = 4, type = "c")
  result[i,3] = as.numeric(f@test$statistic)
  result[i,4] = as.numeric(f@test$p.value)
  
  # constant plus trend
  g = adfTest(x[,i],lags = 0, type = "ct") 
  result[i,5] = as.numeric(g@test$statistic)
  result[i,6] = as.numeric(g@test$p.value)
  
  h = adfTest(x[,i],lags = 4,type = "ct")
  result[i,7] = as.numeric(h@test$statistic)
  result[i,8] = as.numeric(h@test$p.value)
  
  
  #****   KPSS   ***************************
  # constant
  j = urkpssTest(x[,i], type = "mu",use.lag = 8, doplot = FALSE) 
  result[i,9]  = as.numeric(j@test$test@teststat)
  result[i,10] = as.numeric(j@test$test@cval[1,2])
  
  k = urkpssTest(x[,i], type = "mu",use.lag = 12, doplot = FALSE)
  result[i,11] = as.numeric(k@test$test@teststat)
  result[i,12] = as.numeric(k@test$test@cval[1,2])
  
  # constant plus trend
  l = urkpssTest(x[,i], type = "tau",use.lag = 8, doplot = FALSE) 
  result[i,13] = as.numeric(l@test$test@teststat)
  result[i,14] = as.numeric(l@test$test@cval[1,2])

  m = urkpssTest(x[,i], type = "tau",use.lag = 12, doplot = FALSE)
  result[i,15] = as.numeric(m@test$test@teststat)
  result[i,16] = as.numeric(m@test$test@cval[1,2])
}



#*****Add automatically significance stars to Output
z = c(1,3,5,7)

ADF_results= round(result[,1:8],2)
for (r in z){
  for (i in 1:nrow(ADF_results)){
    if (ADF_results[i, r+1] <= 0.05){
      ADF_results[i,r] = paste0(ADF_results[i,r],"*")
    } 
  }
}


KPSS_results = round(result[,9:16],2)
for (i in 1:nrow(KPSS_results)){
  for (r in z){
    if (as.numeric(KPSS_results[i,r]) > (as.numeric(KPSS_results[i,r+1]) %*% 1.96)){
      KPSS_results[i,r] = paste0(KPSS_results[i,r],"*")
    }else{
      KPSS_results[i,r] = KPSS_results[i,r]
    }
  }
}

#*****Print Test Results

print(ADF_results[,z])
print(KPSS_results[,z])

```

automatically created on 2019-08-02