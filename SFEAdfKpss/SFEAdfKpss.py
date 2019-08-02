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
