
################################materyal kodu eklendi###########################################################
library(forecast)
library(XLConnect)

demoExcelFile <- system.file("demoFiles/Kitap4.xlsx", package = "XLConnect")
wb <- loadWorkbook(demoExcelFile)
data2 <- readTable(wb, sheet = "R Input Table", table = "Table1")

##data2
months=matrix(c("jan", "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),nrow=12,ncol=1)
months1=matrix(c( "feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec","jan"),nrow=12,ncol=1)
months2=matrix(c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec","jan","feb"),nrow=12,ncol=1)
months3=matrix(c("apr","may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar"),nrow=12,ncol=1)
months4=matrix(c("may","jun","jul","aug","sep","oct","nov","dec","jan","feb","mar","apr"),nrow=12,ncol=1)
months5=matrix(c("jun","jul","aug","sep","oct","nov","dec","jan","feb","mar","apr","may"),nrow=12,ncol=1)
months6=matrix(c("jul","aug","sep","oct","nov","dec","jan","feb","mar","apr","may","jun"),nrow=12,ncol=1)
months7=matrix(c("aug","sep","oct","nov","dec","jan","feb","mar","apr","may","jun","jul"),nrow=12,ncol=1)
months8=matrix(c("sep","oct","nov","dec","jan","feb","mar","apr","may","jun","jul","aug"),nrow=12,ncol=1)
months9=matrix(c("oct","nov","dec","jan","feb","mar","apr","may","jun","jul","aug","sep"),nrow=12,ncol=1)
months10=matrix(c("nov","dec","jan","feb","mar","apr","may","jun","jul","aug","sep","oct"),nrow=12,ncol=1)
months11=matrix(c("dec","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov"),nrow=12,ncol=1)







nr=nrow(data2)
wb <- loadWorkbook("Malzeme Dosyasi.xlsm", create = FALSE)
createSheet(wb, name = "result")


writeWorksheet(wb,"second method", sheet = "result", startRow = 12, startCol =9 ,header=FALSE)
writeWorksheet(wb,"material codes", sheet = "result", startRow = 12, startCol =3 ,header=FALSE)
writeWorksheet(wb,"best method", sheet = "result", startRow = 12, startCol =4 ,header=FALSE)

howmany1=c(0,0,0,0,0,0,0,0,0,0,0,0)
howmany2=c(0,0,0,0,0,0,0,0,0,0,0,0)



writeWorksheet(wb,"how many in first" , sheet = "result", startRow = 2, startCol =3 ,header=FALSE)

writeWorksheet(wb,"croston" , sheet = "result", startRow = 3, startCol =3 ,header=FALSE)
writeWorksheet(wb, "forecast", sheet = "result", startRow = 4, startCol =3 ,header=FALSE)
writeWorksheet(wb, "meanf", sheet = "result", startRow = 5, startCol =3 ,header=FALSE)
writeWorksheet(wb, "rwf", sheet = "result", startRow = 6, startCol =3 ,header=FALSE)
writeWorksheet(wb, "ses", sheet = "result", startRow = 7, startCol =3 ,header=FALSE)
writeWorksheet(wb, "ses2", sheet = "result", startRow = 8, startCol =3 ,header=FALSE)
writeWorksheet(wb, "holt", sheet = "result", startRow = 3, startCol =5,header=FALSE)
writeWorksheet(wb, "holt2", sheet = "result", startRow = 4, startCol =5 ,header=FALSE)
writeWorksheet(wb, "hw", sheet = "result", startRow = 5, startCol =5 ,header=FALSE)
writeWorksheet(wb, "spl", sheet = "result", startRow = 6, startCol =5 ,header=FALSE)
writeWorksheet(wb, "the", sheet = "result", startRow = 7, startCol =5 ,header=FALSE)
writeWorksheet(wb, "arima", sheet = "result", startRow = 8, startCol =5 ,header=FALSE)

writeWorksheet(wb,"how many in second" , sheet = "result", startRow = 2, startCol =9 ,header=FALSE)

writeWorksheet(wb,"croston" , sheet = "result", startRow = 3, startCol =9 ,header=FALSE)
writeWorksheet(wb, "forecast", sheet = "result", startRow = 4, startCol =9 ,header=FALSE)
writeWorksheet(wb, "meanf", sheet = "result", startRow = 5, startCol =9 ,header=FALSE)
writeWorksheet(wb, "rwf", sheet = "result", startRow = 6, startCol =9 ,header=FALSE)
writeWorksheet(wb, "ses", sheet = "result", startRow = 7, startCol =9 ,header=FALSE)
writeWorksheet(wb, "ses2", sheet = "result", startRow = 8, startCol =9 ,header=FALSE)
writeWorksheet(wb, "holt", sheet = "result", startRow = 3, startCol =11,header=FALSE)
writeWorksheet(wb, "holt2", sheet = "result", startRow = 4, startCol =11 ,header=FALSE)
writeWorksheet(wb, "hw", sheet = "result", startRow = 5, startCol =11 ,header=FALSE)
writeWorksheet(wb, "spl", sheet = "result", startRow = 6, startCol =11 ,header=FALSE)
writeWorksheet(wb, "the", sheet = "result", startRow = 7, startCol =11 ,header=FALSE)
writeWorksheet(wb, "arima", sheet = "result", startRow = 8, startCol =11 ,header=FALSE)







for (i in 1:nr){
    nc=ncol(data2[i,])
    ddd=data2[i,2:nc]
    dddy=as.numeric(ddd)
    mname=data2[i,1]
    sum=0


	for (j in 1:(nc-1)){
  		if(is.na(dddy[j])==1){
   		sum=sum+1
		}
	}
	if(sum!=0){
	sum2=0
	ddd3=1:(nc-1-sum)


		for (k in 1:(nc-1)){
			if(is.na(dddy[k])==0){
			sum2=sum2+1    
			ddd3[sum2]=dddy[k]
			}
		}
	}


	if(sum!=0){
		ddd2=ddd3
	}
	else{
		ddd2=dddy
	}


	sayý=dddy[nc-1]+dddy[nc-2]+dddy[nc-3]
	ln=length(ddd2)
	ddd5=ddd2
	ddd2=ddd2[1:(ln-3)]


	if(sum==0){
		cr=croston(t(dddy[1:(length(dddy)-3)]),h=3,alpha=0.1)
	}
	else{
		cr=croston(ddd2,h=3,alpha=0.1)
	}
   
      crkar=cr[[1]][1]+cr[[1]][2]+cr[[1]][2]
	accuracycroston=accuracy ( cr,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	
###################### aylarrr################################################################
if (((nc-1) %% 12)==0){
writeWorksheet(wb,months, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==1){
writeWorksheet(wb,months1, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==2){
writeWorksheet(wb,months2, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==3){
writeWorksheet(wb,months3, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==4){
writeWorksheet(wb,months4, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==5){
writeWorksheet(wb,months5, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==6){
writeWorksheet(wb,months6, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==7){
writeWorksheet(wb,months7, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==8){
writeWorksheet(wb,months8, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==9){
writeWorksheet(wb,months9, sheet = "result", startRow = 14*i, startCol =3 )
}
else if (((nc-1) %% 12)==10){
writeWorksheet(wb,months10, sheet = "result", startRow = 14*i, startCol =3 )
}
else{
writeWorksheet(wb,months11, sheet = "result", startRow = 14*i, startCol =3 )
}
#####################aylar######################################################################   
	f=forecast(ddd2 ,h = 3, level=c(80,95), fan=FALSE, robust=FALSE, lambda=NULL,find.frequency=FALSE, allow.multiplicative.trend=FALSE)
	accuracyforecast=accuracy ( f,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	fkar=f[[2]][1]+f[[2]][2]+f[[2]][3]

	mf=meanf(ddd2, h=3, fan=FALSE, lambda=NULL)
	accuracymeanf=accuracy ( mf,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	mfkar=mf[[5]][1]+mf[[5]][2]+mf[[5]][3]


	rwf=rwf(ddd2, h=3, drift=TRUE, level=c(80,95), fan=FALSE, lambda=NULL)
	accuracyrwf=accuracy ( rwf,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
      rwfkar=rwf[[5]][1]+rwf[[5]][2]+rwf[[5]][3]

	
	ses=ses(ddd2,h=3,level=c(80,95),fan=FALSE,initial=c("optimal"),alpha=NULL) 
	accuracyses=accuracy ( ses,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	seskar=ses[[2]][1]+ses[[2]][2]+ses[[2]][3]


	ses2=ses(ddd2,h=3,level=c(80,95),fan=FALSE,initial=c("simple"),alpha=NULL) 
	accuracyses2=accuracy ( ses2,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	ses2kar=ses2[[2]][1]+ses2[[2]][2]+ses2[[2]][3]

	
	holt=holt(ddd2, h=3, damped=FALSE, level=c(80,95), fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL)
	accuracyholt=accuracy ( holt,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	holtkar=holt[[2]][1]+holt[[2]][2]+holt[[2]][3]


	holt2=holt(ddd2, h=3, damped=FALSE, level=c(80,95), fan=FALSE,initial=c("simple"), exponential=FALSE, alpha=NULL, beta=NULL)
	accuracyholt2=accuracy ( holt2,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	holt2kar=holt2[[2]][1]+holt2[[2]][2]+holt2[[2]][3]


	hw=hw(ts(ddd2,frequency=12), h=3, seasonal=c("additive"), damped=FALSE, level=c(80,95),fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL, gamma=NULL)
	accuracyhw=accuracy ( hw,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	hwkar=hw[[2]][1]+hw[[2]][2]+hw[[2]][3]

	
  
	spl=splinef(ddd2, h=3, level=c(80,95), fan=FALSE, lambda=NULL, method=c("mle"))
	accuracyspl=accuracy ( spl,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	splkar=spl[[4]][1]+spl[[4]][2]+spl[[4]][3]

	
	the=thetaf(ddd2, h=3, level=c(80,95), fan=FALSE)
	accuracythe=accuracy ( the,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
      thekar=the[[2]][1]+the[[2]][2]+the[[2]][3]

	
	fit <- auto.arima(ddd2)
	arima=forecast(fit,h=3) 
	accuracyarima=accuracy ( arima,ddd5[(ln-3):ln], test = NULL, d = NULL, D = NULL)
	arimakar=arima[[4]][1]+arima[[4]][2]+arima[[4]][3]

	
	accuracyarray=c(accuracycroston[10], accuracyforecast[10], accuracymeanf[10], accuracyrwf[10], accuracyses[10], accuracyses2[10], accuracyholt[10], accuracyholt2[10], accuracyhw[10], accuracyspl[10], accuracythe[10], accuracyarima[10])
	best=which.min(accuracyarray)
	accuracyarray[best]=100000000000000
	best2=which.min(accuracyarray)
      arraykar=c(crkar,fkar,mfkar,rwfkar,seskar,ses2kar,holtkar,holt2kar,hwkar,splkar,thekar,arimakar)
	##################################if (arraykar[best]<sayý){######################
#############################writeWorksheet(wb, "Safety stok", sheet = "result", startRow = 14*i, startCol = 2,header=FALSE)###############################3
#################}#############


writeWorksheet(wb, mname, sheet = "result", startRow = 14*i, startCol = 3,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 15*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 16*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 17*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 18*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 19*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 20*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 21*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 22*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 23*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 24*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 25*i, startCol = 2,header=FALSE)
writeWorksheet(wb, mname, sheet = "result", startRow = 26*i, startCol = 2,header=FALSE)

	

	if ( best==1) {

		if(sum==0){
			cr=croston(t(dddy),h=12,alpha=0.1)
		}
		else{
			cr=croston(ddd5,h=12,alpha=0.1)
		}
howmany1[best]=howmany1[best]+1		
writeWorksheet(wb, cr, sheet = "result", startRow = 14*i, startCol =4 )
writeWorksheet(wb, "croston", sheet = "result", startRow = (14*i-1), startCol =4, header = FALSE )
 
} else if ( best==2) {
   f=forecast(ddd5 ,h = 12, level=c(1,5), fan=FALSE, robust=FALSE, lambda=NULL,find.frequency=FALSE, allow.multiplicative.trend=FALSE)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, f, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "forecast", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==3) {
   mf=meanf(ddd5 , h=12, fan=FALSE, lambda=NULL,level=c(1,5))
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, mf, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "meanf", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==4) {
   rwf=rwf(ddd5 , h=12, drift=TRUE, level=c(1,5), fan=FALSE, lambda=NULL)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, rwf, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "rwf", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==5) {
   ses=ses(ddd5 ,h=12,level=c(1,5),fan=FALSE,initial=c("optimal"),alpha=NULL)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, ses, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "ses", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

}else if ( best==6) {
   ses2=ses(ddd5,h=12,level=c(1,5),fan=FALSE,initial=c("simple"),alpha=NULL) 
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, ses2, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "ses2", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==7) {
   holt=holt(ddd5 , h=12, damped=FALSE, level=c(1,5), fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, holt, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "holt", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

}else if ( best==8) {
   holt2=holt(ddd5 , h=12, damped=FALSE, level=c(1,5), fan=FALSE,initial=c("simple"), exponential=FALSE, alpha=NULL, beta=NULL)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, holt2, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "holt2", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==9) {
   hw=hw(ts(ddd5 ,frequency=12), h=12, seasonal=c("additive"), damped=FALSE, level=c(1,5),fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL, gamma=NULL)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, hw, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "hw", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

}else if ( best==10) {
   spl=splinef(ddd5 , h=12, level=c(1,5), fan=FALSE, lambda=NULL, method=c("mle"))
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, spl, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "splinef", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

} else if ( best==11) {
	the=thetaf(ddd5 , h=12, level=c(1,5), fan=FALSE)
howmany1[best]=howmany1[best]+1	
writeWorksheet(wb, the, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "thetaf", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)

}else {
	fit <- auto.arima(ddd5)
	arima=forecast(fit,h=12) 
howmany1[12]=howmany1[12]+1	
writeWorksheet(wb, arima, sheet = "result", startRow = 14*i, startCol = 4)
writeWorksheet(wb, "arima", sheet = "result", startRow = (14*i-1), startCol = 4, header = FALSE)
}	


############################best2##########################
	if ( best2==1) {

		if(sum==0){
			cr=croston(t(dddy),h=12,alpha=0.1)
		}
		else{
			cr=croston(ddd5,h=12,alpha=0.1)
		}
	writeWorksheet(wb, cr, sheet = "result", startRow = 14*i, startCol =9 )
writeWorksheet(wb, "croston", sheet = "result", startRow = (14*i-1), startCol =9, header = FALSE )
howmany2[best2]=howmany2[best2]+1 
} else if ( best2==2) {
   f=forecast(ddd5 ,h = 12, level=c(1,5), fan=FALSE, robust=FALSE, lambda=NULL,find.frequency=FALSE, allow.multiplicative.trend=FALSE)
writeWorksheet(wb, f, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "forecast", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==3) {
   mf=meanf(ddd5 , h=12, fan=FALSE, lambda=NULL,level=c(1,5))
writeWorksheet(wb, mf, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "meanf", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==4) {
   rwf=rwf(ddd5 , h=12, drift=TRUE, level=c(1,5), fan=FALSE, lambda=NULL)
writeWorksheet(wb, rwf, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "rwf", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==5) {
   ses=ses(ddd5 ,h=12,level=c(1,5),fan=FALSE,initial=c("optimal"),alpha=NULL)
writeWorksheet(wb, ses, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "ses", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
}else if ( best2==6) {
   ses2=ses(ddd5 ,h=12,level=c(1,5),fan=FALSE,initial=c("simple"),alpha=NULL) 
writeWorksheet(wb, ses2, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "ses2", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==7) {
   holt=holt(ddd5 , h=12, damped=FALSE, level=c(1,5), fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL)
writeWorksheet(wb, holt, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "holt", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
}else if ( best2==8) {
   holt2=holt(ddd5 , h=12, damped=FALSE, level=c(1,5), fan=FALSE,initial=c("simple"), exponential=FALSE, alpha=NULL, beta=NULL)
writeWorksheet(wb, holt2, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "holt2", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==9) {
   hw=hw(ts(ddd5 ,frequency=12), h=12, seasonal=c("additive"), damped=FALSE, level=c(1,5),fan=FALSE,initial=c("optimal"), exponential=FALSE, alpha=NULL, beta=NULL, gamma=NULL)
writeWorksheet(wb, hw, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "hw", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
}else if ( best2==10) {
   spl=splinef(ddd5 , h=12, level=c(1,5), fan=FALSE, lambda=NULL, method=c("mle"))
writeWorksheet(wb, spl, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "splinef", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
} else if ( best2==11) {
	the=thetaf(ddd5 , h=12, level=c(1,5), fan=FALSE)
writeWorksheet(wb, the, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "thetaf", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[best2]=howmany2[best2]+1
}else {
	fit <- auto.arima(ddd5 )
	arima=forecast(fit,h=12,level=c(1,5)) 
writeWorksheet(wb, arima, sheet = "result", startRow = 14*i, startCol = 9)
writeWorksheet(wb, "arima", sheet = "result", startRow = (14*i-1), startCol = 9, header = FALSE)
howmany2[12]=howmany2[12]+1
}

}

writeWorksheet(wb,howmany1[1] , sheet = "result", startRow = 3, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[2], sheet = "result", startRow = 4, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[3], sheet = "result", startRow = 5, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[4], sheet = "result", startRow = 6, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[5], sheet = "result", startRow = 7, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[6], sheet = "result", startRow = 8, startCol =4 ,header=FALSE)
writeWorksheet(wb, howmany1[7], sheet = "result", startRow = 3, startCol =6,header=FALSE)
writeWorksheet(wb, howmany1[8], sheet = "result", startRow = 4, startCol =6 ,header=FALSE)
writeWorksheet(wb, howmany1[9], sheet = "result", startRow = 5, startCol =6 ,header=FALSE)
writeWorksheet(wb, howmany1[10], sheet = "result", startRow = 6, startCol =6 ,header=FALSE)
writeWorksheet(wb, howmany1[11], sheet = "result", startRow = 7, startCol =6 ,header=FALSE)
writeWorksheet(wb, howmany1[12], sheet = "result", startRow = 8, startCol =6 ,header=FALSE)


writeWorksheet(wb,howmany2[1] , sheet = "result", startRow = 3, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[2], sheet = "result", startRow = 4, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[3], sheet = "result", startRow = 5, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[4], sheet = "result", startRow = 6, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[5], sheet = "result", startRow = 7, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[6], sheet = "result", startRow = 8, startCol =10 ,header=FALSE)
writeWorksheet(wb, howmany2[7], sheet = "result", startRow = 3, startCol =12,header=FALSE)
writeWorksheet(wb, howmany2[8], sheet = "result", startRow = 4, startCol =12 ,header=FALSE)
writeWorksheet(wb, howmany2[9], sheet = "result", startRow =5, startCol =12 ,header=FALSE)
writeWorksheet(wb, howmany2[10], sheet = "result", startRow = 6, startCol =12 ,header=FALSE)
writeWorksheet(wb, howmany2[11], sheet = "result", startRow = 7, startCol =12 ,header=FALSE)
writeWorksheet(wb, howmany2[12], sheet = "result", startRow = 8, startCol =12 ,header=FALSE)





saveWorkbook(wb)
	

