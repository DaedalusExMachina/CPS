install.packages("lavaan")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("qgraph")
install.packages("semPlot")
install.packages("simsem")
install.packages("semTools")
install.packages("dplyr")

library(lavaan)
library(corrplot)
library(Hmisc)
library(qgraph)
library(semPlot)
library(simsem)
library(semTools)
library(dplyr)

mydata <- read.csv2("E:26102016/MADATA2.csv", header=TRUE, sep=";")
mydata[mydata==-77]<-NA

mydata2 <- mydata %>% mutate(rk = (rk1+rk2+rk3)/15)

nu <- mydata %>% filter(!is.na(MA_rec))
nu2 <- mydata %>% filter(votat1 > 0)

sapply(mydata, sd)
summary(mydata)

cps.aov <-aov(int ~ vo11, data=mydata)
summary(cps.aov)
par(mfrow = c(2,2))
plot(cps.aov)

boxplot(int ~ vo11, data= mydata)

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		cps=~rk+ra

		SK =~ Q1+Q2+Q3+Q4+Q5
		#cps=~acq+rk+ra 

		votat ~ a*SK + b*int
		acq~va*votat +aa*SK + bb*int
		cps~vb*acq +aaa*SK + bbb*int		
		
		SKA:=a*va+aa
		SKAA:=(a*va+aa)*vb+aaa

		INTB:= b*va + bb
		INTBB:=(b*va+bb)*vb+bbb

		VV := va*vb

		int ~~ SK
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

measurementInvariance(sem, data=mydata, group = "SK_cat2")

standardizedSolution(fit1)
fitted(fit1)
#coef(fit1)
inspect(fit1)
reliability(fit1)
modindices(fit1, alpha = 0.05)

sem<- '

	INT1 =~ i1+i2+i3+i4+i5+i6+i7+i8+i9+i10
	INT2 =~ i12+i13+i14+i15+i16+i17+i18+i19+i20
	INT3 =~ i21+i22+i23+i24+i25+i26+i27+i28+i29+i30
	INT4 =~ i31+i32+i33+i34+i35+i36+i37+i38

	INT=~INT1+INT2+INT3+INT4

	ra=~ra1+ra2+ra3

	ra~INT
'

fit1 	<- 	cfa(sem, data=mydata, estimator="WLSM", 
		ordered=c(
		"i1","i2","i3","i4","i5","i6","i7","i8","i9","i10",
		"i11","i12","i13","i14","i15","i16","i17","i18","i19","i20",
		"i21","i22","i23","i24","i25","i26","i27","i28","i29","i30",
		"i31","i32","i33","i34","i35","i36","i37","i38"))

summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
fitted(fit1)
#coef(fit1)
inspect(fit1)
reliability(fit1)

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
			 
		acq~vot*votat 
		rk~acq_acq*acq 
		ra~rk_rk*rk	

		ra_ges:= acq_acq*rk_rk
		rk_vot:=vot*acq_acq
		ra_vot:=rk_vot*rk_rk

	

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
#fitted(fit1)
#coef(fit1)
#inspect(fit1)
reliability(fit1)
#modindices(fit1, alpha = 0.05)

sem<- '
		#SK=~Q1+Q2+Q3+Q4+Q5

		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		votat~vot_int*int 
		acq~vot*votat + acq_int*int 
		rk~acq_acq*acq +  rk_int*int 
		ra~rk_rk*rk	+ ra_int*int 


		int_acq:=vot_int*vot+acq_int
		int_rk:=acq_int*acq_acq+rk_int
		int_ra:=int_rk*rk_rk+ra_int

	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
#fitted(fit1)
#coef(fit1)
#inspect(fit1)
#reliability(fit1)
#modindices(fit1, alpha = 0.05)

sem<- '
		SK=~Q1+Q2+Q3+Q4+Q5

		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		votat~ vot_sk*SK
		acq~vot*votat + acq_sk*SK
		rk~acq_acq*acq + rk_sk*SK
		ra~rk_rk*rk	+ ra_sk*SK

	

		sk_acq:=vot_sk*vot+acq_sk
		sk_rk:=acq_sk*acq_acq+rk_sk
		sk_ra:=rk_sk*rk_rk + ra_sk


'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
fitted(fit1)
#coef(fit1)
inspect(fit1)
reliability(fit1)
modindices(fit1, alpha = 0.05)

sem<- '
		SK=~Q1+Q2+Q3+Q4+Q5

		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		votat~vot_int*int + vot_sk*SK
		acq~vot*votat + acq_int*int + acq_sk*SK
		rk~acq_acq*acq +  rk_int*int + rk_sk*SK
		ra~rk_rk*rk	+ ra_int*int + ra_sk*SK

		SK ~~ int

		int_acq:=vot_int*vot+acq_int
		int_rk:=acq_int*acq_acq+rk_int
		int_ra:=int_rk*rk_rk+ra_int

		sk_acq:=vot_sk*vot+acq_sk
		sk_rk:=acq_sk*acq_acq+rk_sk
		sk_ra:=rk_sk*rk_rk + ra_sk
		
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
fitted(fit1)
#coef(fit1)
inspect(fit1)
reliability(fit1)
modindices(fit1, alpha = 0.05)

sem<- '
		SK=~Q1+Q2+Q3+Q4+Q5

		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		votat~a*int + a*SK
		acq~votat + b*int + b*SK
		rk~acq +  c*int + c*SK
		ra~rk	+ d*int + d*SK

		SK ~~ int

	

'

fit2 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit2, fit.measures= TRUE, standardized=TRUE)

anova(fit1, fit2)

sem<- '
		SK=~Q1+Q2+Q3+Q4+Q5

		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		votat~a*int + b*SK
		acq~votat + a*int + b*SK
		rk~acq +  a*int + b*SK
		ra~rk	+ a*int + b*SK

		SK ~~ int

	

'

fit2 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5" ))
summary(fit2, fit.measures= TRUE, standardized=TRUE)

anova(fit1, fit2)

sem<- '

	vot1=~vo11+vo12+vo13+vo14
	vot2=~vo21+vo22+vo23+vo24
	vot3=~vo31+vo32+vo33+vo34

	INT=~int	
	SK=~Q1+Q2+Q3+Q4+Q5	
'

fit1 <- 	sem(sem, data=mydata, estimator="WLSM", 
		ordered=c("vo11","vo12","vo13","vo14",
				"vo21","vo22","vo23","vo24",
				"vo31","vo32","vo33","vo34",
				"Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
#inspect(fit1,'r2')
standardizedSolution(fit1)
#fitted(fit1)
#coef(fit1)
#inspect(fit1)
#reliability(fit1)

sem<- '

	v1=~vo11+vo21+vo31
	v2=~vo12+vo22+vo32
	v3=~vo13+vo23+vo33
	v4=~vo14+vo24+vo34

	#INT=~int	
	SK=~Q1+Q2+Q3+Q4+Q5

	v1~int+SK 
	v2~int+SK 
	v3~int+SK 
	v4~int+SK 

	int~~SK 
'

fit2 <- 	sem(sem, data=mydata, estimator="WLSM", 
		ordered=c("vo11","vo12","vo13","vo14",
				"vo21","vo22","vo23","vo24",
				"vo31","vo32","vo33","vo34",
				"Q1","Q2","Q3","Q4","Q5"))
summary(fit2, fit.measures= TRUE, standardized=TRUE)
inspect(fit2,'r2')
standardizedSolution(fit1)
#fitted(fit1)
#coef(fit1)
#inspect(fit1)
#reliability(fit1)


sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ zech_acq1+zech_acq2+zech_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		acq~votat
		rk~acq
		ra~rk

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
modindices(fit1, alpha = 0.05)
reliability(fit1)


sem<- '
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		cps=~rk+ra

SK=~Q1+Q2+Q3+Q4+Q5
'

fit1 <- sem(sem, data=mydata, estimator="WLSM")
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
reliability(fit1)

sem<- '
		task=~rk1+rk2+rk3+ra1+ra2+ra3
		
	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
reliability(fit1)

sem<- '
	vot=~votat1+votat2+votat3
	acq=~zech_acq1+zech_acq2+zech_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	SK=~Q1+Q2+Q3+Q4+Q5
	int ~ SK + vot + acq + rk + ra
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '
	vot=~votat1+votat2+votat3
	acq=~zech_acq1+zech_acq2+zech_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3


	vot ~  int
	acq ~  int + vot
	rk ~ int + acq
	ra ~ int + rk


'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '

	SK=~Q1+Q2+Q3+Q4+Q5

	#votat=~votat1+votat2+votat3
	acq=~zech_acq_stand1+zech_acq_stand2+zech_acq_stand3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3
	
	#votat~int+SK
	acq~int+SK
	rk~int+SK
	ra~int+SK
	
	SK~~int
'

fit1 <- sem(sem, data=mydata, estimator="WLSM",
		group="dich_vot")
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '

	SK=~Q1+Q2+Q3+Q4+Q5

	rk=~rk1+rk2+rk3
	c_rk=~c_rk1+c_rk2+c_rk3
	s_rk=~s_rk1+s_rk2+s_rk3

	rk~int + SK
	c_rk~int + SK + rk
	s_rk~int + SK + rk + c_rk

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '

	SK=~Q1+Q2+Q3+Q4+Q5

	acq=~zech_acq1+zech_acq2+zech_acq3
	c_acq=~conf_acq1+conf_acq2+conf_acq3
	s_acq=~sec_acq1+sec_acq2+sec_acq3

	acq~int + SK
	c_acq~int + SK + acq
	s_acq~int + SK + acq + c_acq

	SK~~int
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
	ordered=c("conf_acq1", "conf_acq2", "conf_acq3", 
	"Q1","Q2","Q3","Q4","Q5",
	"sec_acq1", "sec_acq2", "sec_acq3"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')



sem<- '

	ra11~~cra11
	ra12~~cra12
	ra13~~cra13
	ra14~~cra14
	ra15~~cra15

	ra12~ra11 + int
	ra13~ra12 + int
	ra14~ra13 + int
	ra15~ra14 + int

	cra12~cra11 + int
	cra13~cra12 + int
	cra14~cra13 + int
	cra15~cra14 + int
	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
	ordered=c("ra11","ra12","ra13","ra14","ra15",
			"cra11","cra12","cra13","cra14","cra15",
			"Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')




sem<- '
	vot=~votat1+votat2+votat3
	acq=~zech_acq1+zech_acq2+zech_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3
	
	SK=~Q1+Q2+Q3+Q4+Q5


	cps=~vot+acq+rk+ra


	cps~~1*cps
	cps~SK+ int
	SK~~int
'

fit1 <- sem(sem,data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '

VOT1 =~ vo11 + vo12 + vo13 + vo14
VOT2 =~ vo21 + vo22 + vo23 + vo24
VOT3 =~ vo31 + vo32 + vo33 + vo34

VOTAT=~VOT1+VOT2+VOT3

acq=~zech_acq1+zech_acq2+zech_acq3

RK1 =~ rk11+rk12+rk13+rk14+rk15
RK2 =~ rk21+rk22+rk23+rk24+rk25
RK3 =~ rk31+rk32+rk33+rk34+rk35

RK =~RK1+RK2+RK3

RA1 =~ ra11+ra12+ra13+ra14+ra15
RA2 =~ ra21+ra22+ra23+ra24+ra25
RA3 =~ ra31+ra32+ra33+ra34+ra35

RA =~RA1+RA2+RA3

#cps=~VOTAT+acq+RK+RA

#cps~SK + iq
SK=~Q1+Q2+Q3+Q4+Q5

VOTAT~iq + SK
acq~VOTAT+iq + SK
RK~acq+iq + SK
RA~RK+iq + SK

iq~~SK

iq~~225*iq
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
	ordered=c("vo11","vo12","vo13","vo14",
		"vo21","vo22","vo23","vo24",
		"vo31","vo32","vo33","vo34",
		"rk11","rk12","rk13","rk14","rk15",
		"rk21","rk22","rk23","rk24","rk25",
		"rk31","rk32","rk33","rk34","rk35",
		"ra11","ra12","ra13","ra14","ra15",
		"ra21","ra22","ra23","ra24","ra25",
		"ra31","ra32","ra33","ra34","ra35",
		"Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')



sem<- '
	vot=~a*votat1+a*votat2+a*votat3
	acq=~b*zech_acq1+b*zech_acq2+b*zech_acq3
	rk=~c*rk1+c*rk2+c*rk3
	ra=~d*ra1+d*ra2+d*ra3

	SK=~e*Q1+e*Q2+e*Q3+e*Q4+Q5

	vot ~  SK + int 
	acq ~  SK + int 
	rk ~ SK + int 
	ra ~ SK + int

	int~~SK 

	vot~~1*vot
	acq~~1*acq
	rk~~1*rk
	ra~~1*ra
	SK~~1*SK

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)


sem<- '

	SK=~Q1+Q2+Q3+Q4+Q5
	vot =~ votat1+votat2+votat3
	cacq=~conf_acq1+conf_acq2+conf_acq3
	crk=~c_rk1+c_rk2+c_rk3
	cra=~c_ra1+c_ra2+c_ra3
	

	vot ~ int + SK
	cacq ~ int + SK
	crk ~ int+ SK
	cra ~ int + SK

	int~~SK

	conf =~ cacq+crk+cra
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
		ordered=c("conf_acq1","conf_acq2", "conf_acq3","Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)



sem<- '

	INT1 =~ 	a*i1+a*i2+a*i3+a*i4+a*i5+a*i6+a*i7+a*i8+a*i9+a*i10+
			a*i11+a*i12+a*i13+a*i14+a*i15+a*i16+a*i17+a*i18+a*i19+a*i20+
			a*i21+a*i22+a*i23+a*i24+a*i25+a*i26+a*i27+a*i28+a*i29+a*i30+
			a*i31+a*i32+a*i33+a*i34+a*i35+a*i36+a*i37+a*i38

	i1~~b*i1
	i2~~b*i2
	i3~~b*i3
	i4~~b*i4
	i5~~b*i5
	i6~~b*i6
	i7~~b*i7
	i8~~b*i8
	i9~~b*i9
	i10~~b*i10
	i11~~b*i11
	i12~~b*i12
	i13~~b*i13
	i14~~b*i14
	i15~~b*i15
	i16~~b*i16
	i17~~b*i17
	i18~~b*i18
	i19~~b*i19
	i20~~b*i20
	i21~~b*i21
	i22~~b*i22
	i23~~b*i23
	i24~~b*i24
	i25~~b*i25
	i26~~b*i26
	i27~~b*i27
	i28~~b*i28
	i29~~b*i29
	i30~~b*i30
	i31~~b*i31
	i32~~b*i32
	i33~~b*i33
	i34~~b*i34
	i35~~b*i35
	i36~~b*i36
	i37~~b*i37
	i38~~b*i38
	
	SK=~Q1+Q2+Q3+Q4+Q5
'

fit1 	<- 	sem(sem, data=mydata, estimator="WLSM", 
		ordered=c(
		"i1","i2","i3","i4","i5","i6","i7","i8","i9","i10",
		"i11","i12","i13","i14","i15","i16","i17","i18","i19","i20",
		"i21","i22","i23","i24","i25","i26","i27","i28","i29","i30",
		"i31","i32","i33","i34","i35","i36","i37","i38"))

summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
fitted(fit1)
#coef(fit1)
inspect(fit1)
reliability(fit1)

sem<- '
	SK=~Q1+Q2+Q3+Q4+Q5

	#ra1 ~~ ra2 + ra3
	#ra2 ~~ ra3

	ra1~int + SK
	ra2~int + SK + ra1
	ra3~int + SK + ra2 + ra1

	int~~SK
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
		ordered=c("conf_acq1","conf_acq2", "conf_acq3","Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)

sem<- '
	SK=~Q1+Q2+Q3+Q4+Q5

	#ra1 ~~ ra2 + ra3
	#ra2 ~~ ra3

	rk1~int + SK
	rk2~int + SK + rk1
	rk3~int + SK + rk2 + rk1

	int~~SK
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", 
		ordered=c("conf_acq1","conf_acq2", "conf_acq3","Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)

sem<- '
	
	SK=~Q1+Q2+Q3+Q4+Q5

	votat=~votat1+votat2+votat3
	acq=~zech_acq_stand1+zech_acq_stand2+zech_acq_stand3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	cps=~votat+acq+rk+ra

	SK ~ int 

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", std.lv=T,
		ordered=c("conf_acq1","conf_acq2", "conf_acq3","Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)

sem<- '
	
	#SK=~Q1+Q2+Q3+Q4+Q5
	votat=~votat1+votat2+votat3
	acq=~acq1+acq2+acq3
	wacq=~wacq1+wacq2+wacq3

	acq~votat
	wacq~votat


'

fit1 <- sem(sem, data=mydata, estimator="WLSM", std.lv=T,
		ordered=c("conf_acq1","conf_acq2", "conf_acq3","Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)


sem<- '
	
	votat=~votat1+votat2+votat3
	acq=~zech_acq_stand1+zech_acq_stand2+zech_acq_stand3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3
	INT=~kint1+kint2+kint3+kint4
	SK=~Q1+Q2+Q3+Q4+Q5

	votat~INT+SK
	acq~INT+SK
	rk~INT+SK
	ra~INT+SK

	#cps=~acq+rk+ra+votat
	#cps~SK+INT
'

fit1 <- sem(sem, data=mydata, estimator="WLSM")
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)



sem2<- '

	d =~	a*i1+a*i2+a*i3+a*i4+a*i5+a*i6+a*i7+a*i8+a*i9+a*i10+
			a*i11+a*i12+a*i13+a*i14+a*i15+a*i16+a*i17+a*i18+a*i19+a*i20+
			a*i21+a*i22+a*i23+a*i24+a*i25+a*i26+a*i27+a*i28+a*i29+
			a*i31+a*i32+a*i33+a*i34+a*i35+a*i36+a*i37+a*i38

	votat=~votat1+votat2+votat3
	acq=~zech_acq_stand1+zech_acq_stand2+zech_acq_stand3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	SK=~Q1+Q2+Q3+Q4+Q5

	votat~d + SK
	acq~d + SK
	rk~d + SK
	ra~d + SK

	i1~~b*i1
	i2~~b*i2
	i3~~b*i3
	i4~~b*i4
	i5~~b*i5
	i6~~b*i6
	i7~~b*i7
	i8~~b*i8
	i9~~b*i9
	i10~~b*i10
	i11~~b*i11
	i12~~b*i12
	i13~~b*i13
	i14~~b*i14
	i15~~b*i15
	i16~~b*i16
	i17~~b*i17
	i18~~b*i18
	i19~~b*i19
	i20~~b*i20
	i21~~b*i21
	i22~~b*i22
	i23~~b*i23
	i24~~b*i24
	i25~~b*i25
	i26~~b*i26
	i27~~b*i27
	i28~~b*i28
	i29~~b*i29
	i31~~b*i31
	i32~~b*i32
	i33~~b*i33
	i34~~b*i34
	i35~~b*i35
	i36~~b*i36
	i37~~b*i37
	i38~~b*i38

	
'

fit2 <- sem(sem2, data=mydata, estimator="WLSM")
summary(fit2, fit.measures= TRUE, standardized=TRUE)

anova(fit1, fit2)




sem<- '
	vot	=~ votat1+votat2+votat3
	acq	=~ zech_acq_stand1 +  zech_acq_stand2 + zech_acq_stand3
	rk	=~ rk1 + rk2 + rk3
	ra	=~ ra1 + ra2 + ra3

	reas =~ kint1+kint2+kint3+kint4
	SK=~Q1+Q2+Q3+Q4+Q5

	cps=~vot+acq+rk+ra

	g =~ reas+SK+cps

	#vot~reas + SK
	#acq~reas + SK
	#rk~reas + SK
	#ra~reas + SK	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM")
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '
	vot	=~ votat1+votat2+votat3
	acq	=~ zech_acq_stand1 +  zech_acq_stand2 + zech_acq_stand3
	rk	=~ rk1 + rk2 + rk3
	ra	=~ ra1 + ra2 + ra3

	reas =~ kint1+kint2+kint3+kint4
	SK=~Q1+Q2+Q3+Q4+Q5

	cps=~vot+acq+rk+ra

	#SK ~ cps + reas	
'

fit1 <- sem(sem, data=mydata2, estimator="WLSM")
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '
	vot	=~ votat1+votat2+votat3
	acq	=~ dp_acq1 + dp_acq2+dp_acq3
	rk	=~ rk1 + rk2 + rk3
	ra	=~ ra1 + ra2 + ra3
	reas =~ kint1+kint2+kint3+kint4
	sc=~Q1+Q2+Q3+Q4+Q5
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '

	sc=~Q1+Q2+Q3+Q4+Q5

	vot	=~ votat1+votat2+votat3
	acq	=~ dp_acq1 + dp_acq2+dp_acq3
	rk	=~ rk1 + rk2 + rk3
	ra	=~ ra1 + ra2 + ra3

	#cps=~vot+acq+rk+ra

	reas =~ kint1+kint2+kint3+kint4

	vot~sc+reas
	acq~sc+reas
	rk~sc+reas
	ra~sc+reas
	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '


	#acq_hit =~ acq1_hit+acq2_hit+acq3_hit
	acq_fa =~ acq1_FA + acq2_FA + acq3_FA
	#acq_fn =~ acq1_FN + acq2_FN + acq3_FN
	#acq_cr=~ acq1_CR + acq2_CR + acq3_CR

'

fit1 <- sem(sem, data=mydata, estimator="WLSM",ordered=c("acq1_hit","acq1_FA","acq1_FN","acq1_CR",
			"acq2_hit","acq2_FA","acq2FN","acq2CR",
			"acq3_hit","acq3FA","acq3FN","acq3CR")
)
summary(fit1, fit.measures= TRUE, standardized=TRUE)
inspect(fit1,'r2')

sem<- '

	vot=~votat1+votat2+votat3
	acq=~zech_acq1+zech_acq2+zech_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	cps=~vot+acq+rk+ra

	ma~reas+sc

	ma=~MA_rec
	#de=~DE_rec
	#en=~EN_rec

	cps~reas+sc

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint2+kint2+kint3+kint4

	cps~~ma

	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit1,'r2')
standardizedSolution(fit1)
mi<-modindices(fit1,sort=T, standardize=T, minimum.value=3.84)
mi[mi$op == "~~",]


sem<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint2+kint2+kint3+kint4

	vot~sc+reas
	acq~sc+reas
	rk~sc+reas
	ra~sc+reas
	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit1,'r2')

sem2<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint2+kint2+kint3+kint4

	vot~b*sc+a*reas
	acq~b*sc+a*reas
	rk~b*sc+a*reas
	ra~b*sc+a*reas
	
'

fit2 <- sem(sem2, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit2, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit2,'r2')

mi<-modindices(fit2,sort=T, standardize=T, minimum.value=3.84)
mi[mi$op == "~~",]

anova(fit1, fit3)

sem3<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint1+kint2+kint3+kint4

	vot~sc+reas
	acq~sc+reas
	rk~sc+reas
	ra~sc+reas
	
'

fit3 <- sem(sem3, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit3, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit3,'r2')
mi<-modindices(fit3,sort=T, standardize=T, minimum.value=3.84)
mi

sem4<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint1+kint2+kint3+kint4

	vot~sc+reas
	acq~a*sc+b*reas
	rk~a*sc+b*reas
	ra~sc+b*reas
	
'

fit4 <- sem(sem4, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit4, fit.measures= TRUE, standardized=TRUE)
lavaan::inspect(fit4, 'r2')
mi<-modindices(fit4,sort=T, standardize=T, minimum.value=3.84)
mi
standardizedSolution(fit4)
anova(fit3, fit4)

sem5<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint2+kint2+kint3+kint4

	vot~a*sc+b*reas
	acq~a*sc+b*reas
	rk~a*sc+b*reas
	ra~a*sc+b*reas
	
'

fit5 <- sem(sem5, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit5, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit5,sort=T, standardize=T, minimum.value=3.84)
mi
anova(fit4, fit5)


sem1<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint2+kint2+kint3+kint4

	cps=~vot+acq+rk+ra

	cps~reas+sc
	
'

fit1 <- sem(sem1, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)



sem2<- '

	vot=~votat1+votat2+votat3
	acq=~dp_acq1+dp_acq2+dp_acq3
	rk=~rk1+rk2+rk3
	ra=~ra1+ra2+ra3

	sc=~Q1+Q2+Q3+Q4+Q5
	reas=~kint1+kint2+kint3+kint4

	cps=~vot+a*acq+a*rk+ra

	cps~reas+sc

	ra~~reas
	vot~~sc
	
'

fit2 <- sem(sem2, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5","conf_acq1","conf_acq2","conf_acq3","sec_acq1","sec_acq2","sec_acq3"))
summary(fit2, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit2,sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
anova(fit1, fit2)


####REVISION

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		#cps=~votat+acq+rk+ra

		SK =~ Q1+Q2+Q3+Q4+Q5
		reas=~kint1+kint2+kint3+kint4
		#reas=~iq_z_rr
		
		#cps~SK+reas

		#reas~cps+SK

		#votat~SK+reas
		#acq~SK+reas
		#rk~SK+reas
		#ra~SK+reas

		SK~votat+acq+rk+ra
		reas~votat+acq+rk+ra

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
inspect(fit1,'r2')

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		#cps=~votat+acq+rk+ra

		SK =~ Q1+Q2+Q3+Q4+Q5
		#reas=~kint1+kint2+kint3+kint4
		reas=~iq_z_rr
		
		#cps~SK+reas

		votat~SK+reas
		acq~SK+reas
		rk~SK+reas
		ra~SK+reas

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
inspect(fit1,'r2')


sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
		
		cps=~votat+acq+rk+ra

		SK =~ Q1+Q2+Q3+Q4+Q5
		reas=~kint1+kint2+kint3+kint4

		#acq~~ra
		#votat~~rk
		#acq~~rk
		#votat~~ra

		cps~reas + SK

		#Q4~~Q5
		#votat~~rk
		#Q2~~Q5
		#kint1~~kint4 + kint3

		ra~~reas
		votat~~SK
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
inspect(fit1,'r2')


sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3


		#cps=~votat+acq+rk+ra
		#cps=~ votat + acq+rk+ra + reas + SK
		
		reas=~kint1+kint2+kint3+kint4
		SK=~Q1+Q2+Q3+Q4+Q5

		SK~reas

		#strat =~cps+reas + SK

		#cps ~ reas  + SK + votat

		votat~~SK
		acq~~SK
		rk~~SK
		ra~~SK

		#votat~~reas
		#acq~~reas
		#rk~~reas
		#ra~~reas

		#votat~~acq+rk+ra
		#acq~~rk+ra
		#rk~~ra

		votat~reas
		acq~reas
		rk~reas
		ra~reas
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
lavaan::inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
	
		sc=~Q1+Q2+Q3+Q4+Q5

		reas=~kint1+kint2+kint3+kint4

		cps=~votat+acq+rk+ra

		cps~sc+reas

		#ra~~reas
		#votat~~rk + sc

		#Q5 ~~ kint2 + kint4
		

'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
lavaan::inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)

sem<- '
		votat =~ votat1+votat2+votat3
		acq=~ dp_acq1+dp_acq2+dp_acq3
		rk=~rk1+rk2+rk3
		ra=~ra1+ra2+ra3
	
		rkra=~rk+ra

		sc=~Q1+Q2+Q3+Q4+Q5

		reas=~kint1+kint2+kint3+kint4

		votat~reas+sc
		acq~reas+sc
		rkra~reas+sc

	
'

fit1 <- sem(sem, data=mydata, estimator="WLSM", ordered=c("Q1","Q2","Q3","Q4","Q5"))
summary(fit1, fit.measures= TRUE, standardized=TRUE)
mi<-modindices(fit1, sort=T, standardize=T, minimum.value=3.84)
mi[mi$op =="~~",]
lavaan::inspect(fit1,'r2')
standardizedSolution(fit1)
reliability(fit1)


mydata$ra_diff_12 <- mydata$ra1 - mydata$ra2

var<-mydata$ra3

mean(var)
sd(var)

sem<- '
	
int=~ 1*kint1 + 1*kint2 + 1*kint3 + 1*kint4
slope=~ 0*kint1 + 1*kint2 + 2*kint3 + 3*kint4
	
'

fit1 <- growth(sem, data=mydata, estimator="ML")
summary(fit1, fit.measures= TRUE, standardized=TRUE)

sem<- '
	
int=~ 1*ra1 + 1*ra2+ 1*ra3
slope=~ 0*ra1 + 1*ra2 + 2*ra3
	
'

fit1 <- growth(sem, data=mydata, estimator="WLSM")
summary(fit1, fit.measures= TRUE, standardized=TRUE)


anova(ra1, ra2, data)

hist(mydata$ra1, main="Histogramm Anwendungsaufgaben", xlab="Korrekte Lösungen", ylab="Anzahl der Probanden")
