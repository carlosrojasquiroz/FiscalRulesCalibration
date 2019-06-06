cd "E:\IAPM"

Call Setup 
Call VARsystem
Call Forecasting
Call Output

'------------------------------------------------------------------------------------------------------------------------------------------------------------
Subroutine Setup
'------------------------------------------------------------------------------------------------------------------------------------------------------------
create(wf=DSA_Peru) q 2000.1 2028.4
import(resize) "E:\IAPM\Base_DSA.xlsx" range=Base @freq q 2000q1
smpl @all
import(resize) "E:\IAPM\Base_DSA.xlsx" range=Anual @freq a 2000
import(resize) "E:\IAPM\Base_DSA.xlsx" range=FiscalE @freq q 2000q1
stom(afs,sfa) 
for %x 1 2 3 4
stom(errRP{%x},RPerr{%x})
next 
delete afs series01 errRP1 errRP2 errRP3 errRP4
'------------------------------------------------------------------------------
'Some important parameters
'------------------------------------------------------------------------------
'a) Number of variables 
scalar nvar=5 'from the VAR system
scalar ntot=11'total number of variables
'b) Number of forecast periods
scalar yhorz=6 'annually        
scalar nhorz=yhorz*4 'quarterly
'c) Number of simulations      
scalar nsimul=10000
'd) Number of percentiles (from 5 to 95, 10 by 10) plus one  
scalar nperc=10
'e) Share of foreign-denominated debt on total public debt
scalar ext_share=0.32
'f) Likelihood of public debt >=debtlimit
scalar num = 0
'g) Likelihood of re>=relimit
scalar proba=0
'h) Stock-flow adjustment (if yes Adj=1)
scalar Adj=0
'i) Fiscal uncertainty (if yes Fu=1)
scalar Fu=0
'------------------------------------------------------------------------------
'h) Initial public debt
scalar publicdebt0=25.8
'i) Debt limit
scalar debtlimit=40
'------------------------------------------------------------------------------
Endsub
'------------------------------------------------------------------------------------------------------------------------------------------------------------
Subroutine VARsystem
'------------------------------------------------------------------------------------------------------------------------------------------------------------
'Changing the name of the macroeconomic variables 
series var1=ipx 
series var2=rme
'------------------------------------------------------------------------------
'You can change var3 between GDP and non-commidity GDP
'------------------------------------------------------------------------------
series var3=pbi 
series var4=rmn
series var5=q
'Changing the name of the fiscal variables
series var6=rp
series var7=deppub
'Changing the name of gap variables
'------------------------------------------------------------------------------
'You can change var9 and var9n between GDP and non-commidity GDP
'------------------------------------------------------------------------------
series var8=ipx_cyc
series var9=pbinp_cyc
series var8n=ipx_n
series var9n=pbinp_n
'Changing the name of auxiliary fiscal variables
series var10=int
series var11=re
'Changing the name of inflation
series var13=picusa
series var14=dep
series var15=pic
series var17=defl
'------------------------------------------------------------------------------
'Building the macroeconomic system
'------------------------------------------------------------------------------
smpl 2002.1 2018.4
system macro1
macro1.append var1=C(1)*var1(-1)+C(2)*var1(-2)+C(3)
macro1.append var2=C(4)*var2(-1)+C(5)
macro1.append var3=C(6)*var3(-1)+C(7)*var4(-1)+C(8)*var5(-1)+C(9)*var1+C(10)*var2+C(11)
macro1.append var4=C(12)*var3(-1)+C(13)*var4(-1)+C(14)*var5(-1)+C(16)*var1+C(17)*var2+C(18)
macro1.append var5=C(19)*var3(-1)+C(20)*var4(-1)+C(21)*var5(-1)+C(22)*var1+C(23)*var2+C(24)
macro1.ls(cov=hac)
'Making resids from the macroeconomic equations 
macro1.makeresids resid01 resid02 resid03 resid04 resid05
'Fiscal reaction functions
equation eqFRF.ls var6=C(25)*var6(-1)+C(26)*var6(-2)+C(27)*var8+C(28)*var9(-2)+C(29)*var7(-1)+C(30)
eqFRF.makeresids resid06
scalar Numel=resid06.@obs
Endsub
'------------------------------------------------------------------------------------------------------------------------------------------------------------
Subroutine Forecasting
'------------------------------------------------------------------------------------------------------------------------------------------------------------
for !sim=1 to nsimul
'------------------------------------------------------------------------------
'Resample resids
'------------------------------------------------------------------------------
smpl @all
for !x=1 to nvar+1
stom(resid0{!x}, resid0{!x}_b)
matrix(nhorz,1) BootErr_{!x}
BootErr_{!x}=@resample(resid0{!x}_b,nhorz-numel)
next  
matrix(yhorz,1) BootErr_7_1
BootErr_7_1=@resample(sfa,-13)
matrix(nhorz,1) BootErr_7
BootErr_7=0
BootErr_7(4,1)=BootErr_7_1(1)
BootErr_7(8,1)=BootErr_7_1(2)
BootErr_7(12,1)=BootErr_7_1(3)
BootErr_7(16,1)=BootErr_7_1(4)
BootErr_7(20,1)=BootErr_7_1(5)
BootErr_7(24,1)=BootErr_7_1(6)
delete BootErr_7_1
'------------------------------------------------------------------------------
smpl 2019.1 @last
stom(guiaRE,oblimit)
series var17=var15
'------------------------------------------------------------------------------
smpl @all
matrix(4,1) BootErr_8_1
BootErr_8_1=@resample(rpErr1,-15)
matrix(4,1) BootErr_8_2
BootErr_8_2=@resample(rpErr2,-14)
matrix(4,1) BootErr_8_3
BootErr_8_3=@resample(rpErr3,-13)
matrix(12,1) BootErr_8_4
BootErr_8_4=@resample(rpErr4,-10)
matrix(nhorz,1) BootErr_8
BootErr_8=0
BootErr_8(4,1)=BootErr_8_1(1)
BootErr_8(8,1)=BootErr_8_2(1)
BootErr_8(12,1)=BootErr_8_3(1)
BootErr_8(16,1)=BootErr_8_4(1)
BootErr_8(20,1)=BootErr_8_4(2)
BootErr_8(24,1)=BootErr_8_4(3)
delete BootErr_8_1 BootErr_8_2 BootErr_8_3 BootErr_8_4
'------------------------------------------------------------------------------
smpl 2018.4 2018.4
series var7=publicdebt0
'------------------------------------------------------------------------------
for !obs=1 to nhorz
%date = @otod(@dtoo("2018.4")+!obs)
smpl %date %date
matrix(nvar,1) Epsilon
scalar Epsilon1=BootErr_1(!obs,1)
scalar Epsilon2=BootErr_2(!obs,1)
scalar Epsilon3=BootErr_3(!obs,1)
scalar Epsilon4=BootErr_4(!obs,1)
scalar Epsilon5=BootErr_5(!obs,1)
scalar Epsilon6=BootErr_6(!obs,1)
scalar Epsilon7=BootErr_7(!obs,1)
scalar Epsilon8=BootErr_8(!obs,1)
'-------------------------------------------------------------------------------------
'Building the macroeconomic variables 'nhorz' periods ahead
'-------------------------------------------------------------------------------------
'Export price index
'-------------------------------------------------------------------------------------
series var1=C(1)*var1(-1)+C(2)*var1(-2)+C(3)+Epsilon1
stom(var1,x1)
matrix(nhorz,nsimul) mat1
mat1(!obs,!sim)=x1(1)
'-------------------------------------------------------------------------------------
'Export prices gap (Baxter and King filter)
'-------------------------------------------------------------------------------------
series var8n=var8n(-4)*(1+x1(1)/100)
for !j=1 to 12
%date_2 = @otod(@dtoo("2018.4")+!obs+!j)
smpl %date_2 %date_2
series var8n=var8n(-1)
next
smpl @first @last
series lipx=log(var8n)
lipx.bpf var8
smpl %date %date
stom(var8,x8)
matrix(nhorz,nsimul) mat8
mat8(!obs,!sim)=x8(1)
'-------------------------------------------------------------------------------------
'Foreign real interest rate
'-------------------------------------------------------------------------------------
series var2=C(4)*var2(-1)+C(5)+Epsilon2
stom(var2,x2)
matrix(nhorz,nsimul) mat2
mat2(!obs,!sim)=x2(1)
'Zero lower bound on r*
if x2(1)<0 then
series var2=0
endif
'-------------------------------------------------------------------------------------
'GDP
'-------------------------------------------------------------------------------------
series var3=C(6)*var3(-1)+C(7)*var4(-1)+C(8)*var5(-1)+C(9)*var1+C(10)*var2+C(11)+Epsilon3
stom(var3,x3)
matrix(nhorz,nsimul) mat3
mat3(!obs,!sim)=x3(1)
'-------------------------------------------------------------------------------------
'Output gap (Baxter and King filter)
'-------------------------------------------------------------------------------------
series var9n=var9n(-4)*(1+x3(1)/100)
for !j=1 to 12
%date_2 = @otod(@dtoo("2018.4")+!obs+!j)
smpl %date_2 %date_2
series var9n=var9n(-4)*(1+x3(1)/100)
next
smpl @first @last
series lpbi=log(var9n)
lpbi.bpf var9
smpl %date %date
stom(var9,x9)
matrix(nhorz,nsimul) mat9
mat9(!obs,!sim)=x9(1)
'-------------------------------------------------------------------------------------
'Domestic real interest rate
'-------------------------------------------------------------------------------------
series var4=C(12)*var3(-1)+C(13)*var4(-1)+C(14)*var5(-1)+C(16)*var1+C(17)*var2+C(18)+Epsilon4
stom(var4,x4)
matrix(nhorz,nsimul) mat4
mat4(!obs,!sim)=x4(1)
'Zero lower bound on r
if x4(1)<0 then
series var4=0
endif
'-------------------------------------------------------------------------------------
'Real exchange rate
'-------------------------------------------------------------------------------------
series var5=C(19)*var3(-1)+C(20)*var4(-1)+C(21)*var5(-1)+C(22)*var1+C(23)*var2+C(24)+Epsilon5
stom(var5,x5)
matrix(nhorz,nsimul) mat5
mat5(!obs,!sim)=x5(1)
'-------------------------------------------------------------------------------------
'Interests
'-------------------------------------------------------------------------------------
series var18=((1+var4/100)*(1+var17/100)-1)*100
series var19=((1+var2/100)*(1+var17/100)/(1+var14/100)-1)*100
series var11=(1/((1+var3/100)*(1+var17/100)))*(var19/100*(1+var14/100)*ext_share*var7(-1)+var18/100*(1-ext_share)*var7(-1))
stom(var11,x11)
matrix(nhorz,nsimul) mat11
mat11(!obs,!sim)=x11(1)
'-------------------------------------------------------------------------------------
'Primary balance (PB)
'-------------------------------------------------------------------------------------
series var6=C(25)*var6(-1)+C(26)*var6(-2)+C(27)*var8+C(28)*var9(-2)+C(29)*var7(-1)+C(30)+Fu*Epsilon6
stom(var6,x6)
matrix(nhorz,nsimul) mat6
mat6(!obs,!sim)=x6(1)
'If PB is greater than zero, then it will derive to the accumulation of 
'public assets not for decreasing public debt
series var16=var6
if x6(1)>0 then
series var16=0
endif
'-------------------------------------------------------------------------------------
'Overall balance (OB)
'-------------------------------------------------------------------------------------
series var12=var6-var11
stom(var12,x12)
matrix(nhorz,nsimul) mat12
mat12(!obs,!sim)=x12(1)
if x12<=oblimit(!obs) then 
scalar proba=proba+1
endif
'-------------------------------------------------------------------------------------
'Public debt
'-------------------------------------------------------------------------------------
'Public debt dynamics
series var7=(1/(1+var3/100))*((1+var2/100)*(1+var5/100)*ext_share*var7(-1)+(1+var4/100)*(1-ext_share)*var7(-1))-var16+Adj*Epsilon7
stom(var7,x7)
matrix(nhorz,nsimul) mat7
mat7(!obs,!sim)=x7(1)
if x7>=debtlimit then 
scalar num=num+1
endif
'-------------------------------------------------------------------------------------
next
next
Endsub
'------------------------------------------------------------------------------------------------------------------------------------------------------------
Subroutine Output
'------------------------------------------------------------------------------------------------------------------------------------------------------------
'Likelihood of public debt>=debt limit in each year
matrix(nhorz/4,1) Pr_limit
matrix(1,nsimul)  Sr_limit
for !x=1 to yhorz
Sr_limit=@rowextract(mat7,!x*4)
for !y=1 to nsimul
if Sr_limit(1,!y)>=debtlimit then 
Sr_limit(1,!y)=1
else 
Sr_limit(1,!y)=0
endif 
next
Pr_limit(!x,1)=@sum(Sr_limit)/nsimul*100
next
'Likelihood of overall balance>=fiscal consolidation in each year
matrix(nhorz/4,1) Pb_limit
matrix(1,nsimul) Sb_limit
for !x=1 to yhorz
Sb_limit=@rowextract(mat12,!x*4)
for !y=1 to nsimul
if Sb_limit(1,!y)<=oblimit(1,!x*4) then 
Sb_limit(1,!y)=1
else 
Sb_limit(1,!y)=0
endif 
next
Pb_limit(!x,1)=@sum(Sb_limit)/nsimul*100
next
'-------------------------------------------------------------------------------------
for %x 1 2 3 4 5 6 7 11 12
for !obs=1 to nhorz
for !per= 1 to nperc
matrix(nhorz,nperc+1) matpe{%x}
matpe{%x}(!obs,!per)=@quantile(@rowextract(mat{%x},!obs),0.05+0.1*(!per-1))
next
matpe{%x}(!obs,nperc+1)=@quantile(@rowextract(mat{%x},!obs),0.50)
next
next
'Likelihood of public debt>=debt limit in all periods
scalar num=num/(nhorz*nsimul)*100
scalar proba=proba/(nhorz*nsimul)*100
Endsub
