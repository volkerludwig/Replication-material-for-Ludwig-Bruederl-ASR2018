/*
*==============================================================

Stata 14 Do-File #3 of 3 for Replication of article:

Ludwig, Volker and Josef Brüderl. 2018. 
"Is there a Marital Wage Premium? New Evidence from the United States".
American Sociological Review 83(4). 

*==============================================================
*/


//set working directory !!! Adapt this as needed !!!
global mwpUSdir "X:\mwp_US\data\" //where data are stored 
global resultsdir "X:\mwp_US\results" //where results of analysis shall be stored

/*
Note: You need to install 
- xtfeis package by Volker Ludwig 
	xtfeis from http://fmwww.bc.edu/RePEc/bocode/x
- estout package by Ben Jann 
	st0085_2 from http://www.stata-journal.com/software/sj14-2
*/

/*
Note: You find specific parts of the analysis by searching for Tables and Figures, 
e.g. search for "Table 1" to see the Stata code to construct the table
*/

cd $mwpUSdir
use mwp_US_analysis.dta, clear


replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)

cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)

for var exp expq tenure yeduc enrol : gen tm_X=tm*X


recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)


*=================================================================
*** Table A1: Selection of NLSY79 Sample 
*=================================================================

//sample 1

preserve
bys id (year) : replace sample1p=. if _n<_N
tab sample1p
tab sample1
tab sample1p if inlist(sample1p,-1,-2)==0
tab sample1 if inlist(sample1,-1,-2)==0

xttab treat1 if sample1==1

//Sample selection of person-years
for num -26/-3 \ num 26/3 : qui recode sample1 X=Y 
for num -26/-3 \ num 26/3 : qui recode sample1p X=Y 
qui recode sample1 9/12=9 19/23=19
qui recode sample1p 9/12=9 19/23=19	
	
estpost tab sample1 if inlist(sample1,-1,-2)==0 //Post results (pretend table contains estimation results) 
estout using "$resultsdir\TableA1_sample_a.doc", cells("b pct(fmt(2))") ///
	order(Total) collabels("Person-years" "%") mlabels( , none) modelwidth(10) ///
	varlabels( Total "All Men" ///
	1 "Sample 1" ///
	4 "Age below 16 or above 60"  ///
	5 "Currently not working"  ///
	6 "Self-employed"  ///
	7 "Working in public sector"  ///
	9 "Marital status invalid"  ///
	13 "Earlier marriage or married at entry"  ///
	14 "Seperated, divorced, widowed"  ///
	15 "Later than 15 years in 1st marriage"  ///
	16 "Hourly wage missing"  ///
	17 "Wage smaller .5 or larger 500 Euros"  ///
	19 "Covariates missing" ///
	25 "Not observed never-married (after listwise deletion)"  ///
	26 "Fewer than 4 valid person-years" ) ///
	replace
	

//Sample selection of persons
estpost tab sample1p if inlist(sample1p,-1,-2)==0 //Post results 
estout using "$resultsdir\TableA1_sample_b.doc", cells("b pct(fmt(2))") ///
	order(Total) collabels("Persons" "%") mlabels( , none) modelwidth(10) ///
	replace
restore



keep if sample1==1
	

*=================================================================
*** Table 1: Descriptive statistics
*=================================================================

//sample 1
//N years observed before and after marriage
bys id (year) : gen Nbefore=sum(sample1==1 & marry==0)
bys id (year) : replace Nbefore=. if _n<_N
bys id (year) : gen Nafter=sum(sample1==1 & marry==1)
bys id (year) : replace Nafter=. if _n<_N
//Earliest year observed before and latest year after marriage
ge md_dist=round((mardur_ts+6)/12) if treat1>0
bys id (year) : egen earlyyear=min(md_dist)
bys id (year) : egen lateyear=max(md_dist)
drop md_dist

   
preserve 
for var lnw marry mardur nchildm dchild1 dchild2 dchild3 dchild4 exp tenure yeduc enrol age : bys id (year) : gen first_X=X[1]
for var lnw marry mardur nchildm dchild1 dchild2 dchild3 dchild4 exp tenure yeduc enrol age : bys id (year) : gen last_X=X[_N]
for var lnw marry mardur nchildm dchild1 dchild2 dchild3 dchild4 exp tenure yeduc enrol age : bys id (year) : gen diff_X=last_X-first_X
bys id (year) : gen NT=sum(sample1==1)
bys id (year) : replace NT=. if _n<_N
ge d_lnw_exp=diff_lnw/diff_exp
tab treat1, su(d_lnw_exp)
for var lnw marry mardur nchildm dchild1 dchild2 dchild3 dchild4 exp tenure yeduc enrol age : replace diff_X=. if NT==.
estpost tabstat wage lnw marry mardur nchildm dchild1 dchild2 dchild3 dchild4 exp tenure yeduc enrol age cohort ///
				diff_lnw d_lnw_exp diff_marry diff_mardur diff_nchildm diff_dchild1 diff_dchild2 diff_dchild3 diff_dchild4 diff_exp diff_tenure diff_yeduc diff_enrol diff_age  ///
				Nbefore Nafter earlyyear lateyear ///
				if treat1<., by(treat1) stat(mean sd) col(stat)  
estout using "$resultsdir\Table1_desc_a.doc" , cells(mean(fmt(2)) sd(fmt(2) par)) unstack replace 
restore
tab sample1 treat1, m
xttab treat1
mat l r(results)
mat N=r(results)
mat l N
mat N1=N[1..4,4]
mat N2=N[1..4,2]
mat N3=N[1..4,5]
mat N4=N[1..4,3]
cap drop N?1
for num 1/4 : svmat NX, names(NX)
cap drop f
ge f=_n
estpost tabstat N11-N41 in 1/4, by(f) stat(mean) col(stat)
estout using "$resultsdir\Table1_desc_b.doc" , cells(mean(fmt(2)) sd(fmt(1) par)) unstack replace 
xtsum id if treat1==0
xtsum id if treat1==1
xtsum id if treat1==2
xtsum id if sample1==1




*=================================================================
*** 1) Table S3, Online Supplement, Part C. Full Regression results for Fig. 2 and 4
*** 2) Figure 2: Wage Profiles of Ever-married and Never-married Men
*** 3) Figure 4: Time-Path of the Marital Wage Premium
*=================================================================


// 1) Table S3, Online Supplement, Part C. Full Regression results for Fig. 2 and 4

cd $mwpUSdir

use mwp_US_analysis.dta, clear


replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)


cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)
for var exp expq pexp pexpq tenure yeduc enrol : gen tm_X=tm*X


recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)


cap drop help*
bys id (year) : gen help=sum(div==1 | wid==1 | remarry==1)
*bys id (year) : gen help=sum(div==1)
cap drop ever_div
bys id (year) : egen ever_div=max(help)
recode ever_div 1/max=1


keep if sample1==1

replace mardur=mardur*12

ge md_dist=round((mardur_ts+6)/12) if treat1>0
replace md_dist=15 if mardur_ts>=180 & mardur<.
/*
table md_dist, c(min mardur_ts mean mardur_ts med mardur_ts max mardur_ts freq)
table md_dist, c(min mardur mean mardur med mardur max mardur freq)
table treat1, c(min mardur_ts mean mardur_ts med mardur_ts max mardur_ts freq)

tab md_dist
tab md_dist treat1, col
tab md_dist if treat1==0
tab md_dist if treat1==1
tab md_dist if treat1==2
tab md_dist if treat1==3
*/
for num -10/15 \ num 0/25 : ge md_distY=md_dist==X if treat1>0
for num 0/25 : replace md_distX=0 if treat1==0
replace md_dist0=1 if treat1==0

ge md_dist2125=0
for num 21/25 : replace md_dist2125=1 if md_distX==1

replace expq=expq/100
replace tm_exp=tm*exp
replace tm_expq=tm*expq
ge tm_expc=tm*exp^3
*replace tenure=tenure/10
*replace tenureq=tenureq/100
ge mardurq=mardur^2
replace mardur=mardur/(12*10)
replace mardurq=mardurq/(12*100)


for var tm_pexp tm_pexpq \ var pexp pexpq : replace X=tm*Y


//distributed FE model
xtreg lnw md_dist11-md_dist20 md_dist2125 dchild2-dchild4 ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, vce(cluster id) fe
est store FEdist_US
	

// distributed FEGS model	
xtreg lnw md_dist11-md_dist20 md_dist2125 dchild2-dchild4 ///
	exp expq tm_exp tm_expq tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, vce(cluster id) fe
est store FEGSdist_US	
//Test for joint significance of interaction 
//time-constant treatment dummy and work experience	
test tm_exp tm_expq	



//compare distributed FEIS model
xtfeis lnw md_dist11-md_dist20 md_dist2125 dchild2-dchild4 ///
	tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id)
est store FEISdist_US

//FEIS model w/o controls
xtfeis lnw md_dist11-md_dist20 md_dist2125 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	, slope(pexp pexpq) cluster(id)
est store FEISdist_US_pexp


est table FEdist_US FEGSdist_US FEISdist_US FEISdist_US_pexp, star b(%9.4f)



*** Table S3, Online Supplement, Part C. 

estout FEdist_US FEGSdist_US FEISdist_US FEISdist_US_pexp using "$resultsdir\TableS3.doc", replace cells("b(star fmt(%9.3f))" "se(par fmt(%9.3f))") starlevels(* 0.05 ** 0.01 *** 0.001) ///
		stats(r2_w N_g Tbar N, labels("Within R-square" "N persons" "Mean T" "NT person-years") fmt(%9.2f %9.0f %9.1f %9.0f))  ///
		mlabels("") varwidth(40) modelwidth(22) legend ///
		title("") 




		
		
// 2) Figure 2: Wage Profiles of Ever-married and Never-married Men


*** Quadratic specification
xtreg lnw md_dist11-md_dist20 md_dist2125 dchild2 dchild3 dchild4  ///
	i.tm#(c.exp##c.exp) tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, vce(cluster id) fe
estat ic

global md_dist=""
forvalues d=11/20 {
	global md_dist="$md_dist md_dist`d'=(0)"
}
di "$md_dist"

global covar=""
foreach var of varlist dchild2 dchild3 dchild4   tenure yeduc enrol yeargr2-yeargr7 {
	qui sum `var'
	global covar="$covar `var'=(`r(mean)')"
}
di "$covar"


margins, at(exp=(0(1)35) tm=(0 1) $md_dist md_dist2125=(0) $covar ) post
est store FEGS_quad
marginsplot, recast(line) recastci(rline) plotdimension(tm)

* get predicted values of quadratic specification
mat b = r(table)'
mat l b
mat b = b[.,1..6]
local xvars : rownames b
local k=rowsof(b)
cap drop b se z pvalue ll ul
svmat b, names(col)
cap drop xb?
qui ge xb0=.
for num 1/36 : qui replace xb0=b[X] if exp==X-1
qui ge xb1=.
for num 37/`k' : qui replace xb1=b[X] if exp==X-1-36
line xb0 xb1 exp, sort

table exp, c(m xb0 m xb1)


*** Splines specification (NOTE: not included in the published article)
cap drop rcspl_*
mkspline rcspl_ = exp, cubic nknots(5) displayknots
cap drop rcspl_* 
mkspline rcspl_ = exp, cubic knots(1   3.673077   6.923077   11.15385   21.01923) displayknots
xtreg lnw md_dist11-md_dist20 md_dist2125 dchild2 dchild3 dchild4    ///
	c.rcspl_1 c.rcspl_2 c.rcspl_3 c.rcspl_4 i.tm#(c.rcspl_1 c.rcspl_2 c.rcspl_3 c.rcspl_4) ///
	tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, vce(cluster id) fe
estat ic
test 1.tm#c.rcspl_1 1.tm#c.rcspl_2 1.tm#c.rcspl_3 1.tm#c.rcspl_4
cap drop exp_r
ge exp_r=int(exp) //note: we use integers of work experience in the following
margins, over(exp_r) at(tm=(0 1) $md_dist md_dist2125=(0) $covar  ///
		(asobserved) rcspl_1 (asobserved) rcspl_2 (asobserved) rcspl_3 (asobserved) rcspl_4 (asobserved)) post
est store FEGS_spline
marginsplot, recast(line) noci plotdimension(tm) 

* get predicted values of splines specification (NOTE: not included in the published article)
mat b = r(table)'
mat l b
local k=72		
mat b = b[1..`k',1..6]
local xvars : rownames b
local k=rowsof(b)
cap drop b se z pvalue ll ul
svmat b, names(col)
cap drop xbs?
qui ge xbs0=.
for num 1/36 : qui replace xbs0=b[X] if exp==X-1
qui ge xbs1=.
for num 37/`k' : qui replace xbs1=b[X] if exp==X-1-36
line xbs0 xbs1 exp, sort



*** get predicted contrasts (Discrete change effect of treatment)

* quadratic specification

qui xtreg lnw md_dist11-md_dist20 md_dist2125 dchild2 dchild3 dchild4    ///
	i.tm#(c.exp##c.exp) tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, vce(cluster id) fe
	
margins r.tm, at(exp=(0(1)35) $md_dist md_dist2125=(0) $covar ) contrast post
est store FEGS_quad_contrast
marginsplot, recast(line) recastci(rline) plotdimension(tm)

mat b = r(table)'
mat l b
local k=36		
mat b = b[1..`k',1..6]
local xvars : rownames b
local k=rowsof(b)
cap drop b se z pvalue ll ul
svmat b, names(col)

cap drop xb lower upper
qui ge xb=.
for num 1/36 : qui replace xb=b[X] if exp==X-1
qui ge lower=.
for num 1/36 : qui replace lower=ll[X] if exp==X-1
qui ge upper=.
for num 1/36 : qui replace upper=ul[X] if exp==X-1

for var xb lower upper : replace X=(exp(X)-1)*100
for var xb lower upper : replace X=0 if exp==0


* contrast for spline specification (NOTE: not included in the published article)
cap drop xbs
gen xbs=xbs1-xbs0
replace xbs=(exp(xbs)-1)*100


su exp if md_dist10==1
table exp_r, c(m xb0 m xb1 m xb)
table exp_r, c(m xb m lower m upper)
cap drop diff_xb*
ge diff_xb_xbs=xb-xbs
table exp_r, c(m xb m xbs m diff_xb_xbs)


*** Plot results

line xb0 xb1 exp if exp<=30, sort yaxis(1) ///
	lcolor(black black) lpattern(longdash dash) lwidth(thick thick) ///
|| line xb lower upper exp if exp<=30, sort yaxis(2) ///
	lcolor(black black black) lpattern(solid solid solid) lwidth(thick medthick medthick) ///
|| , xlabel(0(5)30) xtitle("Work experience (years)", margin(medium)) ///
		ylabel(0(10)150, angle(0) grid axis(2)) /*yline(0, axis(2))*/ ytitle("Wage differential (%)", axis(2) margin(small)) ///
		ylabel(2(.1)3.5, angle(0) grid axis(1)) ytitle("Hourly wage, natural logarithm", axis(1) margin(small)) ///
		legend(cols(1) position(11) ring(0) symxsize(*.5) order(2 "Ever married, predicted wage" 1 "Never-married, predicted wage" ///
		3 "Wage differential and 95% C.I.")) ///
		saving($resultsdir\Figure2.gph, replace) 
graph export "$resultsdir\Figure2.tif", replace

		
//Figure including spline specification (NOTE: not included in the published article)
line xb0 xb1 xbs0 xbs1 exp if exp<=30, sort yaxis(1) ///
	lcolor(black black gs7 gs7) lpattern(longdash dash shortdash shortdash) lwidth(medthick medthick) ///
|| line xb lower upper xbs exp if exp<=30, sort yaxis(2) ///
	lcolor(black black black gs7) lpattern(solid solid solid shortdash) lwidth(medthick medthin medthin medthick) ///
|| , xlabel(0(5)30) xtitle("Work experience (years)", margin(medium)) ///
		ylabel(0(10)150, angle(0) grid axis(2)) /*yline(0, axis(2))*/ ytitle("Wage differential (%)", axis(2) margin(small)) ///
		ylabel(2(.1)3.5, angle(0) grid axis(1)) ytitle("Hourly wage, natural logarithm", axis(1) margin(small)) ///
		legend(cols(1) position(11) ring(0) symxsize(*.5) order(2 "Ever married, predicted wage" 1 "Never-married, predicted wage" ///
		5 "Wage differential and 95% C.I." 3 "Spline specification")) ///
		saving($resultsdir\Figure2_splines.gph, replace) 
		
		


		´
		
// 3) Figure 4: Time-Path of the Marital Wage Premium



//Graph percentage effects
cap drop b1 b2 b3 b4
for num 1/4 : gen bX =.
cap drop bs1 bs2 bs3 bs4
for num 1/4 : gen bsX =.

local m=1
foreach model of any FEdist_US FEGSdist_US FEISdist_US FEISdist_US_pexp {
	est restore `model'
	forvalues x=11/20 {
		lincom _b[md_dist`x']
		replace b`m'=`r(estimate)' if _n==`x'-10
		replace bs`m'=1 if abs(`r(estimate)'/`r(se)')>=1.96 & _n==`x'-10
		replace bs`m'=2 if abs(`r(estimate)'/`r(se)')>=2.576 & _n==`x'-10
		replace bs`m'=3 if abs(`r(estimate)'/`r(se)')>=3.291 & _n==`x'-10
	}
	lincom _b[md_dist2125]
	replace b`m'=`r(estimate)' if _n==11
	replace bs`m'=1 if (`r(estimate)'/`r(se)')>=1.96 & _n==11
	replace bs`m'=2 if (`r(estimate)'/`r(se)')>=2.576 & _n==11
	replace bs`m'=3 if (`r(estimate)'/`r(se)')>=3.291 & _n==11
	local m=`m'+1
}	

cap drop f
gen f=_n
label var f "Years before / after first marriage"
for num 1/4 : replace bX=(exp(bX)-1)*100
cap drop stars*
for num 1/4 : ge starsX="*" if bsX==1
for num 1/4 : replace starsX="**" if bsX==2
for num 1/4 : replace starsX="***" if bsX==3

list f b? in 1/15

lab var b1 "FE model"
lab var b2 "FEGS model"
lab var b3 "FEIS model"
lab var b4 "FEIS w/o controls"
scatter b1 b3 b4 f in 1/11, ///
		ylabel(-15(2.5)20, angle(0) grid) xlabel(0(1)10 11 "11-15") xtick(0(1)12) ///
		msymbol(O O O) mcolor(black black gs7) c(l l l l) ///
		mlabel(stars1 stars3 stars4) mlabposition(12 12 12) ///
		lpattern(shortdash solid longdash) lwidth(thick thick thick) ///
		lcolor(black black gs7) ///
		sort /*xline(.5, lcolor(black))*/ yline(0, lcolor(black)) fysize(100) fxsize(132.9) ///
		/*yscale(alt)*/ ytitle("Effect of marriage on hourly wage (%)",margin(small)) xtitle("Years in 1st marriage",margin(medium)) ///
		legend(cols(1) ring(0) position(8) symxsize(*.9)) ///
		saving($resultsdir\Figure4.gph, replace)
graph export "$resultsdir\Figure4.tif", replace


//Figure including FEGS model (NOTE: not included in the published article)
scatter b1 b2 b3 b4 f in 1/11, ///
		ylabel(-15(2.5)20, angle(0) grid) xlabel(0(1)10 11 "11-15") xtick(0(1)12) ///
		msymbol(O O O O) mcolor(black black black gs7) c(l l l l) ///
		mlabel(stars1 stars2 stars3 stars4) mlabposition(12 12 12 12) ///
		lpattern(shortdash longdash solid solid) lwidth(medthick medthick medthick medthick medthick) ///
		lcolor(black black black gs7) ///
		sort /*xline(.5, lcolor(black))*/ yline(0, lcolor(black)) fysize(100) fxsize(132.9) ///
		/*yscale(alt)*/ ytitle("Effect on hourly wage (%)",margin(small)) xtitle("Years in 1st marriage",margin(medium)) ///
		legend(cols(1) ring(0) position(8) symxsize(*.9)) ///
		saving($resultsdir\Figure4_FEGS.gph, replace)

		
		
		
		
		
*=================================================================
* Figure 3 : Comparison of the Average MWP across Models 
* Table A2 : Average Male Marital Wage Premium estimated by 
*				POLS, FE, FEGS, and FEIS models  
*=================================================================

cd $mwpUSdir

use mwp_US_analysis.dta, clear

replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)

cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)
for var exp expq tenure yeduc enrol : gen tm_X=tm*X

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)
	
keep if sample1==1


replace expq=expq/100
cap drop tm_*
ge tm_exp=tm*exp
ge tm_expq=tm*expq


reg lnw marry dchild2-dchild4  ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr7  ///
	, vce(cluster id) 
est store POLS_US

xtreg lnw marry dchild2-dchild4 ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr7  ///
	, vce(cluster id) fe
est store FE_US

xtreg lnw marry dchild2-dchild4 ///
	exp expq tm_exp tm_expq tenure yeduc enrol ///
	yeargr2-yeargr7  ///
	, vce(cluster id) fe
est store FEGS_US


xtfeis lnw marry dchild2-dchild4 ///
	tenure yeduc enrol ///
	yeargr2-yeargr7  ///
	, slope(exp expq) cluster(id)
est store FEIS_US

xtfeis lnw marry ///
	enrol yeduc ///
	yeargr2-yeargr7  ///
	, slope(pexp pexpq) cluster(id)
est store FEIS_US_pexp

est table POLS_US FE_US FEGS_US FEIS_US FEIS_US_pexp, star b(%9.4g) 
est table POLS_US FE_US FEGS_US FEIS_US FEIS_US_pexp, b(%9.4g) se(%9.4g)

//FEGS model: Test for diff in effect of work experience (Heterogeneous growth)
est restore FEGS_US
test tm_exp tm_expq


*** Table A2 : Average Male Marital Wage Premium estimated by POLS, FE, FEGS, and FEIS models 

estout POLS_US FE_US FEGS_US FEIS_US FEIS_US_pexp using "$resultsdir\TableA2.doc", replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) starlevels(* 0.05 ** 0.01 *** 0.001) ///
	stats(r2_w N_g N,  labels("Within R-square" "N persons" "NT person-years") fmt(%9.0f %9.0f %9.0f %9.0f)) ///
	varlabels() mlabels("Model 1" "Model 2") varwidth(25) modelwidth(22) legend



*** Figure 3: Wage premium, comparison across models


cap drop b 
cap drop lo hi
for new b lo hi : ge X=.
local n=2
foreach model in POLS_US FE_US FEGS_US FEIS_US FEIS_US_pexp {
	est restore `model'
	replace b=_b[marry] if _n==`n'
	replace lo=b-1.96*_se[marry] if _n==`n'
	replace hi=b+1.96*_se[marry] if _n==`n'
	local n=`n'+1
}
cap drop n
ge n=_n
for var b lo hi : replace X=(exp(X)-1)*100
l b lo hi in 1/6


preserve
for var b lo hi : replace X=. if _n==6
twoway bar b n in 1/6, yline(0) barwidth(.5) lwidth(thick) lcolor(black) fcolor(white) ///
 || rcap lo hi n in 1/6, lwidth(thick) lcolor(black)  ///
 ||, xlabel(2"POLS" 3"FE" 4"FEGS" 5"FEIS") ylabel(-5 0 to 25, angle(0) grid) ymtick(-5 -2.5 to 25, grid) ///
		legend(off) xtitle("") /*yscale(alt)*/ ytitle("Marital wage premium (%) and 95% CI") ///
		yline(0, lcolor(black)) ///
		saving($resultsdir\Figure3.gph, replace)
graph export "$resultsdir\Figure3.tif", replace
restore
	
	
	
/*	
//Compare FEFD model (Loughran/Zissimopoulos 2009)
qui xtreg D.(lnw marry nchildm  ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr4)  ///
	, vce(cluster id) fe
cap drop help*
bys id (year) : egen help1=count(_N) if e(sample)
bys id (year) : egen help2=total(marry) if e(sample)
bys id (year) : replace help2=0 if help2<help1 & e(sample)
xtreg D.(lnw marry nchildm  ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr4)  ///
	if help1>=2 & help2==0, vce(cluster id) fe
//coeffs. for work experience completely off!!



///LZ DID NOT ESTIMATE THEIR model ON FIRST DIFFERENCES!
cd $mwpUSdir

use mwp_US_analysis.dta, clear

replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)

cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)
for var exp expq tenure yeduc enrol : gen tm_X=tm*X

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

preserve
keep if sample2==1 & year<=2004
tab sample2
restore
drop if woman==1
drop if year>2004	
drop if subsample==9
drop if subsample==12
drop if subsample==15
keep if inrange(age,18,41)
for var lnw marstat mardur exp tenure nchildm	: drop if missing(X)
for var lnw marry div nchildm exp expq : bys id (year) : gen D_X=X-X[_n-1]
bys id (year) : drop if _n==1
bys id (year) : ge help=_N
keep if help>=2
replace div=1 if wid==1
xtreg D.(lnw marry div nchildm  ///
	exp expq)  ///
	, vce(cluster id) fe
for num 1996(2)2004 \ num 1995(1)1999 : replace year=Y if year==X
xtset id year
xtreg D.(lnw marry div nchildm  ///
	exp expq)  ///
	, vce(cluster id) fe
xtreg D_*	, vce(cluster id) fe

	
	


//Compare Random-Effects model
xtreg lnw marry nchildm ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr7  ///
	, vce(cluster id) re
est store RE_US
*/



	
	
*=======================================================================
* Robustness checks

* 1) Figure S1 : Test for heterogeneous effects of marriage by 
*       age at marriage, wife’s employment, educational achievement, 
*       ethnicity, and urbanicity (FE and FEIS results)	

* 2) Tests of Sample selection
* a) Table S4. Regression models including selection indicator, test of sample selection
* b) Tables S5 and S6 : Attrition weights 
* c) Table S7: Test using larger sample
*=======================================================================

		
*** 1) Figure S1 : Test for heterogeneous effects of marriage by 
***       age at marriage, wife’s employment, educational achievement, 
***       ethnicity, and urbanicity (FE and FEIS results)	

				
//by age at first marriage	
		
cd $mwpUSdir
use mwp_US_analysis.dta, clear

ren nchild nchildm
recode nchildm 3/max=3, gen(dchild)
tab dchild, gen(dchild)


recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)


keep if sample1==1

xttab marage if treat1>=1

recode marage 16/22=1 23/29=2 30/max=3 *=. 
xttab marage
tab treat1 marage, miss
tab marage, gen(mage)
for num 1/3 : ge marry_mageX=marry*mageX
for num 1/3 : replace marry_mageX=0 if marry==0
lab var marry_mage1 "Married age 16 to 22"
lab var marry_mage2 "Married age 23 to 29"
lab var marry_mage3 "Married age 30 to 55"

xtreg lnw marry_mage1 marry_mage2 marry_mage3 dchild2-dchild4 ///
	exp expq tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, fe cluster(id) 
est store FE_MAGE
xtfeis lnw marry_mage1 marry_mage2 marry_mage3  dchild2-dchild4 ///
	tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id) 
est store FEIS_MAGE
	
test marry_mage1=marry_mage3
	  
est table FE_MAGE FEIS_MAGE, keep(marry_mage1 marry_mage2 marry_mage3) se	
est table FE_MAGE FEIS_MAGE, keep(marry_mage1 marry_mage2 marry_mage3) se	eform
	

coefplot FE_MAGE FEIS_MAGE, keep(marry_mage1 marry_mage2 marry_mage3) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(11) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) ///
		saving($resultsdir\FigureS1a_mage.gph, replace)
graph export "$resultsdir\FigureS1a_mage.tif", replace
	
	
	
****
	
//by wife's employment status last calendar year	(derived from usual hours worked)

cd $mwpUSmydir
use mwp_US_analysis.dta, clear

ren nchild nchildm
recode nchildm 3/max=3, gen(dchild)
tab dchild, gen(dchild)

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)
for var pexp pexpq tenure yeduc enrol : gen tm_X=tm*X


keep if sample1==1


cap drop help
tab emplst, m
bys id (year) : ge help=sum(emplst<. & emplst!=5)
bys id (year) : replace help=help[_N]
keep if help>=4 & emplst<.  & emplst!=5

cap drop help
tab emplst_woman, m
bys id (year) : ge help=sum(emplst_woman<.)
bys id (year) : replace help=help[_N]
keep if help>=4 & emplst_woman<.

//many transitions between states
xttrans emplst_woman, freq 

tab emplst, gen(emplst)
tab emplst_woman, gen(emplst_w)
lab var emplst_w2 "Wife employed FT"
lab var emplst_w3 "Wife employed PT"
lab var emplst_w4 "Wife not employed"

xtreg lnw ///
	exp expq yeduc dchild2-dchild4 tenure ///
	emplst2 emplst3 emplst4 ///
	emplst_w2 emplst_w3 emplst_w4 ///
	yeargr2-yeargr7 ///
	, fe cluster(id) 
est store FE_EMPW
xtreg lnw ///
	i.tm#(c.exp c.expq) yeduc  dchild2-dchild4 tenure ///
	emplst2 emplst3 emplst4 ///
	emplst_w2 emplst_w3 emplst_w4 ///
	yeargr2-yeargr7 ///
	, fe cluster(id) 
est store FEGS_EMPW
xtfeis lnw ///
	yeduc  dchild2-dchild4 tenure ///
	emplst2 emplst3 emplst4 ///
	emplst_w2 emplst_w3 emplst_w4 ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id) 
est store FEIS_EMPW


coefplot FE_EMPW FEGS_EMPW FEIS_EMPW , keep(emplst_w2 emplst_w3 emplst_w4) vertical 


	
coefplot FE_EMPW FEIS_EMPW , keep(emplst_w2 emplst_w3 emplst_w4) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(11) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		yline(0, lcolor(black)) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		saving($resultsdir\FigureS1b_wifeemp.gph, replace)
graph export "$resultsdir\FigureS1b_wifeemp.tif", replace
		
/*			
coefplot FE_EMPW FEGS_EMPW FEIS_EMPW , keep(emplst_w2 emplst_w3 emplst_w4) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) citop ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(11) order(1 "FE model" 3 "FEGS model" 5 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) 
*/				





// By race, education, urbanicity

		
cd $mwpUSmydir
use mwp_US_analysis.dta, clear

ren nchild nchildm
recode nchildm 3/max=3, gen(dchild)
tab dchild, gen(dchild)

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

cap drop tm tm_* 
ge tm=(treat1>0 & treat1<.)
for var pexp pexpq tenure yeduc enrol : gen tm_X=tm*X


keep if sample1==1


//by race		
xttab race		
recode race 3=1 1=3
tab race, gen(race)
for num 1 2 3 : ge marry_raceX=marry*raceX
lab var marry_race1 "White" 
lab var marry_race2 "Afro American"
lab var marry_race3 "Hispanic"

		

xtfeis lnw marry_race1 marry_race2 marry_race3 ///
	dchild2-dchild4 tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id) 
est store FEIS_RACE
		
xtreg lnw marry_race1 marry_race2 marry_race3 ///
	dchild2-dchild4 tenure exp expq yeduc enrol ///
	yeargr2-yeargr7 ///
	, fe vce(cluster id)
est store FE_RACE
		
		
coefplot FE_RACE FEIS_RACE, keep(marry_race1 marry_race2 marry_race3) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(1) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) ///
		saving($resultsdir\FigureS1c_ethnic.gph, replace)
graph export "$resultsdir\FigureS1c_ethnic.tif", replace
	

/*	
	xtfeis lnw marry_race1 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==1, slope(pexp pexpq) cluster(id)
est store FEIS_W
xtfeis lnw marry_race2 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==2, slope(pexp pexpq) cluster(id) 
est store FEIS_B
xtfeis lnw marry_race3 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==3, slope(pexp pexpq) cluster(id) 
est store FEIS_H
		
xtreg lnw marry_race1 ///
	pexp pexpq yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==1, fe vce(cluster id)
est store FE_W
xtreg lnw marry_race2 ///
	pexp pexpq yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==2, fe vce(cluster id) 
est store FE_B
xtreg lnw marry_race3 ///
	pexp pexpq yeduc enrol ///
	yeargr2-yeargr7 ///
	if race==3, fe vce(cluster id) 
est store FE_H

coefplot FE_W FEIS_W FE_B FEIS_B FE_H FEIS_H, keep(marry_race1 marry_race2 marry_race3) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.125) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(1) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) 
*/	
	
				
//by educational level
cap drop help*
bys id (year) : egen help=max(yeduc)
bys id (year) : gen educ=help[_N]
recode educ 0/12=1 13/20=2
xttab educ


tab educ, gen(educ)
for num 1 2 : ge marry_educX=marry*educX
lab var marry_educ1 "Education < 13 yrs."
lab var marry_educ2 "Education 13+ yrs."

				
xtreg lnw ///
	exp expq yeduc enrol dchild2-dchild4 tenure ///
	marry_educ1 marry_educ2 ///
	yeargr2-yeargr7 ///
	, fe cluster(id) 
est store FE_EDUC
xtfeis lnw ///
	yeduc enrol dchild2-dchild4 tenure ///
	marry_educ1 marry_educ2 ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id) 
est store FEIS_EDUC


coefplot FE_EDUC FEIS_EDUC, keep(marry_educ1 marry_educ2) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(11) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) ///
		saving($resultsdir\FigureS1d_educ.gph, replace)
graph export "$resultsdir\FigureS1d_educ.tif", replace
	
/*	
xtfeis lnw marry_educ1 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	if educ==1, slope(pexp pexpq) cluster(id) 
est store FEIS_LE
xtfeis lnw marry_educ2 ///
	yeduc enrol ///
	yeargr2-yeargr7 ///
	if educ==2, slope(pexp pexpq) cluster(id) 
est store FEIS_HE
		
xtreg lnw marry_educ1 ///
	pexp pexpq yeduc enrol ///
	yeargr2-yeargr7 ///
	if educ==1, fe cluster(id) 
est store FE_LE
xtreg lnw marry_educ2 ///
	pexp pexpq yeduc enrol ///
	yeargr2-yeargr7 ///
	if educ==2, fe cluster(id) 
est store FE_HE

coefplot FE_LE FEIS_LE FE_HE FEIS_HE , keep(marry_educ1 marry_educ2) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.2) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(1) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect of marriage on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black))
	
*/


	
//by urbanicity
ta urban
lab var urban "Urban residence"
lab var marry "Married"
recode urban min/-1 2=.
cap drop help
bys id (year) : ge help=sum(urban<.)
bys id (year) : replace help=help[_N]
keep if help>=4 & urban<.
//identify urban/rural at start of panel
bys id (year) : gen urban1=urban[1]

xttab urban1
xttab urban if urban1==0 //initially rural
xttab urban if urban1==1 //initially urban

xtfeis lnw marry ///
	yeduc enrol urban dchild2-dchild4 tenure ///
	yeargr2-yeargr7 ///
	if urban1==0, slope(exp expq) cluster(id) 
est store FEIS_R
xtfeis lnw marry ///
	yeduc enrol urban dchild2-dchild4 tenure ///
	yeargr2-yeargr7 ///
	if urban1==1, slope(exp expq) cluster(id) 
est store FEIS_U
		
xtreg lnw marry ///
	exp expq yeduc enrol urban dchild2-dchild4 tenure ///
	yeargr2-yeargr7 ///
	if urban1==0, fe cluster(id) 
est store FE_R
xtreg lnw marry ///
	exp expq yeduc enrol urban dchild2-dchild4 tenure ///
	yeargr2-yeargr7 ///
	if urban1==1, fe cluster(id) 
est store FE_U
				
coefplot FE_R FEIS_R , keep(marry urban) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(1) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) ///
		title("Initially rural residence") /// 
		saving($resultsdir\FigureS1e_1.gph, replace)

coefplot FE_U FEIS_U, keep(marry urban) vertical ///
	yline(0) transform(* = (exp(@)-1)*100) recast(bar) lcolor(black) fcolor(none) barwidth(.25) lwidth(medium) ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
	legend(ring(0) position(1) order(2 "FE model" 4 "FEIS model") col(1) symxsize(*.4)) ///
	ylabel(-5 0 to 20, angle(0) grid) ymtick(-5 -2.5 to 20, grid) ///
		xtitle("") /*yscale(alt)*/ ytitle("Effect on hourly wages (%) and 95% C.I.") ///
		yline(0, lcolor(black)) ///
		title("Initially urban residence") /// 
		saving($resultsdir\FigureS1e_2.gph, replace)

graph combine "$resultsdir\FigureS1e_1.gph" "$resultsdir\FigureS1e_2.gph"
graph export "$resultsdir\FigureS1e_urban.tif", replace

/*
ta urban	
cap drop marry_urban*
ge marry_urban0=(marry==1 & urban==0)
ge marry_urban1=(marry==1 & urban==1)
lab var marry_urban0 "Married, urban residence"
lab var marry_urban1 "Married, rural residence"
xtfeis lnw marry_urban0 marry_urban1 nchildm ///
	tenure yeduc enrol urban ///
	yeargr2-yeargr7 ///
	if urban1==0, slope(exp expq) cluster(id) 
est store FEIS_R
xtfeis lnw marry_urban0 marry_urban1 nchildm ///
	tenure yeduc enrol urban ///
	yeargr2-yeargr7 ///
	if urban1==1, slope(exp expq) cluster(id) 
est store FEIS_U
		
xtreg lnw marry_urban0 marry_urban1 nchildm ///
	exp expq tenure yeduc enrol urban ///
	yeargr2-yeargr7 ///
	if urban1==0, fe cluster(id) 
est store FE_R
xtreg lnw marry_urban0 marry_urban1 nchildm ///
	exp expq tenure yeduc enrol urban ///
	yeargr2-yeargr7 ///
	if urban1==1, fe cluster(id) 
est store FE_U
				
coefplot FE_R FEIS_R, keep(marry_urban0 marry_urban1 urban) vertical ///
	yline(0) msymbol(Oh O Dh D Sh S)

coefplot FE_U FEIS_U, keep(marry_urban0 marry_urban1 urban) vertical ///
	yline(0) msymbol(Oh O Dh D Sh S)
*/



*** 2) Tests of Sample selection

* a) Table S4. Regression models including selection indicator, test of sample selection


cd $mwpUSdir

use mwp_US_analysis.dta, clear


replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)

cap drop age
ge age=year-cohort
	
recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

replace expq=expq/100

keep if sample1==1


//construct selection indicator (t+1)
ge s=1
bys id (year) : replace s=0 if year[_n+1]-year==1 
bys id (year) : replace s=0 if year>=1994 & year[_n+1]-year==2

bys id (year) : ge last=(_n==_N)

table year subsample, c(m s)


cap drop help
bys id (year) : ge help=sum(sample1==1) if last==0
cap drop N
bys id (year) : egen N=max(help) if last==0





xtfeis lnw s marry dchild2-dchild4 tenure  ///
	yeduc enrol ///
	yeargr2-yeargr7  ///
	if last==0 & N>=4 & N<., slope(exp expq) cluster(id)
est store FEIS_US


reg lnw s marry dchild2-dchild4 tenure ///
	exp expq yeduc enrol ///
	yeargr2-yeargr7  ///
	if e(sample), vce(cluster id) 
est store POLS_US

xtreg lnw s marry dchild2-dchild4 tenure  ///
	exp expq yeduc enrol ///
	yeargr2-yeargr7  ///
	if e(sample), vce(cluster id) fe
est store FE_US

cap drop tm*
recode treat1 1/2=1, gen(tm)
for  var exp expq : ge tm_X=tm*X
xtreg lnw s marry dchild2-dchild4 tenure  ///
	exp expq tm_exp tm_expq yeduc enrol ///
	yeargr2-yeargr7  ///
	if e(sample), vce(cluster id) fe
est store FEGS_US

est table POLS_US FE_US FEGS_US FEIS_US , star stats(N r2 r2_w)

estout POLS_US FE_US FEGS_US FEIS_US  using "$resultsdir\TableS4_selectind.doc", ///
		replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) starlevels(* 0.05 ** 0.01 *** 0.001) ///
		stats(r2_w N_g N, labels("Within R-square" "N persons" "NT person-years") fmt(%9.0f %9.0f %9.0f)) ///
		varlabels() mlabels("Model 1" "Model 2") varwidth(25) modelwidth(22) legend
	
	

*	b) Tables S5 and S6 : Attrition weights 

cd $mwpUSdir

use mwp_US_analysis.dta, clear


bys id (year) : egen minyear=min(year)
tab minyear, gen(minyear)
bys id (year) : egen maxyear=max(year)
bys id (year) : egen attr=max(year==maxyear & year<2012)
recode attr 1/30=1

cap drop mw
bys id (year) : egen mwage=mean(wage)

ren subsample psample

xttab treat1
keep if sample1==1		//keep only persons of sample 1


bys id (year) :  keep if _n==1 //keep only first person-year
ta treat1 
ta attr

logit attr i.treat1
margins, dydx(*)

cap drop qw
xtile qw=lnw, nquantiles(5)
logit attr i.treat1 ib1.qw c.nchild c.yeduc i.enrol c.age c.exp c.tenure i.cohort ib1.psample if psample!=9, or vce(cluster id)
est store LOGIT_US
margins, dydx(*) post
eret li
est store AME_US



recode attr 0=1 1=0, gen(ret)
logit ret i.treat1 ib1.qw c.nchild c.yeduc i.enrol c.age c.exp c.tenure i.cohort ib1.psample  if psample!=9, or vce(cluster id)
predict pxav if e(sample)
logit ret ib1.psample   if e(sample), or vce(cluster id)
predict pxres if e(sample)

ge attrw=pxres/pxav
su attrw, d

keep id attr attrw
save attrw_US, replace

estout LOGIT_US AME_US using "$resultsdir\TableS5_Attrition_LOGIT.doc", replace cells("b(star fmt(%9.3f)) se(fmt(%9.3f))") starlevels(* 0.05 ** 0.01 *** 0.001) ///
		stats(r2_w N_g Tbar N, labels("Within R-square" "N persons" "Mean T" "NT person-years") fmt(%9.2f %9.0f %9.1f %9.0f))  ///
		mlabels("") varwidth(40) modelwidth(22) legend ///
		title("") 



cd $mwpUSdir

use mwp_US_analysis.dta, clear
merge m:1 id using attrw_US

keep if sample1==1
su attrw, d

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

ren subsample psample





//attrition weighted	
//FEIS

cap drop w_*
xtfeis lnw marry ///
	yeduc enrol tenure nchild ///
	yeargr2-yeargr7  ///
	if psample!=9, slope(exp expq) cluster(id) transformed(w_)

xtreg w_* ///
	if psample!=9 [pweight=attrw], fe vce(cluster id) 
est store FEIS_w_US

//FE
xtreg lnw marry ///
	exp expq yeduc enrol tenure nchild ///
	yeargr2-yeargr7  ///
	if psample!=9 [pweight=attrw], fe vce(cluster id) 
est store FE_w_US
	

//unweighted	
//FEIS
xtfeis lnw marry ///
	yeduc enrol tenure nchild ///
	yeargr2-yeargr7  ///
	if psample!=9, slope(exp expq) cluster(id) 
est store FEIS_unw_US
	
xtreg w_* ///
	if psample!=9, fe vce(cluster id) 

//FE
xtreg lnw marry ///
	exp expq yeduc enrol tenure nchild ///
	yeargr2-yeargr7  ///
	if psample!=9, fe vce(cluster id) 
est store FE_unw_US
	
est table FE_unw_US FEIS_unw_US FE_w_US FEIS_w_US, star
	
est table LOGIT_US AME_US , star


	
estout FE_unw_US FEIS_unw_US FE_w_US FEIS_w_US using "$resultsdir\TableS6_Attrition_weighted.doc", replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) starlevels(* 0.05 ** 0.01 *** 0.001) ///
		stats(r2_w N_g Tbar N, labels("Within R-square" "N persons" "Mean T" "NT person-years") fmt(%9.2f %9.0f %9.1f %9.0f))  ///
		mlabels("") varwidth(40) modelwidth(22) legend ///
		title("") 

	
	
	

***	c) Table S7: Test using larger sample



cd $mwpUSdir

use mwp_US_analysis.dta, clear


replace mardur=mardur/12
ren nchild nchildm
recode nchildm 3/20=3, gen(dchild)
tab dchild, gen(dchild)

cap drop age
ge age=year-cohort
	
ge tm=treat2>=1
	
	

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)



cap drop pexpq
ge pexpq=pexp^2
cap drop pexpc
ge pexpc=pexp^3



replace expq=expq/100
cap drop expc
ge expc=(exp^3)/1000
ge yeducq=(yeduc^2)/100
cap drop tenureq
ge tenureq=(tenure^2)/100

recode working min/0=0 1/2=1

keep if sample2!=-1
ta sample2, m
ta sample2p, m
ta sample2p, 


keep if sample2==1

xttab entry

xttab marage

recode marage 16/22=1 23/29=2 30/max=3 *=. 
xttab marage
tab treat1 marage, miss
tab marage, gen(mage)
for num 1/3 : ge marry_mageX=marry*mageX
for num 1/3 : replace marry_mageX=0 if marry==0
lab var marry_mage1 "Married age 16 to 22"
lab var marry_mage2 "Married age 23 to 29"
lab var marry_mage3 "Married age 30 to 55"

xtreg lnw marry div wid remarry dchild2-dchild4 ///
	exp expq tenure yeduc enrol  ///
	yeargr2-yeargr7 self working ///
	, fe cluster(id) 
est store FE_US

xtfeis lnw marry div wid remarry  dchild2-dchild4 ///
	tenure yeduc enrol self working ///
	yeargr2-yeargr7 ///
	, slope(exp expq) cluster(id) 
test marry=div		
est store FEIS_US


//Check: 
//At least 4 person-years pre-treatment required
cap drop pre* 
bys id (year) : gen pretreat=sum(sample2==1 & marstat==1) 
bys id (year) : replace pretreat=pretreat[_N]
xttab pretreat
xtsum pretreat
xttab pretreat if treat2==2
xtsum pretreat if treat2==2
cap drop help*
bys id (year) : gen help=sum(div==1 | wid==1 | remarry==1)
cap drop everdiv
bys id (year) : egen everdiv=max(help)

xtsum pretreat if treat2==2 & everdiv==0
xtsum pretreat if treat2==2 & everdiv>0
recode pretreat 1/4=1 5/10=2 11/20=3 21/35=4, gen(pretreat2)
xttab pretreat2 
xttab pretreat2 if treat2==2
xttab pretreat2 if treat2==2 & everdiv==0
xttab pretreat2 if treat2==2 & everdiv>0

cap drop post* 
bys id (year) : gen posttreat=sum(sample2==1 & marstat!=1) //& inlist(working,1,2)
bys id (year) : replace posttreat=posttreat[_N]
xttab posttreat



xtreg lnw marry div wid remarry dchild2-dchild4 ///
	exp expq tenure yeduc enrol  ///
	yeargr2-yeargr7 self  working ///
	if pretreat>=4, fe cluster(id) 
est store FE_US_pretreat


xtfeis lnw marry div wid remarry dchild2-dchild4 ///
	tenure yeduc enrol self working ///
	yeargr2-yeargr7 ///
	if pretreat>=4, slope(exp expq) cluster(id) 
test marry=div		
est store FEIS_US_pretreat


test marry remarry


//Check: 
//Higher polynomials for experience, education, tenure

xtfeis lnw marry div wid remarry dchild2-dchild4 ///
	tenure tenureq yeduc yeducq enrol self working ///
	yeargr2-yeargr7 ///
	, slope(exp expq expc) cluster(id) 
test marry=div		
est store FEIS_US_exp3


xtreg lnw marry div wid remarry dchild2-dchild4 ///
	exp expq expc 	tenure tenureq yeduc yeducq enrol self working ///
	yeargr2-yeargr7 ///
	if e(sample), fe cluster(id) 
est store FE_US_exp3


est table FE_US FEIS_US FE_US_exp3 FEIS_US_exp3 FE_US_pretreat FEIS_US_pretreat , star stats(N r2_w)


*** Regression results, including sep./divorced/widowed/remarried

estout FE_US FEIS_US  FE_US_exp3 FEIS_US_exp3 FE_US_pretreat FEIS_US_pretreat using "$resultsdir\TableS7_sample_large.doc", ///
		replace cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) starlevels(* 0.05 ** 0.01 *** 0.001) ///
		stats(r2_w N_g N, labels("Within R-square" "N persons" "NT person-years") fmt(%9.0f %9.0f %9.0f)) ///
		varlabels() mlabels("Model 1" "Model 2") varwidth(25) modelwidth(22) legend
		
		
	
***********************************************************************
***********************************************************************

*** Further tests 


*** Test using alternative measure of work experience (see footnote 7)
*** (1 year of part-time work counts only 0.5 years of full-time exp)

cd $mwpUSdir

use mwp_US_analysis.dta, clear


replace mardur=mardur/12
ren nchild nchildm
recode nchildm 2/20=2, gen(dchild)
tab dchild, gen(dchild)
	
keep if sample1==1

recode year 1979/1980=1 1981/1985=2 1986/1990=3 1991/1995=4 1996/2000=5 2001/2005=6 2006/2012=7, gen(yeargr)
tab yeargr, gen(yeargr)

gen exp_altq=exp_alt^2

xtfeis lnw marry dchild2 dchild3 ///
	tenure yeduc enrol ///
	yeargr2-yeargr7 ///
	, slope(exp_alt exp_altq) cluster(id) 
est store FEIS_US

xtreg lnw marry dchild2 dchild3  ///
	exp_alt exp_altq tenure yeduc enrol  ///
	yeargr2-yeargr7 ///
	, fe cluster(id) 
est store FE_US

est table FE_US FEIS_US , stats(N N_g) star


