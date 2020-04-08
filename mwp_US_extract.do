/*
*==============================================================

Stata 14 Do-File #1 of 3 for Replication of article:

Ludwig, Volker and Josef Brüderl. 2018. 
"Is there a Marital Wage Premium? New Evidence from the United States".
American Sociological Review 83(4). 

*==============================================================
*/




*==============================================================

* 1) Unzip downloaded files, convert to Stata 14 format files
* 2) Recode variables
* 3) Merge all information, store as long-format panel data set

*==============================================================


//set working directory !!! Adapt this as needed !!!
global mwpUSdir "X:\mwp_US\data\"
capture mkdir "$mwpUSdir"


/*
*==============================================================

* 1) Unzip downloaded files, convert to Stata 14 format files
* Downloaded NLSY data :
* NLSY1979, version 1979-2012, released Feb 3rd 2015
* Retrieved from www.nlsinfo.org on Sep 20th 2015

*==============================================================
*/

//directory where downloaded data are stored !!! Adapt this as needed !!!
cd "$mwpUSdir\NLSY79\rawdata\"

//unzip downloaded files, convert to Stata 14 format files

foreach file in 		///
		mwp_us_intweek                                           ///
		mwp_us_intmonth                                           ///
		mwp_us_wage                                           ///
		mwp_us_earnings                                           ///
		mwp_us_workhours										///
		mwp_us_marstat                                           ///
		mwp_us_mdauer                                           ///
		mwp_us_childbirth                                           ///
		mwp_us_cohabitation2                                           ///
		mwp_us_enroll                                           ///
		mwp_us_educ                                           ///
		mwp_us_weeks                                           ///
		mwp_us_weeks_miss                                           ///
		mwp_us_anhours                                           ///
		mwp_us_cps                                           ///
		mwp_us_ten                                           ///
		mwp_us_lfs                                           ///
		mwp_us_class                                           ///
		mwp_us_controls                   						///
		mwp_us_wifeempl			{
	cd "$mwpUSdir\NLSY79\rawdata\"
	qui unzipfile "`file'", replace
	qui infile using "`file'.dct", clear
	qui ds
	foreach var in `r(varlist)' {
		local newvar=lower("`var'")
		ren `var' `newvar'
	}	
	cd "$mwpUSdir\NLSY79\"
	save "`file'.dta", replace
}	






*==============================================================

* 2) Recode variables

*==============================================================




cd "$mwpUSdir\NLSY79"

************************************************************************************************
*** Interviewdate
************************************************************************************************

//week of current interview (work history data)
use mwp_us_intweek.dta, clear


ren  r0000100 id


local year=1979
foreach var of varlist w0001900-w1196000 {
	ren `var' intweek`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}

reshape long intweek, i(id) j(year)

su intweek, d
ta intweek if intweek<=0
ta year intweek if intweek<=0
recode intweek -4 -3=.
ge nointerview_w=intweek==.
forvalues y=1979/2012 {
	quietly su intweek if year==`y'
	quietly replace intweek=int(r(mean)) if intweek==. & year==`y'
}

drop  r0214700 r0214800
sort id year
save intweek, replace


//month of current interview (survey methodology data)
use mwp_us_intmonth.dta, clear


ren  r0000100 id

drop r4100202 //intyear 1993 not needed

local year=1979
foreach var of varlist r0172500-r3917600 r4100200 {
	ren `var' intmonth`year'
	ge intyear`year'=`year'
	local year=`year'+1
}
	
local year=1994
local i=1
foreach var of varlist r4500201-t3195602 {
	if `i'==1 {
		ren `var' intmonth`year'
		local i=2
	}
	else if `i'==2 {
		ren `var' intyear`year'
		local i=1
		local year=`year'+2
	}
}

//check --> ok
*tab1 intyear*	
*tab1 intmonth*	

reshape long intmonth intyear, i(id) j(year)

su intmonth, d
ta intmonth if intmonth<=0
ta year intmonth if intmonth<=0
ge nointerview_m=intmonth==-5
recode intmonth -5 -4 -3=.
recode intyear -5 -4 -3=.
forvalues y=1979/2012 {
	quietly su intmonth if year==`y'
	quietly replace intmonth=int(r(mean)) if intmonth==. & year==`y'
	quietly su intyear if year==`y'
	quietly replace intyear=int(r(mean)) if intyear==. & year==`y'
}

sort id year
save intmonth, replace


************************************************************************************************
*** wage
************************************************************************************************

use mwp_us_wage.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

local year=1979
foreach var of varlist r0047010-t4110900 {
	ren `var' wage`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}


reshape long wage, i(id) j(year)

sort id year
save wage, replace


************************************************************************************************
*** Marital status, sex, birth cohort, race
************************************************************************************************

use mwp_us_marstat.dta, clear


ren  r0000100 id
ren  r0214700 race
ren r0214800 woman 
ren  r0000500 cohort

local year=1979
foreach var of varlist  r0217501-t4112900 {
	ren `var' marstat`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}

reshape long marstat, i(id) j(year)

sort id year
save marstat, replace


************************************************************************************************
*** Marriage history
************************************************************************************************

use mwp_us_mdauer.dta, clear


ren  r0000100 id
drop  r0214700 r0214800
drop r9909300 r9909400 r9909500 r9909600

for var r9908700 r9908800 r9908900 r9909000 r9909100 r9909200 \ ///
	new begin1m begin1y end1m end1y begin2m begin2y : 			///
	ren X Y
	
for var begin1m begin1y end1m end1y begin2m begin2y : recode X -1 -2 -3=.i -5=.n -999=.l -998=.f -997=.d
//assign random marriage month if only year is known
set seed 1073741823
cap drop month
ge month=1+int((12-1+1)*runiform()) 
for var begin1m end1m begin2m \ var begin1y end1y begin2y : replace X=month if X==.i & Y<.

sort id
save mdauer, replace


****************************************************
*** MARRIAGE DURATION, Number of current marriage

//Match info on marital status and marriage duration to month of interview
use intmonth, clear
merge m:1 id using mdauer
drop _merge
merge 1:1 id year using marstat
drop _merge

//Recode marital status
ta marstat
ta year marstat
recode marstat 0=1 1=2 2=3 3=4 5=5 6=6

//Marriage Date: begin and end of 1st marriage, months counted as of Jan 1960
gen begin1=ym(begin1y, begin1m)
gen end1=ym(end1y, end1m)
gen begin2=ym(begin2y, begin2m)

//Interview date: months counted as of Jan 1960
gen intdate=ym(intyear, intmonth)

//Persons reporting more than one date of 1st marriage
//Recode to inconsistent after marriage
cap drop help*
bysort id (year) : egen help1=min(begin1)
bysort id (year) : egen help2=max(begin1)
bysort id (year) : gen help3=sum(help1!=help2 & help1<. & help2<.)
bysort id (year) : gen help4=help3[_N]>0
l id year intdate marstat begin1 end1 begin2 help3 if help4>0, sepby(id)
// check --> ok
ge nohist=0
bysort id (year) : replace nohist=1 if nohist==0 & help3>0 
bysort id (year) : replace nohist=1 if nohist==0 & help4>0 & marstat>1 


//Persons reporting no date for 1st marriage
//Date can be recovered by interpolation if never-married, then married w/o gap in between
//if gap 1 year or less : assign marriage month in between
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2) & marstat[1]==1 & begin1==.)
bysort id (year) : ge help2=help1[_N]>0
l id year intdate marstat begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
//recover by interpolation
bysort id (year): replace begin1=round(intdate-(intdate-intdate[_n-1])/2) if begin1==. & marstat==2 & marstat[_n-1]==1 & intyear-intyear[_n-1]==1 & help2>0
l id year intdate marstat begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
/*
*check
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2) & marstat[1]==1 & begin1==.)
count if help1==1
bysort id (year) : ge help2=help1[_N]>0
l id year intdate marstat begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
*/
cap drop help*
bys id (year) : egen help=min(begin1)
replace begin1=help if begin1==. /*& intmonth<=help*/ & help>0 & help<.
cap drop help*

/*
*check
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2) & marstat[1]==1 & begin1==.)
count if help1==1
bysort id (year) : ge help2=help1[_N]>0
l id year intdate marstat begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
*/


//Marriage duration 
//Difference of interview date and marriage date (months)
gen mardur=(intdate-begin1)
replace mardur=0 if begin1y==.l //begin1y==.l are persons never-married throughout
replace mardur=0 if intdate<begin1 & begin1<.
/*
*check : never-married, but positive marriage duration --> ok
* all cases are inconsistent marital biografies : 
* never-married, then married/divorced/widowed, then never-married again
* inconsistencies identified below
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,1) & mardur>0 & mardur<.)
bysort id (year) : ge help2=help1[_N]>0
l id year intdate marstat mardur begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
*/
replace mardur=0 if marstat==1
/*
*check : never-married, but positive marriage duration --> ok
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,1) & mardur>0 & mardur<.)
bysort id (year) : ge help2=help1[_N]>0
l id year intdate marstat mardur begin1 end1 begin2 help1 nohist if help2>0, sepby(id)
*/

//first marriages
gen n_marr=0 if marstat==1
//date of second marriage before current interview date
replace n_marr=1 if intdate>begin1 
//date of second marriage at current interview date and marital status is married
replace n_marr=1 if intdate==begin1 & marstat==2
replace n_marr=1 if marstat>1 & marstat<.

//higher order marriages 
//date of second marriage before current interview date
replace n_marr=2 if intdate>=begin2 & begin2<. 
cap drop help*
//currently married and at least once sep./div./wid. earlier
bysort id (year) : ge help1=sum(inlist(marstat,2))
bysort id (year) : ge help2=sum(inlist(marstat,3,4,5,6))
replace n_marr=2 if marstat==2 & help1>0 & help2>0  
cap drop help*
bysort id (year) : ge help=sum(n_marr==2)
replace n_marr=2 if n_marr==1 & help>0 & help<.

/*
*check : --> ok
*l id year intdate n_marr marstat mardur begin1 end1 begin2 help nohist, sepby(id)
*/

*** inconsistent marital histories

//never-married again after 1st marriage
//recode to inconsistent after marriage
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2,3,4,5,6))
bysort id (year) : ge help2=sum(marstat==1 & help1>0 & help1<.)
bysort id (year) : ge help3=help2[_N]>0
bysort id (year) : replace nohist=2 if nohist==0 & help2>0 
bysort id (year) : replace nohist=2 if nohist==0 & help3>0 & marstat>1 

//never-married, then 1st marriage, but marriage date not given (and gap > 1 year)
//recode to inconsistent after marriage
cap drop help*
bysort id (year) : ge help0=sum(marstat==1 & nohist==0)
bysort id (year) : ge help1=sum(help0>0 & help0<. & marstat==2 & begin1==.)
bysort id (year) : ge help2=help1[_N]>0
bysort id (year) : replace nohist=3 if nohist==0 & help1>0 
bysort id (year) : replace nohist=3 if nohist==0 & help2>0 & marstat>1 
/*
* check : --> ok
 l id year intdate n_marr marstat mardur begin1 end1 begin2 help* nohist if help2>0, sepby(id)
*/
drop help*


//inconsistency marital status never-married and marriage date
cap drop help*
bysort id (year) : ge help1=sum(marstat==1 & n_marr==1 & nohist==0)
bysort id (year) : ge help2=help1[_N]>0
bysort id (year) : replace nohist=2.5 if nohist==0 & help1>0 
bysort id (year) : replace nohist=2.5 if nohist==0 & help2>0 & marstat>1 
drop help*
/*
*check : --> ok
levelsof id if marstat==1 & n_marr==1, local(levels) sep(,)
l id year intdate n_marr marstat mardur begin1 end1 begin2 nohist if inlist(id,`l'), sepby(id)
*/


sort id year
save marrdur, replace


preserve 
bys id (year) : keep if _n==_N
tab nohist
restore


************************************************************************************************
*** Number of biological children (ever born)
*** age of children
************************************************************************************************

use mwp_us_childbirth, clear


ren  r0000100 id
drop  r0214700 r0214800

ren r0898801 incons_fert
recode incons -4 1=0 2 3=1

local i=1
local c=1
foreach var of varlist r9900001-r9907402 {
	if `i'==1 {
		ren `var' bmonth`c'
		local i=2
	}
	else if `i'==2 {
		ren `var' byear`c'
		local i=1
		local c=`c'+1
	}
}

save childbirth, replace


use intmonth, clear
sort id year
merge m:1 id using childbirth
drop _merge
sort id year

bys id (year) : egen help=max(incons)
replace incons=help if year<1982

//Interview date: months counted as of Jan 1960
gen intdate=ym(intyear, intmonth)

//Birth dates: months counted as of Jan 1960
for num 1/11 : gen bdateX=ym(byearX, bmonthX) 
for num 1/11 : replace bdateX=byearX if byearX<0 
for num 1/11 : replace bdateX=. if incons==1


//birth month of child 1,...,11
for num 1/11 : bys id (year) : egen helpX=max(bdateX)
for num 1/11 : replace bdateX=helpX

l id year intdate bdate1 bdate2 bdate3 incons nointerview_m if _n<500, sepby(id)

//dummy for child expectancy (pregnancy of partner)
for num 1/11 : ge pregnantX=(bdateX<. & bdateX>0 & (bdateX-9)<=intdate & bdateX>intdate)   
egen pregnant=rowmax(pregnant1-pregnant11)

//birth month of youngest child
for num 1/11 : replace helpX=. if (bdateX<. & bdateX>0 & bdateX>intdate)   
egen cbdate=rowmax(help*) 
replace cbdate=. if cbdate<0
//age of youngest child
gen ageyc=intdate-cbdate if cbdate>0
recode ageyc .=0
//number of children born
for num 1/11 : ge cXborn=(helpX<. & helpX>0 & helpX<=intdate)   
egen nchild=rowtotal(c*born) 

l id year intdate pregnant cbdate ageyc nchild bdate1 bdate2 bdate3 incons nointerview_m if _n<500, sepby(id)

ren incons biovalid
keep id year nchild* ageyc pregnant cbdate biovalid

sort id year
save childbirth, replace


preserve 
bys id (year) : keep if _n==_N
tab biovalid
restore


************************************************************************************************
*** Cohabitation
************************************************************************************************

use mwp_us_cohabitation2.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

order t1026702 r9910800 r9911000 r9911400, last


local year=1979
foreach var of varlist r0218004-r9911400 {
	if `year'<1994 {
		ren `var' cohabitation`year'
		local year=`year'+1
	}
	else if `year'>=1994 {
		ren `var' cohabitation`year'
		local year=`year'+2
	}
}
	
	
reshape long cohabitation, i(id) j(year)

tab cohab, m
tab year cohab, m
recode cohab 1=1 36=. .=. *=0, gen(spouse)
recode cohab 33=1 36=. .=. *=0
tab cohab spouse, m

keep cohabitation spouse id year
			 
sort id year
save cohab, replace


/*
 -999: Never reported spouse/partner
    0: No current spouse/partner
    1: Spouse
   33: Partner
   36: Other
*/	  
	  



************************************************************************************************
*** Enrollment in education
************************************************************************************************

use mwp_us_enroll.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

local year=1979
foreach var of varlist r0216600-t0988700 {
	ren `var' enrol`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}

local year=2008
foreach var of varlist t1213900 t2272400 t3212500 {
	ren `var' enrol`year'
	local year=`year'+2
}

keep id enrol*

reshape long enrol, i(id) j(year)

tab year enrol
recode enrol 2 3=1 1 4=0 -5/-1=. if year<2008
recode enrol -4=0 -5=. if year>=2008
tab year enrol, m

/*
before 2008 : 
  1: 1  NOT ENROLLED, COMPLETED LESS THAN 12TH GRADE
  2: 2  ENROLLED IN HIGH SCHOOL
  3: 3  ENROLLED IN COLLEGE
  4: 4  NOT ENROLLED, HIGH SCHOOL GRADUATE
*/

sort id year
save enroll, replace




************************************************************************************************
*** Years of education
************************************************************************************************

use mwp_us_educ.dta, clear


ren  r0000100 id
drop  r0214700 r0214800


local year=1979
foreach var of varlist r0216700-t4113000 {
	ren `var' yeduc`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}


reshape long yeduc, i(id) j(year)

sort id year
save educ, replace


************************************************************************************************

************************************************************************************************

use mwp_us_weeks.dta, clear


ren  r0000100 id
drop  r0214700 r0214800


local year=1979
foreach var of varlist  r0215300- t4113900 {
	ren `var' exp`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}


reshape long exp, i(id) j(year)

sort id year
save weeks, replace

************************************************************************************************

************************************************************************************************

use mwp_us_weeks_miss.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

local year=1979
foreach var of varlist r0215301-t4114200 {
	ren `var' gap`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}


reshape long gap, i(id) j(year)

sort id year
save weeks_miss, replace


****************************************************
*** WORK EXPERIENCE
****************************************************

use weeks, clear
merge 1:1 id year using weeks_miss
*bysort id (year): gen help1=sum(gap>25) // any gap in history larger than 3 months (25 percent of year)
bysort id (year): gen help2=sum(gap) if gap>=0 // cumulated gaps (summing up to more than 1 year (> 100 percent of year)
replace exp=. if exp<0
bysort id (year): replace exp=sum(exp) 
replace exp=exp/52
replace exp=.n if gap==-5 // attrition or wave non-response
*replace exp=. if help1>1
replace exp=.g if help2>100 & help2<. // gaps sum up to more than 1 year
sort id year
keep id year exp gap
save exp, replace



************************************************************************************************
*** ALTERNATIVE MEASURE WORK EXPERIENCE
************************************************************************************************

use mwp_us_anhours.dta, clear

ren  r0000100 id

local year=1979
foreach var of varlist  r0215310- t4113800 {
	ren `var' anhours`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}


reshape long anhours, i(id) j(year)

sort id year
save anhours, replace


use weeks, clear
merge 1:1 id year using weeks_miss
drop _merge
merge 1:1 id year using anhours
drop _merge

replace anhours=. if anhours<0
*bysort id (year): gen help1=sum(gap>25) // any gap in history larger than 3 months (25 percent of year)
bysort id (year): gen help2=sum(gap) if gap>=0 // cumulated gaps (summing up to more than 1 year (> 100 percent of year)
replace exp=. if exp<0
ge exp_ft=exp if (anhours/(35*52))>=(exp/52) & (anhours/(35*52))<.
ge exp_pt=exp if (anhours/(35*52))<(exp/52)
replace exp=exp*.5 if (anhours/(35*52))<(exp/52)
l id year exp anhours in 1/500, sepby(id)
for var exp exp_ft exp_pt : bysort id (year): replace X=sum(X) 
for var exp exp_ft exp_pt : replace X=X/52
for var exp exp_ft exp_pt : replace X=.n if gap==-5 // attrition or wave non-response
*replace exp=. if help1>1
for var exp exp_ft exp_pt : replace X=.g if help2>100 & help2<. // gaps sum up to more than 1 year
l id year exp* anhours in 1/500, sepby(id)
ren exp exp_alt
sort id year
keep id year exp_ft exp_pt exp_alt anhours
save exp_alt, replace



************************************************************************************************
*** Indicator for main (=CPS) employer
************************************************************************************************

use mwp_us_cps.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

local n=1
foreach var of varlist  r0337700-r4003300 {
	ren `var' cps_`n'
	local n=`n'+1
}

local year=1980
forvalues n=1 6 to 65 {
	ren cps_`n' cps1`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1980
forvalues n=2 7 to 65 {
	ren cps_`n' cps2`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1980
forvalues n=3 8 to 65 {
	ren cps_`n' cps3`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1980
forvalues n=4 9 to 65 {
	ren cps_`n' cps4`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1980
forvalues n=5 10 to 65 {
	ren cps_`n' cps5`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}

reshape long cps1 cps2 cps3 cps4 cps5, i(id) j(year)

for var cps1 cps2 cps3 cps4 cps5 : replace X=. if X<0
egen check=rsum(cps1 cps2 cps3 cps4 cps5)

gen cps=.
forvalues n=1/5 {
	replace cps=`n' if cps`n'==1 
}
drop cps1 cps2 cps3 cps4 cps5
sort id year
save cps, replace



/* 
NOTE : 
Until 1994, the current or most recent employer, called the "CPS employer," 
is differentiated in the data set from other employers 
for whom the respondent reported working since the last interview by title 
(that is, start date for CPS job, start date for Job #2, start date for Job #3, and so forth).  

Beginning in 1994 CPS job information is simply labeled as "job #1" 
because job specific information is all collected in the Employer Supplement.  

Every employer for whom a respondent worked since the last interview, including the CPS employer, 
is identified within the data set by a yearly job number, 
for example, Job #1, Job #5, with the number reflecting the order in which the job was reported.  
The detailed job characteristic information at the end of this section is collected for each CPS job, 
regardless of whether it is a full- or part-time job.  
*/


************************************************************************************************
*** TENURE
************************************************************************************************

use mwp_us_ten.dta, clear


ren  r0000100 id
drop  r0214700 r0214800

local n=1
foreach var of varlist r0068710-t4110800 {
	ren `var' ten_`n'
	local n=`n'+1
}

local year=1979
forvalues n=1 6 to 125 {
	ren ten_`n' ten1`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1979
forvalues n=2 7 to 125  {
	ren ten_`n' ten2`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1979
forvalues n=3 8 to 125 {
	ren ten_`n' ten3`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1979
forvalues n=4 9 to 125 {
	ren ten_`n' ten4`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
local year=1979
forvalues n=5 10 to 125 {
	ren ten_`n' ten5`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}

reshape long ten1 ten2 ten3 ten4 ten5, i(id) j(year)


****************************************************

sort id year
merge 1:1 id year using cps

gen tenure=.
replace tenure=ten1 if year==1979 | year>1992

forvalues n=1/5 {
	replace tenure=ten`n' if cps==`n' & (year>1979 & year<=1992)		
}

*replace tenure=0 if tenure==-4
replace tenure=. if tenure<0
*for num 1/5 : replace tenure=tenX if tenure==. & tenX>0 & tenX<. 
replace tenure=tenure/52


sort id year
keep id year tenure check
save tenure, replace



************************************************************************************************
*** Current Labor Force Status
************************************************************************************************

use mwp_us_lfs.dta, clear

ren  r0000100 id

local week=0
foreach var of varlist w0061200-w1263500 {
		ren `var' lfs`week'
		local week=`week'+1
}

//reshape weekly array to long format (person-week observations)
//Note: reshape at once takes much computation time!
//Therefore, reshape data set in parts (takes 12 min on my machine)
/*
reshape long lfs, i(id) j(week)
sort id week
save lfs, replace
*/
preserve
keep id-lfs500
reshape long lfs, i(id) j(week)
sort id week
save lfs1, replace
restore
preserve
keep id lfs501-lfs1000
reshape long lfs, i(id) j(week)
sort id week
save lfs2, replace
restore
preserve
keep id lfs1001-lfs1500
reshape long lfs, i(id) j(week)
sort id week
save lfs3, replace
restore
preserve
keep id lfs1501-lfs1879
reshape long lfs, i(id) j(week)
sort id week
save lfs4, replace
restore

//match parts
use lfs1, clear
append using lfs2
append using lfs3
append using lfs4
save lfs, replace


************************************************
*** Match complete lfs weekly array to week of interview
*** define employment status as currently working 
*** if working in interview week (working=1)
*** or any of the 6 preceding weeks (working=2)


use intweek, clear
ge week=intweek
merge 1:1 id week using lfs

sort id week
ge working_w=lfs if week==intweek
for num 1/6 : qbys id (week) : replace working_w=lfs if working_w==. & week[_n+X]==intweek[_n+X] & week==(intweek[_n+X]-X) 
l in 1/500, sepby(id) //check : -->ok
recode working_w 0=-1 1/10=0 100/max=1 
ge working=working_w if week==intweek
for num 1/6 : bys id (week) : replace working=2 if working!=1 & working_w[_n-X]==1 & intweek==(week[_n-X]+X) 
l in 1/500, sepby(id) //check : -->ok

ta working if week==intweek, m
ta working if week==intweek & noint==0, m

drop if _merge==2
keep id year intweek lfs working nointerview_w
sort id year
save labforstat, replace


tab lfs working, m


*************************************************************************
*** Current Work Hours All Jobs
*************************************************************************

use mwp_us_workhours.dta, clear

ren  r0000100 id

local week=0
foreach var of varlist w0002000-w1210400 {
		ren `var' hours`week'
		local week=`week'+1
}

//reshape weekly array to long format (person-week observations)
//Note: reshape at once takes much computation time!
//Therefore, reshape data set in parts (takes 12 min on my machine)
/*
reshape long hours, i(id) j(week)
sort id week
save hours, replace
*/
preserve
keep id-hours500
reshape long hours, i(id) j(week)
sort id week
save hours1, replace
restore
preserve
keep id hours501-hours1000
reshape long hours, i(id) j(week)
sort id week
save hours2, replace
restore
preserve
keep id hours1001-hours1500
reshape long hours, i(id) j(week)
sort id week
save hours3, replace
restore
preserve
keep id hours1501-hours1879
reshape long hours, i(id) j(week)
sort id week
save hours4, replace
restore

//match parts
use hours1, clear
append using hours2
append using hours3
append using hours4
save hours, replace


************************************************
*** Match complete hours weekly array to week of interview
*** define work hours as current work hours of interview week 
*** if missing for interview week, use most recent hours (up to 6 weeks earlier)


use intweek, clear
ge week=intweek
merge 1:1 id week using hours

sort id week
recode hours -20/-1=-1
*ge hours_w=hours if week==intweek
*for num 1/6 : qbys id (week) : replace hours_w=hours if hours_w==. & week[_n+X]==intweek[_n+X] & week==(intweek[_n+X]-X) 
*l in 1/500, sepby(id) //check : -->ok
*recode hours_w 0=-1 1/200=1 
*ge workhours=working_w if week==intweek
for num 1/6 : bys id (week) : replace hours=hours[_n-X] if hours<=0 & hours[_n-X]>0 & hours[_n-X]<. & intweek==(week[_n-X]+X) 
l in 1/500, sepby(id) //check : -->ok

ta hours if week==intweek, m
ta hours if week==intweek & noint==0, m

drop if _merge==2
keep id year intweek hours
sort id year
save hours, replace

/*
tab hours, m
recode hours 1/200=1, gen(help)
tab year help, m row
*/

************************************************************************************************


************************************************************************************************
*** Occupation: self-employed, public, private sector employees 
************************************************************************************************

use mwp_us_class.dta, clear


ren  r0000100 id
drop  r0214700 r0214800


*lookfor class of wrkr @ cur/m-rcnt job cp

/*
NOTE: answer list changed
    1979-93

 1       private company
 2       government 
 3       self-employed
 4       without pay 

    1994 and later

  1      government
  2      private for profit company
  3      nonprofit organization
  4      self-employed
  5      working in family business 
*/		
		



local year=1979
foreach var of varlist r0046800 r0263500 r0446600 r0702300 r0945400 r1255800 ///
						r1650600 r1923200 r2318000 r2525800 r2924800 r3127500 ///
						r3523200 r3728200 r4182300 {
	ren `var' occstat`year'
	recode occstat`year' 3 4=3
	local year=`year'+1
}

local year=1994
foreach var of varlist r4587905 r5270700 r6473100 r6592300 r7210100 ///
						r7898500 t1037500 t1298500 t3186500 t3309200 {
	ren `var' occstat`year'
	recode occstat`year' 2 3=1 1=2 4 5=3
	local year=`year'+2
}

keep id occstat*

reshape long occstat, i(id) j(year)

sort id year
save occstat, replace



************************************************************************************************
*** Control variables: urbanicity, region
************************************************************************************************

use mwp_us_controls.dta, clear


ren  r0000100 id
ren r0173600 subsample

drop  r0214700 r0214800
drop r8498600 t0990400

local year=1979
foreach var of varlist r0215200 r0393520 r0647000 r0897900 r1146400 r1521700 r1892400 r2259500 ///
						r2447000 r2872800 r3076500 r3403200 r3658600 r4009100 r4420200 r5083200 ///
						r5168500 r6481300 r7009000 r7706300 r8498700 t0990500 t2212300 t3110200 t4114700 {
	if `year'<1994 {
		ren `var' smsa`year'
		local year=`year'+1
	}
	else if `year'>=1994 {
		ren `var' smsa`year'
		local year=`year'+2
	}
}


local year=1979
foreach var of varlist r0215100 r0393510 r0646900 r0897800 r1146500 r1521600 r1892300 r2259400 ///
						r2446900 r2872700 r3076400 r3403100 r3658500 r4009000 r4420100 r5083100 ///
						r5168400 r6481200 r7008900 r7706200 r8498601 t0990401 t2212200 t3110100 t4114600 {
	if `year'<1994 {
		ren `var' urban`year'
		local year=`year'+1
	}
	else if `year'>=1994 {
		ren `var' urban`year'
		local year=`year'+2
	}
}


keep id smsa* urban* subsample

reshape long smsa urban, i(id) j(year)

sort id year
save controls, replace



************************************************************************************************

************************************************************************************************



/*
NOTE:

Top Coding: Because the NLSY79 is a public use data set distributed widely throughout the research and public policy communities, 
the survey takes extensive measures to protect the confidentiality of respondents. One method of ensuring confidentiality is to "top code" unusually high income values. 
The NLSY79 top code values were originally designed to prevent identification of the top two percent of respondents.

The NLSY79 has used four top coding algorithms for income. 
(1) From 1979 to 1984, every NLSY79 income question that elicited a response above $75,000 was truncated to $75,001. 
(2) From 1985 to 1988, the values were increased to $100,000 and $100,001 respectively. 
Unfortunately, this algorithm results in a sharp downward bias in the mean value of NLSY79 income holdings since the entire right hand tail is truncated. 
(3) To fix this problem, a new algorithm was introduced beginning in 1989. The new top code algorithm replaced all values above the cutoff with the average of all outlying values. 

(4) Beginning in 1996, another new algorithm was used. 
This algorithm takes the top two percent of respondents with valid values and averages them. That averaged value replaces all values in the top range.

Top coding primarily affects seven of the NLSY79 income variables. The seven variables that are top coded include 
the income from respondent's wages, respondent's business, spouse's wages, spouse's business, partner's wages, rest of the family, 
and other sources such as rents, interest, and dividends.

A second issue with NLSY79 data concerns individuals living outside the U.S. Living outside the U.S. does not preclude a respondent from being interviewed. 
For example, in 1992, 125 respondents lived abroad. 
Between 1989 and 1992, for people who hold assets denominated in foreign currency, little effort was made to transform these assets into dollar figures. 
Instead, such values are classified as "invalid skips" in the data. Beginning in 1993, an effort was made to convert these currencies whenever the unit in which the response was made could be determined. While researchers are warned that this occurs, relatively few individuals live outside the U.S.



*/


************************************************************************************************
*** Original info on pay rate and time unit of rate of pay 
*** (used by NLSY staff to compute hourly wages)
************************************************************************************************

use mwp_us_earnings.dta, clear


ren  r0000100 id


local year=1979
local v=1
foreach var of varlist r0091700-r4228800 {
	if inlist(`v',1,3,5,7,9) {
		ren `var' earnings`=((`v'+1)/2)'_`year'
	}
	if inlist(`v',2,4,6,8,10) {
		ren `var' tu`=(`v'/2)'_`year'
	}
	local v=`v'+1
	if `v'>10 {
		local v=1
		local year=`year'+1
	}
}

order r5109800-r5110200, after(tu5_1993)
order r6473800-r6474200 r5958800-r5959200, after( r5424400 )
order r5273600 r5313700 r5352700 r5389800 r5424300 ///
		r5273700 r5313800 r5352800 r5389900 r5424400, after( r4753600 )

		
		
local year=1994
local v=1
foreach var of varlist r5109800-t3342200 {
	if inrange(`v',1,5) {
		cap ren `var' earnings`v'_`year'
		local v=`v'+1
	}
	if inrange(`v',6,11) {
		cap ren `var' tu`=(`v'-6)'_`year'
		local v=`v'+1
	}
	if `v'>11 {
		local v=1
		local year=`year'+2
	}
}

reshape long earnings1_ earnings2_ earnings3_ earnings4_ earnings5_ ///
			tu1_ tu2_ tu3_ tu4_ tu5_, i(id) j(year)

for num 1/5 : ren earningsX_ earningsX			
for num 1/5 : ren tuX_ tuX			
			

****************************************************
*** EARNINGS and TIME UNIT

sort id year
merge 1:1 id year using cps

gen earnings=.
replace earnings=earnings1 if year==1979 | year>1992

forvalues n=1/5 {
	replace earnings=earnings`n' if cps==`n' & (year>1979 & year<=1992)		
}
replace earnings=. if earnings<0

gen tu=.
replace tu=tu1 if year==1979 | year>1992

forvalues n=1/5 {
	replace tu=tu`n' if cps==`n' & (year>1979 & year<=1992)		
}
replace tu=. if tu<0

/*
  1: per hour
  2: per day
  3: per week
  4: bi-weekly (every 2 weeks)
  5: per month
  6: per year
  7: other (SPECIFY)
  8: bi-monthly (twice a month)
*/

sort id year
keep id year earnings tu
save earnings, replace



************************************************************************************************
*** Wife's employment : hours usually worked in last year 
************************************************************************************************

cd "$mwpUSdir\NLSY79"

use mwp_us_wifeempl.dta, clear


ren  r0000100 id
drop r0214800


local year=1980
foreach var of varlist  r0226200-t3506600 {
	ren `var' wife_hours`year'
	if `year'<1994 {
	local year=`year'+1
	}
	else { 
	local year=`year'+2
	}
}
ge wife_hours1979=.

reshape long wife_hours, i(id) j(year)

sort id year
save wife_empl, replace



************************************************************************************************
*** CPI: Consumer Price Index
************************************************************************************************

/*
Consumer price index as published by U.S. Bureau of Labor Statistics
Malik Crawford, Jonathan Church, Bradley Akin:
"CPI Detailed Report: Data for January 2015"
(Table 24: Historical Consumer Price Index for All Urban Consumers (CPI-U):  U. S. city average, all)
http://www.bls.gov/cpi/cpid1501.pdf
(last access : 24/09/2015)

original base is average 1982-84;
conversion to base year 2006
original value for 2006 is 201.6, will be 100.0
*/

clear 
set obs 36

ge year=_n+1978

ge cpi=.
replace cpi =  72.6   if year==1979  
replace cpi =  82.4   if year==1980  
replace cpi =  90.9   if year==1981  
replace cpi =  96.5   if year==1982  
replace cpi =  99.6   if year==1983  
replace cpi = 103.9   if year==1984  
replace cpi = 107.6   if year==1985  
replace cpi = 109.6   if year==1986  
replace cpi = 113.6   if year==1987  
replace cpi = 118.3   if year==1988  
replace cpi = 124.0   if year==1989  
replace cpi = 130.7   if year==1990  
replace cpi = 136.2   if year==1991  
replace cpi = 140.3   if year==1992  
replace cpi = 144.5   if year==1993  
replace cpi = 148.2   if year==1994  
replace cpi = 152.4   if year==1995  
replace cpi = 156.9   if year==1996  
replace cpi = 160.5   if year==1997  
replace cpi = 163.0   if year==1998  
replace cpi = 166.6   if year==1999  
replace cpi = 172.2   if year==2000  
replace cpi = 177.1   if year==2001  
replace cpi = 179.9   if year==2002  
replace cpi = 184.0   if year==2003  
replace cpi = 188.9   if year==2004  
replace cpi = 195.3   if year==2005         
replace cpi = 201.6   if year==2006         
replace cpi = 207.342 if year==2007  
replace cpi = 215.303 if year==2008  
replace cpi = 214.537 if year==2009  
replace cpi = 218.056 if year==2010  
replace cpi = 224.939 if year==2011  
replace cpi = 229.594 if year==2012  
replace cpi = 232.957 if year==2013  
replace cpi = 236.736 if year==2014  

replace cpi=cpi/2.016
ren year intyear

cd "$mwpUSdir\NLSY79"

save mwp_us_cpi_2006, replace




*==============================================================

* Merge all information, store as long-format panel data set

*==============================================================

cd "$mwpUSdir\NLSY79"


use wage, clear
merge 1:1 id year using exp
tab _merge
drop _merge
sort id year
merge 1:1 id year using exp_alt
tab _merge
drop _merge
sort id year
merge 1:1 id year using tenure
tab _merge
drop _merge
sort id year
merge 1:1 id year using occstat
tab _merge
drop _merge
sort id year
merge 1:1 id year using marrdur
tab _merge
drop _merge
sort id year
merge 1:1 id year using cohab
tab _merge
drop _merge
sort id year
merge 1:1 id year using childbirth
tab _merge
drop _merge
sort id year
merge 1:1 id year using enroll
tab _merge
drop _merge
sort id year
merge 1:1 id year using educ
tab _merge
drop _merge
sort id year
merge 1:1 id year using controls
tab _merge
drop _merge
sort id year
merge 1:1 id year using intweek
tab _merge
drop _merge
sort id intweek
merge 1:1 id intweek using labforstat
tab _merge
drop _merge
sort id intyear
merge m:1 intyear using mwp_us_cpi_2006 
drop if _merge==2
drop _merge
sort id year
merge 1:1 id year using earnings
tab _merge
drop _merge
sort id year
merge 1:1 id year using hours
tab _merge
drop _merge
sort id year
merge 1:1 id year using wife_empl
tab _merge
drop _merge
sort id year

cd "$mwpUSdir"

sort id year
save mwp_US, replace





