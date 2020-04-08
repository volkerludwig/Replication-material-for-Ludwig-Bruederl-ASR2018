/*
*==============================================================

Stata 14 Do-File #2 of 3 for Replication of article:

Ludwig, Volker and Josef Brüderl. 2018. 
"Is there a Marital Wage Premium? New Evidence from the United States".
American Sociological Review 83(4). 

*==============================================================
*/



//set working directory !!! Adapt this as needed !!!
global mwpUSdir "X:\mwp_US\data\"


****************************************************
****************************************************
*** Generating / Recoding variables
****************************************************
****************************************************


cd $mwpUSdir

use mwp_US, clear

xtset id year


for var wage exp tenure occstat marstat nchild enrol yeduc : replace X=. if X<0


*** Dep var : gross hourly wages

replace wage=wage/100 //convert to dollars
replace wage=(wage/cpi)*100 // convert to 2006 prices
gen lnw=ln(wage)
lab var wage "Gross hourly wage (2006 US dollars)"
lab var lnw "Ln (Gross hourly wage, 2006 US dollars)"
//NOTE : very high and very low wages are very likely data errors
//set to missing below
su wage, d
su wage if wage>0, d
su D.wage if wage<.5, d
su D.wage if wage>500 & wage<., d


** Cohort
replace cohort=1900+cohort
tab cohort, gen(dcoh)
lab var cohort "Birth cohort"

** Age
ge age=year-cohort
lab var age "Age (years)"


*** Marital status, Marriage duration, Number of current marriage


//Marital status
tab marstat, m
lab var marstat "Current marital status"
lab def marstat ///
	1 "Never-married" ///
	2 "Married" ///
	3 "Married, Separated" ///
	4 "Divorced" ///
	6 "Widowed"
lab val marstat marstat


//Number of current marriage
tab n_marr
lab var n_marr "Number of current marriage"
//Interview in first month of 1st marriage --> recode to 1st marriage (ok, no cases left)
replace n_marr=1 if marstat==2 & mardur==0 & intdate==begin1 


//Identify and recode incomplete / inconsistent histories

//no interview in current survey year (gap)
ta gap
ta nointerview_m nointerview_w, m
ta gap if nointerview_m==1
ta gap if nointerview_m==0 & nointerview_w==1
replace nohist=1 if nointerview_m==1
replace nohist=1.5 if nointerview_m==0 & nointerview_w==1
tab marstat n_marr if nohist==0, m


*** Final recodes to marital status, marriage duration, number of marriage

//Never-married, but date of 1st marriage earlier
//Assume that date of marriage is correct,
//Recode to 1st  marriage, recode marriage duration
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,1) & n_marr==1 & inlist(nohist,0,2.5))
bysort id (year) : ge help2=help1[_N]>0
xttab help2
l id year intdate marstat mardur  begin1 end1 begin2 n_marr gap help1 nohist begin1m month if help2>0, sepby(id)
replace mardur=(intdate-begin1) if marstat==1 & n_marr==1 & help1>0 & help2>0
replace marstat=2 if marstat==1 & n_marr==1 & help1>0 & help2>0
//Check --> ok
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,1) & n_marr==1 & inlist(nohist,0,2.5))
bysort id (year) : ge help2=help1[_N]>0
xttab help2
l id year intdate marstat mardur  begin1 end1 begin2 n_marr gap help1 nohist begin1m month if help2>0, sepby(id)


//Additional checks

//Never-married, marriage duration > 0 
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,1) & mardur>0 & inlist(nohist,0,2.5))
bysort id (year) : ge help2=help1[_N]>1
l id year intdate marstat mardur  begin1 end1 begin2 n_marr help1 nohist if help2>1, sepby(id)
// ok

//1st marriage, marriage duration 0 more than once
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2) & mardur==0 & inlist(nohist,0,2.5))
bysort id (year) : ge help2=help1[_N]>1
l id year intdate marstat mardur  begin1 end1 begin2 n_marr help1 nohist if help2>1, sepby(id)
// ok

//1st marriage, marriage duration < marriage duration [_n-1]
cap drop help*
bysort id (year) : ge help1=sum(inlist(marstat,2) & n_marr==1 & mardur<mardur[_n-1] & inlist(nohist,0,2.5))
bysort id (year) : ge help2=help1[_N]>1
l id year intdate marstat mardur  begin1 end1 begin2 n_marr help1 nohist if help2>1, sepby(id)
// ok


*** Duration to / from 1st marriage

//Compute year of 1st marriage (year first observed in 1st marriage)
bysort id (year) : ge help=year if marstat==2 & n_marr<=1
bysort id (year) : egen maryear=min(help)
drop help
lab var maryear "year of 1st marriage (year first observed in 1st marriage)"

//Compute month of marriage
cap drop help*
bysort id (year) : ge help1=begin1 if year==maryear
bysort id (year) : egen mardate=max(help1)
lab var mardate "Month of marriage (count months since Jan 1960)"

//Compute duration months to / since 1st marriage (for entering never-married eventually observed married)
ge mardur_ts=mardur if marstat==2 //months since 
bysort id (year) : replace mardur_ts=intdate-mardate if marstat==1
lab var mardur_ts "Duration months to / since 1st marriage"
//Check --> ok
count if marstat==1 & mardur_ts>0 & mardur_ts<. & inlist(nohist,0,2.5)

ren nohist nohistory
/*
lab def nohistory ///
1   "gap, no interview" ///
1.5 "no interview week" ///
2   "married -> never-married" ///
2.5 "incons. never-marr., marr.date" 
3   "no marr.date" ///
lab val nohistory nohistory
ta nohist
* NOTE: persons with values 2, 2.5, 3 will be excluded from sample
*/

//Compute age at marriage (using monthly information)
su mardate
ge birthdate=ym(cohort,6)
ge marage=round((mardate-birthdate)/12)
lab var marage "Age at 1st marriage"	
su marage, d
*l id year age marage marstat in 1/500, sepby(id)
*--> ok

// Marriage dummy (time-varying), dummy ever married (time-constant)
ge marry=(marstat==2 & n_marr==1 & nohist==0)
lab var marry "Dummy married in 1st marriage"
bysort id (year) : egen evermarr=max(marry) //used previously, but several men actually observed married later, but marriage date not known exactly
cap drop help
bysort id (year) : replace evermarr=1 if evermarr==0 & maryear<. & maryear>0
ta evermarr marry, m
lab var evermarr "Dummy whether resp. ever married (time-constant)"
replace mardur_ts=0 if evermarr==0
replace mardur_ts=0 if mardur_ts==. & marstat==1 & n_marr==0


//Cohabitation (while never-married) 
//(NOTE: cohabitation not included in published article)
tab marstat cohab, m
tab marstat spouse, m
tab cohab spouse if marstat==1, m
replace cohab=1 if cohab==0 & marstat==1 & spouse==1
tab cohab spouse if marstat==2, m
replace cohab=0 if cohab==1 & marstat==2
tab marstat cohab, m
tab marry cohab, m

/*
ta year spouse, m
ta year cohab, m
table year cohab, c(m spouse freq)
*/
tab marry cohab
replace cohab=0 if marry==1 & cohab==1

tab marstat cohab
tab cohab spouse if marry==0
replace cohab=1 if cohab==0 & marry==0 & spouse==1


*** Number of biological children
tab nchild, m
tab nchild biovalid, m 
xttab biovalid
//Inconsistent birth date or number of children
//Recode number of children to missing
l id year intdate pregnant cbdate ageyc nchild nointerview_m biovalid if _n<500, sepby(id)
for var nchild pregnant : replace X=. if biovalid==1


*** Work experience
lab var exp "Work experience (years)"
ge expq=exp^2
lab var expq "Experience squared"


** Tenure with current employer
label var tenure "Tenure (years)"
gen tenureq=tenure^2


** Private / public sector, self-employed
ge public=occstat==2 if occstat<.
ge self=occstat==3 if occstat<.
lab var self "Dummy currently self-employed"

*** Education
ta yeduc, m
recode yeduc 95=.
recode yeduc 0=3
cap drop help*
ge help=yeduc
bys id (year): replace yeduc=yeduc[_n-1] if yeduc==. & yeduc[_n-1]<.
bys id (year): replace yeduc=yeduc[_n-1] if yeduc<yeduc[_n-1] & yeduc[_n-1]<.
*l id year age yeduc help gap exp nohist in 1/500
label var yeduc "Education (years of general schooling)"
ta yeduc

*** Enrollment
ta enrol, m


* Potential work experience
gen pexp=age-yeduc-5
cap drop help
bys id (year)  : egen help=min(pexp)
l id year age yeduc enrol pexp exp if help<0, sepby(id)
//problem with early career: 
//pot exp should not be negative
replace pexp=0 if pexp<0
lab var pexp "Potential experience (yrs.) (age-yeduc-5)"
/*
//pot exp should not decrease over time
cap drop help*
bys id (year) : gen help=pexp-pexp[_n-1] 
bys id (year) : egen help2=min(help)
*l id year age yeduc enrol pexp exp help help2 if help2<0, sepby(id)
bys id (year) : replace pexp=pexp[_n-1] if pexp==. & pexp[_n-1]<.
bys id (year) : replace pexp=pexp[_n-1] if pexp<pexp[_n-1] & pexp[_n-1]<.
//l id year age yeduc enrol pexp exp help help2 if help2<0, sepby(id)
// --> ok
*/
cap drop pexpq
ge pexpq=pexp^2
lab var pexpq "Pot. experience square"

* Entry to stable employment (a,b,c applies)
//a) has reached highest educational level attained at age 25
cap drop help*
bys id (year) : ge help=yeduc if age==25
bys id (year) : egen yeduc25=max(help) 
//b) works year-round full-time
su anhours, d
//c) is currently not enrolled, has valid wage
cap drop help*
ge help=0
replace help = 1 if yeduc>=yeduc25 & yeduc<. & anhours>=1800 & anhours<. & enrol==0 & inlist(working,1,2)
bys id (year) : gen entry=sum(help)
recode entry 1/max=1
lab var entry "Dummy entry to stable employment"
l id year age yeduc yeduc25 anhours enrol working entry in 1/2000, sepby(id)
xttab entry
xttrans entry, freq
tab entry marstat, row

*** Employment
* labor force status and whether currently working 
ta working, m
lab var working "Dummy whether currently working for pay"		
lab def working -1 "No info reported", modify
lab def working 0 "Not working", modify
lab def working 1 "Working in interview week", modify
lab def working 2 "Not working, but within 6 prev. weeks", modify
lab val working working
ta lfs working if nointerview_m==0, row m
/*
  	0= 	no information reported to account for week.
  	2= 	not working (unemployment vs. out of the labor force cannot be determined.)
  	3= 	associated with an employer but the periods not working for the employer are missing. If all of the time with the employer cannot be accounted for, a 3 is loaded into the STATUS array instead of a job code.
  	4= 	unemployed. If a respondent is not working and part of the time is spent looking for work or on layoff, the exact weeks spent looking for work is unknown. As a result, the number of weeks spent looking is assigned to the middle part of the period not working.
  	5= 	out of the labor force.
  	7= 	active military service. If a respondent has a civilian job while in active military service, the civilian job code is loaded into the array instead of a code of 7.
  	>100= 	worked. The code represents the appropriate work history year multiplied by 100 plus the job number for that employer in that year. 
	For example, 102=year 1, job 2; 305=year 3, job 5. 
	This allows one to associate any characteristic for a job with that week. 
	If a respondent has more than one job at the same time, 
	the job number that is loaded into the array is determined by the starting date of the job with the lowest job number, 
	not by any particular characteristics of the job such as the number of hours worked at the job. 
	The year in the job code is the year in which the job is reported. 
	Jobs held in year 2, but reported in year 10 would be assigned job numbers beginning with 1001 instead of 201.
*/
* work hours at all jobs
ta hours, m
count if inlist(working,1,2)==1 & nointerview_m==0 & inlist(hours,.,-1,0)
count if inlist(working,1,2)==1 & nointerview_m==0 & inlist(hours,.)
count if inlist(working,0)==1 & inlist(hours,.,-1,0)
count if inlist(working,0)==1 & inrange(hours,1,200)

* Employment status
ge emplst=1 if inrange(hours,31,200)
replace emplst=2 if inrange(hours,15,30)
replace emplst=4 if inrange(hours,1,14)
replace emplst=5 if hours==0
replace emplst=3 if enrol==1
lab var emplst "Current employment status"
lab def emplst 1 "Full-time (>30 hrs.)", modify
lab def emplst 2 "Part-time (>=15 hrs.)", modify
lab def emplst 3 "Enrolled", modify
lab def emplst 4 "Marginal (<15 hrs.)", modify
lab def emplst 5 "Not working (=0 hrs.)", modify
lab val emplst emplst
tab working emplst , row m
tab working emplst if nointerview_m==0, row m
tab working emplst if nointerview_m==0 & inlist(working,1,2), row m

*** Wife's employment
ta wife_hours, m
lab var wife_hours "Wife's usual work hours, last year"
ge emplst_woman=1 if marry==1 & inrange(wife_hours,31,200)
replace emplst_woman=2 if marry==1 & inrange(wife_hours,1,30)
replace emplst_woman=3 if marry==1 & wife_hours==0
replace emplst_woman=3 if marry==1 & wife_hours==-4
replace emplst_woman=0 if marry==0
lab var emplst_woman "Wife's employment status (usual), last year"
lab def emplst_woman 0 "n.a., unmarried man", modify
lab def emplst_woman 1 "Full-time (>30 hrs.)", modify
lab def emplst_woman 2 "Part-time (>=15 hrs.)", modify
lab def emplst_woman 3 "Not working (=0 hrs.)", modify
lab val emplst_woman emplst_woman
tab marry emplst_woman, m
tab marry emplst_woman if year>1979, m
tab marry emplst_woman if emplst<. & year>1979, m
tab emplst emplst_woman, m

** Dummies for survey year
tab year, gen(year)


*** gender
recode woman 1=0 2=1


*** drop person-years w/o interview
drop if nointerview_m==1


sort id year
xtset id year
save mwp_US_analysis.dta, replace







*====================================================

* Construct samples

*====================================================

use mwp_US_analysis.dta, clear







*** Define exclusion criteria
cap drop miss*
for num 0/16 : ge missX=0
//Males only
replace miss0=woman
//Exclude persons entering the NLSY after 1st marriage 
cap drop help*
bys id (year) : egen help=max(marstat==1 & n_marr==0) //ever observed never-married
replace miss2=1 if help==0
//Exclude persons entering the NLSY after separation of 1st marriage
cap drop help*
bys id (year) : egen help1=max(marstat==2 & n_marr==1) //ever observed in 1st marriage
replace miss3=1 if help1==0
//Exclude person-years if not working / self-employed
replace miss4=-1 if working==-1
replace miss4=1 if working==0
replace miss4=2 if self==1
//Person-years employed in Private / public sector only
replace miss5=public 
//Exclude persons with invalid marital histories
tab nohistory, m
replace miss6=1 if inlist(nohistory,2,2.5,3) 
//Person-years with missings on further variables needed in the analysis
replace miss7=1 if marstat==.
for num 8/14 \ var lnw wage exp yeduc enrol tenure nchild : replace missX=1 if missing(Y)
//Trim wage distribution : set wages below 0.5 or above 500 Dollars to missing (data errors)
replace miss9=1 if wage<.5
replace miss9=1 if wage>500
//Exclude Person-years after separation 
replace miss16=1 if (inlist(marstat,3,4,6)==1 | n_marr==2)
//Exclude Person-years later than 15 years after 1st marriage 
replace miss16=2 if miss16!=1 & mardur>180


save mwp_US_analysis.dta, replace




*========================================================================================================

* Sample 1: never-married men, to-be-married men, minimum 4 person-years 

*========================================================================================================

use mwp_US_analysis.dta, clear

xtsum id 

*** Apply selection criteria  
cap drop sample1*
gen sample1=1
gen sample1p=1
xtsum id if sample1==1
//Only males
replace sample1=-1 if miss0==1
replace sample1p=-1 if miss0==1
xtsum id if sample1==1
/*
//Only subsamples
xttab subsample
tab year subsample, m
replace sample1=.x if sample1==1 & inlist(subsample,9,15,16,17)==1
xtsum id if sample1==1
/*
    2236       1 CROSS MALE WHITE
     203       2 CROSS MALE WH. POOR
     346       3 CROSS MALE BLACK
     218       4 CROSS MALE HISPANIC
    2279       5 CROSS FEMALE WHITE
     198       6 CROSS FEMALE WH POOR
     405       7 CROSS FEMALE BLACK
     226       8 CROSS FEMALE HISPANIC
     742       9 SUP MALE WH POOR
    1105      10 SUP MALE BLACK
     729      11 SUP MALE HISPANIC
     901      12 SUP FEM WH POOR
    1067      13 SUP FEMALE BLACK
     751      14 SUP FEMALE HISPANIC
     609      15 MIL MALE WHITE
     162      16 MIL MALE BLACK
      53      17 MIL MALE HISPANIC
     342      18 MIL FEMALE WHITE
      89      19 MIL FEMALE BLACK
      25      20 MIL FEMALE HISPANIC
*/
*/
//Only person-years working and not self-employed
//Currently working
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-5 if sample1==1 & miss4==1
xtsum id if sample1==1
replace sample1=-5 if sample1==1 & miss4==-1
xtsum id if sample1==1
bys id (year) : gen help2=sum(sample1==-5) 
bys id (year) : replace sample1p=-5 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
xtsum id if sample1==-5 
xtsum id if sample1==-5 & wage<. & lnw<. //Number of person-years excluded despite valid wages
//Currently not self-employed
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-6 if sample1==1 & miss4==2
bys id (year) : gen help2=sum(sample1==-6) 
bys id (year) : replace sample1p=-6 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
*replace sample1=.w if sample1==1 & inlist(nohistory,1.5)==1 //interview week not known
*xtsum id if sample1==1
/*
//Only when at least 4 person-years provided (required for FE estimation)
cap drop help*
bys id (year) : gen help=sum(sample1==1)
bys id (year) : replace sample1=.i if sample1==1 & help[_N]<4
xtsum id if sample1==1
*/
//Only persons with valid marital bio
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-9 if sample1==1 & miss6==1
bys id (year) : gen help2=sum(sample1==-9) 
bys id (year) : replace sample1p=-9 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-10 if sample1==1 & miss7==1
bys id (year) : gen help2=sum(sample1==-10) 
bys id (year) : replace sample1p=-10 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//Only entering never-married 
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-13 if sample1==1 & miss2==1
bys id (year) : gen help2=sum(sample1==-13) 
bys id (year) : replace sample1p=-13 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//Only before separation of 1st marriage
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-14 if sample1==1 & miss16==1
bys id (year) : gen help2=sum(sample1==-14) 
bys id (year) : replace sample1p=-14 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//Only if marriage duration is <=15 years 
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-15 if sample1==1 & miss16==2
bys id (year) : gen help2=sum(sample1==-15) 
bys id (year) : replace sample1p=-15 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//Only with valid info for all other variables needed
//Current wage missing
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-16 if sample1==1 & miss8==1
bys id (year) : gen help2=sum(sample1==-16) 
bys id (year) : replace sample1p=-16 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//wages below 50 Cent and above 500 USD are very likely data errors
//set to missing
su wage if sample1==1
count if sample1==1 & wage<.5
count if sample1==1 & wage>500 & wage<.
/*
ge Dw1=(wage-wage[_n-1])/wage[_n-1] if sample1==1 
for num 2/15 : replace Dw1=((wage-wage[_n-X])/wage[_n-X]) if sample1==1 & Dw1==. & wage[_n-X]<.
su Dw1 if sample1==1 , d
*replace sample1=.g if sample1==1 & Dw1<`r(p1)' | (Dw1>`r(p99)' & Dw1<.)
ge wout1=1 if sample1==1 & Dw1<`r(p1)' | (Dw1>`r(p99)' & Dw1<.)
su Dw1 if sample1==1 , d
su wage if sample1==1 , d
xtsum id if sample1==1
ge Dw2=(wage[_n+1]-wage)/wage if sample1==1 
for num 2/15 : replace Dw2=((wage[_n+X]-wage)/wage) if sample1==1 & Dw2==. & wage[_n+X]<.
su Dw2 if sample1==1 , d
ge wout2=1 if sample1==1 & Dw1<`r(p1)' | (Dw1>`r(p99)' & Dw1<.)
*replace sample1=.g if sample1==1 & Dw2<`r(p1)' | (Dw2>`r(p99)' & Dw2<.)
su Dw2 if sample1==1 , d
su Dw1 if sample1==1 , d
su wage if sample1==1 , d
su wage if sample1==1 & wout1==1 & wout2==1, d
su Dw1 if sample1==1 , d
su wage if sample1==1 , d
xtsum id if sample1==1
*/
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-17 if sample1==1 & miss9==1
bys id (year) : gen help2=sum(sample1==-17) 
bys id (year) : replace sample1p=-17 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//exp tenure yeduc enrol missing
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-19 if sample1==1 & miss10==1
bys id (year) : gen help2=sum(sample1==-19) 
bys id (year) : replace sample1p=-19 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-20 if sample1==1 & miss11==1
bys id (year) : gen help2=sum(sample1==-20) 
bys id (year) : replace sample1p=-20 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-21 if sample1==1 & miss12==1
bys id (year) : gen help2=sum(sample1==-21) 
bys id (year) : replace sample1p=-21 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1

cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-22 if sample1==1 & miss13==1
bys id (year) : gen help2=sum(sample1==-22) 
bys id (year) : replace sample1p=-22 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
replace sample1=-23 if sample1==1 & miss14==1
bys id (year) : gen help2=sum(sample1==-23) 
bys id (year) : replace sample1p=-23 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1

//Only entering never-married AFTER taking missings into account
cap drop help*
bys id (year) : gen help1=sum(sample1==1) 
bys id (year) : egen help0=max(marstat==1 & n_marr==0 & sample1==1) //man ever observed never-married
cap drop miss2a
gen miss2a=1 if miss2==1 | help0==0
replace sample1=-25 if sample1==1 & miss2a==1 
bys id (year) : gen help2=sum(sample1==-25) 
bys id (year) : replace sample1p=-25 if sample1p==1 & help1[_N]==help2[_N] 
xtsum id if sample1==1
//Only when at least 2 person-years provided (required for FE estimation)
cap drop help*
bys id (year) : gen help=sum(sample1==1)
bys id (year) : replace sample1=-26 if sample1==1 & help[_N]<2
bys id (year) : replace sample1p=-26 if sample1p==1 & help[_N]<2
xtsum id if sample1==1
xttab subsam if sample1==1
//Only when at least 4 person-years provided (required for FE estimation)
cap drop help*
bys id (year) : gen help=sum(sample1==1)
bys id (year) : replace sample1=-27 if sample1==1 & help[_N]<4
bys id (year) : replace sample1p=-27 if sample1p==1 & help[_N]<4
xtsum id if sample1==1
xttab subsam if sample1==1


xtsum lnw marry mardur nchild exp tenure yeduc enrol age if sample1==-27
xtsum lnw marry mardur nchild exp tenure yeduc enrol age if sample1p==-27


for var sample1 sample1p : recode X -27=-26


bys id (year) : replace sample1p=. if _n<_N
tab sample1p
tab sample1
tab sample1p if inlist(sample1p,-1,-2)==0
tab sample1 if inlist(sample1,-1,-2)==0


xttab marstat if sample1==1
cap drop help*
bys id (year) : egen help1=max(marry) if sample1==1
bys id (year) : egen help2=min(marry) if sample1==1
bys id (year) : gen treat1=0 if help1==0 & help2==0 & sample1==1 //never married
//NOTE: there are never-married men which we know marry further on!!!
//These are included in the control group for estimation of dist FE model!!!
*levelsof id if treat1==0 /*& mardur_ts<0*/ & evermarr==1, separate(,) local(ids)
*l id year intdate marstat n_marr mardur mardur_ts begin end marrym kohabm partstat idp nohistory mardate sample1 if inlist(id,`ids'), sepby(id) nolab compress
bys id (year) : replace treat1=1 if treat1==0 & evermarr==1 //to-be-married, but not married in estimation sample
bys id (year) : replace treat1=2 if help1==1 & help2==0 & sample1==1 //to-be-married 
bys id (year) : replace treat1=3 if help1==1 & help2==1 & sample1==1 //married
cap drop help*
lab var treat1 "Time-constant treatment status"
lab def treat 0 "Never-married", modify 
lab def treat 1 "Later married", modify
lab def treat 2 "Ever-married", modify 
lab val treat1 treat

xttab treat1 if sample1==1

xttrans marry if treat1==0, freq
xttrans marry if treat1==1, freq
xttrans marry if treat1==2, freq
xttrans marry if treat1==3, freq

xtsum id if treat1==0
xtsum id if treat1==1
xtsum id if treat1==2
xtsum id if treat1==3


save mwp_US_analysis.dta, replace



*========================================================================================================

* Sample 2: least restrictive sample including 
* - sep./div., widowed and remarried
* - all available person-years after marriage
* - not working during int. week or previous 6 weeks, use wage information from last CPS employer
* - self-employed
*========================================================================================================



use mwp_US_analysis.dta, clear

xtsum id 

*** Apply selection criteria  
cap drop sample2*
gen sample2=1
gen sample2p=1
xtsum id if sample2==1
//Only males
replace sample2=-1 if miss0==1
replace sample2p=-1 if miss0==1
xtsum id if sample2==1
/*
//Only subsamples
xttab subsample
tab year subsample, m
replace sample2=.x if sample2==1 & inlist(subsample,9,15,16,17)==1
xtsum id if sample2==1
/*
    2236       1 CROSS MALE WHITE
     203       2 CROSS MALE WH. POOR
     346       3 CROSS MALE BLACK
     218       4 CROSS MALE HISPANIC
    2279       5 CROSS FEMALE WHITE
     198       6 CROSS FEMALE WH POOR
     405       7 CROSS FEMALE BLACK
     226       8 CROSS FEMALE HISPANIC
     742       9 SUP MALE WH POOR
    1105      10 SUP MALE BLACK
     729      11 SUP MALE HISPANIC
     901      12 SUP FEM WH POOR
    1067      13 SUP FEMALE BLACK
     751      14 SUP FEMALE HISPANIC
     609      15 MIL MALE WHITE
     162      16 MIL MALE BLACK
      53      17 MIL MALE HISPANIC
     342      18 MIL FEMALE WHITE
      89      19 MIL FEMALE BLACK
      25      20 MIL FEMALE HISPANIC
*/
*/
//!!! NOTE : restrictions on employment not applied !!!
//Only person-years working and not self-employed
//Currently working
/*
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-5 if sample2==1 & miss4==1
replace sample2=-5 if sample2==1 & miss4==-1
bys id (year) : gen help2=sum(sample2==-5) 
bys id (year) : replace sample2p=-5 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
*/
//Currently not self-employed
/*
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-6 if sample2==1 & miss4==2
bys id (year) : gen help2=sum(sample2==-6) 
bys id (year) : replace sample2p=-6 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
*replace sample2=.w if sample2==1 & inlist(nohistory,1.5)==1 //interview week not known
*xtsum id if sample2==1
*/
//Only persons with valid marital bio
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-9 if sample2==1 & miss6==1
bys id (year) : gen help2=sum(sample2==-9) 
bys id (year) : replace sample2p=-9 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-10 if sample2==1 & miss7==1
bys id (year) : gen help2=sum(sample2==-10) 
bys id (year) : replace sample2p=-10 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
//Only entering never-married 
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-13 if sample2==1 & miss2==1
bys id (year) : gen help2=sum(sample2==-13) 
bys id (year) : replace sample2p=-13 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
/*
//!!! NOTE : restriction on seperation/divorce/widowhood not applied !!!
//Only before separation of 1st marriage
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-14 if sample2==1 & miss16==1
bys id (year) : gen help2=sum(sample2==-14) 
bys id (year) : replace sample2p=-14 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
//!!! NOTE : restriction on years married not applied !!!
//Only if marriage duration is <=15 years 
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-15 if sample2==1 & miss16==2
bys id (year) : gen help2=sum(sample2==-15) 
bys id (year) : replace sample2p=-15 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
*/
//Only with valid info for all other variables needed
//Current wage missing
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-16 if sample2==1 & miss8==1
bys id (year) : gen help2=sum(sample2==-16) 
bys id (year) : replace sample2p=-16 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
//wages below 50 Cent and above 500 USD are very likely data errors
//set to missing
su wage if sample2==1
count if sample2==1 & wage<.5
count if sample2==1 & wage>500 & wage<.
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-17 if sample2==1 & miss9==1
bys id (year) : gen help2=sum(sample2==-17) 
bys id (year) : replace sample2p=-17 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
//exp tenure yeduc enrol nchild missing
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-19 if sample2==1 & miss10==1
bys id (year) : gen help2=sum(sample2==-19) 
bys id (year) : replace sample2p=-19 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-20 if sample2==1 & miss11==1
bys id (year) : gen help2=sum(sample2==-20) 
bys id (year) : replace sample2p=-20 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-21 if sample2==1 & miss12==1
bys id (year) : gen help2=sum(sample2==-21) 
bys id (year) : replace sample2p=-21 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-22 if sample2==1 & self==.
bys id (year) : gen help2=sum(sample2==-22) 
bys id (year) : replace sample2p=-22 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*

bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-22 if sample2==1 & miss13==1
bys id (year) : gen help2=sum(sample2==-22) 
bys id (year) : replace sample2p=-22 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
replace sample2=-23 if sample2==1 & miss14==1
bys id (year) : gen help2=sum(sample2==-23) 
bys id (year) : replace sample2p=-23 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1

//Only entering never-married AFTER taking missings into account
cap drop help*
bys id (year) : gen help1=sum(sample2==1) 
bys id (year) : egen help0=max(marstat==1 & n_marr==0 & sample2==1) //man ever observed never-married
cap drop miss3a
gen miss3a=1 if miss2==1 | help0==0
replace sample2=-25 if sample2==1 & miss3a==1 
bys id (year) : gen help2=sum(sample2==-25) 
bys id (year) : replace sample2p=-25 if sample2p==1 & help1[_N]==help2[_N] 
xtsum id if sample2==1
//Only when at least 2 person-years provided (required for FE estimation)
cap drop help*
bys id (year) : gen help=sum(sample2==1)
bys id (year) : replace sample2=-26 if sample2==1 & help[_N]<2
bys id (year) : replace sample2p=-26 if sample2p==1 & help[_N]<2
xtsum id if sample2==1
//Only when at least 4 person-years provided (required for FEIS estimation)
cap drop help*
bys id (year) : gen help=sum(sample2==1)
bys id (year) : replace sample2=-26 if sample2==1 & help[_N]<4
bys id (year) : replace sample2p=-26 if sample2p==1 & help[_N]<4
xtsum id if sample2==1


bys id (year) : replace sample2p=. if _n<_N
tab sample2p
tab sample2

xttab marstat if sample2==1
cap drop help*
bys id (year) : egen help1=max(marry) if sample2==1
bys id (year) : egen help2=min(marry) if sample2==1
bys id (year) : gen treat2=0 if help1==0 & help2==0 & sample2==1 //never married
//NOTE: there are never-married men which we know marry further on!!!
//These are included in the control group for estimation of dist FE model!!!
*levelsof id if treat2==0 /*& mardur_ts<0*/ & evermarr==1, separate(,) local(ids)
*l id year intdate marstat n_marr mardur mardur_ts begin end marrym kohabm partstat idp nohistory mardate sample2 if inlist(id,`ids'), sepby(id) nolab compress
bys id (year) : replace treat2=1 if treat2==0 & evermarr==1 //to-be-married, but not married in estimation sample
bys id (year) : replace treat2=2 if help1==1 & help2==0 & sample2==1 //to-be-married 
bys id (year) : replace treat2=3 if help1==1 & help2==1 & sample2==1 //married
cap drop help*
lab var treat2 "Time-constant treatment status, less restricted sample"
lab val treat2 treat
xttab treat2 if sample2==1
xttrans marry if sample2==1
xttrans marstat if sample2==1
xttrans n_marr if sample2==1


//new variables needed
//Indicators seperation/divorce, widowhood, remarriage
ge div=inlist(marstat,3,4)
lab var div "Dummy separated/divorced"
ge wid=inlist(marstat,6)
lab var wid "Dummy widowed"
ge remarry=(marstat==2 & n_marr>1 & n_marr<.)
lab var remarry "Remarried"


tab marstat n_marr if sample2==1, m
tab marry n_marr if sample2==1, m

tab marry remarry if sample2==1
tab marry div if sample2==1
tab marry wid if sample2==1


save mwp_US_analysis.dta, replace


*===========================================================================
* Some final coding
*===========================================================================

tab sample1 sample2, m
tab sample1p sample2p, m




keep id year wage lnw marry evermarr marstat mardur mardate mardur_ts n_marr marage div wid remarry cohabitation nchild ///
		enrol yeduc subsample urban exp expq pexp pexpq exp_alt exp_ft exp_pt tenure lfs working self entry ///
		intdate intmonth intyear intweek age cohort race woman ///
		cpi earnings tu hours emplst emplst_woman wife_hours sample1 sample1p sample2 sample2p treat1 treat2

order id year wage lnw marry evermarr marstat mardur mardate mardur_ts n_marr marage div wid remarry cohabitation nchild ///
		enrol yeduc subsample urban exp expq pexp pexpq exp_alt exp_ft exp_pt tenure lfs working self entry ///
		intdate intmonth intyear intweek age cohort race woman ///
		cpi earnings tu hours emplst emplst_woman wife_hours sample1 sample1p sample2 sample2p treat1 treat2

		
lab var id "Person id"		
lab var year "Survey year"		
lab var mardur "Marriage duration (Years in 1st marriage)"		
lab var cohabitation "Dummy whether person lives with partner unmarried"		
lab var nchild "Number of biological children"		
lab var enrol "Dummy currently enrolled in education"		
lab var urban "Dummy whether person currently lives in a city"		
lab var exp_alt "Alternative measure exper.: PT employment counts half"		
lab var exp_ft "Full-time experience (years)"		
lab var exp_pt "Part-time experience (years)"		
lab var lfs "Current Labor Force Status (from weekly history)"		
lab var intdate "Month of interview (count months since Jan 1960)"		
lab var intmonth "Month of interview, 1..12"		
lab var intyear "Year of interview, 1979..2013"		
lab var intweek "Week of interview (count weeks since Jan 1978)"		
lab var race "Ethnic group"		
lab def race ///
		1 "Hispanic origin" ///
		2 "Afro-American origin" ///
		3 "White American origin"
lab val race race
lab var woman "Dummy whether resp. is female"		
lab var cpi "Consumer Price Index (2006=100)"		
lab var earnings "Original earnings info"		
lab var tu "Original time-unit reported for earnings"		
lab var hours "Current weekly work hours"
lab var sample1 "Main analysis sample"
lab var sample2 "Less restricted sample"
lab var sample1p "Main analysis sample, person-level"
lab var sample2p "Less restricted sample, person-level"
		

save mwp_US_analysis.dta, replace


