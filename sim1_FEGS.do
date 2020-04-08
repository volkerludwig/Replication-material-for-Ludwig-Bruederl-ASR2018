/*
*==============================================================================

Stata 14 Do-File for Replication of Monte Carlo simulation
comparing FE, FEGS and FEIS models (Online Supplement, Part A, Table S1)

Ludwig, Volker and Josef Brüderl. 2018. 
"Is there a Marital Wage Premium? New Evidence from the United States".
American Sociological Review 83(4).

*==============================================================================
*/

/*
Note: You need to install 
- xtfeis package by Volker Ludwig 
	xtfeis from http://fmwww.bc.edu/RePEc/bocode/x
- estout package by Ben Jann 
	st0085_2 from http://www.stata-journal.com/software/sj14-2
*/


/*
*===================================================================
Do-file proceeds in two steps
1) Define program panelsim to set up data and run regression models
2) Run replications using panelsim and store estimated statistics
3) Produce table of simulation results
*===================================================================
*/

//set working directory !!! Adapt this as needed !!!
global mydir "X:\mwp_US\"


/*
*===================================================================
1) Define program panelsim to set up data and run regression models
*===================================================================
*/

capture program drop panelsim
program def panelsim, rclass
syntax, [FE] [FEEXT]  [FEEXT2] [FEIS] [robust] ///
		n(numlist) t(numlist) ///
		beta(numlist) ic(numlist min=0 max=2) [is(numlist min=0 max=2)] [is2(numlist min=0 max=2)] ///
		[theta1(numlist min=1 max=2)] [theta2(numlist min=1 max=2)] [theta3(numlist min=1 max=2)] [theta4(numlist min=1 max=2)] ///
		[propafterrandom]


		
*_________________________________________________


*** return parameters of simulation setup

foreach par in t n beta {
	if length("`macval(`par')'")>0 {
		return scalar `par'=`macval(`par')'
	}
	else if length("`macval(`par')'")==0 {
		return scalar `par'=.
	}
}

tokenize `ic'
local icm `1'
macro shift
local icsd `1'
if length("`icsd'")==0 {
	local icsd = 1
}
if length("`is'")>0 {
	tokenize `is'
	local ism `1'
	macro shift
	local issd `1'
	if length("`issd'")==0 {
		local issd = 1
	}
}
if length("`is2'")>0 {
	tokenize `is2'
	local is2m `1'
	macro shift
	local is2sd `1'
	if length("`is2sd'")==0 {
		local is2sd = 1
	}
}
if length("`theta1'")>0 {
	tokenize `theta1'
	local theta1m `1'
	macro shift
	local theta1sd `1'
	if length("`theta1m'")==0 {
		local theta1m = 0
	}
	if length("`theta1sd'")==0 {
		local theta1sd = 0
	}
}
if length("`theta2'")>0 {
	tokenize `theta2'
	local theta2m `1'
	macro shift
	local theta2sd `1'
	if length("`theta2m'")==0 {
		local theta2m = 0
	}
	if length("`theta2sd'")==0 {
		local theta2sd = 0
	}
}
if length("`theta3'")>0 {
	tokenize `theta3'
	local theta3m `1'
	macro shift
	local theta3sd `1'
	if length("`theta3m'")==0 {
		local theta3m = 0
	}
	if length("`theta3sd'")==0 {
		local theta3sd = 0
	}
}

if length("`theta4'")>0 {
	tokenize `theta4'
	local theta4m `1'
	macro shift
	local theta4sd `1'
	if length("`theta4m'")==0 {
		local theta4m = 0
	}
	if length("`theta4sd'")==0 {
		local theta4sd = 0
	}
}

foreach par in icm icsd ism issd is2m is2sd cons theta1m theta1sd theta2m theta2sd theta3m theta3sd theta4m theta4sd {
	if length("`macval(`par')'")>0 {
		return scalar `par'=`macval(`par')'
	}
	else if length("`macval(`par')'")==0 {
		return scalar `par'=.
	}
}


 


		
*_________________________________________________
		
*** set up data
		
clear
set obs `n'
qui ge N=_n
expand `t'
qbys N : ge T=_n
xtset N T


qbys N (T) : ge a_i=rnormal(`ism',`issd') if _n==_N
qbys N (T) : replace a_i=a_i[_N]

qbys N (T) : ge theta1=rnormal(`theta1m',`theta1sd') if _n==_N
qbys N (T) : replace theta1=theta1[_N]

qbys N (T) : ge theta2=rnormal(`theta2m',`theta2sd') if _n==_N
qbys N (T) : replace theta2=theta2[_N]

qbys N (T) : ge theta3=rnormal(`theta3m',`theta3sd') if _n==_N
qbys N (T) : replace theta3=theta3[_N]

qbys N (T) : ge theta4=rnormal(`theta4m',`theta4sd') if _n==_N
qbys N (T) : replace theta4=theta4[_N]

*** set up treatment indicator
qui ge X_it=invlogit(theta1+theta2*(T-1)+theta3*a_i+theta4*a_i*(T-1))
qui ge logit=X_it
qui replace X_it=cond(X_it < .5, 0,1)

qbys N (T) : replace X=1 if X==0 & X[_n-1]==1
qbys N : egen treat=max(X_it)

if length("`propafterrandom'")>0 {
	qui drop X_it
	qui gen help=runiform()
	qbys N (T) : ge X_it=1 if _n>=(help[1]*_N) & treat==1 & _n>1
	recode X_it .=0
}

*** set up disturbances u and e
qbys N (T) : ge u_i=rnormal(0,`icsd') if treat==0 & _n==1
qbys N (T) : replace u_i=rnormal(`icm',`icsd') if treat==1 & _n==1
qbys N (T) : replace u_i=u_i[1]
qbys N (T) : ge e_it=rnormal(0,1) 


*** set up slope vars
if length("`is'")>0 {
	local slopevar "(T-1)"
}
	

*** set up response var

local setup "`beta'*X_it + u_i + e_it"
if length("`is'")>0 {
	local is " + a_i*`slopevar'" 
}
if length("`is2'")>0 {
	local is2 " + a2_i*(`slopevar'^2)" 
}
local setup "`setup' `is' `is2'"
qui ge Y_it=`setup'


*** define estimation sample 
qbys N (T) : drop if _N<3
if length("`is2'")>0 {
	qbys N (T) : drop if _N<4
}
qbys N X_it (T) : drop if treat==1 & X_it==0 & _N<1 
qbys N (T) : drop if treat==1 & X_it[1]==1 
qbys N (T) : drop if _N<3
if length("`is2'")>0 {
	qbys N (T) : drop if _N<4
}


* robust s.e.
if length("`robust'")>0 {
	local robse "vce(cluster N)"

}



*** set up regressors

local xvars "X_it"
if length("`covariate'")>0 {
	local xvars "`xvars' Z_it"
}
local slopevars ""
if length("`slopefcttime'")==0 {
	if length("`is'")>0 {
		local slopevar "T "
	}
	if length("`is2'")>0 {
		local slopevar "T T2"
		qui ge T2=T*T
	}
}

*_________________________________________________

*** Infos on sample composition

cap mat drop sam
qui xttab X
mat sam=r(results)
return scalar SAM1=sam[2,3]
return scalar SAM2=sam[2,5]

*_________________________________________________

*** estimate models

if length("`fe'")>0 {
	qui xtreg Y `xvars' `slopevar', fe `robse'
	return local FEcmd "xtreg Y `xvars' `slopevar', fe `robse'"
	return scalar FE = _b[X_it]
	return scalar FEse = _se[X_it]
	qui test _b[X_it]=`beta'
	if `r(p)'<=.05 {
		return scalar FEf = 1
	}
	else {
		return scalar FEf = 0
	}
	return scalar FENT = `e(N)'
	return scalar FEN = `e(N_g)'
	return scalar FET = `e(Tbar)'
	return scalar FEfrac = `e(rho)'
	qui xtserial Y `xvars' `slopevar'
	return scalar FEscorr = `r(corr)'
	return scalar FEscorrp = `r(p)'
}
if length("`feext'")>0 {
	local extslopevar ""
	foreach var of varlist `slopevar' {
		cap drop treat_`var'
		qui ge treat_`var'=treat*`var'
		local extslopevar "`extslopevar' treat_`var'"
	}
	qui xtreg Y `xvars' `slopevar' `extslopevar', fe `robse'
	return local FEEXTcmd "xtreg Y `xvars' `slopevar' `extslopevar', fe `robse'"
	return scalar FEEXT = _b[X_it]
	return scalar FEEXTse = _se[X_it]
	qui test _b[X_it]=`beta'
	if `r(p)'<=.05 {
		return scalar FEEXTf = 1
	}
	else {
		return scalar FEEXTf = 0
	}
	return scalar FEEXTNT = `e(N)'
	return scalar FEEXTN = `e(N_g)'
	return scalar FEEXTT = `e(Tbar)'
	qui cap drop a_T
	qui ge a_T=a_i*(T-1)
	qui xtfeis X_it `slopevar' `extslopevar', transformed(d_)
	qui pcorr d_X_it a_T d_treat_T d_T
	qui mat pc=r(p_corr)
	return scalar FEEXTpcorr=pc[1,1]
}
if length("`feext2'")>0 {
	qbys N (T): egen float m=mean(X_it)
	qui replace m=round(m,.001)
	qui ta m if m>0, gen(dm)
	local extslopevar ""
	foreach var of varlist `slopevar' {
		qui ds dm*
		foreach l in `r(varlist)' {
			qui ge treat_`var'_`l'=`l'*T
			local extslopevar "`extslopevar' treat_`var'_`l'"
		}
	}
	qui xtreg Y `xvars' `slopevar' `extslopevar', fe `robse'
	return local FEEXT2cmd "xtreg Y `xvars' `slopevar' `extslopevar', fe `robse'"
	return scalar FEEXT2 = _b[X_it]
	return scalar FEEXT2se = _se[X_it]
	qui test _b[X_it]=`beta'
	if `r(p)'<=.05 {
		return scalar FEEXT2f = 1
	}
	else {
		return scalar FEEXT2f = 0
	}
	return scalar FEEXT2NT = `e(N)'
	return scalar FEEXT2N = `e(N_g)'
	return scalar FEEXT2T = `e(Tbar)'
	qui cap drop a_T
	qui ge a_T=a_i*(T-1)
	qui xtfeis X_it `slopevar' `extslopevar', transformed(dm_)
	qui pcorr d_X_it a_T dm_treat_* dm_T*
	qui mat pc=r(p_corr)
	return scalar FEEXT2pcorr=pc[1,1]
}
if length("`feis'")>0 {
	if length("`robust'")>0 {
		local robsefeis "cluster(N)"
	}
	qui xtfeis Y `xvars' , slope(`slopevar') `robsefeis'
	return local FEIScmd "xtfeis Y `xvars' , slope(`slopevar') `robsefeis'"
	return scalar FEIS = _b[X_it]
	return scalar FEISse = _se[X_it]
	qui test _b[X_it]=`beta'
	if `r(p)'<=.05 {
		return scalar FEISf = 1
	}
	else {
		return scalar FEISf = 0
	}
	return scalar FEISNT = `e(N)'
	return scalar FEISN = `e(N_g)'
	return scalar FEIST = `e(g_avg)'
}

//return current seed
return local SEED "`c(seed)'"

end



/*
* Try panelsim

panelsim, n(1000) t(10) beta(1) ic(0,0) is(1,1) theta1(-.9,.1) theta2(.1,.1) theta3(.1,0) theta4(0,0) fe feext feis

xtreg X_ T c.a_i#c.T i.treat#c.T , fe
xtreg Y X_it T , fe
xtreg Y X_it T i.treat#c.T , fe
xtfeis Y X_it, slope(T) transformed(dt_)

replace a_T=a_i*(T-1)
replace treat_T=treat*(T-1)
xtfeis X_it treat_T T, transformed(dm_)
reg dm_* 
xtreg X_ T i.treat#c.T , fe

xtreg X_ T c.a_i#c.T i.treat#c.T , fe
pcorr d_X_it a_T d_treat_T d_T

*/




/*
*===================================================================
2) Run replications using panelsim and store estimated statistics
*===================================================================
*/



// specify subdirectories 
capture mkdir "$mydir\simulation\"
capture mkdir "$mydir\simulation\sim1_FEGS\"
global mydir "$mydir\simulation\sim1_FEGS\"

cd "$mydir"

set seed 1073741827


//Scenario 1
local reps=1000 //number of replications
simulate FE=r(FE) FEse=r(FEse) FEEXT=r(FEEXT) FEEXTse=r(FEEXTse) FEEXTpcorr=r(FEEXTpcorr) FEEXT2=r(FEEXT2) FEEXT2se=r(FEEXT2se) FEEXT2pcorr=r(FEEXT2pcorr)  FEIS=r(FEIS) FEISse=r(FEISse) FEISN=r(FEISN) FEIST=r(FEIST) prop1=r(SAM1) prop2=r(SAM2), ///
		reps(`reps') saving($mydir\sim1, replace every(100)) seed(`c(seed)'): ///
	panelsim, n(1000) t(10) beta(1) ic(0,0) is(1,1) theta1(-.9,.1) theta2(.1,.1) theta3(0,0) theta4(0,0) robust fe feext feext2 feis
su

//Scenario 2
local reps=1000 //number of replications
simulate FE=r(FE) FEse=r(FEse) FEEXT=r(FEEXT) FEEXTse=r(FEEXTse) FEEXTpcorr=r(FEEXTpcorr) FEEXT2=r(FEEXT2) FEEXT2se=r(FEEXT2se) FEEXT2pcorr=r(FEEXT2pcorr)  FEIS=r(FEIS) FEISse=r(FEISse) FEISN=r(FEISN) FEIST=r(FEIST) prop1=r(SAM1) prop2=r(SAM2), ///
		reps(`reps') saving($mydir\sim2, replace every(100)) seed(`c(seed)'): ///
		panelsim, n(1000) t(10) beta(1) ic(0,0) is(1,1) theta1(-.9,.1) theta2(.1,.1) theta3(.1,0) theta4(0,0) robust fe feext feext2 feis
su


set seed 1073741828

//Scenario 2b
local reps=1000 //number of replications
simulate FE=r(FE) FEse=r(FEse) FEEXT=r(FEEXT) FEEXTse=r(FEEXTse) FEEXTpcorr=r(FEEXTpcorr) FEEXT2=r(FEEXT2) FEEXT2se=r(FEEXT2se) FEEXT2pcorr=r(FEEXT2pcorr)  FEIS=r(FEIS) FEISse=r(FEISse) FEISN=r(FEISN) FEIST=r(FEIST) prop1=r(SAM1) prop2=r(SAM2), ///
		reps(`reps') saving($mydir\sim3, replace every(100)) seed(`c(seed)'): ///
		panelsim, n(1000) t(10) beta(1) ic(0,0) is(1,1) theta1(-.9,.1) theta2(.1,.1) theta3(.1,0) theta4(0,0) propafterrandom robust fe feext feext2 feis
su


//Scenario 3
local reps=1000 //number of replications
simulate FE=r(FE) FEse=r(FEse) FEEXT=r(FEEXT) FEEXTse=r(FEEXTse) FEEXTpcorr=r(FEEXTpcorr) FEEXT2=r(FEEXT2) FEEXT2se=r(FEEXT2se) FEEXT2pcorr=r(FEEXT2pcorr)  FEIS=r(FEIS) FEISse=r(FEISse) FEISN=r(FEISN) FEIST=r(FEIST) prop1=r(SAM1) prop2=r(SAM2), ///
		reps(`reps') saving($mydir\sim4, replace every(100)) seed(`c(seed)'): ///
		panelsim, n(1000) t(10) beta(1) ic(0,0) is(1,1) theta1(-.9,.1) theta2(0,0) theta3(.1,0) theta4(.1,0) robust fe feext feext2 feis
su



/*
*===================================================================
3) Produce table of simulation results
*===================================================================
*/


cd "$mydir"

use sim1, clear
sum 

clear
forvalues s=1/4 {
	append using sim`s'
}
ge n=_n
ge cond=.
forvalues s=1/4 {
	replace cond=`s' if inrange(n,`=(`s'-1)*1000',`=`s'*1000-1')
}
replace cond=cond[_n-1] if _n==_N

preserve
collapse (mean) FE FEse FEEXT FEEXTse FEEXT2 FEEXT2se  FEIS FEISse FEISN FEIST prop1 prop2 FEEXTpcorr FEEXT2pcorr, by(cond)
l

estpost tabstat FE FEse FEEXT FEEXTse /*FEEXT2 FEEXT2se*/  FEIS FEISse FEISN FEIST prop1 prop2 FEEXTpcorr FEEXT2pcorr ///
				, stat(mean) col(stat)  by(cond)
estout . using "$mydir\TableS1_simulation_FEGS.doc" , cells(mean(fmt(3))) replace 

restore


scatter FEEXT FEEXTpcorr , msymbol(oh) || lfit FEEXT FEEXTpcorr || , by(cond) xline(0) yline(1) saving("$mydir\simulation_FEGS.gph", replace)
scatter FEEXT FEEXTpcorr , msymbol(oh) || lfit FEEXT FEEXTpcorr || , xline(0) yline(1) saving("$mydir\simulation_FEGS_all.gph", replace)


