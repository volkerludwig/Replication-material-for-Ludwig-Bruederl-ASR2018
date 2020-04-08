/*
*==============================================================================

Stata 14 Do-File for Replication of Monte Carlo simulation
using FEIS model to estimate time-varying treatment effect (Online Supplement, Part B, Table S2)

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
syntax, [FEIS] [robust] ///
		n(numlist) t(numlist) [propafterrandom] ///
		beta1(numlist min=1 max=1) beta2(numlist min=1 max=1) ///
		ic(numlist min=0 max=2) [is(numlist min=0 max=2)] [is2(numlist min=0 max=2)] 
		
		
*_________________________________________________


*** return parameters of simulation setup

foreach par in t n beta1 beta2 {
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
foreach par in icm icsd ism issd is2m is2sd {
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

*** define treatment group
qui ge treat=_n>=(_N/2+1)
if length("`proptreated'")>0 {
	qui drop treat
	qui gen treat=_n>=((1-`proptreated')*_N)
	qbys N (T) : egen help=mean(treat) 
	qui replace treat=1 if help>=.5
	qui replace treat=0 if help<.5
	qui drop help
}

*** set up binary treatment variable X
qbys N (T) : ge X_it=_n>=(_N/2+1)
if length("`propafterrandom'")>0 {
	qui drop X_it
	qui gen help=runiform()
	qbys N (T) : gen X_it=_n>=(help[1]*_N)
}
replace X_it=0 if treat==0

*** set up time since treatment
qbys N (T) : ge XT_it=sum(X_it==1)
qui tab XT_it, gen(XT_it_)
qui su XT_it
local maxXT=`r(max)'+1
qui replace XT_it=XT_it-1 if XT_it>0

*** set up disturbances u and e
qbys N (T) : ge u_i=rnormal(0,`icsd') if treat==0 & _n==1
qbys N (T) : replace u_i=rnormal(`icm',`icsd') if treat==1 & _n==1
qbys N (T) : replace u_i=u_i[1]
qbys N (T) : ge e_it=rnormal(0,1) 

*** set up disturbance a 
if length("`is'")>0 {
	qbys N (T) : ge a_i=rnormal(0,`issd') if treat==0 & _n==1
	qbys N (T) : replace a_i=rnormal(`ism',`issd') if treat==1 & _n==1
	qbys N (T) : replace a_i=a_i[1]
}
if length("`is2'")>0 {
	qbys N (T) : ge a2_i=rnormal(0,`is2sd') if treat==0 & _n==1
	qbys N (T) : replace a2_i=rnormal(`is2m',`is2sd') if treat==1 & _n==1
	qbys N (T) : replace a2_i=a2_i[1]
}


*** set up slope vars

if length("`is'")>0 {
	local slopevar "(T-1)"
}	

*** set up response var

* modify simulation setup
local setup "`beta1'*X_it + `beta2'*XT_it + u_i + e_it"
if length("`is'")>0 {
	local is " + a_i*`slopevar'" 
}
if length("`is2'")>0 {
	local is2 " + a2_i*(`slopevar'^2)" 
}
local setup "`setup' `is' `is2' `covar'"
qui ge Y_it=`setup'


cap drop treat
qbys N (T) : egen treat=max(X_it)


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
if length("`beta2'")>0 {
	local xvars "`xvars' XT_it"
	local xvars2 "XT_it_2-XT_it_`maxXT'"
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

*** estimate models

if length("`feis'")>0 {
	if length("`robust'")>0 {
		local robsefeis "cluster(N)"
	}
	qui xtfeis Y `xvars' , slope(`slopevar') `robsefeis'
	return local FEIScmd "xtfeis Y `xvars' , slope(`slopevar') `robsefeis'"
	return scalar FEIS = _b[X_it]
	return scalar FEISse = _se[X_it]
	qui test _b[X_it]=`beta1'
	if `r(p)'<=.05 {
		return scalar FEISf = 1
	}
	else {
		return scalar FEISf = 0
	}
	return scalar FEIS2 = _b[XT_it]
	return scalar FEIS2se = _se[XT_it]
	qui test _b[XT_it]=`beta2'
	if `r(p)'<=.05 {
		return scalar FEIS2f = 1
	}
	else {
		return scalar FEIS2f = 0
	}
	return scalar FEISNT = `e(N)'
	return scalar FEISN = `e(N_g)'
	return scalar FEIST = `e(g_avg)'

/* dummy variable specification */	
	qui xtfeis Y `xvars2' , slope(`slopevar') `robsefeis'
	return local FEIScmd2 "xtfeis Y `xvars2' , slope(`slopevar') `robsefeis'"
	forvalues tv=2/`maxXT' {
		return scalar FEIS_`tv' = _b[XT_it_`tv']
		return scalar FEIS_`tv'se = _se[XT_it_`tv']
		qui test _b[XT_it_`tv']=`beta1'+`beta2'*(`t'-1)
		if `r(p)'<=.05 {
			return scalar FEIS_`tv'f = 1
		}
		else {
			return scalar FEIS_`tv'f = 0
		}
	}
}

//return current seed
return local SEED "`c(seed)'"

end


*try panelsim
*panelsim, n(200) t(10) beta1(1) beta2(.1) ic(1 .1) is(.1 .01) is2(-.01 .001)  robust feis


/*
*===================================================================
2) Run replications using panelsim and store estimated statistics
*===================================================================
*/

// specify subdirectories 
capture mkdir "$mydir\simulation\"
capture mkdir "$mydir\simulation\sim2_tv_treatment\"
global mydir "$mydir\simulation\sim2_tv_treatment\"

cd "$mydir"


global tv_estimates ""		
forvalues tv=2/10 {
	global tv_estimates "$tv_estimates FEIS_`tv'=r(FEIS_`tv') FEIS_`tv'se=r(FEIS_`tv'se)"

}
		
set seed 1073741827
set seed XAA6db0febb663cf52a9ad36ed92cca24b5482f53498101d66933ab442f01d12e364d59252d460f005d3bc4348c4181fcade44fa8adfd27edac5532a420b38d15cb5479234f3c6ca71f39e44a7322678d073209126fea6edbaead905715038f3e1a4684a0b27a1055edd676ef80246e3703803e6b01c184697a15689d195c0a20b6b843e62879f64a7b72f10ab78947d42e42a5960658ae7c5e95791e9b80648a88f365de0aa3178fe443c7f2c0a427604d130f434c7270631ff77257ca6a257aab0d24bb717ec355f9bec31f0fdfc4a36d2c6b335a165ffec38b39e2efdfa3145d891700fe6db65d16575f60b58711d27ff31a2f776cbee7266223f9975f52f8b4e4fad0a5b15168efad7d65a8dc6627a5dc4e7989c802f735bef4fe4c22181c96fa576ee0116ea040bb2f7ed00097c27b1a4a064ac40d4410737dc073509686185b5fed6218e486d88bc72acc8fa077ad59556c347bafefbe6623fbdb8f85bf05e7c644280049c8b2181db5baf17fd89389d955e749cfc39389e59505b358c58156eb4277dcf4731b1b9ea5b858d83adff8ab0d075fd90630c64c605d715bcd6fb15b7e2b92f3340bb21bcf1ffe7cdfa9b6b3d68f5c81b7b8867263ea11bc8f19489964309f52b13fe5cc94374dff405f5f9e8713237ddec54b464771d53cada7f220073a96001b8a7af08ccb090d863fb99bbad692706ea65e73379be9577ceefeb0f0ba1a6907c7061a289d19d2b913c8246fa043192caa9154cc8494d2eb64ded4589276a55a78ca8b313a8d9119ed7061ae852df120f6e99aa1288efa8d85909d69da55dd6d8b2fdb10c5d33ea10104ec5ac47857c9859c9e64840b660cd38dbfd2bb79fbfab29e089827c6f3086dc9bc7e89507bacbdb0796dbd4f05bf51d1d0862d3d60fe8a7c76ebf9cb142b6aa3847533ff85c7cdf3f9f5adacce6a85e00bb889ba9f535f64b0f13a036f536021d057e4c8ee93c2a3eacb773c08cfc792addc7973a4316866ffd56f90e8e34b6b5f9515dcaaac30a8e292237a46ae8b593e0502d5bfdb506b1428e1254262493dbf7ba707a93a60c1233dd0a945707aeea5278fbf384f268d8dee24e50684c87319e79300329d3da2da01a148aba696aef574bd1ce5eabe867d5f585ba87ccf3814f318002cac9918a84776a142525344d50206da5790d616b646f80b5adc2e8b214beca7a646f62c007a0b9f791d2ae55f3a3ec8857433bcaa8095a177ca234ca5aac66d731d58bb60a8e8c5e3dac531aaef5ba679cc0576b46add6d0eaf0ad410db51780c6b494018546dc5406d7dbcd3fb1834560695391fe91c822cd59e7c0ff73b40dcd05e3552bc5579b97da9647bfa5ef32c9dabf757047a963f065b3a1eea577f6123a5bd75c53e5bf983c311019e0586360609c6e1c645d177cecf04adeefe14e8ec067e6768cf39483d545c14f2cfa1d1da00e22664014ad7a6e48117838989d57698502bc4fac66bd2fdff64ea0b27cc040d26cdf33a4cdf46384786a5d18da35d37d1619f80a607a22667698eef33bf910190e1e6836da67f6018a3e8bc2371549728f44c4dbce23ecc18acb9944b73b955a9e94b0ee678db587a4253f1340adea4f6c75a4e4312753b5fe3685f391a6531da7a19316f8b8b98f192bac435f7b2e6674fe0734f044b4b49aa7a9af1cbef3b3cdd704cd48722b55d3e9da143de699fe2d5c5367afe7a593f64cced0834abb33a3805c85c35847437532221b0d418f53b14976a5366ef7635caf275a446c1f7e19dc4857b6898b4ae9af12b81393f9a32c75e3ab62937952913162ef13c546886dbbe498f1c19274aeed7c983af6d0328e06017859cbd78adec60ff7867c18db315970a971eb59529aa1888ece0dd7aa6e05e6bf535511d5e9c1db560c2345a314319d363692e0f23b426ab96da4d4c679a7e42afab7a9f187af6d0cc3072d789d0942e9f7c427fce7525f77f89eaed6e553267ce58a78f62dd7ab92d3cf4d1beb9f1760a3564557240bb80abb28d506b67358062f75f399e5ebf9f6c0ecdae729bbfe047bd833be42a76a7adfe5db93b65a1a427815c361d05209e52eaf05386f08f60afa9124504524fe3d41eedba9b51ac68a2c2e19dbc9a2152ba6601936b44b75b6c20b0a3270b4f5a612102800e42c18f4a5ff0ccfe8204345edc95221da0b53645a443924efdf79a934fd31d9dfdce64da3c42f9f725fd444afa7712411a97d84b1bb4808b13cf65c73f78bd81a96fceb65416fe0dabd6fa6dda1802ffae7925377318ecd9dec84f3c0e60997b6caa8ba071e75319734fe2aba4eb927da29f83e43916e9b7b370c7495763e8ccf8b42737596df515e2df14d449a2ff7a902abbae81ee54950dd714fef7158a25ba32662137aded44effe4e1af93b25b0a5d2fab95fae08614d18deddcaad9d0d9d0eaa7cdd249fe31572e2f88037fe7f610423c0e2ae8d736db0d3fe5c8d664a99e13437c821d41d2f0aca439eda43fb7814219ce70573a3d82ade005173d2d9bbc40d495a796a8e3ad667f235de9d7a4b9f3b3170e3d514a36505b70790e821cc52695170e92db4cb9cd9e990166506bf80e9b919ff641c63b5c605ce6c8284aac22df4d5700e9023258fb34e9250c06c0a61dac30bd6a17f3651d2f015feb52536e907263209c9f1f6a4a7ae19c5eb8e2b905d0d9d664e76a13b647def98c016d3bde096d9a1f73a5d4b342d9b8ee40d749e5efb695767e933a3fb91e24fc51cd996d37b72f53d561e4f3be75f0467f3b11ce665484b2f19b5c1b65849c830695315161f00bb791877b847ae054b006f99e87647297a55666250f82a153ec1821b64b54b76c8a00e47805e5f58a10b7107f7d1600dcafc1eb53083dd56fd602f90962ca8f68cf33fd59e5b0a6d3b77f35e6b976b5dd0216be3ab3781923a79046a44b18186f3a59a4076ea84625b454fa157f9153a5480d18bbf563aa1dceecc903301c5094a016ba9c1195528ad4be453eb8a78acc9798293b5cde720e82404ca24f9079f2fadbbb1bc0c57c3cc41f9c249d03afcb4e078d0e9ea5b1f394f2b21205389692c954f30b7280a44fcd7fde13fb5a10b8cddd8d314c474be9092bf4a19b26e0d017422d7412514facf80c9b54408224bac341e0b21fa4b073c450d965706bb73e9c24fb68d0821f49e758269a5563a4460c0f4e057ff8a6db62c93acc7d7a7c949c6ad3c974ebd00e1901048ed55e3c730ad292eea1736bfecb9be72cf0014e9a301ab3bb18ad46a88b70a11715324a3ebf999692e1faf4c928a938dbe2ae19a10add8bd51c73fd1346cf447b437d294c25ff36b5bd2c0dbda6ff3df3037fc27d535fd29a780988c9551674e29029e5b70f8f4e65e3c93e69ef3767bbcaa5eec35eef9790e7602a389889e8f27a44777c9160adf091cd061fef5421b43bd50a3610a7f10e87220d80da7f34aa94651064b4a96a730a53ef2bf99a79df066b9cbce62b19019d00fb91614706eca45eb1b987d040c2a297edacc5ce1d54a607895e3000100000130351c 

local reps=10000 //number of replications
simulate FEIS=r(FEIS) FEISse=r(FEISse) FEIS2=r(FEIS2) FEIS2se=r(FEIS2se) $tv_estimates, ///
		reps(`reps') saving($mydir\sim, replace every(100)) seed(`c(seed)'): ///
		panelsim, n(200) t(10) beta1(1) beta2(.1) ic(1 .1) is(.1 .01) is2(-.01 .001) propafterrandom robust feis

		
//simulation results		
cd "$mydir"

use sim, clear
sum FEIS FEIS2
sum FEIS_? FEIS_??		

global tv_results ""		
forvalues tv=2/10 {
	global tv_results "$tv_results FEIS_`tv' FEIS_`tv'se"

}		

estpost tabstat FEIS FEISse FEIS2 FEIS2se $tv_results ///
				, stat(mean) col(stat)  
estout using "$mydir\TableS2_simulation_tv_treatment.doc" , cells(mean(fmt(3))) replace 
