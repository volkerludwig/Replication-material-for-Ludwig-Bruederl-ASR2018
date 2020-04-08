

//Figure 1a

clear
set obs 5

ge Time=_n-1
ge Wage1=.5*Time if inrange(Time,1,3)
ge Wage2=.5*Time+.5 if inrange(Time,2,3)
ge Wage3=Wage1+1*(Time-2) if inrange(Time,2,3)
recode Time 0=.75 4=3.25

line Wage3 Wage2 Wage1 Time, lpattern(dash longdash solid) lwidth(thick thick thick) lcolor(black black black) ///
		ylab(0(1)3, noticks nolabels) xlab(.75 1(1)3 3.25, noticks nolabels) xline(2) ///
		ytitle("Wage", size(large)) xtitle("Work" "experience", size(large)) ///
		legend(ring(0) pos(11) col(1) order(3 "Never-married man" - "" - "Married man with" 1 "Specialization" 2 "Work effort," - "Domestication," - "Employer favoritism") size(large)) ///
		subtitle("a. Causal effects", size(vlarge)) text(0 2 "Entry to" "marriage", place(s) size(large))




//Figure 1b

clear
set obs 5

ge Time=_n-1
ge Wage1=.5*Time if inrange(Time,1,3)
ge Wage2=.5*Time+.5 if inrange(Time,1,3)
ge Wage3=-.75+1*Time if inrange(Time,1,3)
recode Time 0=.75 4=3.25

line Wage3 Wage2 Wage1 Time, lpattern(dash longdash solid) lwidth(thick thick thick) lcolor(black black black) ///
		ylab(0(1)3, noticks nolabels) xlab(.75 1(1)3 3.25, noticks nolabels) xline(2) ///
		ytitle("Wage", size(large)) xtitle("Work" "experience", size(large)) ///
		legend(ring(0) pos(11) col(1) order(3 "Never-married man" - "" - "Married man with" 2 "Selection on wage level" 1 "Selection on wage growth" /*- "(Promising young man)"*/) size(large)) ///
		subtitle("b. Spurious effects", size(vlarge)) text(0 2 "Entry to" "marriage", place(s) size(large))








