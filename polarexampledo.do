********************************************************************************
********************************************************************************
*********************Polarization Measure Example*******************************
********************************************************************************
********************************************************************************


bysort cwc: egen ProC=mean(F120) //Generates country level mean values of whether abortion is ever justifiable
**cwc is country-wave code, i.e. level 2 unit identifier
*gen MEAS=round(VARIABLE,.1) // Not necessary since F120 is 10 point ordinal scale
gen polarProC=0
levelsof cwc, local(levels)  
foreach i in `levels' {
	tab F120 if cwc==`i', matcell(freq) matrow(name)
	local buks=r(r)
	gen tot=r(N)
	if r(r)==0{
		drop tot
		continue
		}
	forvalues j=1/`buks'{ //Getting number of cases
		gen v`j'=freq[`j',1] if cwc==`i'
	}
	forvalues a=1/`buks'{ //Getting scores
		gen s`a'=name[`a',1] if cwc==`i'
	}

	forvalues k=1/`buks'{ //getting total proportion of cases
		gen p`k'=v`k'/tot if cwc==`i'
	}

	forvalues m=1/`buks'{ //calculating the statistic
		forvalues l=1/`buks'{
			gen pox`m'`l'=(p`m'^(2.6)*p`l')*abs(s`m'-s`l') if cwc==`i'&p`m'!=.&s`m'!=.&s`l'!=.&p`l'!=.
			replace polarProC=polarProC+pox`m'`l' if cwc==`i'&p`m'!=.&s`m'!=.&s`l'!=.&p`l'!=.
			drop pox*
		}
	}
	drop tot
	forvalues d=1/`buks'{
		drop v`d' s`d' p`d'
	}
} 
replace polarProC=. if ProC==.

corr polarProC ProC //Correlation and Scatter show that they are inversely related
twoway(scatter polarProC ProC)(lfit polarProC ProC), title("Polarization and Mean Value") ///
legend(label(1 Polarization))  ytitle("Mean Abortion Justifiability")
