********************************************************************************
********************************************************************************
****************************Polarization Measure********************************
***********************STATA .DO FILE, STATA VERSION 14*************************
********************************************************************************
********************************************************************************
********************************************************************************

/*This file creates a measure of polarization without reference to another
variable(i.e. does not require a second variable to group respondents, such as
Republican or Democrat).The measure was derived by Esteban and Rey in 
Econometrica and isused to assess the degree of polarization of level one 
variables in different level 2 clusters(For example survey responents in 
countries or students in schools). See the article for derivation of the measure
and a discussion of its properties. 
Joan-Maria Esteban and Debraj Ray. On the measurement of polarization. 
Econometrica, 62(4):819–851, 1994.*/

gen MEAS=round(MEASURE,.1) /*If the measure is continuous rounding is 
necesary to limit the time to compute the statistic as the rough number of 
calculations is the number of unique values squared, which increases quickly.
This step is not necessary in ordinal measures, provided few enough categories*/
gen POLAR=0 /*Note: if there is missing data, zeros should be recoded missing to 
if the measure used is missing in that level 2 unit (recode POLAR=0 if MEAS==.)*/
local meas="MEAS"
local LVL2="LVL2ID"
levelsof LVL2UNIT, local(levels)
foreach i in `levels' {
	tab `meas' if `LVL2'==`i', matcell(freq) matrow(name)
	local buks=r(r)
	gen tot=r(N)
	if r(r)==0{
		drop tot
		continue //Advances program if MEAS is missing in that LVL2 case
		}
	forvalues j=1/`buks'{ //Getting number of cases
		gen v`j'=freq[`j',1] if `LVL2'==`i'
		}
	forvalues a=1/`buks'{ //Getting scores
		gen s`a'=name[`a',1] if `LVL2'==`i'
		}

	forvalues k=1/`buks'{ //getting total proportion of cases
		gen p`k'=v`k'/tot if `LVL2'==`i'
		}

	forvalues m=1/`buks'{ //calculating the statistic
		forvalues l=1/`buks'{
			gen pox`m'`l'=(p`m'^(2.6)*p`l')*abs(s`m'-s`l') if `LVL2'==`i'&p`m'!=.&s`m'!=.&s`l'!=.&p`l'!=.
			replace POLAR=POLAR+pox`m'`l' if `LVL2'==`i'&p`m'!=.&s`m'!=.&s`l'!=.&p`l'!=.
			drop pox*
		}
	}
	drop tot
	forvalues d=1/`buks'{
		drop v`d' s`d' p`d'
	}
}

/*Statistic is basically the proportion of each pair of categories raised to 
a polarization sensitivity term and multiplied by the absolute difference between 
the scores associated with those two categories(see article for equation and 
demonstration of the properties of the measure).
