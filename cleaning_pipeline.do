cd "~/Dropbox/Research/Intergenerational Education Mobility/code"


#delimit; 
use 

				// Universe and ID variables
	
STU_ID 			// Student ID
STRAT_ID 		// Stratum
psu 			// Primary sampling unit.  (variable name is lowercase!)
F1SCH_ID 		// Link to first follow-up school (only ELS SCH_ID)
F1UNIV1 		// Sample member status in BY and F1 rounds
F1UNIV2A 		// Base year status and how sample member entered F1 sample
F1UNIV2B 		// Sample member F1 status
F2UNIV_P 		// Sample member status in first 3 rounds
F3UNIV 			// Cross-round sample member status summary (BY to F3)
F3UNIVG10 		// Cross-round sample member status summary (BY to F3) specific to 10th grade cohort
F3UNIVG12 		// Cross-round sample member status summary (BY to F3) specific to 12th grade cohort
G10COHRT 		// Sophomore cohort member in 2001-2002 school year
G12COHRT		// Spring 2004 senior cohort member
bystuwt			// Base year student weight
F1QWT			// First follow-up questionnaire weight

				// Outcome variables
				
F2PS1LVL 		// 	Level of offering of first postsecondary institution
F3ERN2011		// 	2011 employment income:  Respondent only
F3PSSELECT		// Highest selectivity among all PEIs

				// Individual (Demographic)
				
bysex			// Sex
byrace			// Student’s race/ethnicity

				// Individual (Academic)
				
bytxmstd		// Math test standardized score
bytxrstd		// Reading test standardized score
bymathse		// BY mathematics self-efficacy
byenglse		// BY English self-efficacy scale
byconexp		// Control expectation scale (``success expectation'')
byinstmo		// Instrumental motivation (utility interest) scale (``extrinsic motivation'')
byactctl		// Action control: general effort and persistence scale

				// Family
				
byincome		// Total family income from all sources 2001
BYSES1			// Socio-economic status composite, v.1

using "data/els_02_12_byf3stu_v1_0.dta",  // this indicates the data file
clear // clear out what we have in memory
;
# delimit cr

// for some reason, some of these are lowercase, and others are not.
foreach v of var STU_ID STRAT_ID psu F1SCH_ID F1UNIV1 F1UNIV2A F1UNIV2B F2UNIV_P F3UNIV F3UNIVG10 F3UNIVG12 G10COHRT G12COHRT bystuwt bysex byrace byincome BYSES1 bytxmstd bytxrstd bymathse byenglse byconexp byinstmo byactctl F1QWT F2PS1LVL F3PSSELECT F3ERN2011 {
	rename `v' `=lower("`v'")' 
}



/*	ELS02 provides data with the following missing codes:
	
	-1: "Don't know" represents respondents who indicated that they didn't
	know the answer to the question.
	
	-2: "Refused" represents respondents who indicated that they refused
	to answer the question
	
	-3: "Item legitimate skip/NA" is filled for questions that are not
	administered based on routing logic; i.e., the items are not
	applicable based on responses to prior questions.
	
	-4: "Nonrespondent" is filled for all variables across the entire
	questionnaire when a sample member did not respond to the
	questionnaire.
	
	-5: "Out of range" represents questionnaire respondents who
	reported values that are out of range.
	
	-6: "Multiple response" represents hard copy questionnaire respondents
	who reported more than one response for an item that requires
	only one response.
	
	-7: "Partial interview-breakoff" is filled for questions that are not
	answered because the respondent does not wish to continue the
	interview or they have run out of time.  This also includes particular
	items that are not included on an abbreviated version questionnaire.
	
	-8: "Survey component legitimate skip/NA" is filled for all items
	within a survey component for sample members who were not administered
	that component by design for one of the following reasons:  1) the
	component was not administered based on their status (e.g., transfer
	students did not receive certain items on the in-school survey), 2)
	the sample member was not a part of the study at the time of
	administration (e.g., first follow-up freshened sample members were by
	definition not eligible for the base-year survey), or 3) the sample
	member was not capable of completing the survey component (e.g.,
	students who were questionnaire-ineligible due to a language barrier
	or disability at the time of the survey).
	
	-9: "Missing" is filled for questions that are not answered when the
	routing suggests that a response should have been provided.
*/

// the below converts all of those missing values to missing in Stata
// we now know why a variable is missing, but don't store a fake data value
// this lets us do analysis
mvdecode _all, mv(-1=.a \ -2=.b \ -3=.c \ -4=.d \ -5=.e \ -6=.f \ -7=.g \ -8=.h \ -9=.i)

// first college
gen first_pei=f2ps1lvl
lab def first_pei 0 "no college" 1 "less than 2-years" 2 "2-years" 3 "4-years or more"


// set legit skips to no college (i.e., they answered survey questions, but it doesn't apply to them)
codebook f2ps1lvl
recode first_pei 1=3 2=2 3=1 .c=0
lab val first_pei first_pei
tab first_pei

// survey set
svyset psu [pweight = bystuwt], strata(strat_id)


// SES var
sum byses1, d

sum byses1
gen byses1_percentile_wtd=.

// set bottom percentile4 for everyone
replace byses1_percentile_wtd=1 if ! missing(byses1)

// loop through percentiles and set to 1 if value is greater than lower bound.
// since we go through all of the percentiles, this will result in deciles
forval i=10(10)90 {
	_pctile byses1 [pweight=bystuwt], p(`i')
	di r(r1)
	replace byses1_percentile_wtd = (`i'/10)+1 if byses1 > r(r1) & !missing(byses1)
}

tab byses1_percentile_wtd

bys byses1_percentile_wtd: sum byses1

// non-weighted percentile
egen byses1_percentile = cut(byses1), group(10)
tab byses1_percentile byses1_percentile_wtd

outsheet stu_id strat_id psu f1sch_id f1univ1 f1univ2a f1univ2b f2univ_p f3univ f3univg10 f3univg12 g10cohrt g12cohrt bystuwt bysex byrace byincome byses1 bytxmstd bytxrstd bymathse byenglse byconexp byinstmo byactctl f1qwt f2ps1lvl f3psselect f3ern2011 byses1_percentile_wtd byses1_percentile first_pei using data/data.csv, comma replace


