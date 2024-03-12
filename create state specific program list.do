
/*This program replicates program list for every state*/

use programlist_generic, clear
gen stateAbbrev="AL"
gen Organization="Other"
save AL, replace

clear
foreach state in  "AK" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"{
use AL, clear
replace stateAbbrev="`state'"
save `state', replace
}

use AL, clear
save AllStates, replace

foreach state in  "AK" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"{
    use AllStates, clear
	append using "`state'"
	save AllStates, replace
	erase "`state'.dta"
}

use AllStates, clear

/*VA- no change*/
/*TX- no changes*/


/*NY*/
replace benefit2="Temporary Cash Assistance (TANF)"  if benefit2=="Temporary Assistance for Needy Families (TANF)" & stateAbbrev=="NY" 
replace benefit2="Child Health Plus" if benefit2=="Medicaid for Children/CHIP" & stateAbbrev=="NY"

/*CA*/
replace benefit2="CalFresh" if benefit2=="Supplemental Nutrition Assistance Program (SNAP)"  & stateAbbrev=="CA"
replace benefit2= "CalWorks (TANF)" if benefit2=="Temporary Assistance for Needy Families (TANF)"  & stateAbbrev=="CA"
replace benefit2="Medi-Cal for Adults" if benefit2=="Medicaid for Adults"  & stateAbbrev=="CA"
replace benefit2="Medi-Cal for Children" if benefit2=="Medicaid for Children/CHIP"  & stateAbbrev=="CA"

/*IL*/
replace benefit2="AllKids (Medicaid for Children/CHIP)" if  benefit2=="Medicaid for Children/CHIP" & stateAbbrev=="IL"
replace benefit2="Child Care Assistance Program (CCAP/CCDF)" if benefit2=="Child Care Subsidy (CCDF)" & stateAbbrev=="IL"

/*WA*/
replace benefit2="WorkFirst (TANF)" if benefit2=="Temporary Assistance for Needy Families (TANF)" & stateAbbrev=="WA"
replace benefit2="Apple Health for Kids" if benefit2=="Medicaid for Children/CHIP" & stateAbbrev=="WA"
replace benefit2="Apple Health for Adults" if benefit2=="Medicaid for Adults" & stateAbbrev=="WA"
replace benefit2="Working Connections Child Care Program (CCDF)" if benefit2=="Child Care Subsidy (CCDF)" & stateAbbrev=="WA" 

/*GA*/
replace benefit2="Childcare and Parent Services (CAPS)" if benefit2=="Child Care Subsidy (CCDF)" & stateAbbrev=="GA" 
replace benefit2="PeachCare/Medicaid for Kids"  if benefit2=="Medicaid for Children/CHIP"  & stateAbbrev=="GA" 

/*KY*/
replace benefit2="Kentucky Transitional Assistance Program (K-TAP)" if benefit2=="Temporary Assistance for Needy Families (TANF)" & stateAbbrev=="KY"

/*MD*/
replace benefit2="Child Care Scholarship (CCS)" if benefit2=="Child Care Subsidy (CCDF)" & stateAbbrev=="MD"
replace benefit2="Adult Medical Assistance (Medicaid)" if benefit2=="Medicaid for Adults" & stateAbbrev=="MD"
replace benefit2="Maryland Children's Health Program (Medicaid for Children/CHIP)" if benefit2=="Medicaid for Children/CHIP" & stateAbbrev=="MD"
replace benefit2="Temporary Cash Assistance" if benefit2=="Temporary Assistance for Needy Families (TANF)" & stateAbbrev=="MD"

order stateAbbrev Organization benefit2
sort stateAbbrev Organization benefit2

save AllStates, replace

use AllStates if stateAbbrev=="VA", clear
replace Organization = "ARISE Alexandria" 
save AA, replace

use AllStates if stateAbbrev=="GA", clear
replace Organization ="Atlanta Abundant Birth Project"
save AAB, replace
replace Organization = "In Her Hands"
save IHH, replace
replace Organization = "Urban League Greater Atlanta"
save ULGA, replace
replace Organization = "The Atlanta Abundant Birth Project"
save TAABP, replace

use AllStates if stateAbbrev=="NY", clear
replace Organization ="Children's Defense Fund"
save CDF, replace
replace Organization ="The BRIDGE Project"
save BP, replace
replace Organization ="NY Creative Rebuild"
save NYCR, replace

use AllStates if stateAbbrev=="IL", clear
di `lastrow'
set obs `=_N+7'

replace benefit2 = "Aid for the Aging, Blind, and Disabled (AABD)" in 19
replace benefit2 = "Downpayment Plus programs from Federal Home Loan Bank of Chicago" in 20
replace benefit2 = "FamilyCare" in 21
replace benefit2 = "Former Foster Care" in 22
replace benefit2 = "Health Benefits for Workers with Disabilities" in 23
replace benefit2 = "Medicare Savings Program" in 24
replace benefit2 = "Moms and Babies" in 25

replace stateAbbrev="IL"
replace Organization ="Cook County Promise"
/*additional benefits for cook county*/
save CCP, replace

use AllStates if stateAbbrev=="CA", clear
replace Organization ="Elevate MV"
save EMV, replace 

use AllStates if stateAbbrev=="TX", clear
replace Organization = "Empower House"
save SMH, replace

use AllStates if stateAbbrev=="MD", clear
replace Organization="CASH Campaign of Maryland"
save C, replace 

di `lastrow'
set obs `=_N+3'

replace benefit2 ="Senior Dental, Maternity Partnership, or Care4Kids" in 19
replace benefit2 ="Working Parents Childcare Assistance" in 20
replace benefit2= "Montgomery County Rental Assistance Program" in 21
replace stateAbbrev="MD"
replace Organization = "MoCo Boost"
save MB, replace

use AllStates if stateAbbrev=="KY", clear
replace Organization="Louisville, KY"
save LK, replace

use AllStates if stateAbbrev=="PA", clear
di `lastrow'
set obs `=_N+1'
replace Organization="City of Philadelphia"
replace stateAbbrev= "PA"
replace benefit2 ="LIHEAP" in 19
save CP, replace

use AllStates, clear
foreach file in AA AAB IHH CDF CCP EMV SMH BP C MB LK CP ULGA TAABP NYCR{
append using `file'
erase `file'.dta
}

erase AL.dta

outsheet using "ProgramList_StateSpecific.csv", comma replace




