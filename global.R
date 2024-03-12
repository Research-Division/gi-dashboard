#######################################################
#######################################################
# Global - NO CUSTOMIZATION IS REQUIRED
#######################################################
#######################################################
library(shinyjs)
library(shinycssloaders)
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
   
# Call all the functions
source("mainFiles/loadFilesandFunctions.R", local=TRUE) # Load auxiliary files and required functions
source("mainFiles/libraries.R", local=TRUE) # Load required packages

# external locationa and occupations lists, respectively 
loc_meta <- fread('locations_list.csv')
org_meta <- fread('organizations_list.csv')
benefit_meta <- fread('ProgramList_StateSpecific.csv')
#occ_meta <- fread('occupations_list.csv')

USEALICE<-TRUE #USE ALICE EXPENSES INSTEAD OF DEFAULT EXPENSES
budget.ALICE<-"survivalforcliff" 

     
ruleYear <- as.numeric(format(Sys.Date(), "%Y"))


#childcare needs assumptions
k_ftorpt <- "FT" #not used anywhere in benefits calc right now
schoolagesummercare <- "PT" #don't change - ben calc not yet set up to handle FT
headstart_ftorpt <-"PT" #use the same for both headstart & earlyheadstart 
preK_ftorpt <- "PT"
# contelig.headstart <- TRUE
# contelig.earlyheadstart <- TRUE
#contelig.ccdf <- TRUE

# Austin, w hy do you need all these settings below ? Can they go some place else

line_colors <- c("#661100","#E69F00")
line_colors2 <- c("#661100","#E69F00")
line_colors3 <- c("#661100","#E69F00")
line_colors4 <- c("#661100","#D55E00")
line_colors3 <- c("#E69F00", "#661100")
FATES_colors <- c("Net Resources without FATES", "Net Resources with FATES")

# Global parameters for lifetime projections
# Lifetime horizon is fixed
years<-as.numeric(format(Sys.Date(), "%Y"))
#income<-seq(1000,119000,by=1000)

# Assumption on when child leaves the house
child.leaves.house<-19

 #use for the action buttons
 jscode1 <- "shinyjs.getresults = function() {window.scrollTo(0, 0);}"
 
 
 # function to produce the necessary cross section tables
 cross.section.employer <- function(benefit1, benefit2, empl_healthcare, city.name, numadults, numkids,age_adult_1, age_adult_2, age_adult_3, age_adult_4, age_adult_5, age_adult_6,  age_child_1, age_child_2, age_child_3, age_child_4, age_child_5, age_child_6, disability1, disability2, disability3, disability4, disability5,  disability6, disability7, disability8, disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi, income.otherfamily, income_earned, cash_bonus, child_support, investment_income, gift_income, assets, stipend_childcare, stipend_transportation, stipend_housing, stipend_utilities, stipend_health, stipend_food, stipend_other, employee_monthly_contribution, taxable_transportation, taxable_childcare, taxable_housing, taxable_utilities, taxable_health, taxable_food, taxable_other,rentexp,utilityexp, childcareexp,contelig.ccdf,contelig.headstart, contelig.earlyheadstart,housing_expenses,childcare_expenses,FilingStatus,married){
              
    benefit1 <<- benefit1
    benefit2 <<-benefit2
    empl_healthcare <<- empl_healthcare
    city.name <<- city.name
    numadults <<- numadults
    numkids <<- numkids
    age_adult_1 <<- age_adult_1 
    age_adult_2 <<- age_adult_2 
    age_adult_3 <<- age_adult_3 
    age_adult_4 <<- age_adult_4 
    age_adult_5 <<- age_adult_5 
    age_adult_6 <<- age_adult_6 
    age_child_1 <<- age_child_1 
    age_child_2 <<- age_child_2 
    age_child_3 <<- age_child_3 
    age_child_4 <<- age_child_4 
    age_child_5 <<- age_child_5 
    age_child_6 <<- age_child_6 
    disability1 <<- disability1
    disability2 <<- disability2
    disability3 <<- disability3
    disability4 <<- disability4
    disability5 <<- disability5
    disability6 <<- disability6
    disability7 <<-disability7
    disability8 <<- disability8
    disability9 <<- disability9
    disability10 <<- disability10
    disability11 <<- disability11
    disability12 <<- disability12
    ssdiPIA1 <<- ssdiPIA1
    ssdiPIA2 <<- ssdiPIA2
    ssdiPIA3 <<- ssdiPIA3
    ssdiPIA4 <<- ssdiPIA4
    ssdiPIA5 <<- ssdiPIA5
    ssdiPIA6 <<- ssdiPIA6
    blind1 <<- blind1
    blind2 <<-blind2
    blind3 <<- blind3
    blind4 <<- blind4
    blind5 <<- blind5
    blind6 <<- blind6
    disab.work.exp <<- disab.work.exp
    prev_ssi <<- prev_ssi
    income.otherfamily <<- income.otherfamily
    income_earned <<- income_earned
    cash_bonus <<- cash_bonus
    child_support <<- child_support
    investment_income <<- investment_income
    gift_income <<- gift_income
    #other_income <<- other_income
    assets <<- assets
    stipend_childcare <<- stipend_childcare
    stipend_transportation <<- stipend_transportation
    stipend_housing <<- stipend_housing
    stipend_utilities <<- stipend_utilities
    stipend_health <<- stipend_health
    stipend_food <<- stipend_food
    stipend_other <<- stipend_other
    employee_monthly_contribution <<- employee_monthly_contribution
    taxable_transportation <<- taxable_transportation
    taxable_childcare <<- taxable_childcare
    taxable_housing <<- taxable_housing
    taxable_utilities <<- taxable_utilities
    taxable_health <<- taxable_health
    taxable_food <<- taxable_food
    taxable_other <<- taxable_other 
    rentexp <<- rentexp
    utilityexp <<-utilityexp
    #housingexp <<- housingexp
    childcareexp <<- childcareexp
    contelig.ccdf <<- contelig.ccdf
    contelig.headstart <<- contelig.headstart
    contelig.earlyheadstart <<- contelig.earlyheadstart
    
    # cash_bonus, child_support, investment_income, gift_income, other_income, assets,
    
 #create initial dataset
   csdata<-expand_grid(
     numadults = numadults
     , numkids = numkids
     , locations = city.name
     , income = income_earned+cash_bonus#-(employee_monthly_contribution*12)
     , Year = ruleYear
     , ruleYear = ruleYear
     , income.gift= gift_income
     , income.child_support=child_support
     #, income.other_income = other_income
     , empl_healthcare = empl_healthcare
     , income.otherfamily = income.otherfamily
     , employee_monthly_contribution = employee_monthly_contribution*12
     , stipend_transportation = stipend_transportation*12
     , stipend_childcare = stipend_childcare*12
     , stipend_housing = stipend_housing*12
     , stipend_utilities = stipend_utilities*12
     , stipend_health = stipend_health*12
     , stipend_food = stipend_food*12
     , stipend_other = stipend_other*12
     , disability1 = disability1
     , disability2 = disability2
     , disability3 = disability3
     , disability4 = disability4
     , disability5 = disability5
     , disability6 = disability6
     , disability7 = disability7
     , disability8 = disability8
     , disability9 = disability9
     , disability10 = disability10
     , disability11 = disability11
     , disability12 = disability12
     , disab.work.exp = disab.work.exp
     , prev_ssi = prev_ssi
     , ssdiPIA1 = ssdiPIA1
     , ssdiPIA2 = ssdiPIA2
     , ssdiPIA3 = ssdiPIA3
     , ssdiPIA4 = ssdiPIA4
     , ssdiPIA5 = ssdiPIA5
     , ssdiPIA6 = ssdiPIA6
     , blind1 = blind1
     , blind2 = blind2
     , blind3 = blind3
     , blind4 = blind4
     , blind5 = blind5
     , blind6 = blind6
     , FilingStatus = FilingStatus
     , married = married
       ) 

    #csdata$income <- csdata$income + csdata$income.other_income
   
    
    childcare_expenses <- childcare_expenses
    housing_expenses <- housing_expenses
         
     # csdata$income.gift <- csdata$transportation_stipend + csdata$childcare_stipend + csdata$other_stipend
    # add other household income if family has two adults or more
    csdata$individual_income <- csdata$income
    csdata$income[csdata$numadults > 1] <- csdata$income[csdata$numadults > 1] + (income.otherfamily)
    csdata$Year<-as.numeric(csdata$Year)
         
         
         # Specify switches for Benefits profile
         if(benefit1=="All programs"){
                 APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
                 APPLY_HEADSTART<-TRUE
                 APPLY_CCDF<-TRUE
                 APPLY_PREK<-TRUE
                 APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
                 APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
                 APPLY_MEDICAID_ADULT<-TRUE
                 APPLY_MEDICAID_CHILD<-TRUE
                 APPLY_ACA<-TRUE
                 APPLY_SECTION8<-TRUE
                 APPLY_SNAP<-TRUE
                 APPLY_SLP<-TRUE 
                 APPLY_WIC<-TRUE
                 APPLY_EITC<-TRUE
                 APPLY_CTC<-TRUE
                 APPLY_CDCTC<-TRUE
                 APPLY_TANF<-TRUE
                 APPLY_SSDI<-TRUE
                 APPLY_SSI<-TRUE
                 APPLY_RAP<-FALSE
                 APPLY_FATES<-FALSE
                 
                 
         }else if(benefit1=="No programs"){
                 APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
                 APPLY_HEADSTART<-FALSE
                 APPLY_CCDF<-FALSE
                 APPLY_PREK<-FALSE
                 APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
                 APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
                 APPLY_MEDICAID_ADULT<-FALSE
                 APPLY_MEDICAID_CHILD<-FALSE
                 APPLY_ACA<-FALSE
                 APPLY_SECTION8<-FALSE
                 APPLY_SNAP<-FALSE
                 APPLY_SLP<-FALSE 
                 APPLY_WIC<-FALSE
                 APPLY_EITC<-FALSE
                 APPLY_CTC<-FALSE
                 APPLY_CDCTC<-FALSE
                 APPLY_TANF<-FALSE
                 APPLY_SSDI<-FALSE
                 APPLY_SSI<-FALSE
                 APPLY_RAP<-FALSE
                 APPLY_FATES<-FALSE
                 
        
         }else if(benefit1=="Select a custom list"){
                 APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
                 APPLY_HEADSTART<-FALSE
                 # contelig.headstart <- TRUE
                 # contelig.earlyheadstart <- TRUE
                 APPLY_CCDF<-FALSE
            #    contelig.ccdf <- TRUE
                 APPLY_PREK<-FALSE
                 APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
                 APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
                 APPLY_MEDICAID_ADULT<-FALSE
                 APPLY_MEDICAID_CHILD<-FALSE
                 APPLY_ACA<-FALSE
                 APPLY_SECTION8<-FALSE
                 APPLY_SNAP<-FALSE
                 APPLY_SLP<-FALSE 
                 APPLY_WIC<-FALSE
                 APPLY_EITC<-FALSE
                 APPLY_CTC<-FALSE
                 APPLY_CDCTC<-FALSE
                 APPLY_TANF<-FALSE
                 APPLY_SSDI<-FALSE
                 APPLY_SSI<-FALSE
                 APPLY_RAP<-FALSE
                 APPLY_FATES<-FALSE
                 
                 if("Supplemental Nutrition Assistance Program (SNAP)" %in% benefit2) {APPLY_SNAP<-TRUE}else{APPLY_SNAP<-FALSE}
                 if("Free or Reduced Price School Meals" %in% benefit2) {APPLY_SLP<-TRUE}
                 if("Earned Income Tax Credit (EITC)" %in% benefit2) {APPLY_EITC<-TRUE}
                 if("Child Tax Credit (CTC)" %in% benefit2) {APPLY_CTC<-TRUE}
                 if("Child and Dependent Care Tax Credit (CDCTC)" %in% benefit2) {APPLY_CDCTC<-TRUE}
                 if("Head Start/Early Head Start" %in% benefit2) {APPLY_HEADSTART<-TRUE}
                 if("Section 8 Housing Voucher" %in% benefit2) {APPLY_SECTION8<-TRUE}
                 if("Child Care Subsidy (CCDF)" %in% benefit2) {APPLY_CCDF<-TRUE}
                 if("State-Funded Pre-Kindergarten" %in% benefit2) {APPLY_PREK<-TRUE}
                 if("Medicaid for Adults" %in% benefit2) {APPLY_MEDICAID_ADULT<-TRUE}
                 if("Medicaid for Children/CHIP"  %in% benefit2) {APPLY_MEDICAID_CHILD<-TRUE}
                 if("Health Insurance Marketplace Subsidy" %in% benefit2) {APPLY_ACA<-TRUE}
                 if("Women, Infants and Children Nutrition Program (WIC)" %in% benefit2) {APPLY_WIC<-TRUE}
                 if("Home Energy Assistance" %in% benefit2) {APPLY_LIHEAP<-TRUE}
                 if("Temporary Assistance for Needy Families (TANF)" %in% benefit2) {APPLY_TANF<-TRUE}
                 if ("RAP" %in% benefit2) {APPLY_RAP <- TRUE}
                 if ("FATES" %in% benefit2) {APPLY_FATES <- TRUE}
                 if ("Social Security Disability Insurance (SSDI)" %in% benefit2) {APPLY_SSDI <- TRUE}
                 if ("Supplemental Security Income (SSI)" %in% benefit2) {APPLY_SSI <- TRUE}
                 
                 
                 #change switches for continuous eligiblity if specified by user. Default is use cont elig rules.
                 #insert switches
                 if(contelig.ccdf==TRUE){contelig.ccdf<-FALSE}else if(contelig.ccdf==FALSE){contelig.ccdf<-TRUE}
                 if(contelig.headstart==TRUE){contelig.headstart<-FALSE}else if(contelig.headstart==FALSE){contelig.headstart<-TRUE}
                 if(contelig.earlyheadstart==TRUE){contelig.earlyheadstart<-FALSE}else if(contelig.earlyheadstart==FALSE){contelig.earlyheadstart<-TRUE}
                 
         }else{
            APPLY_CHILDCARE<-TRUE# Childcare block - Head Start and CCDF
            APPLY_HEADSTART<-FALSE
            APPLY_CCDF<-FALSE
            APPLY_PREK<-FALSE
            APPLY_LIHEAP<-FALSE #NOT IN BEN CALC FOR MOST STATES
            APPLY_HEALTHCARE<-TRUE # Healthcare block - ACA, Medicaid and Employer Sponsored Healthcare
            APPLY_MEDICAID_ADULT<-FALSE
            APPLY_MEDICAID_CHILD<-FALSE
            APPLY_ACA<-FALSE
            APPLY_SECTION8<-FALSE
            APPLY_SNAP<-FALSE
            APPLY_SLP<-FALSE 
            APPLY_WIC<-FALSE
            APPLY_EITC<-FALSE
            APPLY_CTC<-FALSE
            APPLY_CDCTC<-FALSE
            APPLY_TANF<-FALSE
            APPLY_SSDI<-FALSE
            APPLY_SSI<-FALSE
            APPLY_RAP<-FALSE
            APPLY_FATES<-FALSE
         }

         
         csdata<-csdata %>% 
                 #-----------------------------
         # 1. Demographics
         #-----------------------------
         # Initialize age of each member of the household
         
         mutate(agePerson1=NA_real_  
                ,agePerson2=NA_real_
                ,agePerson3=NA_real_ 
                ,agePerson4=NA_real_
                ,agePerson5=NA_real_
                ,agePerson6=NA_real_
                ,agePerson7=NA_real_
                ,agePerson8=NA_real_
                ,agePerson9=NA_real_
                ,agePerson10=NA_real_
                ,agePerson11=NA_real_
                ,agePerson12=NA_real_) %>% 
                 
                 # Tax Filing Status:
                 # 1 - single
                 # 2 - married filing jointly (make a note in Dashboard)
                 # 3 - Heads of Household (for later)
                 # 4 - Married Filing Separately (for later)
                 
                 
               #  mutate(FilingStatus=case_when(numadults>=2 ~ 2 # If two or more adults, then married
               #                                ,TRUE ~ 1)) %>%  
                 #-----------------------------
         # 2. Finances
         #-----------------------------
         mutate(income.investment=investment_income
                ,income_tm12 = income #need this for tax credits function to work
                ,ownorrent="rent"
                  ,assets.cash=assets                  #userinput
                ,assets.car1=0
                ,income_ind = income) %>% 
            
            mutate(income1=NA_real_
                   ,income2=NA_real_
                   ,income3=NA_real_
                   ,income4=NA_real_
                   ,income5=NA_real_
                   ,income6=NA_real_
                   ,income7=NA_real_
                   ,income8=NA_real_
                   ,income9=NA_real_
                   ,income10=NA_real_
                   ,income11=NA_real_
                   ,income12=NA_real_) %>%
                 
                 #-----------------------------
         # 3. Family types settings (Manually set up start ages of family members)
         #-----------------------------
         
         # Person 1 & 2 - adults
         # Person 3,4,5,6,7 - children
         mutate(agePerson1=case_when(numadults>=1  ~ as.numeric(age_adult_1) #override with age_adult_1 like with the children age
                                     ,TRUE ~ agePerson1),
                
                agePerson2=case_when(numadults>=2 ~ as.numeric(age_adult_2)
                                     ,TRUE ~ agePerson2),
                
                agePerson3=case_when(numadults>=3 ~ as.numeric(age_adult_3)
                                     ,TRUE ~ agePerson3),
                
                agePerson4=case_when(numadults>=4 ~ as.numeric(age_adult_4)
                                     ,TRUE ~ agePerson4),
                
                agePerson5=case_when(numadults>=5 ~ as.numeric(age_adult_5)
                                     ,TRUE ~ agePerson5),
                
                agePerson6=case_when(numadults>=6 ~ as.numeric(age_adult_6)
                                     ,TRUE ~ agePerson6),
                
                agePerson7=case_when(numkids>=1 ~ as.numeric(age_child_1)
                                     ,TRUE ~ agePerson7),
                
                agePerson8=case_when(numkids>=2 ~ as.numeric(age_child_2)
                                     ,TRUE ~ agePerson8),
                
                agePerson9=case_when(numkids>=3 ~ as.numeric(age_child_3)
                                     ,TRUE ~ agePerson9),
                
                agePerson10=case_when(numkids>=4 ~ as.numeric(age_child_4)
                                     ,TRUE ~ agePerson10),
                
                agePerson11=case_when(numkids>=5 ~ as.numeric(age_child_5)
                                      ,TRUE ~ agePerson11),
                
                agePerson12=case_when(numkids>=6 ~ as.numeric(age_child_6)
                                      ,TRUE ~ agePerson12),
         ) %>%
         
        #allocate other family income equally across other family members (assumption required for SSI & SDI)
         mutate(income1 = income
                   ,income2 = case_when(numadults>=2 ~ income.otherfamily/(numadults-1), TRUE~0) # Distribute other fam income among adults other than main user
                   ,income3 = case_when(numadults>=3 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income4 = case_when(numadults>=4 ~ income.otherfamily/(numadults-1), TRUE~0) 
                   ,income5 = case_when(numadults>=5 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income6 = case_when(numadults>=6 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income7 = case_when(numadults>=7 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income8 = case_when(numadults>=8 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income9 = case_when(numadults>=9 ~ income.otherfamily/(numadults-1), TRUE~0) 
                   ,income10 = case_when(numadults>=10 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income11 = case_when(numadults>=11 ~ income.otherfamily/(numadults-1), TRUE~0)
                   ,income12 = case_when(numadults>=12 ~ income.otherfamily/(numadults-1), TRUE~0)# When expanding fam sizes add to this section
            )
         
        csdata_try <<- csdata
         
         
         
         # Map state Abbrev to StateFIPS and county of choice to MSA for the wage growth projections
         csdata<-csdata %>% 
                 separate(locations, c("countyortownName","stateAbbrev"), sep=", ") %>% 
                 
                 left_join(table.statemap, by="stateAbbrev")# %>% 
           #      left_join(table.msamap, by=c("stateFIPS", "stateAbbrev","countyortownName"))
         
         
       
         csdata<-as.data.frame(csdata)
         
        # csdata<-function.InitialTransformations(csdata)
         csdata$year.index <- 1
         
         csdata <- csdata %>% left_join(table.countypop, by=c("stateFIPS", "countyortownName", "stateAbbrev", "stateName")) %>%
           left_join(table.msamap, by=c("stateAbbrev", "countyortownName"))
         
         csdata$numadults=rowSums(cbind(csdata$agePerson1, csdata$agePerson2, csdata$agePerson3, csdata$agePerson4, csdata$agePerson5, csdata$agePerson6, csdata$agePerson7, csdata$agePerson8, csdata$agePerson9, csdata$agePerson10, csdata$agePerson11, csdata$agePerson12)>=19,na.rm=TRUE) 
         csdata$numkids=rowSums(cbind(csdata$agePerson1, csdata$agePerson2, csdata$agePerson3, csdata$agePerson4, csdata$agePerson5, csdata$agePerson6, csdata$agePerson7, csdata$agePerson8, csdata$agePerson9, csdata$agePerson10, csdata$agePerson11, csdata$agePerson12)<=18,na.rm=TRUE) # Need to think about hte assumption on who is "kid". For example EITC defines it under age 19 & USDA adult category starts at 19
         csdata$numkidsunder13=rowSums(cbind(csdata$agePerson1, csdata$agePerson2, csdata$agePerson3, csdata$agePerson4, csdata$agePerson5, csdata$agePerson6, csdata$agePerson7, csdata$agePerson8, csdata$agePerson9, csdata$agePerson10, csdata$agePerson11, csdata$agePerson12)<=12,na.rm=TRUE)
         csdata$ageofYoungestChild=rowMins(cbind(csdata$agePerson1, csdata$agePerson2, csdata$agePerson3, csdata$agePerson4, csdata$agePerson5, csdata$agePerson6, csdata$agePerson7, csdata$agePerson8, csdata$agePerson9, csdata$agePerson10, csdata$agePerson11, csdata$agePerson12),na.rm=TRUE)
         
         # By default - allocate total family income EQUALLY to each adult family member (income1-income12)
         # Default Assumption 1: Equal allocation of income
         # Default Assumption 2: All adults are working 
         csdata$income1[!is.na(csdata$agePerson1) & csdata$agePerson1>=19]<-csdata$income[!is.na(csdata$agePerson1) & csdata$agePerson1>=19]/csdata$numadults[!is.na(csdata$agePerson1) & csdata$agePerson1>=19]
         csdata$income2[!is.na(csdata$agePerson2) & csdata$agePerson2>=19]<-csdata$income[!is.na(csdata$agePerson2) & csdata$agePerson2>=19]/csdata$numadults[!is.na(csdata$agePerson2) & csdata$agePerson2>=19]
         csdata$income3[!is.na(csdata$agePerson3) & csdata$agePerson3>=19]<-csdata$income[!is.na(csdata$agePerson3) & csdata$agePerson3>=19]/csdata$numadults[!is.na(csdata$agePerson3) & csdata$agePerson3>=19]
         csdata$income4[!is.na(csdata$agePerson4) & csdata$agePerson4>=19]<-csdata$income[!is.na(csdata$agePerson4) & csdata$agePerson4>=19]/csdata$numadults[!is.na(csdata$agePerson4) & csdata$agePerson4>=19]
         csdata$income5[!is.na(csdata$agePerson5) & csdata$agePerson5>=19]<-csdata$income[!is.na(csdata$agePerson5) & csdata$agePerson5>=19]/csdata$numadults[!is.na(csdata$agePerson5) & csdata$agePerson5>=19]
         csdata$income6[!is.na(csdata$agePerson6) & csdata$agePerson6>=19]<-csdata$income[!is.na(csdata$agePerson6) & csdata$agePerson6>=19]/csdata$numadults[!is.na(csdata$agePerson6) & csdata$agePerson6>=19]
         
         csdata<-csdata %>% 
           mutate(famsize = numadults + numkids # Family size
                  , hasdependent = case_when(  numkids>0 ~ 1 # Whether family has dependends (simple logic for now)
                                               , TRUE ~ 0)
                  , AKorHI = case_when(  stateAbbrev=="AK" ~ "AK"
                                         , stateAbbrev=="HI" ~ "HI"
                                         , TRUE ~ "0")
                  , totalassets = assets.car1 + assets.cash)
         
         csdata$income.child_support[csdata$numkids==0]<-0
         
         csdata_exp <<- csdata
         # Attach expenses
         csdata<-BenefitsCalculator.ALICEExpenses(csdata)
         
       csdata_alice <<- csdata
         
         if((benefit1 == "Select a custom list" & 'Childcare Subsidy (CCDF)' %in% benefit2) | benefit1 == "All programs"){
            childcareexp<-NA_real_
         }else{
            childcareexp <- childcareexp
         } 
         
         
         if(childcare_expenses != 'Enter your childcare costs'){
           childcareexp<-NA_real_
         }else{
           childcareexp <- childcareexp
         }
         
         if(!is.na(utilityexp))
         {
           csdata$exp.utilities <- utilityexp*12
         }
         
         
         if(!is.na(rentexp)){
           csdata$exp.rentormortgage <- rentexp*12
          
           csdata$exp.housing <- csdata$exp.rentormortgage + csdata$exp.utilities
           }
         
         
         if(!is.na(childcareexp)){
           csdata$exp.childcare <- childcareexp*12
         }
       csdata_0 <<- csdata
     #    csdata_prior <<- csdata
       zyx <<- 595
         # Benefits Calculator for benefits, tax credits and taxes
         csdata<-BenefitsCalculator.OtherBenefits(csdata, APPLY_TANF, APPLY_SSDI, APPLY_SSI)
         csdata_1 <<- csdata
         #note!! initital vs cont elig rules will only work for datset with ONE family in it.... b/c min function is not family specific!!
         zyx <<- 596
           csdata<-BenefitsCalculator.Childcare(csdata, APPLY_CHILDCARE, APPLY_HEADSTART, APPLY_PREK, APPLY_CCDF,APPLY_FATES, contelig.ccdf = contelig.ccdf,contelig.headstart = contelig.headstart, contelig.earlyheadstart = contelig.earlyheadstart) 
         zyx <<- 597
         csdata_2 <<- csdata
           csdata<-BenefitsCalculator.Healthcare(csdata, APPLY_HEALTHCARE, APPLY_MEDICAID_ADULT, APPLY_MEDICAID_CHILD, APPLY_ACA)
           csdata_3 <<- csdata
           csdata$townFIPS <- NULL
           csdata$stateName <- NULL
          # csdata$stcountyfips2010 <- NULL
         zyx <<- 598
           csdata<-BenefitsCalculator.FoodandHousing(csdata, APPLY_SECTION8, APPLY_LIHEAP, APPLY_SNAP, APPLY_SLP, APPLY_WIC,APPLY_RAP)
          csdata_4 <<- csdata
         zyx <<- 599
         
          csdata$income_ind[is.na(csdata$income_ind)] <- 0
          csdata$income1[is.na(csdata$income1)] <- 0
          csdata$income_tm12[is.na(csdata$income_tm12)] <- 0
          
         csdata<-BenefitsCalculator.TaxesandTaxCredits(csdata, APPLY_EITC, APPLY_CTC, APPLY_CDCTC)
         
         # need to create net expenses for transportation, and then subtract transportation stipend
         # also need to subtract childcare stipend from net childcare expenses 
         csdata$netexp.transportation <- 0
         csdata$netexp.transportation <- csdata$exp.transportation# - (csdata$transportation_stipend)
         
       
         #   csdata$exp.utilities[is.na(csdata$exp.utilities)] <- 0
         #   csdata$exp.rentormortgage[is.na(csdata$exp.rentormortgage)] <- 0
         #   csdata$netexp.rentormortgage[is.na(csdata$netexp.rentormortgage)] <- 0
         #   if(is.null(csdata$netexp.utilities)){
         #     csdata$netexp.utilities <- 0
         #   }
         
         #   if(is.null(csdata$value.liheap)){
         #     csdata$value.liheap <- 0
         #     }
         
         #if(csdata$empl_healthcare==1){
         #                               x <- csdata$netexp.healthcare
         #        csdata$netexp.healthcare <- csdata$employee_monthly_contribution
         #        }
         
         #csdata$netexp.rentormortgage[is.na(csdata$netexp.rentormortgage)] <- 0
         
         
         #if(csdata$empl_healthcare==1){
         #                               x <- csdata$netexp.healthcare
         #        csdata$netexp.healthcare <- csdata$employee_monthly_contribution
         #        }
         
         
         
         csdata<-function.createVars(csdata)
         
         if(csdata$empl_healthcare==1){
            csdata$living.expenses <- csdata$total.expenses - csdata$exp.healthcare + csdata$netexp.healthcare + csdata$value.medicaid + csdata$value.aca#- csdata$value.employ
         }else{
            csdata$living.expenses <- csdata$total.expenses #- csdata$exp.healthcare + csdata$netexp.healthcare #- csdata$value.employ
            
         }
         csdata$pub.assistance <- csdata$total.transfers# - csdata$value.aca - csdata$value.medicaid
         
       
         #, housing_stipend = other_stipend_housing*12
         #, utilities_stipend = other_stipend_utilities*12
         #, health_stipend = other_stipend_utilities*12
         #, food_stipend = other_stipend_food*12
         #, other_stipend = other_stipend_other*12
         
         
         csdata$cash.bonus <- cash_bonus
         
          
         return(csdata)
         
 } #end cross.section function
 