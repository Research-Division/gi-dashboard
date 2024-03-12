
shinyServer(function(input, output, session) {
  
  
  
  ############## KEEP ORGANIZATION INFO
  
  ############## FOR ORGANIZATIONS THAT ARE "OTHER", THEY NEED TO BE ABLE TO MANUALLY SELECT THEIR OWN WAIVERS FOR CCDF, SECTION 8, SNAP, TANF, AND WIC, THEN ABLE HERE AS APPROPRIATE 
  
  
  
    
###################################################################################
# NEXT/BACK BUTTONS
###################################################################################

  #allows start button to take user from starting page to "Results" page 
  
  observeEvent(input$next1, {
    js$next1();
    updateTabsetPanel(session, "main", selected = "Results")
  })  
   

###################################################################################
# ERROR MESSAGES
###################################################################################  
      
  # error message in case of missing inputs / other issues
  
  output$error_info_1 <- renderText({
    "Fill in all inputs to continue"
  })
  
  output$text1 <- renderText({
    "Without Guaranteed Income"
  })
  output$text2 <- renderText({
    "With Guaranteed Income"
  })
  output$specify <- renderText({
    "Specify Your Family's Characteristics"
  })
  
  
  toListenState <- reactive({list(input$organization)})
  observeEvent(toListenState(),{
    organization<-isolate(as.character(input$organization))
    updateSelectizeInput(session, 'state',
                         choices =  org_meta[Organization == organization, stateAbbrev],
                         server = TRUE,
                         selected = character(0))
  })
  
  
  
  # Sets list of counties to select based on state selected 
  toListenCounty <- reactive({list(input$state)})
  observeEvent(toListenCounty(),{
    state<-isolate(as.character(input$state))
    updateSelectizeInput(session, 'county_main',
                         choices =  loc_meta[stateAbbrev == state, countyortownName],
                         server = TRUE,
                         selected = character(0))
  })
  
  toListenBenefit2 <- reactive({list(input$state)})
  observeEvent(toListenBenefit2(),{
    state<-isolate(as.character(input$state))
    organization<-isolate(as.character(input$organization))
    updateSelectizeInput(session, 'benefit2',
                         choices =  benefit_meta[stateAbbrev == state & Organization == organization, benefit2],
                         server = TRUE,
                         selected = character(0))
  })
  
  
  
  toListen <- reactive({ # Recalculate if "Get Results" or "Calculate Budgets" are clicked
    list(input$getresults)  
  })
  
  
  observeEvent(toListen(),{
    
    if(input$getresults>0){
      
      updateActionButton(session, "getresults", label = "Recalculate")
      
      #B4 LISTENING FOR INPUTS BEGINS 'CALCULATE BUTTON' MUST BE CLICKED' (ONCE CLICKED ONCE IT STAYS 'CLICKED')
     
      benefit1<-isolate(as.character(input$benefit1))
     benefit2<-isolate(as.character(input$benefit2))
    #  benefit2b<-isolate(as.character(input$benefit2b))
    #  benefit2d<-isolate(as.character(input$benefit2d))
    #  benefit2e<-isolate(as.character(input$benefit2e))
      state <- isolate(as.character(input$state))
      organization <-isolate(as.character(input$organization))
      
    ccdf_waiver <- FALSE
    caps_waiver <- FALSE
    
      waiver.ccdf <- "No"
      waiver.section8 <- "No"
      waiver.snap<- "No"
      waiver.tanf <- "No"
      waiver.wic <- "No"
      
      if(organization == "Custom Analysis"){
        
        
        
        if(state != "GA" & state != "IL" & state !=  "MD" & state != "empty" & state != "WA" & state != "empty"){
          waiver.ccdf <- isolate(as.character(input$ccdf_waiver))
        }else if(state == "GA"){
          waiver.ccdf <- isolate(as.character(input$caps_waiver))
        }else if(state == "IL"){
          waiver.ccdf <- isolate(as.character(input$ccap_ccdf_waiver))
        }else if(state == "MD"){
          waiver.ccdf <- isolate(as.character(input$ccs_waiver))
        }else if(state == "WA"){
          waiver.ccdf <- isolate(as.character(input$wccc_waiver))
        }else{
          waiver.ccdf  <- "No"
        }
        
        if(state  != "empty"){
          waiver.section8 <- isolate(as.character(input$section8_waiver))
        }else{
          waiver.section8 <- "No"
        }
        
        if(state != "CA" & state  != "empty" & state !='AL' & state !='AZ' & state != 'DE' & state != 'FL' & 
           state != 'GA' & state != 'IA' & state != 'KS' & state != 'ME' & state != 'NC' & state != 'VT' & state != 'WA' & state != 'WI' & state != "empty"){
          waiver.snap <- isolate(as.character(input$snap_waiver))
        }else if(state == "CA"){
          waiver.snap <- isolate(as.character(input$calfresh_waiver))
        }else if(state == "AL" | state == "FL" | state == "IA" | state == "KS"){
          waiver.snap <- isolate(as.character(input$fap_waiver))
        }else if(state == "DE" | state == "ME"){
          waiver.snap <- isolate(as.character(input$fsp_waiver))
        }else if(state == "AZ'"){
          waiver.snap <- isolate(as.character(input$nutrition_assistance_waiver))
        }else if(state == "GA'"){
          waiver.snap <- isolate(as.character(input$food_stamp_waiver))
        }else if(state == "NC'"){
          waiver.snap <- isolate(as.character(input$fns_waiver))
        }else if(state == "VT'"){
          waiver.snap <- isolate(as.character(input$vt_waiver))
        }else if(state == "WA'"){
          waiver.snap <- isolate(as.character(input$bfp_waiver))
        }else if(state == "WI"){
          waiver.snap <- isolate(as.character(input$foodshare_waiver))
        }else{
          waiver.snap  <- "No"
        }
        
        
        
        
        
        if(state != "empty" & (state == 'DC' | state == 'GA' | state == 'HI' | state == 'IL' | state == 'IN'
                               | state == 'ME' | state == 'MS' | state == 'NV' | state == 'OK' | state == 'PA' | state == 'SD')){
          waiver.tanf <- isolate(as.character(input$tanf_waiver))
        }else if(state == "AK"){
          waiver.tanf <- isolate(as.character(input$AK_waiver))
        }else if(state == "AL"){
          waiver.tanf <- isolate(as.character(input$AL_waiver))
        }else if(state == "AR"){
          waiver.tanf <- isolate(as.character(input$AR_waiver))
        }else if(state == "AZ"){
          waiver.tanf <- isolate(as.character(input$AZ_waiver))
        }else if(state == "CA"){
          waiver.tanf <- isolate(as.character(input$CA_waiver))
        }else if(state == "CO"){
          waiver.tanf <- isolate(as.character(input$CO_waiver))
        }else if(state == "CT"){
          waiver.tanf <- isolate(as.character(input$CT_waiver))
        }else if(state == "DE"){
          waiver.tanf <- isolate(as.character(input$DE_waiver))
        }else if(state == "FL"){
          waiver.tanf <- isolate(as.character(input$FL_waiver))
        }else if(state == "IA"){
          waiver.tanf <- isolate(as.character(input$IA_waiver))
        }else if(state == "ID"){
          waiver.tanf <- isolate(as.character(input$ID_waiver))
        }else if(state == "KS"){
          waiver.tanf <- isolate(as.character(input$KS_waiver))
        }else if(state == "KY"){
          waiver.tanf <- isolate(as.character(input$KY_waiver))
        }else if(state == "LA"){
          waiver.tanf <- isolate(as.character(input$LA_waiver))
        }else if(state == "MA"){
          waiver.tanf <- isolate(as.character(input$MA_waiver))
        }else if(state == "MD"){
          waiver.tanf <- isolate(as.character(input$MD_waiver))
        }else if(state == "MN"){
          waiver.tanf <- isolate(as.character(input$MN_waiver))
        }else if(state == "MO"){
          waiver.tanf <- isolate(as.character(input$MO_waiver))
        }else if(state == "MT"){
          waiver.tanf <- isolate(as.character(input$MT_waiver))
        }else if(state == "NC"){
          waiver.tanf <- isolate(as.character(input$NC_waiver))
        }else if(state == "ND"){
          waiver.tanf <- isolate(as.character(input$ND_waiver))
        }else if(state == "NE"){
          waiver.tanf <- isolate(as.character(input$NE_waiver))
        }else if(state == "NH" | state == "NY"){
          waiver.tanf <- isolate(as.character(input$family_assistance_waiver))
        }else if(state == "NJ"){
          waiver.tanf <- isolate(as.character(input$NJ_waiver))
        }else if(state == "NM"){
          waiver.tanf <- isolate(as.character(input$NM_waiver))
        }else if(state == "OH"){
          waiver.tanf <- isolate(as.character(input$OH_waiver))
        }else if(state == "OR"){
          waiver.tanf <- isolate(as.character(input$OR_waiver))
        }else if(state == "RI" | state == "SC"){
          waiver.tanf <- isolate(as.character(input$fip_waiver))
        }else if(state == "TN"){
          waiver.tanf <- isolate(as.character(input$TN_waiver))
        }else if(state == "TX"){
          waiver.tanf <- isolate(as.character(input$TX_waiver))
        }else if(state == "UT"){
          waiver.tanf <- isolate(as.character(input$UT_waiver))
        }else if(state == "VA"){
          waiver.tanf <- isolate(as.character(input$VA_waiver))
        }else if(state == "VT"){
          waiver.tanf <- isolate(as.character(input$VT_waiver))
        }else if(state == "WA"){
          waiver.tanf <- isolate(as.character(input$workfirst_waiver))
        }else if(state == "WI"){
          waiver.tanf <- isolate(as.character(input$WI_waiver))
        }else if(state == "WV"){
          waiver.tanf <- isolate(as.character(input$WV_waiver))
        }else if(state == "WY"){
          waiver.tanf <- isolate(as.character(input$WY_waiver))
        }else{
          waiver.tanf  <- "No"
        }
        
        
        if(state  != "empty"){
          waiver.wic <- isolate(as.character(input$wic_waiver))
        }else{
          waiver.wic<- "No"
        }
        
        
        
        
        
        
        
        
        
        
        
        
      }
      
      
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
      
    #### CUSTOM NAMES FOR PROGRAMS
      
      #change the names of the programs for table display output 
      #note!!!! this programs must be added in benfits_list.csv & in conditional logic statements in ui. & changed below the final table output
      
      # AK
      benefit2[benefit2=="Alaska Temporary Assistance Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2=="DenaliCare (Medicaid for Adults)"] <-"Medicaid for Adults"
      benefit2[benefit2=="Denali Kidcare (Medicaid for Children/CHIP)"] <-"Medicaid for Children/CHIP"
      
      # AL
      benefit2[benefit2=="Food Assistance Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2=="Family Assistance Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2=="All Kids (Medicaid for Children/CHIP)"] <-"Medicaid for Children/CHIP"
      
      #AR
      benefit2[benefit2=="Transitional Employment Assistance (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2=="Health Care (Medicaid for Adults)"] <-"Medicaid for Adults"
      benefit2[benefit2=="AR Kids First B (Medicaid for Children/CHIP)"] <-"Medicaid for Children/CHIP"
      
      #AZ
      benefit2[benefit2=="Nutrition Assistance (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2=="EMPOWER (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2=="Arizona Health Care Cost Containment System (Medicaid for Adults)"] <-"Medicaid for Adults"
      benefit2[benefit2=="KidsCare (Medicaid for Children/CHIP)"] <-"Medicaid for Children/CHIP"
     
      # CA
      benefit2[benefit2== "CalWorks (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "CalFresh (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Medi-Cal for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Medi-Cal for Adults (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # CO
      benefit2[benefit2== "Colorado Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Colorado CHP+ (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Health First Colorado (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # CT
      benefit2[benefit2== "JOBS FIRST (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "HUSKY-B (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "HuskyHealth (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # DC
      benefit2[benefit2== "DC Healthy Families (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Healthy Families (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      #DE
      benefit2[benefit2== "A Better Chance (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Food Supplement Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Delaware Healthy Children's Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Diamond State Health Plan (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      #FL
      benefit2[benefit2== "Welfare Transition Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Food Assistance Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Florida KidCare (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Statewide Medicaid Managed Care Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # GA
      benefit2[benefit2=="PeachCare for Kids (Medicaid for Children/CHIP)"] <-"Medicaid for Children/CHIP"
      benefit2[benefit2=="Childcare and Parent Services (CAPS)"] <- "Child Care Subsidy (CCDF)"
      benefit2[benefit2=="Food Stamp Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      
      # HI
      benefit2[benefit2== "MedQuest (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # IA
      benefit2[benefit2== "Family Investment Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Food Assistance Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Healthy and Well Kids in Iowa (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "IA Health Link (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # ID
      benefit2[benefit2== "Temporary Assistance for Families in Idaho (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # IL
      benefit2[benefit2== "All Kids (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Child Care Assistance Program (CCAP/CCDF)"]<-"Child Care Subsidy (CCDF)"
      benefit2[benefit2== "Medical Assistance Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # IN
      benefit2[benefit2== "Hoosier Healthwise (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # KS
      benefit2[benefit2== "Kansas Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Food Assistance Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "KanCare (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "KanCare Medical Assistance Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      #KY
      benefit2[benefit2=="K-TAP - Kentucky Transitional Assistance Program (TANF)"]<-"Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2=="KCHIP - Kentucky Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # LA
      benefit2[benefit2== "FITAP - Family Independence Temporary Assistance Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Louisiana Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Healthy Louisiana (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MA
      benefit2[benefit2== "TAFDC (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "MassHealth for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Adult MassHealth (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MD
      benefit2[benefit2=='Child Care Scholarship (CCS)']<-"Child Care Subsidy (CCDF)"
      benefit2[benefit2=="Medical Assistance (Medicaid for Adults)"] <-"Medicaid for Adults"
      benefit2[benefit2=="Maryland Children's Health Program (Medicaid for Children/CHIP)"]<-"Medicaid for Children/CHIP"
      benefit2[benefit2=="Family Investment Program (TANF)"]<-"Temporary Assistance for Needy Families (TANF)"
      
      # ME
      benefit2[benefit2== "Food Supplement Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "MaineCare Cub Care (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "MaineCare (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      
      # MI
      benefit2[benefit2== "FIP - Family Independence Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "MIChild (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Medical Assistance (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MN
      benefit2[benefit2== "MFIP (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "MinnesotaCare (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Medical Assistance (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MO
      benefit2[benefit2== "Beyond Welfare (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "MO HealthNet for Kids (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "MO HealthNet (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MS
      benefit2[benefit2== "Mississippi Health Benefits (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Mississippi Coordinated Access Network (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # MT
      benefit2[benefit2== "FAIM - Families Achieving Independence in Montana (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Healthy Montana Kids (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # NC
      benefit2[benefit2== "Work First (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Food & Nutrition Services (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "North Carolina Health Choice (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Division of Health Benefits (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # ND
      benefit2[benefit2== "TEEM - Training, Employment, Education Management (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "North Dakota Medicaid Expansion Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # NE
      benefit2[benefit2== "Employment First (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Nebraska Medical Assistance Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # NJ
      benefit2[benefit2== "Work First New Jersey (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "NJ FamilyCare for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "NJ FamilyCare (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # NM
      benefit2[benefit2== "NM Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Centennial Care for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Centennial Care (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # Nv
      benefit2[benefit2== "Nevada Check Up (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
 
      # NY
      benefit2[benefit2== "Family Assistance Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Child Health Plus (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Medicaid Managed Care (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # OH
      benefit2[benefit2== "Ohio Works First (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Healthy Start (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # OK
      benefit2[benefit2== "SoonerCare for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "SoonerCare (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # OR
      benefit2[benefit2== "JOBS - Job Opportunities and Basic Skills Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Oregon Health Plan for Children (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Oregon Health Plan (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # PA
      benefit2[benefit2== "Pennsylvania’s Children’s Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Medical Assistance (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # RI
      benefit2[benefit2== "FIP - Family Independence Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "RIte Care (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "RI Medical Assistance Program (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # SC
      benefit2[benefit2== "Family Independence Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "South Carolina Healthy Connections (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Healthy Connections (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # SD
      benefit2[benefit2== "South Dakota Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      
      # TN
      benefit2[benefit2== "Families First (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "CoverKids (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "TennCare (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # TX
      benefit2[benefit2== "Texas Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "STAR+PLUS (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # UT
      benefit2[benefit2== "Family Employment Program (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      
      # VA
      benefit2[benefit2== "VIEW - Virginia Initiative for Employment, Not Welfare (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Family Access to Medical Insurance Security (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Cardinal Care (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # VT
      benefit2[benefit2== "ANFC - Aid to Needy Families with Children (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "3SquaresVT (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Dr. Dynasaur (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Green Mountain Care (Medicaid for Adults)"] <- "Medicaid for Adults"
      
      # WA
      benefit2[benefit2== "WorkFirst (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Basic Food Program (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "Apple Health for Kids (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Apple Health for Adults (Medicaid for Adults)"] <- "Medicaid for Adults"
      benefit2[benefit2== 'Working Connections Child Care Program (CCDF)']<-"Child Care Subsidy (CCDF)"
      
      # WI
      benefit2[benefit2== "Wisconsin Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "FoodShare (SNAP)"] <- "Supplemental Nutrition Assistance Program (SNAP)"
      benefit2[benefit2== "BadgerCare Plus (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "BadgerCare (Medicaid for Adults)"] <- "Medicaid for Adults" 
      
      # WV
      benefit2[benefit2== "West Virginia Works (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "West Virginia Children's Health Insurance Program (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
         
      # WY
      benefit2[benefit2== "POWER - Personal Opportunities With Employment Responsibility (TANF)"] <- "Temporary Assistance for Needy Families (TANF)"
      benefit2[benefit2== "Kid Care CHIP (Medicaid for Children/CHIP)"] <- "Medicaid for Children/CHIP"
      benefit2[benefit2== "Equality Care (Medicaid for Adults)"] <- "Medicaid for Adults" 
      
      
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       
      #income_specification <- isolate(as.character(input$income_specification))
      # isolate all inputs
      #hourly_wage_current <- isolate(as.numeric(input$hourly_wage_current))
      monthly_income_current <- isolate(as.numeric(input$monthly_income_current))
      working_hours_current <- isolate(as.numeric(input$working_hours_current))
      
      city.name <- isolate(as.character(input$county_main))
      region <- as.character(paste(city.name, state, sep=", ", collapse = NULL))
      city.name <- region
      county_main <- isolate(as.character(input$county_main))
      
     # hourly_wage_current <<- hourly_wage_current
      monthly_income_current <<- monthly_income_current
      working_hours_current <<- working_hours_current
  
      income_current <- monthly_income_current*12
      
      childcareexp <- NA_real_
      
      
      childcare_expenses <- isolate(as.character(input$childcare_expenses))
      housing_expenses <- isolate(as.character(input$housing_expenses))
      
     
      
      
      housing_error <- 0
      util_error <- 0
      
      utilityexp<-NA_real_
      rentexp <- NA_real_
   
      
      if(housing_expenses == 'Enter your housing costs'){
       
        
        if(benefit1 == "Select a custom list" & ('Section 8 Housing Voucher' %in% benefit2 | 'Supplemental Nutrition Assistance Program (SNAP)' %in% benefit2)){
          
          
          utilityexp<-isolate(as.numeric(input$utilexp))
          if(is.na(utilityexp)){
            utilityexp <- 0
            util_error <- 1
          }
          
        }
        
        
   
        
        if((benefit1 == "Select a custom list" & 'Section 8 Housing Voucher' %in% benefit2) | benefit1=="All programs"){
          rentexp<-isolate(as.numeric(input$rentexp1))
         
        }
        
        
        if(benefit1 == "Select a custom list" & 'Section 8 Housing Voucher' %nin% benefit2 & 'Supplemental Nutrition Assistance Program (SNAP)' %in% benefit2){
          rentexp<-isolate(as.numeric(input$rentexp2))
          
        }
        
        
        
      }else{
        rentexp <- NA_real_
        utilityexp<-NA_real_
      }
      
       utilityexp <<- utilityexp
      rentexp <<- rentexp
      util_error <<- util_error
      # benefit2[benefit2== "Temporary Cash Assistance"] <- "Temporary Assistance for Needy Families (TANF)"
      # benefit2[benefit2== "Child Health Plus"] <- "Medicaid for Children/CHIP"
      
      
      
xyz <<- 0
      
          
  cash_bonus_current <- isolate(as.numeric(input$cash_bonus_current))
  includes_health_insurance_current <- isolate(as.character(input$includes_health_insurance_current))
  other_income_sources <- isolate(as.character(input$other_income_sources))
 
 numadults <- isolate(as.numeric(input$numadults))
 if(numadults>1){
   income.otherfamily <- isolate(as.numeric(input$income.otherhousehold))
   income.otherfamily <- income.otherfamily*12
 }else{
   income.otherfamily <- 0
 }
 if(numadults>6){numadults<-6}
 
 abcde <<- 1
 
 
 numkids <- isolate(as.numeric(input$numkids))
 
 abcde <<- 2 
 
 if(numkids>6){numkids<-6}
 age_adult_1<-isolate(as.numeric(input$age_adult_1))
 age_adult_2<-isolate(as.numeric(input$age_adult_2))
 age_adult_3<-isolate(as.numeric(input$age_adult_3))
 age_adult_4<-isolate(as.numeric(input$age_adult_4))
 age_adult_5<-isolate(as.numeric(input$age_adult_5))
 age_adult_6<-isolate(as.numeric(input$age_adult_6))
 age_child_1<-isolate(as.numeric(input$age_child_1))
 age_child_2<-isolate(as.numeric(input$age_child_2))
 age_child_3<-isolate(as.numeric(input$age_child_3))
 age_child_4<-isolate(as.numeric(input$age_child_4))
 age_child_5<-isolate(as.numeric(input$age_child_5))
 age_child_6<-isolate(as.numeric(input$age_child_6))
 
 
 if(numadults==1){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-NA_real_
   age_adult_3<-NA_real_
   age_adult_4<-NA_real_
   age_adult_5<-NA_real_
   age_adult_6<-NA_real_
 }else if(numadults==2){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-isolate(as.numeric(input$age_adult_2))
   age_adult_3<-NA_real_
   age_adult_4<-NA_real_
   age_adult_5<-NA_real_
   age_adult_6<-NA_real_
 }else if(numadults==3){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-isolate(as.numeric(input$age_adult_2))
   age_adult_3<-isolate(as.numeric(input$age_adult_3))
   age_adult_4<-NA_real_
   age_adult_5<-NA_real_
   age_adult_6<-NA_real_
 }else if(numadults==4){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-isolate(as.numeric(input$age_adult_2))
   age_adult_3<-isolate(as.numeric(input$age_adult_3))
   age_adult_4<-isolate(as.numeric(input$age_adult_4))
   age_adult_5<-NA_real_
   age_adult_6<-NA_real_
 }else if(numadults==5){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-isolate(as.numeric(input$age_adult_2))
   age_adult_3<-isolate(as.numeric(input$age_adult_3))
   age_adult_4<-isolate(as.numeric(input$age_adult_4))
   age_adult_5<-isolate(as.numeric(input$age_adult_5))
   age_adult_6<-NA_real_
 }else if(numadults==6){
   age_adult_1<-isolate(as.numeric(input$age_adult_1))
   age_adult_2<-isolate(as.numeric(input$age_adult_2))
   age_adult_3<-isolate(as.numeric(input$age_adult_3))
   age_adult_4<-isolate(as.numeric(input$age_adult_4))
   age_adult_5<-isolate(as.numeric(input$age_adult_5))
   age_adult_6<-isolate(as.numeric(input$age_adult_6))
 }
 
 
 
 if(numkids==1){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-NA_real_
   age_child_3<-NA_real_
   age_child_4<-NA_real_
   age_child_5<-NA_real_
   age_child_6<-NA_real_
 }else if(numkids==2){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-isolate(as.numeric(input$age_child_2))
   age_child_3<-NA_real_
   age_child_4<-NA_real_
   age_child_5<-NA_real_
   age_child_6<-NA_real_
 }else if(numkids==3){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-isolate(as.numeric(input$age_child_2))
   age_child_3<-isolate(as.numeric(input$age_child_3))
   age_child_4<-NA_real_
   age_child_5<-NA_real_
   age_child_6<-NA_real_
 }else if(numkids==4){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-isolate(as.numeric(input$age_child_2))
   age_child_3<-isolate(as.numeric(input$age_child_3))
   age_child_4<-isolate(as.numeric(input$age_child_4))
   age_child_5<-NA_real_
   age_child_6<-NA_real_
 }else if(numkids==5){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-isolate(as.numeric(input$age_child_2))
   age_child_3<-isolate(as.numeric(input$age_child_3))
   age_child_4<-isolate(as.numeric(input$age_child_4))
   age_child_5<-isolate(as.numeric(input$age_child_5))
   age_child_6<-NA_real_
 }else if(numkids==6){
   age_child_1<-isolate(as.numeric(input$age_child_1))
   age_child_2<-isolate(as.numeric(input$age_child_2))
   age_child_3<-isolate(as.numeric(input$age_child_3))
   age_child_4<-isolate(as.numeric(input$age_child_4))
   age_child_5<-isolate(as.numeric(input$age_child_5))
   age_child_6<-isolate(as.numeric(input$age_child_6))
 }
 
 error_child_age <- 0
 
 if(numkids == 1 & is.na(age_child_1)){
   error_child_age <- 1
 }else if(numkids == 2 & (is.na(age_child_1) | is.na(age_child_2))){
   error_child_age <- 1
 }else if(numkids == 3 & (is.na(age_child_1) | is.na(age_child_2) | is.na(age_child_3))){
   error_child_age <- 1
 }else if(numkids == 4 & (is.na(age_child_1) | is.na(age_child_2) | is.na(age_child_3)| is.na(age_child_4))){
   error_child_age <- 1
 }else if(numkids == 5 & (is.na(age_child_1) | is.na(age_child_2) | is.na(age_child_3)| is.na(age_child_4)| is.na(age_child_5))){
   error_child_age <- 1
 }else if(numkids == 6 & (is.na(age_child_1) | is.na(age_child_2) | is.na(age_child_3)| is.na(age_child_4)| is.na(age_child_5)| is.na(age_child_6))){
   error_child_age <- 1
 }else{
   error_child_age <- 0
 }
 
 error_age_adult <- 0
 
 if(numadults==1 & is.na(age_adult_1)){
   error_age_adult <- 1
   age_adult_1 <- 25
 }else if(numadults==2 & (is.na(age_adult_1) | is.na(age_adult_2))){
   error_age_adult <- 1
   age_adult_1 <- 25
   age_adult_2 <- 25
 }else if(numadults==3 & (is.na(age_adult_1) | is.na(age_adult_2)| is.na(age_adult_3))){
   error_age_adult <- 1
   age_adult_1 <- 25
   age_adult_2 <- 25
   age_adult_3 <- 25
 }else if(numadults==4 & (is.na(age_adult_1) | is.na(age_adult_2)| is.na(age_adult_3)| is.na(age_adult_4))){
   error_age_adult <- 1
   age_adult_1 <- 25
   age_adult_2 <- 25
   age_adult_3 <- 25
   age_adult_4 <- 25
 }else if(numadults==5 & (is.na(age_adult_1) | is.na(age_adult_2)| is.na(age_adult_3)| is.na(age_adult_4)| is.na(age_adult_5))){
   error_age_adult <- 1
   age_adult_1 <- 25
   age_adult_2 <- 25
   age_adult_3 <- 25
   age_adult_4 <- 25
   age_adult_5 <- 25
 }else if(numadults==6 & (is.na(age_adult_1) | is.na(age_adult_2)| is.na(age_adult_3)| is.na(age_adult_4)| is.na(age_adult_5)| is.na(age_adult_6))){
   error_age_adult <- 1
   age_adult_1 <- 25
   age_adult_2 <- 25
   age_adult_3 <- 25
   age_adult_4 <- 25
   age_adult_5 <- 25
   age_adult_6 <- 25
 }else{
   error_age_adult <- 0
 }
 
 
 
 partnered <- "No"
 marital_status <- "No"
 FilingStatus<-1
 married<-0
 
 if(numadults>=2){
   partnered <- isolate(as.character(input$partnered))
 }else{
   partnered <- "No"
 }
 
 if(partnered == "Yes"){
   marital_status<-isolate(as.character(input$marital_status))
 }else{
   marital_status <- "No"
 }
   
   
 if(marital_status=="No"){
   FilingStatus<-1
   married<-0
 }else if(marital_status=="Yes"){
   FilingStatus<-2
   married<-1
 }
 
 
 
 
 xyz <<- 1
 
 #Set childcare expenses and look for errors
 
 if(childcare_expenses != 'Enter your childcare costs' & (benefit1 =="All programs" | (benefit1 == "Select a custom list" & 'Child Care Subsidy (CCDF)' %in% benefit2))){
   childcareexp<-NA_real_
 }else{
   childcareexp <- isolate(as.numeric(input$childcareexp1))
 }
 
 childcareexp <<- childcareexp
 
 if(numkids > 0 & childcare_expenses == 'Enter your childcare costs' & (benefit1 == "Select a custom list" & !('Child Care Subsidy (CCDF)' %in% benefit2) 
  &  ('Supplemental Nutrition Assistance Program (SNAP)' %in% benefit2 | 'Section 8 Housing Voucher' %in% benefit2))){
   childcare_manual <- 1
 }else{
   childcare_manual <- 0
 }
 
 
 childcare_expenses <<- childcare_expenses
 
 childcare_manual <<- childcare_manual
 
 error_childcareexp <- 0
 
 ccm <<- 1
 childcareexp <<- childcareexp
 
 if(childcare_manual == 1 & is.na(childcareexp) & numkids > 0 & childcare_expenses == 'Enter your childcare costs'){
   error_childcareexp <- 1
 }else{
   error_childcareexp <- 0
 }
 
 
 
  gift_income_current<-0
  gift_income_change <- isolate(as.numeric(input$gift_income_change))
 
 
 ############## other income sources
 
 if(other_income_sources == "Yes"){
   cash_bonus_specify_current <- isolate(as.character(input$cash_bonus_specify_current))
   cash_bonus_current <- isolate(as.numeric(input$cash_bonus_current))
   investment_income_specify_current <- isolate(as.character(input$investment_income_specify_current))
   investment_income_current <- isolate(as.numeric(input$investment_income_current))
   
   other_income_specify_current <- isolate(as.character(input$other_income_specify_current))
   

 }else{
   cash_bonus_specify_current <- "Annual"
   cash_bonus_current <- 0
   investment_income_specify_current <- "Annual"
   investment_income_current <- 0
   
   other_income_specify_current <- "Annual"
   
   
 }
 
 xyz <<- 11
 
 ######################## CHILD SUPPORT
 
 if(other_income_sources == "Yes" & numkids >= 1){
   child_support_specify_current <- isolate(as.character(input$child_support_specify_current))
   child_support_current <- isolate(as.numeric(input$child_support_current))
   
 }else{
   child_support_specify_current <- "Annual"
   child_support_current <- 0
 }
 

 #CONVERT ALL OTHER INCOME SOURCE VARIABLES TO ANNUAL 
 if(cash_bonus_specify_current == "Annual"){
   cash_bonus_current <- cash_bonus_current
 }else{
   cash_bonus_current <- cash_bonus_current*12
 }
 
 child_support_current <- child_support_current*12
 investment_income_current <- investment_income_current*12
 
 gift_income_change <- gift_income_change*12
 
 ####################### ASSETS
 
 assets_current <- isolate(as.numeric(input$assets_current))
 assets_change <- isolate(as.numeric(input$assets_change))
   

 xyz <<- 2
#EMPLOYER TOOL VARIABLES 

   stipend_current_childcare <- 0
   stipend_change_childcare <- 0
   taxable_current_childcare <- "No"
   taxable_change_childcare <- "No"
   stipend_current_transportation <- 0
   stipend_change_transportation <- 0
   taxable_current_transportation <- "No"
   taxable_change_transportation <- "No"
   stipend_current_housing <- 0
   stipend_change_housing <- 0
   taxable_current_housing <- "No"
   taxable_change_housing <- "No"
   stipend_current_utilities <- 0
   stipend_change_utilities <- 0
   taxable_current_utilities <- "No"
   taxable_change_utilities <- "No"
   stipend_current_health <- 0
   stipend_change_health <- 0
   taxable_current_health <- "No"
   taxable_change_health <- "No"
   stipend_current_food <- 0
   stipend_change_food <- 0
   taxable_current_food <- "No"
   taxable_change_food <-"No"
   stipend_current_other <- 0
   stipend_change_other <- 0
   taxable_current_other <-"No"
   taxable_change_other <- "No"
 
    employee_monthly_contribution_current <- 0
    employee_monthly_contribution_change <- 0
    
  ####################
    
#If employer offers health insurance, tell ben calculator, its' an option
      empl_healthcare_current <- 0

  if(includes_health_insurance_current == "yes_current"){
    empl_healthcare_current <- 1
  }
  
  
  #DISABILITY VARIABLES
  fam_disab <- "No"
  if(state != 'AL'){
  fam_disab <- isolate(as.character(input$fam_disab)) 
  }# ER 8/17: CONTINUE ASKING ABOUT DISABILITY BC OF SNAP PROVISIONS FOR PPL W/ DISABILITIES
  #fam_disab <- "No"
  prev_ssi<-'No'
  
  
  if(fam_disab=='Yes' & state != 'AL' & 'Medicaid for Adults' %in% benefit2){
  prev_ssi <- isolate(as.character(input$prev_ssi))
  }
  
  
  
  
  
  
 #ER : WHEN INCLUDING MEDICAID WHILE WORKING UNCOMMENT LINE ABOVE AND SECTIONS IN UI.R
  
  disability1 <- 0
  disability2 <- 0
  disability3 <- 0
  disability4 <- 0
  disability5 <- 0
  disability6 <- 0
  disability7 <- 0
  disability8 <- 0
  disability9 <- 0
  disability10 <- 0
  disability11 <- 0
  disability12 <- 0
  disab.work.exp<-0
  ssdiPIA1<-0
  ssdiPIA2<-0
  ssdiPIA3<-0
  ssdiPIA4<-0
  ssdiPIA5<-0
  ssdiPIA6<-0
  blind1<-0
  blind2<-0
  blind3<-0
  blind4<-0
  blind5<-0
  blind6<-0
  
  if (fam_disab=='Yes' & state != 'AL') {
    
    
   disability1<-isolate(as.logical(input$disab1))
   if(disability1==TRUE & numadults>=1){
     disability1<-1
   }else{
     disability1<-0
   }
   
   disability2<-isolate(as.logical(input$disab2))
   if(disability2==TRUE & numadults>=2){
     disability2<-1
   }else{
    disability2<-0
   }
   
   disability3<-isolate(as.logical(input$disab3))
   if(disability3==TRUE & numadults>=3){
     disability3<-1
   }else{
     disability3<-0
   }
   
   disability4<-isolate(as.logical(input$disab4))
   if(disability4==TRUE & numadults>=4){
     disability4<-1
   }else{
     disability4<-0
   }
   
   disability5<-isolate(as.logical(input$disab5))
   if(disability5==TRUE & numadults>=5){
     disability5<-1
   }else{
     disability5<-0
   }
   
   disability6<-isolate(as.logical(input$disab6))
   if(disability6==TRUE & numadults>=6){
     disability6<-1
   }else{
     disability6<-0
   }
   
   disability7<-isolate(as.logical(input$disab7))
   if(disability7==TRUE & numkids>=1){
     disability7<-1
   }else{
     disability7<-0
   }
   
   disability8<-isolate(as.logical(input$disab8))
   if(disability8==TRUE & numkids>=2){
     disability8<-1
   }else{
     disability8<-0
   }
   
   disability9<-isolate(as.logical(input$disab9))
   if(disability9==TRUE & numkids>=3){
     disability9<-1
   }else{
     disability9<-0
   }
   
   disability10<-isolate(as.logical(input$disab10))
   if(disability10==TRUE & numkids>=4){
     disability10<-1
   }else{
     disability10<-0
   }
   
   disability11<-isolate(as.logical(input$disab11))
   if(disability11==TRUE & numkids>=5){
     disability11<-1
   }else{
     disability11<-0
   }
   
   disability12<-isolate(as.logical(input$disab12))
   if(disability12==TRUE & numkids>=6){
     disability12<-1
   }else{
     disability12<-0
   }
  }else{
    disability1 <- 0
    disability2 <- 0
    disability3 <- 0
    disability4 <- 0
    disability5 <- 0
    disability6 <- 0
    disability7 <- 0
    disability8 <- 0
    disability9 <- 0
    disability10 <- 0
    disability11 <- 0
    disability12 <- 0
  }

  if(prev_ssi=="Yes"){
    prev_ssi<-1}
  else
  {prev_ssi<-0}
   
   if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Supplemental Security Income (SSI)' %in% benefit2)) & fam_disab=="Yes"){
     disab.work.exp<-isolate(as.numeric(input$disab.work.exp))
     if(is.na(disab.work.exp) | state == 'AL'){
       disab.work.exp<-0
     }
   }else{
     disab.work.exp <- 0
   }

  
  ssdi_error_1 <- 0
  ssdi_error_2 <- 0
  ssdi_error_3 <- 0
  ssdi_error_4 <- 0
  ssdi_error_5 <- 0
  ssdi_error_6 <- 0
  
  if(state != 'AL'){
  
  if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Social Security Disability Insurance (SSDI)' %in% benefit2)) & fam_disab=="Yes"){

    if(numadults>=1 & disability1==1){
      ssdiPIA1<-isolate(as.numeric(input$ssdiPIA1))
      
      if(is.na(ssdiPIA1) | ssdiPIA1<= 0){
        ssdi_error_1 <- 1
      }else{
        ssdi_error_1 <- 0
      }
      
      
    }else{
      ssdiPIA1<-0
    }

    if(numadults>=2 & disability2==1){
      ssdiPIA2<-isolate(as.numeric(input$ssdiPIA2))
      
      
      if(is.na(ssdiPIA2) | ssdiPIA2<= 0){
        ssdi_error_2 <- 1
      }else{
        ssdi_error_2 <- 0
      }
      
      
    }else{
      ssdiPIA2<-0
    }

    if(numadults>=3 & disability3==1){
      ssdiPIA3<-isolate(as.numeric(input$ssdiPIA3))
      
      if(is.na(ssdiPIA3) | ssdiPIA3<= 0){
        ssdi_error_3 <- 1
      }else{
        ssdi_error_3 <- 0
      }
      
    }else{
      ssdiPIA3<-0
    }

    if(numadults>=4 & disability4==1){
      ssdiPIA4<-isolate(as.numeric(input$ssdiPIA4))
      
      if(is.na(ssdiPIA4) | ssdiPIA4<= 0){
        ssdi_error_4 <- 1
      }else{
        ssdi_error_4 <- 0
      }
      
    }else{
      ssdiPIA4<-0
    }

    if(numadults>=5 & disability5==1){
      ssdiPIA5<-isolate(as.numeric(input$ssdiPIA5))
      
      if(is.na(ssdiPIA5) | ssdiPIA5<= 0){
        ssdi_error_5 <- 1
      }else{
        ssdi_error_5 <- 0
      }
      
    }else{
      ssdiPIA5<-0
    }

    if(numadults>=6 & disability6==1){
      ssdiPIA6<-isolate(as.numeric(input$ssdiPIA6))
      
      if(is.na(ssdiPIA6) | ssdiPIA6<= 0){
        ssdi_error_6 <- 1
      }else{
        ssdi_error_6 <- 0
      }
      
    }else{
      ssdiPIA6<-0
    }


    blind1<-isolate(as.logical(input$blind1))
    if(blind1==TRUE){
      blind1<-1
    }else{
      blind1<-0
    }

    blind2<-isolate(as.logical(input$blind2))
    if(blind2==TRUE){
      blind2<-1
    }else{
      blind2<-0
    }

    blind3<-isolate(as.logical(input$blind3))
    if(blind3==TRUE){
      blind3<-1
    }else{
      blind3<-0
    }

    blind4<-isolate(as.logical(input$blind4))
    if(blind4==TRUE){
      blind4<-1
    }else{
      blind4<-0
    }


    blind5<-isolate(as.logical(input$blind5))
    if(blind5==TRUE){
      blind5<-1
    }else{
      blind5<-0
    }

    blind6<-isolate(as.logical(input$blind6))
    if(blind6==TRUE){
      blind6<-1
    }else{
      blind6<-0
    }

  }
  else{
    ssdiPIA1<-0
    ssdiPIA2<-0
    ssdiPIA3<-0
    ssdiPIA4<-0
    ssdiPIA5<-0
    ssdiPIA6<-0
    blind1<-0
    blind2<-0
    blind3<-0
    blind4<-0
    blind5<-0
    blind6<-0
  }
}
 
  disab_error <- 0
  if(fam_disab == "Yes" & 
     disability1 == 0 & 
     disability2 == 0 & 
     disability3 == 0 & 
     disability4 == 0 & 
     disability5 == 0 & 
     disability6 == 0 & 
     disability7 == 0 & 
     disability8 == 0 & 
     disability9 == 0 & 
     disability10 == 0 & 
     disability11 == 0 & 
     disability12 == 0){
    disab_error <- 1
  }
  
  
  contelig.ccdf <<- TRUE
  contelig.headstart <<- TRUE
  contelig.earlyheadstart <<-TRUE
  

 xyzxyz <<- 1
 
 benefit1 <<- benefit1
 benefit2 <<-benefit2
 empl_healthcare_current <<- empl_healthcare_current
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
 income_current <<- income_current
 cash_bonus_current <<- cash_bonus_current
 child_support_current <<- child_support_current
 investment_income_current <<- investment_income_current
 gift_income_change <<- gift_income_change
 assets_current <<- assets_current
 stipend_current_childcare <<- stipend_current_childcare
 stipend_current_transportation <<- stipend_current_transportation
 stipend_current_housing <<- stipend_current_housing
 stipend_current_utilities <<- stipend_current_utilities
 stipend_current_health <<- stipend_current_health
 stipend_current_food <<- stipend_current_food
 stipend_current_other <<- stipend_current_other
 employee_monthly_contribution_current <<- employee_monthly_contribution_current
 taxable_current_transportation <<- taxable_current_transportation
 taxable_current_childcare <<- taxable_current_childcare
 taxable_current_housing <<- taxable_current_housing
 taxable_current_utilities <<- taxable_current_utilities
 taxable_current_health <<- taxable_current_health
 taxable_current_food <<- taxable_current_food
 taxable_current_other <<- taxable_current_other
 rentexp <<- rentexp
 utilityexp <<-utilityexp
 #housingexp <<- housingexp
 childcareexp <<- childcareexp




 
 if(housing_expenses == 'Enter your housing costs' & is.na(rentexp)){
   
   if((benefit1 == "Select a custom list" & 'Section 8 Housing Voucher' %in% benefit2) | benefit1=="All programs"){
     housing_error <- 1
   }else if(benefit1 == "Select a custom list" & 'Section 8 Housing Voucher' %nin% benefit2 & 'Supplemental Nutrition Assistance Program (SNAP)' %in% benefit2){
     housing_error <- 1
   }else{
     housing_error <- 0
   }
 }else{
   housing_error <- 0
 }
 
 
 housing_error <<- housing_error
 
 xyzxyz <<- 2
 
 housing_expenses <<- housing_expenses
 childcare_expenses <<- childcare_expenses
 
 
 missing_wage <- 0
 
 # if(income_specification == "Hourly" & (is.na(input$hourly_wage_current) | is.na(input$working_hours_current))){
 #   missing_wage <- 1
 # }else if(income_specification == "Monthly" & is.na(input$monthly_income_current)){
 #   missing_wage <- 1
 # }else{
 #   missing_wage <- 0
 # }
 
 if(is.na(input$monthly_income_current)){
      missing_wage <- 1
    }else{
      missing_wage <- 0
    }

ssdi_error <- 0
if(ssdi_error_1 == 1 | ssdi_error_2 == 1 | ssdi_error_3 == 1 | ssdi_error_4 == 1 | ssdi_error_5 ==1 | ssdi_error_6 == 1){
  ssdi_error <- 1
}


ssdi_no_adults <- 0

if((benefit1 == "All programs" | (benefit1 == "Select a custom list" & 'Social Security Disability Insurance (SSDI)' %in% benefit2)) & fam_disab=="Yes"){
  
  if(disability1 == 0 & disability2 == 0 & disability3 == 0 & disability4 == 0 & disability5 == 0 & disability6 == 0){
    ssdi_no_adults <- 1
  }else{
    ssdi_no_adults <- 0
  }
  
}


ssi_ssdi_error <- 0

if(fam_disab == "No" & benefit1 == "Select a custom list" & ('Supplemental Security Income (SSI)' %in% benefit2 | 'Social Security Disability Insurance (SSDI)' %in% benefit2)){
  ssi_ssdi_error <- 1
}else{
  ssi_ssdi_error <- 0
}

 
 
numadults <<- numadults
state <<- state
county_main <<- county_main
benefit1 <<- benefit1
organization <<- organization
error_child_age <<- error_child_age
income_current <<- income_current
gift_income_change <<- gift_income_change
housing_error <<- housing_error
error_childcareexp <<- error_childcareexp

################ WITHOUT ERRORS

if(numadults!="empty" & 
  state!="empty" &
  county_main!="" &
  benefit1!="empty" &
  organization!="empty" &
  error_child_age==0 &
  missing_wage==0 &
  !is.na(income_current) &
  !is.na(gift_income_change) &
  housing_error==0 & 
  util_error==0 & 
  error_childcareexp==0&
  fam_disab!="empty"
  & ssdi_error==0
  & disab_error == 0
  & ssdi_no_adults == 0
  & ssi_ssdi_error == 0){

  
  
  xyzxyz <<- 21
  
  csdata.current  <<- cross.section.employer(benefit1, benefit2, empl_healthcare_current, city.name, numadults, numkids, age_adult_1, age_adult_2, age_adult_3, age_adult_4, age_adult_5, age_adult_6, age_child_1, age_child_2, age_child_3, age_child_4, age_child_5, age_child_6, disability1, disability2, disability3, disability4, disability5, disability6, disability7,  disability8,disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi, income.otherfamily, income_current, cash_bonus_current, child_support_current, investment_income_current, gift_income_current, assets_current, stipend_current_childcare, stipend_current_transportation, stipend_current_housing, stipend_current_utilities, stipend_current_health, stipend_current_food, stipend_current_other, employee_monthly_contribution_current, taxable_current_transportation, taxable_current_childcare, taxable_current_housing, taxable_current_utilities, taxable_current_health, taxable_current_food, taxable_current_other, rentexp, utilityexp, childcareexp, contelig.ccdf,contelig.headstart,contelig.earlyheadstart,housing_expenses,childcare_expenses,FilingStatus,married)
 
  xyzxyz <<- 22
  
   csdata.change <<- cross.section.employer(benefit1, benefit2, empl_healthcare_current, city.name, numadults, numkids, age_adult_1, age_adult_2, age_adult_3, age_adult_4, age_adult_5, age_adult_6, age_child_1, age_child_2, age_child_3, age_child_4, age_child_5, age_child_6, disability1, disability2, disability3, disability4, disability5, disability6, disability7,  disability8, disability9, disability10, disability11, disability12, ssdiPIA1, ssdiPIA2, ssdiPIA3, ssdiPIA4, ssdiPIA5, ssdiPIA6, blind1, blind2, blind3, blind4, blind5, blind6, disab.work.exp, prev_ssi, income.otherfamily, income_current, cash_bonus_current, child_support_current, investment_income_current, gift_income_change, assets_change, stipend_change_childcare, stipend_change_transportation, stipend_change_housing, stipend_change_utilities, stipend_change_health, stipend_change_food, stipend_change_other, employee_monthly_contribution_change, taxable_change_transportation, taxable_change_childcare, taxable_change_housing, taxable_change_utilities, taxable_change_health, taxable_change_food, taxable_change_other, rentexp, utilityexp, childcareexp, contelig.ccdf,contelig.headstart,contelig.earlyheadstart,housing_expenses,childcare_expenses,FilingStatus,married)
  
  xyzxyz <<- 3
  
  ece <<- error_childcareexp
  
  csdata.change$income_level <- csdata.change$income
  csdata.current$income_level <- csdata.current$income 
  
  #################################################################
  # CALL CROSS SECTION FUNCTION - GENERATES DATA FOR TABLE & CHARTS
  ################################################################
  
  #CROSS SECTION FUNCTION GENERATES THE CROSS SECTION DATA, CREATES SOME VARS, AND  RUNS THE BENEFIT CALCULATOR ON IT
  #WITHIN THE CROSS.SECTION FUNCTION ARE A BUNCH OF OTHER FUNCTIONS 
  

  
  #override values of programs that have benefit waivers with pre GI values!!
  #note this is not perfect because it doesn't consider any interactions. For example, if person loses ccdf, their childcare expenses go up and thus their snap would actually increase.
  
  #####UPDATE AS NEEDED FOR DIFF PROGRAMS that have waivers!!!####
  
  tanf.waiver<-FALSE
  snap.waiver<-FALSE
  section8.waiver<-FALSE
  ccdf.waiver<-FALSE
  wic.waiver<-FALSE
  
  section8.waiver.kindof<-FALSE #will add 'kind of' langauge but not override values
  
#  csdata.diff4netresources<-0
  
  #CDF is still figuring out their waiver. They will message us in octber 2022 with teh answer
   # if(organization=="Children's Defense Fund"){
   #   tanf.waiver=TRUE
   #   snap.waiver=TRUE
   #csdata.diff4netresources= (changevars.change$value.snap +changevars.change$value.tanf) - (changevars.current$value.snap +changevars.current$value.tanf)
  #  }
  #
  
  if(organization  == "Custom Analysis"){
    if(waiver.ccdf== "Yes"){
      ccdf.waiver  <-  TRUE
    }
    
    if(waiver.section8=="Yes"){
      section8.waiver <- TRUE
    }
    
    if(waiver.snap == "Yes"){
      snap.waiver <-TRUE
    }
    
    if(waiver.tanf == "Yes"){
      tanf.waiver<-TRUE
    }
    
    if(waiver.wic == "Yes"){
      wic.waiver <- TRUE
    }
  }
  
    if(organization=="ARISE Alexandria"){
    tanf.waiver<-TRUE
    ccdf.waiver<-TRUE
    snap.waiver<-TRUE
    }
  
  
  if(organization=="BREATHE (Los Angeles County)"){
    tanf.waiver<-TRUE
 #   ccdf.waiver<-TRUE
    snap.waiver<-TRUE
  }
  
  section8.waiver.kindof.Alameda <- FALSE
  
  if(organization=="City of Alameda"){
    tanf.waiver<-TRUE
    section8.waiver.kindof.Alameda <- TRUE
    snap.waiver<-TRUE
  }
  
  if(organization=="NYS Direct Cash Transfer"){
   tanf.waiver<-TRUE
   ccdf.waiver<-TRUE
   snap.waiver<-TRUE
  }
  
  
  if(organization=="Children's Defense Fund"){
    tanf.waiver<-TRUE
    ccdf.waiver<-TRUE
    snap.waiver<-TRUE
  }
  
  if(organization %in% c("Atlanta Abundant Birth Project", "Empower House", "Urban League Greater Atlanta")){
    section8.waiver.kindof<-TRUE
  }
    
    
  if(organization=="Cook County Promise"){
    tanf.waiver<-TRUE
    section.waiver<-TRUE
    ccdf.waiver<-TRUE
    snap.waiver<-TRUE
    section8.waiver.kindof<-TRUE
    #note custom text about aid for aged, blinkd disabled, medical, health benefits for workers with diabilitys and medicare savings program (if waiver is approved- waiting as of 10/08/2022)
  }
  

  if(organization=="City of Philadelphia"){
    tanf.waiver<-TRUE
  }
    
  if(organization=="MoCo Boost"){
      ccdf.waiver<-TRUE
  }
    
  if(organization=="CASH Campaign of Maryland"){
         section8.waiver<-TRUE
  }   
  
 
  
 
  
  snap.waiver <<- snap.waiver
  ccdf.waiver <<- ccdf.waiver
  section8.waiver <<- section8.waiver
  tanf.waiver <<- tanf.waiver
  wic.waiver <<- wic.waiver
  
  
  
  if (tanf.waiver==TRUE) {
    
    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.tanf + csdata.current$value.tanf
   csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.tanf + csdata.current$value.tanf
   csdata.change$value.tanf <- csdata.current$value.tanf
   
    }
  
  if (snap.waiver==TRUE) {
    
    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.snap + csdata.current$value.snap
    csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.snap + csdata.current$value.snap
    csdata.change$value.snap <- csdata.current$value.snap
    
  }
  
  if (ccdf.waiver==TRUE) {
    
    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.CCDF + csdata.current$value.CCDF
    csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.CCDF + csdata.current$value.CCDF
    csdata.change$value.CCDF <- csdata.current$value.CCDF
    
  }
  
  if (section8.waiver==TRUE){
    

    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.section8 + csdata.current$value.section8
    csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.section8 + csdata.current$value.section8
    csdata.change$value.section8 <- csdata.current$value.section8
    
  }
  
  if (wic.waiver==TRUE) {
    

    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.wic + csdata.current$value.wic
    csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.wic + csdata.current$value.wic
    csdata.change$value.wic <- csdata.current$value.wic
    
  }
  
  if (section8.waiver.kindof.Alameda==TRUE){
    
    csdata.change$pub.assistance <- csdata.change$pub.assistance - csdata.change$value.section8 + csdata.current$value.section8
    csdata.change$NetResources <- csdata.change$NetResources - csdata.change$value.section8 + csdata.current$value.section8
    csdata.change$value.section8 <- csdata.current$value.section8
    
  }
  
#  csdata.diff4netresources <<-  csdata.diff4netresources
  
  nochangevars<-csdata.current %>%
    select(income, numadults, numkids)
  
  changevars.change<-csdata.change %>%
    select(NetResources,value.aca,value.medicaid.adult,value.medicaid.child,value.section8,value.HeadStart,value.earlyHeadStart,value.PreK,value.CCDF,
           value.snap,value.wic,value.tanf,value.schoolmeals,value.cdctc,value.ctc,value.eitc,value.liheap,value.ssdi,value.ssi,tax.income.fed,tax.income.state,tax.FICA,
           netexp.transportation,netexp.childcare,netexp.food,netexp.healthcare,#tax.sales,
           netexp.housing, netexp.utilities,netexp.rentormortgage,exp.misc,tax.income.fed,
           tax.income.state,tax.FICA,income_level, income.aftertax.noTC, total.transfers, total.expenses, living.expenses, cash.bonus, pub.assistance, AfterTaxIncome)
  changevars.change<-changevars.change/12
  
  changevars.current<-csdata.current %>%
    select(NetResources,value.aca,value.medicaid.adult,value.medicaid.child,value.section8,value.HeadStart,value.earlyHeadStart,value.PreK,value.CCDF,
           value.snap,value.wic,value.tanf,value.schoolmeals,value.cdctc,value.ctc,value.eitc,value.liheap, value.ssdi,value.ssi,tax.income.fed,tax.income.state,tax.FICA,
           netexp.transportation,netexp.childcare,netexp.food,netexp.healthcare,#tax.sales, 
           netexp.housing, netexp.utilities,netexp.rentormortgage,exp.misc,tax.income.fed,
           tax.income.state,tax.FICA,income_level,income.aftertax.noTC, total.transfers, total.expenses, living.expenses, cash.bonus, pub.assistance, AfterTaxIncome)
  changevars.current<-changevars.current/12
  
  changevars<- changevars.change-changevars.current
  
  changevars <<- changevars
  changevars.current <<- changevars.current
  changevars.change <<- changevars.change
  
  csdata.changes<-cbind(nochangevars,changevars)
  csdata.initial<-cbind(nochangevars,changevars.current)
  csdata.new <- cbind(nochangevars,changevars.change)
  
  
csdata.changes <<- csdata.changes
csdata.initial <<- csdata.initial
csdata.new <<- csdata.new


#List of states where TANF is in the PRD; all states not in this list have tanf set to missing
#if(csdata.change$stateAbbrev != "AL" & csdata.change$stateAbbrev != "CA" & csdata.change$stateAbbrev != "CO" & csdata.change$stateAbbrev != "CT" 
#   & csdata.change$stateAbbrev != "DC" & csdata.change$stateAbbrev != "FL" & csdata.change$stateAbbrev != "KY" & csdata.change$stateAbbrev != "LA" 
#   & csdata.change$stateAbbrev != "ME" & csdata.change$stateAbbrev != "MD" & csdata.change$stateAbbrev != "MA" & csdata.change$stateAbbrev != "NY" 
#   & csdata.change$stateAbbrev != "TN" & csdata.change$stateAbbrev != "TX" & csdata.change$stateAbbrev != "WA"){
#  csdata.new$value.tanf <- NA_real_
#  csdata.initial$value.tanf <- NA_real_
##  csdata.changes$value.tanf <- NA_real_
#}

#i think this was just temp until we got ssi and ssdi working ??
#  csdata.changes$value.ssi <- NA_real_
#  csdata.initial$value.ssi <- NA_real_
#  csdata.new$value.ssi <- NA_real_
# # 
#  csdata.changes$value.ssdi <- NA_real_
#  csdata.initial$value.ssdi <- NA_real_
#  csdata.new$value.ssdi <- NA_real_

 ece <<- error_childcareexp

  #GENERATE TABLE SHOWING CURRENT VALUE OF EACH PROGRAM / EXPENSE / NET RESOURCES AND CHANGE IN VALUE, ALONG WITH ROW NAME
  
  # initial - estimated amount earned
  # change - amount change when implementing "new value" variable amounts
  
  ##########################
  # PUBLIC ASSISTANCE RECEIVED
  ###########################
  
  datafortable.initial<-csdata.initial %>% 
    filter(income==income) %>% 
    mutate(value.taxcredits=value.cdctc +value.ctc + value.eitc, 
           value.HeadStart= value.HeadStart + value.earlyHeadStart,
           "Social Security" = NA_real_,
           #"Supplemental Security Income (SSI)" = NA_real_,
           #"Social Security Disability Insurance (SSDI)" = NA_real_,
           "Unemployment Insurance"=NA_real_,
           "Home Energy Assistance" = NA_real_) %>% 
    rename(
      "Health Insurance Marketplace Subsidy" = value.aca,
      "Medicaid for Adults" = value.medicaid.adult,
      "Medicaid for Children/CHIP" = value.medicaid.child,
      "Section 8 Housing Voucher" = value.section8,
      "Head Start/Early Head Start"= value.HeadStart,
      "State-Funded Pre-Kindergarten" = value.PreK,
      "Child Care Subsidy (CCDF)" = value.CCDF,
      "Supplemental Nutrition Assistance Program (SNAP)"= value.snap,
      "Women, Infants and Children Nutrition Program (WIC)" = value.wic,
      "Temporary Assistance for Needy Families (TANF)"=value.tanf,
      "Free or Reduced Price School Meals" = value.schoolmeals,
      "Supplemental Security Income (SSI)" = value.ssi,
      "Social Security Disability Insurance (SSDI)" = value.ssdi,
      "Child and Dependent Care Tax Credit (CDCTC)"= value.cdctc,
      "Child Tax Credit (CTC)" = value.ctc,
      "Earned Income Tax Credit (EITC)"= value.eitc,
      "Federal Income Tax" = tax.income.fed,
      "State Income Tax" = tax.income.state,
      "FICA Tax" = tax.FICA
     ) 
  
  
  datafortable.initial2<-pivot_longer(datafortable.initial, cols=c("Health Insurance Marketplace Subsidy":"Head Start/Early Head Start","State-Funded Pre-Kindergarten":"Free or Reduced Price School Meals",
                                                                   "Social Security", "Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)", "Unemployment Insurance",
                                                                   "Child and Dependent Care Tax Credit (CDCTC)","Child Tax Credit (CTC)","Earned Income Tax Credit (EITC)", "Federal Income Tax", "State Income Tax", "FICA Tax"),
                                       values_to="Without GI") %>% 
    select(name, 'Without GI')
    
  
  datafortable.new<-csdata.new %>% 
    filter(income==income) %>% 
    mutate(value.taxcredits=value.cdctc +value.ctc + value.eitc, 
           value.HeadStart= value.HeadStart + value.earlyHeadStart,
           "Social Security" = NA_real_,
           #"Supplemental Security Income (SSI)" = NA_real_,
           #"Social Security Disability Insurance (SSDI)" = NA_real_,
           "Unemployment Insurance"=NA_real_,
           "Home Energy Assistance" = NA_real_) %>% 
    rename(
      "Health Insurance Marketplace Subsidy" = value.aca,
      "Medicaid for Adults" = value.medicaid.adult,
      "Medicaid for Children/CHIP" = value.medicaid.child,
      "Section 8 Housing Voucher" = value.section8,
      "Head Start/Early Head Start"= value.HeadStart,
      "State-Funded Pre-Kindergarten" = value.PreK,
      "Child Care Subsidy (CCDF)" = value.CCDF,
      "Supplemental Nutrition Assistance Program (SNAP)"= value.snap,
      "Women, Infants and Children Nutrition Program (WIC)" = value.wic,
      "Temporary Assistance for Needy Families (TANF)"=value.tanf,
      "Free or Reduced Price School Meals" = value.schoolmeals,
      "Supplemental Security Income (SSI)" = value.ssi,
      "Social Security Disability Insurance (SSDI)" = value.ssdi,
      "Child and Dependent Care Tax Credit (CDCTC)"= value.cdctc,
      "Child Tax Credit (CTC)" = value.ctc,
      "Earned Income Tax Credit (EITC)"= value.eitc,
      "Federal Income Tax" = tax.income.fed,
      "State Income Tax" = tax.income.state,
      "FICA Tax" = tax.FICA
    ) 
  
  
  datafortable.new2<-pivot_longer(datafortable.new, cols=c("Health Insurance Marketplace Subsidy":"Head Start/Early Head Start","State-Funded Pre-Kindergarten":"Free or Reduced Price School Meals",
                                                           "Social Security", "Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)", "Unemployment Insurance",
                                                           "Child and Dependent Care Tax Credit (CDCTC)","Child Tax Credit (CTC)","Earned Income Tax Credit (EITC)", "Federal Income Tax", "State Income Tax", "FICA Tax"),
                                  values_to="With GI") %>% 
    select('With GI') 
  
  
  #changes in value of program (csdata.changees)
  datafortable.changes<-csdata.changes %>% 
    filter(income==income) %>% 
    mutate(value.taxcredits=value.cdctc +value.ctc + value.eitc,
           value.HeadStart= value.HeadStart + value.earlyHeadStart, 
           "Social Security" = 0,
           #"Supplemental Security Income (SSI)" = NA_real_,
           #"Social Security Disability Insurance (SSDI)" = NA_real_,
           "Unemployment Insurance"=0,
           "Home Energy Assistance" = NA_real_) %>% 
    rename(
      "Health Insurance Marketplace Subsidy" = value.aca,
      "Medicaid for Adults" = value.medicaid.adult,
      "Medicaid for Children/CHIP" = value.medicaid.child,
      "Section 8 Housing Voucher" = value.section8,
      "Head Start/Early Head Start"= value.HeadStart,
      "State-Funded Pre-Kindergarten" = value.PreK,
      "Child Care Subsidy (CCDF)" = value.CCDF,
      "Supplemental Nutrition Assistance Program (SNAP)"= value.snap,
      "Women, Infants and Children Nutrition Program (WIC)" = value.wic,
      "Temporary Assistance for Needy Families (TANF)"=value.tanf,
      "Free or Reduced Price School Meals" = value.schoolmeals,
      "Supplemental Security Income (SSI)" = value.ssi,
      "Social Security Disability Insurance (SSDI)" = value.ssdi,
      "Child and Dependent Care Tax Credit (CDCTC)"= value.cdctc,
      "Child Tax Credit (CTC)" = value.ctc,
      "Earned Income Tax Credit (EITC)"= value.eitc,
      "Federal Income Tax" = tax.income.fed,
      "State Income Tax" = tax.income.state,
      "FICA Tax" = tax.FICA
    ) 
  
  datafortable.changes2<-pivot_longer(datafortable.changes, cols=c("Health Insurance Marketplace Subsidy":"Head Start/Early Head Start","State-Funded Pre-Kindergarten":"Free or Reduced Price School Meals",
                                                                   "Social Security", "Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)", "Unemployment Insurance",
                                                                   "Child and Dependent Care Tax Credit (CDCTC)","Child Tax Credit (CTC)","Earned Income Tax Credit (EITC)", "Federal Income Tax", "State Income Tax", "FICA Tax"),
                                     values_to="Difference") %>% 
    select('Difference') 
  
  #description of the changes
  
  datafortable.description <-csdata.changes %>% 
    filter(income==income) %>% 
    mutate(value.taxcredits=value.cdctc +value.ctc + value.eitc,
           value.HeadStart= value.HeadStart + value.earlyHeadStart,
           value.earlyHeadStart=NULL,
           "Social Security" = "This benefit should be unaffected by receipt of GI. This is a difficult benefit to get back; encourage recipient to double check with a representative from the agency.",
           "Supplemental Security Income (SSI)" = "GI is likely to affect your eligibility (via income-tests and asset-tests). The rules for this program are complex and there are significant work incentive programs that allow people on SSI to work and keep some or all of their public benefits. Check with the Work Incentives and Planning Assistance Program in your state to learn more. If you lose SSI you may also lose Medicaid for Adults or Medicaid for Kids.",
           "Social Security Disability Insurance (SSDI)" =  "This benefit should not be be affected by receipt of GI and there are considerable trial work periods. This is a difficult benefit to get back; encourage recipient to double check with a representative from the Work Incentives and Planning Assistance Program in your state.",
           "Unemployment Insurance"= "This benefit should be unaffected by receipt of GI.",
           "Home Energy Assistance" = "Home Energy Assistance Program calculations are not included and GI MAY affect your eligibility. Check with your local office." ,
           "Health Insurance Marketplace Subsidy" = "This benefit should be unaffected by receipt of GI.",
           "Medicaid for Adults" = "If you are receiving MAGI-based Medicaid (most common) this benefit should be unaffected by receipt of GI. If you are receiving non-MAGI medicaid (uncommon) your eligiblity could be affected due to income and asset-tests. See FAQ for details.",
           "Medicaid for Children/CHIP" = "If you are receiving MAGI-based Medicaid (most common) this benefit should be unaffected by receipt of GI. If you are receiving non-MAGI medicaid (uncommon) your eligiblity could be affected due to income and asset-tests. See FAQ for details.",
           "Section 8 Housing Voucher"=paste("Your personal rent responsibility would increase by an estimated $",round(abs((value.section8)), 2)," per month. Talk to your benefits counselor to learn more. "),
           "Head Start/Early Head Start"= "This benefit should be unaffected by receipt of GI.",
           "State-Funded Pre-Kindergarten" =  paste("The next time you re-certify eligibility, we estimate your childcare costs would increase by $",round(abs(value.PreK), 2), " per month."),
           "Child Care Subsidy (CCDF)" = paste("The next time you re-certify eligibility, we estimate your child care copay would increase by $",round(abs((value.CCDF*12/52)), 2)," per week."),
           "Supplemental Nutrition Assistance Program (SNAP)"= paste("The next time you re-certify eligibility, we estimate your cash benefit will decline by $",round(abs(value.snap), 2) , " per month."),
           "Women, Infants and Children Nutrition Program (WIC)" = paste("The next time you re-certify eligibility we estimate your cash benefit will decline by $",round(abs(value.wic), 2) , " per month."),
           "Temporary Assistance for Needy Families (TANF)" = "GI is likely to affect your eligibility (via income-tests and asset-tests). Check with your case manager. If you lose TANF you may also lose Medicaid and Childcare Assistance.",
           "Free or Reduced Price School Meals" = paste("The next time you re-certify eligibility, we estimate your school meals cost will increase by $",round(abs(value.schoolmeals*12/180), 2) , " per school day."),
           "Child and Dependent Care Tax Credit (CDCTC)"= "This benefit should be unaffected by receipt of GI unless your childcare costs increase. If your childcare costs increase your CDCTC may increase.",
           "Child Tax Credit (CTC)" = "This benefit should be unaffected by receipt of GI.",
           "Earned Income Tax Credit (EITC)" = "This benefit should be unaffected by receipt of GI.",
           "Federal Income Tax" =  "This benefit should be unaffected by receipt of GI.",
           "State Income Tax" =  "This benefit should be unaffected by receipt of GI.",
           "FICA Tax" = "This benefit should be unaffected by receipt of GI." ) 
  
  
 if (snap.waiver==TRUE){datafortable.description$`Supplemental Nutrition Assistance Program (SNAP)`<-"This benefit has been protected and thus should be unaffected by GI. Recipients of SNAP are eligible for School Meals & WIC even if otherwise they would be ineligible. Therefore, this waiver should also protect you from loss of WIC and School Meals."}
 if (tanf.waiver==TRUE){datafortable.description$`Temporary Assistance for Needy Families (TANF)`<-"This benefit has been protected and thus should be unaffected by GI. Recipients of TANF are eligible for School Meals, WIC, SNAP, and Head Start/Early Head Start even if otherwise they would be ineligible. Therefore, this waiver should also protect you from loss of School Meals, WIC, SNAP, and Head Start/Early Head Start."}
 if (section8.waiver==TRUE){datafortable.description$`Section 8 Housing Voucher`<-"This benefit has been protected and thus should be unaffected by GI."}
 if (ccdf.waiver==TRUE){datafortable.description$`Child Care Subsidy (CCDF)`<-"This benefit has been protected and thus should be unaffected by GI."}
 if (wic.waiver==TRUE){datafortable.description$`Women, Infants and Children Nutrition Program (WIC)`<-"This benefit has been protected and thus should be unaffected by GI."}
 if (state=="IL"){datafortable.description$`Supplemental Security Income (SSI)` <- "GI is likely to affect your eligibility (via income-tests and asset-tests). The rules for this program are complex and there are significant trial work periods. Check with the Work Incentives and Planning Assistance Program (1-866-968-7842) to learn more. If you lose SSI you may also lose Medicaid for Adults or Medicaid for Kids."} 
 if (state=="IL"){datafortable.description$`Social Security Disability Insurance (SSDI)`<-"This benefit should not be be affected by receipt of GI and there are considerable trial work periods. This is a difficult benefit to get back; encourage recipient to double check with a representative from the Work Incentives and Planning Assistance Program (1-866-968-7842) in your state."}
  
 if (section8.waiver.kindof==TRUE)
   {datafortable.description$`Section 8 Housing Voucher`<-paste0("Your eligiblity may be affected, depending on which public housing authority services your voucher. If you are affected, we estimate your personal rent responsibility would increase by $",
                                                                round(abs((csdata.changes$value.section8)), 2),
                                                                " per month. Talk to your benefits counselor to learn more.")}
  
  if (section8.waiver.kindof.Alameda==TRUE)
  {datafortable.description$`Section 8 Housing Voucher`<-paste0("All housing assistance programs provided through HUD/ the housing authority have been protected, with the exception of the following programs: the Veterans Affairs Supportive Housing (VASH), the Emergency Housing Vouchers (EHV), the Stability Voucher Program, the Moderate Rehabilitation (SRO) program, and the Shelter Plus Care program. If you voucher with any the listed programs, your eligiblity for said programs may be affected. Talk to your benefits counselor to learn more.")}
  
  datafortable.description2<-pivot_longer(datafortable.description, cols=c("Health Insurance Marketplace Subsidy":"Head Start/Early Head Start","State-Funded Pre-Kindergarten":"Free or Reduced Price School Meals",
                                                                           "Social Security", "Supplemental Security Income (SSI)", "Social Security Disability Insurance (SSDI)", "Unemployment Insurance",
                                                                           "Child and Dependent Care Tax Credit (CDCTC)","Child Tax Credit (CTC)","Earned Income Tax Credit (EITC)", "Federal Income Tax", "State Income Tax", "FICA Tax"),
                                          values_to="Description") %>% 
                            select('Description') 
  
  
     
  ##################################################
  #combine the three tables together 
  ####################################################
  datafortable<-cbind(datafortable.initial2,datafortable.new2,datafortable.changes2,datafortable.description2)
  
  
  ##########CHICAGO SPECIFIC BENEFITS
  if(organization=="Cook County Promise"){
    
    name<- c("IL Home Weatherization Program (IHWAP)" ,
             "Low Income Home Water Assistance Program (LIHWAP)",
             "Moms and Babies",
             "Former Foster Care",
             "FamilyCare", 
             "Downpayment Plus programs from Federal Home Loan Bank of Chicago",
             "Aid for the Aging, Blind, and Disabled (AABD)",
             "Medicare Savings Program",
             "Health Benefits for Workers with Disabilities")
    
    addbenefits<-as.data.frame(name)
    
    addbenefits<-addbenefits %>% 
      mutate("Without GI"=(
        case_when(name=="IL Home Weatherization Program (IHWAP)" ~ NA_real_,
        name=="Low Income Home Water Assistance Program (LIHWAP)" ~ NA_real_,
        name=="Moms and Babies" ~ NA_real_,
        name=="Former Foster Care" ~ NA_real_,
        name=="FamilyCare" ~ NA_real_, 
        name=="Downpayment Plus programs from Federal Home Loan Bank of Chicago" ~ NA_real_,
        name=="Aid for the Aging, Blind, and Disabled (AABD)" ~ NA_real_,
        name=="Medicare Savings Program" ~ NA_real_,
        name=="Health Benefits for Workers with Disabilities" ~ NA_real_,
        TRUE~NA_real_
      ))) %>% 
    
      mutate("With GI"=(
        case_when(name=="IL Home Weatherization Program (IHWAP)" ~ NA_real_,
                  name=="Low Income Home Water Assistance Program (LIHWAP)" ~ NA_real_,
                  name=="Moms and Babies" ~ NA_real_,
                  name=="Former Foster Care" ~ NA_real_,
                  name=="FamilyCare" ~ NA_real_, 
                  name=="Downpayment Plus programs from Federal Home Loan Bank of Chicago" ~ NA_real_,
                  name=="Aid for the Aging, Blind, and Disabled (AABD)" ~ NA_real_,
                  name=="Medicare Savings Program" ~ NA_real_,
                  name=="Health Benefits for Workers with Disabilities" ~ NA_real_,
                  TRUE~NA_real_
        ))) %>%
      
      #need to double check with kazymn.. she is investigating 
      mutate("Difference"=(
        case_when(name=="IL Home Weatherization Program (IHWAP)" ~ 0,
                  name=="Low Income Home Water Assistance Program (LIHWAP)" ~ 0,
                  name=="Moms and Babies" ~ 0,
                  name=="Former Foster Care" ~ 0,
                  name=="FamilyCare" ~ 0, 
                  name=="Downpayment Plus programs from Federal Home Loan Bank of Chicago" ~0,
                  name=="Aid for the Aging, Blind, and Disabled (AABD)" ~ NA_real_,
                  name=="Medicare Savings Program" ~ NA_real_,
                  name=="Health Benefits for Workers with Disabilities" ~ NA_real_,
                  TRUE~NA_real_
      ))) %>% 
   
    
      mutate("Description"=(
        case_when(
          name=="IL Home Weatherization Program (IHWAP)" ~"This benefit has been protected and thus should be unaffected by GI.", 
          name=="Low Income Home Water Assistance Program (LIHWAP)" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="Moms and Babies" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="Former Foster Care" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="FamilyCare" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="Downpayment Plus programs from Federal Home Loan Bank of Chicago" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="Aid for the Aging, Blind, and Disabled (AABD)" ~ "A guaranteed income disregard waiver has been secured for the CASH benefit and thus the CASH benefit should be unaffected by receipt of GI. However, the MEDICAL benefit may be affected. Call 1-800-842-1461 to learn more.", 
          name=="Medicare Savings Program" ~ "Calculations not included. Your eligiblity may be affected by the receipt of GI. Call 1-800-843-6154 to learn more.", 
          name=="Health Benefits for Workers with Disabilities" ~ "Calculations not included. Your eligiblity may be affected by the receipt of GI. Call 1-800-226-0768 to learn more.",  
          TRUE~""
        ))) 
    
    datafortable<-rbind(datafortable,addbenefits)
    }
  
  
  ##########MOCO BOOS SPECIFIC BENEFITS
  if(organization=="MoCo Boost"){
    
    name<- c("Senior Dental, Maternity Partnership, or Care4Kids" ,
             "Working Parents Childcare Assistance",
             "Moms and Babies",
             "Montgomery County Rental Assistance Program",
             "FamilyCare")
    
    addbenefits<-as.data.frame(name)
    
    addbenefits<-addbenefits %>% 
      mutate("Without GI"=(
        case_when(name=="Senior Dental, Maternity Partnership, or Care4Kids" ~ NA_real_,
                  name=="Working Parents Childcare Assistance" ~ NA_real_,
                  name=="Montgomery County Rental Assistance Program" ~ NA_real_,
                  TRUE~NA_real_
        ))) %>% 
      
      mutate("With GI"=(
        case_when(name=="Senior Dental, Maternity Partnership, or Care4Kids" ~ NA_real_,
                  name=="Working Parents Childcare Assistance" ~ NA_real_,
                  name=="Montgomery County Rental Assistance Program" ~ NA_real_,
                  TRUE~NA_real_
        ))) %>%
      
      #need to double check with kazymn.. she is investigating 
      mutate("Difference"=(
        case_when(name=="Senior Dental, Maternity Partnership, or Care4Kids" ~ 0,
                  name=="Working Parents Childcare Assistance" ~ 0,
                  name=="Montgomery County Rental Assistance Program" ~ 0,
                  TRUE~NA_real_
        ))) %>% 
      
      
      mutate("Description"=(
        case_when(
          name=="Senior Dental, Maternity Partnership, or Care4Kids" ~"This benefit has been protected and thus should be unaffected by GI.", 
          name=="Working Parents Childcare Assistance" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          name=="Montgomery County Rental Assistance Program" ~ "This benefit has been protected and thus should be unaffected by GI.", 
          TRUE~""
        ))) 
    
    datafortable<-rbind(datafortable,addbenefits)
  }
  
  
  
  ##########Phildelphia specific 
  if(organization=="City of Philadelphia"){
    
    name<- c("LIHEAP")
    
    addbenefits<-as.data.frame(name)
    
    addbenefits<-addbenefits %>% 
      mutate("Without GI"=(
        case_when(name=="LIHEAP" ~ NA_real_,
                  TRUE~NA_real_
        ))) %>% 
      
      mutate("With GI"=(
        case_when(name=="LIHEAP" ~ NA_real_,
                  TRUE~NA_real_
        ))) %>%
      
      #need to double check with kazymn.. she is investigating 
      mutate("Difference"=(
        case_when(name=="LIHEAP" ~ 0,
                  TRUE~NA_real_
        ))) %>% 
      
      
      mutate("Description"=(
        case_when(
          name=="LIHEAP" ~"This benefit has been protected and thus should be unaffected by GI.", 
          TRUE~""
        ))) 
    
    datafortable<-rbind(datafortable,addbenefits)
  }
  
  #show only the benefits that they chose on the left hand menu & rename programs with state specific cnames
  if(benefit1 == "Select a custom list"){
      datafortable<-datafortable %>% 
      filter(name %in% c(benefit2,"Federal Income Tax","State Income Tax","FICA Tax"))
  }
  
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  # CHANGE NAMES BACK TO STATE NAMES UGH
  
  #change the names of the programs for table display output
  
  if(state=="AK"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "Denali Kidcare (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "DenaliCare (Medicaid for Adults)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Alaska Temporary Assistance Program (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="AL"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "All Kids (Medicaid for Children/CHIP)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~ "Food Assistance Program (SNAP)", 
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Family Assistance Program (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="AR"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "AR Kids First B (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "Health Care (Medicaid for Adults)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Transitional Employment Assistance (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="AZ"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "KidsCare (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "Arizona Health Care Cost Containment System (Medicaid for Adults)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "EMPOWER (TANF)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~ "Nutrition Assistance (SNAP)", 
                            TRUE~name)) 
  }
  
  
  if(state=="CA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~"Medi-Cal for Children (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~"CalWorks (TANF)",
                            name=="Medicaid for Adults" ~"Medi-Cal for Adults (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~"CalFresh (SNAP)",
                            TRUE~name)) 
  }
  
  
  if(state=="CO"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "Colorado CHP+ (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "Health First Colorado (Medicaid for Adults)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Colorado Works (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="CT"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "HUSKY-B (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "HuskyHealth (Medicaid for Adults)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "JOBS FIRST (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="DC"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "DC Healthy Families (Medicaid for Children/CHIP)",
                            name== "Medicaid for Adults" ~ "Healthy Families (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="DE"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~"Delaware Healthy Children's Program (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~"A Better Chance (TANF)",
                            name=="Medicaid for Adults" ~"Diamond State Health Plan (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~"Food Supplement Program (SNAP)",
                            TRUE~name)) 
  }
  
  if(state=="FL"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~ "Florida KidCare (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Welfare Transition Program (TANF)",
                            name=="Medicaid for Adults" ~ "Statewide Medicaid Managed Care Program (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~ "Food Assistance Program (SNAP)",
                            TRUE~name)) 
  }
  
  if(state=="GA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Child Care Subsidy (CCDF)" ~"Childcare and Parent Services (CAPS)",
                            name=="Medicaid for Children/CHIP" ~ "PeachCare for Kids (Medicaid for Children/CHIP)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~ "Food Stamp Program (SNAP)",
                            TRUE~name)) 
  }
  
  if(state=="HI"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Adults" ~  "MedQuest (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="IA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Healthy and Well Kids in Iowa (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "Family Investment Program (TANF)",
                            name=="Medicaid for Adults" ~ "IA Health Link (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~  "Food Assistance Program (SNAP)",
                            TRUE~name)) 
  }
  
  if(state=="ID"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "Temporary Assistance for Families in Idaho (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="IL"){ #need to figure out how to add all the county and state specific benefit programs
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Child Care Subsidy (CCDF)" ~ "Child Care Assistance Program (CCAP/CCDF)",
                            name=="Medicaid for Children/CHIP" ~  "All Kids (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults" ~  "Medical Assistance Program (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="IN"){ #need to figure out how to add all the county and state specific benefit programs
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Hoosier Healthwise (Medicaid for Children/CHIP)",
                            TRUE~name)) 
  }
  
  
  if(state=="KS"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "KanCare (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "Kansas Works (TANF)",
                            name=="Medicaid for Adults" ~  "KanCare Medical Assistance Program (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~   "Food Assistance Program (SNAP)",
                            TRUE~name)) 
  }
  
  
  if(state=="KY"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "KCHIP - Kentucky Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "K-TAP - Kentucky Transitional Assistance Program (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="LA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Louisiana Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "FITAP - Family Independence Temporary Assistance Program (TANF)",
                            name=="Medicaid for Adults" ~  "Healthy Louisiana (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="MA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name== "Medicaid for Children/CHIP" ~ "MassHealth for Children (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "TAFDC (TANF)",
                            name== "Medicaid for Adults" ~  "Adult MassHealth (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="MD"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Child Care Subsidy (CCDF)" ~'Child Care Scholarship (CCS)',
                            name=="Medicaid for Children/CHIP" ~ "Maryland Children's Health Program (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~ "Medical Assistance (Medicaid for Adults)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~ "Family Investment Program (TANF)",
                            TRUE~name))
  }
  
  if(state=="ME"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "MaineCare Cub Care (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~ "MaineCare (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~  "Food Supplement Program (SNAP)",
                            TRUE~name))
  } 
  
  if(state=="MI"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "MIChild (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~  "Medical Assistance (Medicaid for Adults)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~  "FIP - Family Independence Program (TANF)",
                            TRUE~name))
  }
  
  if(state=="MN"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "MinnesotaCare (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~   "Medical Assistance (Medicaid for Adults)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~   "MFIP (TANF)",
                            TRUE~name))
  }
  
  
  if(state=="MO"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "MO HealthNet for Kids (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~  "MO HealthNet (Medicaid for Adults)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~  "Beyond Welfare (TANF)",
                            TRUE~name))
  }
  
  
  
  if(state=="MS"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Mississippi Health Benefits (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~   "Mississippi Coordinated Access Network (Medicaid for Adults)",
                            TRUE~name))
  }
  
  
  if(state=="MT"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Healthy Montana Kids (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~   "FAIM - Families Achieving Independence in Montana (TANF)",
                            TRUE~name))
  }
  
  if(state=="NC"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~ "North Carolina Health Choice (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~ "Work First (TANF)",
                            name=="Medicaid for Adults" ~ "Division of Health Benefits (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~ "Food & Nutrition Services (SNAP)",
                            TRUE~name)) 
  }
  
  
  if(state=="ND"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~ "Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "TEEM - Training, Employment, Education Management (TANF)",
                            name=="Medicaid for Adults" ~  "North Dakota Medicaid Expansion Program (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  
  if(state=="NE"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name== "Temporary Assistance for Needy Families (TANF)" ~   "Employment First (TANF)",
                            name=="Medicaid for Adults" ~  "Nebraska Medical Assistance Program (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="NJ"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "NJ FamilyCare for Children (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "Work First New Jersey (TANF)",
                            name=="Medicaid for Adults" ~  "NJ FamilyCare (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="NM"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Centennial Care for Children (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "NM Works (TANF)",
                            name=="Medicaid for Adults" ~ "Centennial Care (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="NV"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Nevada Check Up (Medicaid for Children/CHIP)",
                            TRUE~name)) 
  }
  
  
  
  if(state=="NY"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Child Health Plus (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "Family Assistance Program (TANF)",
                            name=="Medicaid for Adults" ~  "Medicaid Managed Care (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  
  #---------------
  # IDK WHY THIS CCDF NAME IS CHANGED BUT IT'S WHAT ELLIE PUT
  
  if(organization=="NYS Direct Cash Transfer"){
    datafortable <- datafortable %>% 
      mutate(name=case_when(name=="Child Care Subsidy (CCDF)" ~"Child Care Assistance Program (CCAP/CCDF)",
                            TRUE~name)) 
  }
  #---------------
  
  
  
  if(state=="OH"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "Healthy Start (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~   "Ohio Works First (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="OK"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "SoonerCare for Children (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults" ~  "SoonerCare (Medicaid for Adults)",
                            TRUE~name)) 
  }
   
  
  if(state=="OR"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Oregon Health Plan for Children (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "JOBS - Job Opportunities and Basic Skills Program (TANF)",
                            name=="Medicaid for Adults" ~  "Oregon Health Plan (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="PA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Pennsylvania’s Children’s Health Insurance Program (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults" ~  "Medical Assistance (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="RI"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "RIte Care (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "FIP - Family Independence Program (TANF)",
                            name=="Medicaid for Adults" ~  "RI Medical Assistance Program (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  
  if(state=="SC"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "South Carolina Healthy Connections (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "Family Independence Program (TANF)",
                            name=="Medicaid for Adults" ~ "Healthy Connections (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="SD"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "South Dakota Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            TRUE~name)) 
  }
  
  if(state=="TN"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~   "CoverKids (Medicaid for Children/CHIP)",
                            name== "Temporary Assistance for Needy Families (TANF)" ~  "Families First (TANF)",
                            name=="Medicaid for Adults" ~  "TennCare (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  
  if(state=="TX"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name== "Temporary Assistance for Needy Families (TANF)" ~  "Texas Works (TANF)",
                            name=="Medicaid for Adults" ~  "STAR+PLUS (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="UT"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name== "Temporary Assistance for Needy Families (TANF)" ~  "Family Employment Program (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="VA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Family Access to Medical Insurance Security (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~  "VIEW - Virginia Initiative for Employment, Not Welfare (TANF)",
                            name=="Medicaid for Adults" ~  "Cardinal Care (Medicaid for Adults)",
                            TRUE~name)) 
  }
  
  if(state=="VT"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Dr. Dynasaur (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~   "ANFC - Aid to Needy Families with Children (TANF)",
                            name=="Medicaid for Adults" ~  "Green Mountain Care (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~  "3SquaresVT (SNAP)",
                            TRUE~name)) 
  }
  

  
  if(state=="WA"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name== "Temporary Assistance for Needy Families (TANF)" ~ "WorkFirst (TANF)",
                            name=="Medicaid for Children/CHIP" ~  "Apple Health for Kids (Medicaid for Children/CHIP)",
                            name=="Medicaid for Adults"  ~  "Apple Health for Adults (Medicaid for Adults)",
                            name=="Child Care Subsidy (CCDF)" ~  'Working Connections Child Care Program (CCDF)',
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~   "Basic Food Program (SNAP)",
                           TRUE ~name)) 
  }
  
  if(state=="WI"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "BadgerCare Plus (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~  "Wisconsin Works (TANF)",
                            name=="Medicaid for Adults" ~  "BadgerCare (Medicaid for Adults)",
                            name== "Supplemental Nutrition Assistance Program (SNAP)" ~  "FoodShare (SNAP)",
                            TRUE~name)) 
  }

  if(state=="WV"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~ "West Virginia Children's Health Insurance Program (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~  "West Virginia Works (TANF)",
                            TRUE~name)) 
  }
  
  if(state=="WY"){
    datafortable <- datafortable %>%
      mutate(name=case_when(name=="Medicaid for Children/CHIP" ~  "Kid Care CHIP (Medicaid for Children/CHIP)",
                            name=="Temporary Assistance for Needy Families (TANF)" ~   "POWER - Personal Opportunities With Employment Responsibility (TANF)",
                            name=="Medicaid for Adults" ~  "Equality Care (Medicaid for Adults)",
                            TRUE~name)) 
  }


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  


  
   datafortable <- datafortable %>% rename("   "=name)
  

  ###################################
  #####First overall view table######
  ###################################
  
  datafortable.initialNR<-csdata.initial %>% 
    filter(income==income) %>% 
    # mutate(stipend=tax_stipend + nontax_stipend) %>%
    rename("+ Take-Home Pay*" = AfterTaxIncome,
           "+ Public Assistance" = pub.assistance,
           #       "+ Stipends" = stipend,
           #     "+ Cash Bonus" = cash.bonus,
           "- Living Expenses" = living.expenses,
           "= Monthly Budget" = NetResources) 
  
  datafortable.initial2NR<-pivot_longer(datafortable.initialNR, cols=c("+ Take-Home Pay*",  "+ Public Assistance", "- Living Expenses","= Monthly Budget"), values_to="Without GI") %>% 
    select(name, 'Without GI') %>% 
    rename(" "=name)
  
  datafortable.newNR <-csdata.new %>% 
    filter(income==income) %>% 
    #   mutate(stipend = tax_stipend + nontax_stipend) %>%
    rename("+ Take-Home Pay*" = AfterTaxIncome,
           "+ Public Assistance" = pub.assistance,
           #       "+ Stipends" = stipend,
           # "+ Cash Bonus" = cash.bonus,
           "- Living Expenses" = living.expenses,
           "= Monthly Budget" = NetResources) 
  
  datafortable.new2NR <-pivot_longer(datafortable.newNR, cols=c("+ Take-Home Pay*",  "+ Public Assistance","- Living Expenses","= Monthly Budget"), values_to="With GI") %>% 
    select('With GI') 
  
  
  #changes in value of program (csdata.changes)
  datafortable.changesNR <-csdata.changes %>% 
    filter(income==income) %>% 
    # mutate(stipend = tax_stipend + nontax_stipend) %>%
    rename("+ Take-Home Pay*" = AfterTaxIncome,
           "+ Public Assistance" = pub.assistance,
           #      "+ Stipends" = stipend,
           #  "+ Cash Bonus" = cash.bonus,
           "- Living Expenses" = living.expenses,
           "= Monthly Budget" = NetResources) 
  
  datafortable.changes2NR <-pivot_longer(datafortable.changesNR, cols=c("+ Take-Home Pay*",  "+ Public Assistance","- Living Expenses","= Monthly Budget"), values_to="Difference") %>% 
    select('Difference') 
  
  
  #combine the three tables together 
  datafortableNR<-cbind(datafortable.initial2NR,datafortable.new2NR,datafortable.changes2NR)
  
  
  
}else{
  csdata.current <- NA
  csdata.change <- NA
}



  ####################
  #DISPLAY THE TABLE 
  ###################
  output$table.programloss<- renderTable({
    
    validate(
      need(numadults!="empty" & 
             state!="empty" &
             county_main!="" &
             benefit1!="empty" &
             organization!="empty" &
             error_child_age==0 &
             error_age_adult!=1 &
             missing_wage==0 &
               !is.na(income_current) &
             !is.na(gift_income_change) &
             
             fam_disab!="empty" &
             housing_error==0 & 
             util_error==0 &
             error_childcareexp==0 
           ,"                              ")
      
      
      
    )
    
    validate(
      need(disab_error ==0, "                ")
      
      
      
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "               "
      )
    )
    
    validate(
      need(ssdi_no_adults ==0, "                ")
    )
    
    validate(
      need(ssdi_error==0, "                ")
    )
    
    datafortable}, options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '400px', targets = "_all"))),bordered=TRUE, striped=TRUE, align = 'c',digits=0, na= "Calculations not included.") 
  
  #########################################
  # OUT OF POCKET EXPENSES
  #########################################
  
 # N/A
  
  ##############################
  # NET RESOURCES / TOTAL BUDGET
  #############################
 
  #show only the benefits that they chose on the left hand menu
  
  ####################
  #DISPLAY THE TABLE 
  ###################
  output$table.NetResources <- renderTable({
    
    validate(
      need(numadults!="empty" & 
             state!="empty" &
             county_main!="" &
             benefit1!="empty" &
             organization!="empty" &
             error_child_age==0 & 
             error_age_adult!=1 &
             missing_wage==0 &
             !is.na(income_current) &
             !is.na(gift_income_change) &
             fam_disab!="empty" &
                housing_error==0 &
             util_error == 0 &
             error_childcareexp==0 
           
           ,"Input all of the information to the left.")
    ) #end of validate
    
    validate(
      need(disab_error ==0, "If you have selected that at least one member of your family is disabled, please make sure to correctly select that family member if disabled.")
    )
    
    
    validate(
      need(ssdi_no_adults ==0, "You have selected SSDI, but none of the adults have a disability.")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "If you are receiving SSI or SSDI you must select 'Yes' to the Question 'Does anyone have a disability?'"
      )
    )
    
    validate(
      need(ssdi_error==0, "SSDI values need to be positive.")
    )
    
    datafortableNR} #end of render table
    
    ,options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '400px', targets = "_all"))), bordered=TRUE, striped=TRUE, align = 'c',digits=0, na= "Calculations not included. See warnings to the right.") 


  output$effect_budget<- renderText({ 
      validate(
      need(numadults!="empty" & 
             state!="empty" &
             county_main!="" &
             benefit1!="empty" &
             organization!="empty" &
             error_child_age==0 &
             error_age_adult!=1 &
             missing_wage==0 &
             !is.na(income_current) &
             !is.na(gift_income_change) &
             fam_disab!="empty" &
             housing_error==0 & 
             util_error==0 &
             error_childcareexp==0 
           ,"                              ")
      
        )#end of validate
    
    validate(
      need(disab_error ==0, "      ")
    )
    
    validate(
      need(ssdi_no_adults ==0, "      ")
    )
    
    validate(
      need(ssdi_error==0, "      ")
    )
    
    validate(
      need(
        ssi_ssdi_error==0, "               "
      )
    )
    
    
    if(csdata.changes$NetResources < 0 & !is.na(csdata.changes$NetResources)){
    paste("Your family's total budget will ","<font color=\"#661100\"><b>", "decrease by $", round(datafortable.changesNR$`= Monthly Budget`,0), " per month", " ($",round(datafortable.changesNR$`= Monthly Budget`*12,0)," per year)","</b></font>")
     }else if(csdata.changes$NetResources > 0 & !is.na(csdata.changes$NetResources)){
    paste("Your family's total budget will ","<font color=\"#117733\"><b>", "increase by $", round(datafortable.changesNR$`= Monthly Budget`,0),  " per month"," ($",round(datafortable.changesNR$`= Monthly Budget`*12,0)," per year)","</b></font>")
     }else{
    paste("Your family's total budget will ","<font color=\"#000000\"><b>", "remain unchanged.", "</b></font>") 
     }
    
  }) #end of render text
  
  
  
  }
    }
  )
}) # end of shiny server


