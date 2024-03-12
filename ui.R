#######################################################
#######################################################
# User Interface - Requires Certain Customization
#######################################################
#######################################################
   
# Define UI ----
ui <- fluidPage(title = "CLIFF",#theme = "www/custom.css", # change theme of the App here
                theme = "style.css", # Load style
                tags$head(includeHTML(("www/google-analytics.html"))),
                tags$html(lang="en"),
              
                tags$head(tags$style(HTML(".shiny-output-error-validation{color: red; font-size:24px; }"))),
                tags$head(tags$style(".shiny-notification{color: red; font-size:32px; position: fixed; top: 87% ;left: 2%  }")),
               div(class = "header",includeHTML("www/include_header.html")),      
  sidebarLayout(
     
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #includeHTML("include_logo.html"), # Home page - written in HTML
      
      
      selectInput("organization", label = "Organization", 
                  list("select" = "empty",
                       "Custom Analysis",
                       "ARISE Alexandria",
                       "Atlanta Abundant Birth Project",
                       "BREATHE (Los Angeles County)",
                       "CASH Campaign of Maryland",
                       "Children's Defense Fund" ,
                       "City of Alameda",
                       "City of Philadelphia",
                       "Cook County Promise",
                       "Elevate MV",
                       "Empower House",
                       "Hamilton Families",
                       "In Her Hands",
                       "Louisville, KY",
                       "MoCo Boost",
                       "NY Creative Rebuild",
                       "NYS Direct Cash Transfer",
                       "Seattle Workforce Development",
                       "The Atlanta Abundant Birth Project",
                       "The BRIDGE Project",
                       "United Way California Capital Region",
                       "Urban League Greater Atlanta"#,
                     #  "Other"
                       ), selectize=FALSE, selected = "Custom Analysis"),
      

      h5(strong("Note: "), "if you do not see your organization in the above list, please select 'Custom Analysis'."),
      
      
      # Select state based on organization 
      br(),
      conditionalPanel(condition = "input.organization != 'empty' & input.organization!=''",
                       selectizeInput("state", 
                                      label = "State", choices = NULL, options = list(placeholder = 'select'
                                                                                       ,onInitialize = I('function() { this.setValue(""); }')
                                      ))
      ),
      
     
      # Select County, Town, Borough, or Parish, depending on state
      
      # List of counties selected from external list in folder, "locations_list"
      conditionalPanel(condition = "input.state != 'empty'",
                       selectizeInput("county_main", 
                                      label = "County", choices = NULL, options = list(placeholder = 'select'
                                                                                   ,onInitialize = I('function() { this.setValue(""); }')
                                      ))
      ),
      
      
      conditionalPanel(condition =  "input.organization == 'Custom Analysis' & (input.state == 'AL' | input.state == 'AK' | input.state == 'AR' | input.state == 'AZ'|
                      input.state == 'CA' | input.state == 'CO' | input.state == 'CT' | input.state == 'DC'| 
                       input.state == 'DE' | input.state == 'FL' | input.state == 'GA' | input.state == 'HI' | input.state == 'IA'|
                       input.state == 'ID' | input.state == 'IL' | input.state == 'IN' | input.state == 'KS' | input.state == 'KY'|
                       input.state == 'LA' | input.state == 'MA' | input.state == 'MD' | input.state == 'ME' | input.state == 'MI'|
                       input.state == 'MN' | input.state == 'MO' | input.state == 'MS' | input.state == 'MT'|
                       input.state == 'NC' | input.state == 'ND' | input.state == 'NE' | input.state == 'NH'|
                        input.state == 'NJ' | input.state == 'NM' | input.state == 'NV' | input.state == 'NY'|
                       input.state == 'OH' | input.state == 'OK' | input.state == 'OR' | input.state == 'PA'|
                       input.state == 'RI' | input.state == 'SC' | input.state == 'SD' | input.state == 'TN'|
                        input.state == 'TX' | input.state == 'UT' | input.state == 'VA' | input.state == 'VT'|
                       input.state == 'WA' | input.state == 'WI' | input.state == 'WV' | input.state == 'WY' 
                       )" ,
                       
      h5(strong("For Organization: For this GI Program, are you currently receiving a waiver for:"))
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & (input.state == 'AL' | input.state == 'AK' | input.state == 'AR' | input.state == 'AZ'|
                      input.state == 'CA' | input.state == 'CO' | input.state == 'CT' | input.state == 'DC'| 
                       input.state == 'DE' | input.state == 'FL' | input.state == 'HI' | input.state == 'IA'|
                       input.state == 'ID' | input.state == 'IN' | input.state == 'KS' | input.state == 'KY'|
                       input.state == 'LA' | input.state == 'MA' | input.state == 'ME' | input.state == 'MI'|
                       input.state == 'MN' | input.state == 'MO' | input.state == 'MS' | input.state == 'MT'|
                       input.state == 'NC' | input.state == 'ND' | input.state == 'NE' | input.state == 'NH'|
                        input.state == 'NJ' | input.state == 'NM' | input.state == 'NV' | input.state == 'NY'|
                       input.state == 'OH' | input.state == 'OK' | input.state == 'OR' | input.state == 'PA'|
                       input.state == 'RI' | input.state == 'SC' | input.state == 'SD' | input.state == 'TN'|
                        input.state == 'TX' | input.state == 'UT' | input.state == 'VA' | input.state == 'VT'|
                       input.state == 'WI' | input.state == 'WV' | input.state == 'WY' 
                       )",
                       fluidRow(
                         column(4, offset = 1,
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "ccdf_waiver", br(), "Child Care Subsidy (CCDF)?")
                                                              )
                                    )
                                    )
                         ),
                         column(4,
                                radioButtons("ccdf_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      # GA, IL, MD, WA
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'IL' ",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                              #  tags$form(
                               #   class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "ccap_ccdf_waiver", br(), "Child Care Assistance Program (CCAP/CCDF)?")
                                                              )
                                    ))
                                  #) 
                         ),
                         column(4,
                                radioButtons("ccap_ccdf_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'GA' ",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                               # tags$form(
                               #   class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "caps_waiver", br(), "Childcare and Parent Services (CAPS)?")
                                                              )
                                    ))
                               #) 
                         ),
                         column(4,
                                radioButtons("caps_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MD'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                              #  tags$form(
                              #    class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "ccs_waiver", br(), "Child Care Scholarship (CCS)?")
                                                              )
                                    ))
                              #) 
                         ),
                         column(4,
                                radioButtons("ccs_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'WA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #  tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "wccc_waiver", br(), "Working Connections Child Care Program (CCDF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("wccc_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
       
      conditionalPanel(condition =  "input.organization == 'Custom Analysis' & (input.state == 'AL' | input.state == 'AK' | input.state == 'AR' | input.state == 'AZ'|
                      input.state == 'CA' | input.state == 'CO' | input.state == 'CT' | input.state == 'DC'| 
                       input.state == 'DE' | input.state == 'FL' | input.state == 'GA' | input.state == 'HI' | input.state == 'IA'|
                       input.state == 'ID' | input.state == 'IL' | input.state == 'IN' | input.state == 'KS' | input.state == 'KY'|
                       input.state == 'LA' | input.state == 'MA' | input.state == 'MD' | input.state == 'ME' | input.state == 'MI'|
                       input.state == 'MN' | input.state == 'MO' | input.state == 'MS' | input.state == 'MT'|
                       input.state == 'NC' | input.state == 'ND' | input.state == 'NE' | input.state == 'NH'|
                        input.state == 'NJ' | input.state == 'NM' | input.state == 'NV' | input.state == 'NY'|
                       input.state == 'OH' | input.state == 'OK' | input.state == 'OR' | input.state == 'PA'|
                       input.state == 'RI' | input.state == 'SC' | input.state == 'SD' | input.state == 'TN'|
                        input.state == 'TX' | input.state == 'UT' | input.state == 'VA' | input.state == 'VT'|
                       input.state == 'WA' | input.state == 'WI' | input.state == 'WV' | input.state == 'WY' 
                       )" ,
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                             #   tags$form(
                             #     class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "section8_waiver", br(), "Section 8 Housing Voucher?")
                                                              )
                                    ))
                             #) 
                         ),
                         column(4,
                                radioButtons("section8_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
                       
      ),
      
      #------------------ SNAP WAIVERS
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & ( input.state == 'AK' | input.state == 'AR' 
                       | input.state == 'CO' | input.state == 'CT' | input.state == 'DC'| 
                    input.state == 'HI' |
                       input.state == 'ID' | input.state == 'IL' | input.state == 'IN' |  input.state == 'KY'|
                       input.state == 'LA' | input.state == 'MA' | input.state == 'MD' |  input.state == 'MI'|
                       input.state == 'MN' | input.state == 'MO' | input.state == 'MS' | input.state == 'MT'|
                     input.state == 'ND' | input.state == 'NE' | input.state == 'NH'|
                        input.state == 'NJ' | input.state == 'NM' | input.state == 'NV' | input.state == 'NY'|
                       input.state == 'OH' | input.state == 'OK' | input.state == 'OR' | input.state == 'PA'|
                       input.state == 'RI' | input.state == 'SC' | input.state == 'SD' | input.state == 'TN'|
                        input.state == 'TX' | input.state == 'UT' | input.state == 'VA' |
                      input.state == 'WV' | input.state == 'WY' 
                       )"                
                       ,
                       
                       fluidRow(
                         column(4, offset = 1,
                           #     tags$form(
                            #      class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "snap_waiver", br(), "Supplemental Nutrition Assistance Program (SNAP)?")
                                                              )
                                    ))
                           #) 
                         ),
                         column(4,
                                radioButtons("snap_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'CA'",
                       fluidRow(
                         column(4, offset = 1,
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "calfresh_waiver", br(), "CalFresh?")
                                                              )
                                    ))
                         ),
                         column(4,
                                radioButtons("calfresh_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      

      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & (input.state == 'AL' | input.state == 'FL' | input.state == 'IA' | input.state == 'KS')",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "fap_waiver", br(), "Food Assistance Program (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("fap_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),

      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & (input.state == 'DE' | input.state == 'ME')",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "fsp_waiver", br(), "Food Supplement Program (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("fsp_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'AZ'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "nutrition_assistance_waiver", br(), "Nutrition Assistance (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("nutrition_assistance_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'GA'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "food_stamp_waiver", br(), "Food Stamp Program (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("food_stamp_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'NC'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "fns_waiver", br(), "Food & Nutrition Services (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("fns_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'VT'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "vt_waiver", br(), "3SquaresVT (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("vt_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'WA'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "bfp_waiver", br(), "Basic Food Program (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("bfp_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis'  & input.state == 'WI'",
                       fluidRow(
                         column(4, offset = 1,
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "foodshare_waiver", br(), "FoodShare (SNAP)?")
                                                            )
                                  ))
                         ),
                         column(4,
                                radioButtons("foodshare_waiver", label = NULL,
                                             list(
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                       )
      ),
      
      
      
      #---------------------------------------
      
      # TANF WAIVERS
      
      #-----------------------------------------
      
      
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & (input.state == 'DC' | input.state == 'GA' | input.state == 'HI' | input.state == 'IL' | input.state == 'IN'
                       | input.state == 'ME' | input.state == 'MS' | input.state == 'NV' | input.state == 'OK' | input.state == 'PA' | input.state == 'SD')",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                               # tags$form(
                               #   class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "tanf_waiver", br(), "Temporary Assistance for Needy Families (TANF)?")
                                                              )
                                    ))
                               #) 
                         ),
                         column(4,
                                radioButtons("tanf_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'AK'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "AK_waiver", br(), "Alaska Temporary Assistance Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("AK_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'AL'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "AL_waiver", br(), "Family Assistance Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("AL_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'AR'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "AR_waiver", br(), "Transitional Employment Assistance (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("AR_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'AZ'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "AZ_waiver", br(), "EMPOWER (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("AZ_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'CA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "CA_waiver", br(), "CalWorks (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("CA_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'CO'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "CO_waiver", br(), "Colorado Works (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("CO_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'CT'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "CT_waiver", br(), "JOBS FIRST (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("CT_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'DE'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "DE_waiver", br(), "A Better Chance (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("DE_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'FL'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "FL_waiver", br(), "Welfare Transition Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("FL_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'IA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "IA_waiver", br(), "Family Investment Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("IA_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'ID'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "ID_waiver", br(), "Temporary Assistance for Families in Idaho (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("ID_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'KS'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "KS_waiver", br(), "Kansas Works (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("KS_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'KY'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "KY_waiver", br(), "Kentucky Transitional Assistance Program (KTAP)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("KY_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'LA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "LA_waiver", br(), "Family Independence Temporary Assistance Program (FITAP)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("LA_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "MA_waiver", br(), "Transitional Aid to Families with Dependent Children (TAFDC)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("MA_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),  
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MD'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "MD_waiver", br(), "Family Investment Program (FIP)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("MD_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),  
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MN'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "MN_waiver", br(), "Minnesota Family Investment Program (MFIP)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("MN_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),   
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MO'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "MO_waiver", br(), "Beyond Welfare (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("MO_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),   
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'MT'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "MT_waiver", br(), "Families Achieving Independence in Montana (FAIM)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("MT_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),   
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'NC'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "NC_waiver", br(), "Work First?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("NC_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),  
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'ND'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "ND_waiver", br(), "Training, Employment, Education Management (TEEM)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("ND_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),  
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'NE'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "NE_waiver", br(), "Employment First (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("NE_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),  
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & (input.state == 'NH' | input.state=='NY')",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "family_assistance_waiver", br(), "Family Assistance Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("family_assistance_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),   
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'NJ'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "NJ_waiver", br(), "Work First New Jersey (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("NJ_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'NM'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "NM_waiver", br(), "NM Works (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("NM_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'OH'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "OH_waiver", br(), "Ohio Works First (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("OH_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'OR'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "OR_waiver", br(), "Job Opportunities and Basic Skills Program (JOBS)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("OR_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & (input.state == 'RI' | input.state == 'SC')",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "fip_waiver", br(), "Family Independence Program (FIP)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("fip_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'TN'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "TN_waiver", br(), "Families First (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("TN_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'TX'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "TX_waiver", br(), "Texas Works (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("TX_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'UT'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "UT_waiver", br(), "Family Employment Program (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("UT_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'VA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "VA_waiver", br(), "Virginia Initiative for Employment, Not Welfare (VIEW)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("VA_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'VT'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "ANFC_waiver", br(), "Aid to Needy Families with Children (ANFC)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("ANFC_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      

      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'WA'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                             #   tags$form(
                              #    class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "workfirst_waiver", br(), "WorkFirst?")
                                                              )
                                    ))
                             #) 
                         ),
                         column(4,
                                radioButtons("workfirst_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ),
      
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'WI'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "WI_waiver", br(), "Wisconsin Works?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("WI_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'WV'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "WV_waiver", br(), "West Virginia Works (TANF)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("WV_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      conditionalPanel(condition = "input.organization == 'Custom Analysis' & input.state == 'WY'",
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                                #   tags$form(
                                #    class="form-horizontal",
                                
                                tags$div(
                                  class="form-group",   div(style="margin-top:-1.5em;",
                                                            
                                                            h5(tags$label(`for` = "WY_waiver", br(), "Personal Opportunities With Employment Responsibility (POWER)?")
                                                            )
                                  ))
                                #) 
                         ),
                         column(4,
                                radioButtons("WY_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
      ), 
      
      

      
      #-----------------------------------------------------
      #-----------------------------------------------------
      
      conditionalPanel(condition =  "input.organization == 'Custom Analysis' & (input.state == 'AL' | input.state == 'AK' | input.state == 'AR' | input.state == 'AZ'|
                      input.state == 'CA' | input.state == 'CO' | input.state == 'CT' | input.state == 'DC'| 
                       input.state == 'DE' | input.state == 'FL' | input.state == 'GA' | input.state == 'HI' | input.state == 'IA'|
                       input.state == 'ID' | input.state == 'IL' | input.state == 'IN' | input.state == 'KS' | input.state == 'KY'|
                       input.state == 'LA' | input.state == 'MA' | input.state == 'MD' | input.state == 'ME' | input.state == 'MI'|
                       input.state == 'MN' | input.state == 'MO' | input.state == 'MS' | input.state == 'MT'|
                       input.state == 'NC' | input.state == 'ND' | input.state == 'NE' | input.state == 'NH'|
                        input.state == 'NJ' | input.state == 'NM' | input.state == 'NV' | input.state == 'NY'|
                       input.state == 'OH' | input.state == 'OK' | input.state == 'OR' | input.state == 'PA'|
                       input.state == 'RI' | input.state == 'SC' | input.state == 'SD' | input.state == 'TN'|
                        input.state == 'TX' | input.state == 'UT' | input.state == 'VA' | input.state == 'VT'|
                       input.state == 'WA' | input.state == 'WI' | input.state == 'WV' | input.state == 'WY' 
                       )" ,
                       
                       
                       
                       fluidRow(
                         column(4, offset = 1,
                            #    tags$form(
                             #     class="form-horizontal",
                                  
                                  tags$div(
                                    class="form-group",   div(style="margin-top:-1.5em;",
                                                              
                                                              h5(tags$label(`for` = "wic_waiver", br(), "Women, Infants and Children Nutrition Program (WIC)?")
                                                              )
                                    ))
                            #) 
                         ),
                         column(4,
                                radioButtons("wic_waiver", label = NULL,
                                             list(#" " = "empty",
                                               "No" = "No",
                                               "Yes" = "Yes")
                                             , selected = "No", inline=TRUE))
                         
                         
                       )
                       
                       
                       
      ),
      
   
  
      
      hr(), 
     fluidRow(
      column(6, offset = 0,
             numericInput("numadults",label="Number of Adults (19+)", value=1,min=0, max=6)),
      column(5, offset = 0,
             numericInput("numkids",label="Number of Children", value=0,min=0, max=6))
      
    ),
    
    conditionalPanel(condition = "input.state != 'AL'",
    
 selectInput("fam_disab", label="Does anyone in the home have a disability?",
              list("select" = "empty"
                ,"No" = "No"
               ,"Yes" = "Yes"), selectize=FALSE, selected = "No")
 ),
   
    ################
    
    #br(),

    
    # AGE OF ADULTS, AGE_ADULT_1 TO AGE_ADULT_6, min should be 19, max should be 99
    
    conditionalPanel(condition = "input.numadults>=1",
                      numericInput("age_adult_1", label = "Age of First Adult (19+)", value = 25, min=0,max=99)),
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=1)",
                     checkboxInput("disab1", "First adult has a disability", FALSE) ),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=1)",
                     checkboxInput("blind1", label="First adult is legally blind", FALSE)),
     
     
     conditionalPanel(condition = "input.numadults>=2",
                      
                      numericInput("age_adult_2", label = "Age of Second Adult (19+)", value = 25, min=19,max=99)
     ),
     
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=2)",
                     checkboxInput("disab2", "Second adult has a disability", FALSE)),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=2)",
                     checkboxInput("blind2", label="Second adult is legally blind", FALSE)),
     
    conditionalPanel(condition = "input.numadults>=3",
                     numericInput("age_adult_3", label = "Age of Third Adult (19+)", value = 25, min=19,max=99)),
     
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL'  & input.numadults>=3)",
                     checkboxInput("disab3", "Third adult has a disability", FALSE)),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=3)",
                     checkboxInput("blind3", label="Third adult is legally blind", FALSE)),

     
     conditionalPanel(condition = "input.numadults>=4",
                      numericInput("age_adult_4", label = "Age of Fourth Adult (19+)", value = 25, min=19,max=99)),
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL'   & input.numadults>=4)",
                     checkboxInput("disab4", "Fourth adult has a disability", FALSE)),
     
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=4)",
                     checkboxInput("blind4", label="Fourth adult is legally blind", FALSE)),

      
     conditionalPanel(condition = "input.numadults>=5", 
                      numericInput("age_adult_5", label = "Age of Fifth Adult (19+)", value = 25, min=19,max=99)),
     
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL'   & input.numadults>=5)",
                     checkboxInput("disab5", "Fifth adult has a disability", FALSE)),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=5)",
                     checkboxInput("blind5", label="Fifth adult is legally blind", FALSE)),
      conditionalPanel(condition = "input.numadults>=6",
                       numericInput("age_adult_6", label = "Age of Sixth Adult (19+)", value = 25, min=19,max=99)),
     
    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL'   & input.numadults>=6)",
                     checkboxInput("disab6", "Sixth adult has a disability", FALSE)),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numadults>=6)",
                     checkboxInput("blind6", label="Sixth adult is legally blind", FALSE)),
 
    br(),
    conditionalPanel(condition = "input.numadults>=2",
                     
                     radioButtons("partnered", label = "Living with spouse or partner?",
                                  list(#" " = "empty",
                                    "No" = "No",
                                    "Yes" = "Yes")
                                  , selected = "No")   ),
 
 conditionalPanel(condition="input.numadults>=2 & input.partnered=='Yes'",
                  radioButtons("marital_status", label = "Married?",
                               list(#" " = "empty",
                                 "No" = "No",
                                 "Yes" = "Yes")
                               , selected = "No")),
 
 

     
 # Establish child(ren) age(s)
       
     conditionalPanel(condition = "input.numkids>=1",
                      numericInput("age_child_1", label = "Age of First Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=1)",
                     checkboxInput("disab7", "First child has a disability", FALSE)),

    conditionalPanel(condition = "input.numkids>=2",
                     numericInput("age_child_2", label = "Age of Second Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=2)",
                     checkboxInput("disab8", "Second child has a disability", FALSE)),

    conditionalPanel(condition = "input.numkids>=3", numericInput("age_child_3", label = "Age of Third Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=3)",
                     checkboxInput("disab9", "Third child has a disability", FALSE)),

    conditionalPanel(condition = "input.numkids>=4",
                      numericInput("age_child_4", label = "Age of Fourth Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=4)",
                     checkboxInput("disab10", "Fourth child has a disability", FALSE)),

    conditionalPanel(condition = "input.numkids>=5",
                     numericInput("age_child_5", label = "Age of Fifth Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=5)",
                     checkboxInput("disab11", "Fifth child has a disability", FALSE)),

    conditionalPanel(condition = "input.numkids>=6",
                     numericInput("age_child_6", label = "Age of Sixth Child (18 and under)", value = NA, min=0,max=18)
                      , h6("If child is less than 1 year old put 0.")),

    conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & input.numkids>=6)",
                     checkboxInput("disab12", "Sixth child has a disability", FALSE)),
 
  
                 fluidRow(
                   column(6, h4("Monthly Earned Income of Individual"), h5("Do NOT include income from OTHER adults. Do NOT include unearned income (e.g. TANF, SSI, child support, etc) . If any children have earned income, include their earnings here.")),
                   column(3, currencyInput("monthly_income_current", label=NULL, value = NA, format = "dollar", align = "left")),
                 ),
 
 conditionalPanel(condition = "input.numadults>=2",
               fluidRow(
                 column(6, h4("Monthly Earned Income of Other Adults")),
                 column(3, currencyInput("income.otherhousehold", label=NULL, value = NA, format = "dollar", align = "left")),
               )
 ),
 

hr(),
########### GIFT INCOME


  fluidRow(          
    column(6, h4("Amount of Guaranteed Income Per Month"), h5("Non-taxable cash assistance/ gift income")),
    column(3,currencyInput("gift_income_change", label=NULL, value = NA, format = "dollar", align = "left"))
  ), 

#############Other sources of income
hr(),
fluidRow(
  column(6, h4("Do you have any other sources of income?")),
  column(3, radioButtons("other_income_sources", label = NULL, list("Yes","No"),selected = "No"))
),

######### BONUS

conditionalPanel(condition = "input.other_income_sources == 'Yes'",
                 fluidRow(          
                   column(6, h4("Bonuses"),h6("Examples: performance bonus, sales commission, signing bonus.")),
                   column(3,currencyInput("cash_bonus_current", label=NULL, value = 0, format = "dollar", align = "left"),)
                 ), 
                 fluidRow(
                   column(3, h5("This bonus is:")),
                   column(6, radioButtons("cash_bonus_specify_current", label = NULL, list("Monthly", "Annual"), selected = "Annual", inline=TRUE),),
                 align="middle"),
            
),

################## CHILD SUPPORT

conditionalPanel(
  condition = "input.other_income_sources == 'Yes' & input.numkids>=1",
  fluidRow(          
    column(6, h4("Monthly Child Support Income")),
    column(3,currencyInput("child_support_current", label=NULL, value = 0, format = "dollar", align = "left"),),
  ), 
br(),
),

############ INVESTMENT INCOME

conditionalPanel(
  condition = "input.other_income_sources == 'Yes'",
  fluidRow(          
    column(6, h4("Monthly Investment Income"))
    ,column(3,currencyInput("investment_income_current", label=NULL, value = 0, format = "dollar", align = "left"),)
  ), 
  
),



hr(),

fluidRow(          
  column(6, h4("Does Employer Offer Health Insurance?")),
  column(3,radioButtons("includes_health_insurance_current", label = NULL, 
                         list("No" = "no_current","Yes" = "yes_current"), selected = "no_current"))
 ),

#################PUBLIC ASSISTNACE (populated by ProgramList_StateSpecific.csv)
hr(),
h4(strong("Public Assistance")),
 
selectInput('benefit1', label = "", 
            list("select" = "empty","No programs",# "All programs except Section 8 and CCDF", 
                 "Select a custom list"), selected = "Select a custom list", selectize=FALSE)
,
conditionalPanel(condition ="input.benefit1=='Select a custom list'",
                selectizeInput("benefit2", 
                   label = "What Public Assistance are you or anyone in your household currently receiving?", choices = NULL, multiple = TRUE, options = list(placeholder = 'select'
                               ))
),

hr(),


#NOTE: ALL STATE-SPECIFIC PRGORAM NAMES FOR CCDF, SNAP, AND HOUSING VOUCHER NEED TO BE ADDED TO CONDITIONAL LOGIC BELOW


#input childcare expenses
conditionalPanel(condition = "input.childcare_expenses == 'Enter your childcare costs' & input.numkids>=1 & input.benefit1 =='Select a custom list' &
                  
                  
                      (input.benefit2.includes('Supplemental Nutrition Assistance Program (SNAP)') 
                       | input.benefit2.includes('CalFresh (SNAP)') 
                       | input.benefit2.includes('Food Assistance Program (SNAP)') 
                        | input.benefit2.includes('Nutrition Assistance (SNAP)') 
                         | input.benefit2.includes('Food Supplement Program (SNAP)') 
                          | input.benefit2.includes('Food Stamp Program (SNAP)') 
                           | input.benefit2.includes('Food & Nutrition Services (SNAP)') 
                            | input.benefit2.includes('3SquaresVT (SNAP)') 
                             | input.benefit2.includes('Basic Food Program (SNAP)') 
                              | input.benefit2.includes('Basic Food Program (SNAP)') 
                               
                       | input.benefit2.includes('Section 8 Housing Voucher')
                       ) 
                    
                       & !input.benefit2.includes('Child Care Subsidy (CCDF)')
                       & !input.benefit2.includes('Working Connections Child Care Program (CCDF)')
                       & !input.benefit2.includes('Child Care Assistance Program (CCAP/CCDF)')
                       & !input.benefit2.includes('Childcare and Parent Services (CAPS)')
                       & !input.benefit2.includes('Child Care Scholarship (CCS)')
                      ",
                 numericInput("childcareexp1", label = "Childcare Expenses (per month)", value = NA_real_, min=0)
#option to use estimated childcare expenses or enter actual childcare costs??

),

conditionalPanel(condition = "input.numkids>=1 & input.benefit1 =='Select a custom list' &
                  
                      (input.benefit2.includes('Supplemental Nutrition Assistance Program (SNAP)') 
                       | input.benefit2.includes('CalFresh (SNAP)') 
                       | input.benefit2.includes('Food Assistance Program (SNAP)') 
                        | input.benefit2.includes('Nutrition Assistance (SNAP)') 
                         | input.benefit2.includes('Food Supplement Program (SNAP)') 
                          | input.benefit2.includes('Food Stamp Program (SNAP)') 
                           | input.benefit2.includes('Food & Nutrition Services (SNAP)') 
                            | input.benefit2.includes('3SquaresVT (SNAP)') 
                             | input.benefit2.includes('Basic Food Program (SNAP)') 
                              | input.benefit2.includes('Basic Food Program (SNAP)') 
                               
                       | input.benefit2.includes('Section 8 Housing Voucher')
                       ) 
                    
                       & !input.benefit2.includes('Child Care Subsidy (CCDF)')
                       & !input.benefit2.includes('Working Connections Child Care Program (CCDF)')
                       & !input.benefit2.includes('Child Care Assistance Program (CCAP/CCDF)')
                       & !input.benefit2.includes('Childcare and Parent Services (CAPS)')
                       & !input.benefit2.includes('Child Care Scholarship (CCS)')
                      ",
                  #option to use estimated childcare expenses or enter actual childcare costs??
                 radioButtons("childcare_expenses", NULL,
                               c("Use an estimate of childcare costs","Enter your childcare costs"), selected = "Enter your childcare costs", inline=TRUE)
                 
  
),



#ENTER RENT EXPENSE
conditionalPanel(condition = "input.housing_expenses == 'Enter your housing costs' & (input.benefit1 == 'All programs' | 
                     (input.benefit1 == 'Select a custom list' & input.benefit2.includes('Section 8 Housing Voucher')))",
                 numericInput("rentexp1",label="Total Rent BEFORE Subsidy (per month)", value=NA_real_, min=0)
), 


conditionalPanel(condition = "input.housing_expenses == 'Enter your housing costs' & input.benefit1 == 'Select a custom list' & !input.benefit2.includes('Section 8 Housing Voucher') &
                    (input.benefit2.includes('Supplemental Nutrition Assistance Program (SNAP)') 
                       | input.benefit2.includes('CalFresh (SNAP)') 
                       | input.benefit2.includes('Food Assistance Program (SNAP)') 
                        | input.benefit2.includes('Nutrition Assistance (SNAP)') 
                         | input.benefit2.includes('Food Supplement Program (SNAP)') 
                          | input.benefit2.includes('Food Stamp Program (SNAP)') 
                           | input.benefit2.includes('Food & Nutrition Services (SNAP)') 
                            | input.benefit2.includes('3SquaresVT (SNAP)') 
                             | input.benefit2.includes('Basic Food Program (SNAP)') 
                              | input.benefit2.includes('Basic Food Program (SNAP)') 
                               )",
                 
                 numericInput("rentexp2",label="Rent/Mortgage (per month)", value=NA_real_,min=0)
),

#SEPERATE UTILITIES
conditionalPanel(condition = "input.housing_expenses == 'Enter your housing costs' & input.benefit1 == 'Select a custom list' &
                    (input.benefit2.includes('Supplemental Nutrition Assistance Program (SNAP)') 
                       | input.benefit2.includes('CalFresh (SNAP)') 
                       | input.benefit2.includes('Food Assistance Program (SNAP)') 
                        | input.benefit2.includes('Nutrition Assistance (SNAP)') 
                         | input.benefit2.includes('Food Supplement Program (SNAP)') 
                          | input.benefit2.includes('Food Stamp Program (SNAP)') 
                           | input.benefit2.includes('Food & Nutrition Services (SNAP)') 
                            | input.benefit2.includes('3SquaresVT (SNAP)') 
                             | input.benefit2.includes('Basic Food Program (SNAP)') 
                              | input.benefit2.includes('Basic Food Program (SNAP)') 
                               
                       | input.benefit2.includes('Section 8 Housing Voucher')
                       )",
                 
                 numericInput("utilexp",label="Utilities (per month)", value=NA_real_,min=0)
),

 
#option to use estimated housing expenses or enter actual housing exp
conditionalPanel(condition = "(input.benefit1 == 'All programs' | input.benefit2.includes('Section 8 Housing Voucher')  
                 |input.benefit2.includes('Supplemental Nutrition Assistance Program (SNAP)') 
                       | input.benefit2.includes('CalFresh (SNAP)') 
                       | input.benefit2.includes('Food Assistance Program (SNAP)') 
                        | input.benefit2.includes('Nutrition Assistance (SNAP)') 
                         | input.benefit2.includes('Food Supplement Program (SNAP)') 
                          | input.benefit2.includes('Food Stamp Program (SNAP)') 
                           | input.benefit2.includes('Food & Nutrition Services (SNAP)') 
                            | input.benefit2.includes('3SquaresVT (SNAP)') 
                             | input.benefit2.includes('Basic Food Program (SNAP)') 
                              | input.benefit2.includes('Basic Food Program (SNAP)') 
                               
                )",
                 radioButtons("housing_expenses",NULL,
                              c("Use an estimate of housing costs",
                                "Enter your housing costs"), selected = "Enter your housing costs", inline=TRUE)
),


conditionalPanel(condition = "(input.fam_disab=='Yes' & input.state != 'AL' & 
(input.benefit1=='All programs' 
| input.benefit2.includes('Medicaid for Adults')
| input.benefit2.includes('DenaliCare (Medicaid for Adults)')
| input.benefit2.includes('Health Care (Medicaid for Adults)')
| input.benefit2.includes('Arizona Health Care Cost Containment System (Medicaid for Adults)')
| input.benefit2.includes('Medi-Cal for Adults (Medicaid for Adults)')
| input.benefit2.includes('Health First Colorado (Medicaid for Adults)')
| input.benefit2.includes('HuskyHealth (Medicaid for Adults)')
| input.benefit2.includes('Healthy Families (Medicaid for Adults)')
| input.benefit2.includes('Diamond State Health Plan (Medicaid for Adults)')
| input.benefit2.includes('Statewide Medicaid Managed Care Program (Medicaid for Adults)')
| input.benefit2.includes('MedQuest (Medicaid for Adults)')
| input.benefit2.includes('IA Health Link (Medicaid for Adults)')
| input.benefit2.includes('Medical Assistance Program (Medicaid for Adults)')
| input.benefit2.includes('KanCare Medical Assistance Program (Medicaid for Adults)')
| input.benefit2.includes('Healthy Louisiana (Medicaid for Adults)')
| input.benefit2.includes('Adult MassHealth (Medicaid for Adults)')
| input.benefit2.includes('Medical Assistance (Medicaid for Adults)')
| input.benefit2.includes('MaineCare (Medicaid for Adults)')
| input.benefit2.includes('MO HealthNet (Medicaid for Adults)')
| input.benefit2.includes('Mississippi Coordinated Access Network (Medicaid for Adults)')
| input.benefit2.includes('Division of Health Benefits (Medicaid for Adults)')
| input.benefit2.includes('North Dakota Medicaid Expansion Program (Medicaid for Adults)')
| input.benefit2.includes('Nebraska Medical Assistance Program (Medicaid for Adults)')
| input.benefit2.includes('NH Medicaid (Medicaid for Adults)')
| input.benefit2.includes('NJ FamilyCare (Medicaid for Adults)')
| input.benefit2.includes('Centennial Care (Medicaid for Adults)')
| input.benefit2.includes('Medicaid Managed Care (Medicaid for Adults)')
| input.benefit2.includes('SoonerCare (Medicaid for Adults)')
| input.benefit2.includes('Oregon Health Plan (Medicaid for Adults)')
| input.benefit2.includes('RI Medical Assistance Program (Medicaid for Adults)')
| input.benefit2.includes('Healthy Connections (Medicaid for Adults)')
| input.benefit2.includes('TennCare (Medicaid for Adults)')
| input.benefit2.includes('STAR+PLUS (Medicaid for Adults)')
| input.benefit2.includes('Cardinal Care (Medicaid for Adults)')
| input.benefit2.includes('Green Mountain Care (Medicaid for Adults)')
| input.benefit2.includes('Apple Health for Adults (Medicaid for Adults)')
| input.benefit2.includes('BadgerCare (Medicaid for Adults)')
| input.benefit2.includes('Equality Care (Medicaid for Adults)')))",
                 radioButtons("prev_ssi", "Has anyone in the home ever received SSI?",
                              c("No" = "No",
                                "Yes" = "Yes"), selected = "No", inline=TRUE)),

     conditionalPanel(condition = "(input.benefit1 == 'All programs' | input.benefit2.includes('Supplemental Security Income (SSI)')) ",
                      numericInput("disab.work.exp", label="Amount spent ($) per month on specialized equipment or services that enable household member(s) with disabilities to work",value=0, min=0, max=10000)),
    
     conditionalPanel(condition = "(input.disab1==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)'))) ",
                      numericInput("ssdiPIA1", label="Amount ($) of SSDI received per month for first adult's disability",value=0, min=0, max=10000, step=100),
                      h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

     conditionalPanel(condition = "(input.disab2==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)'))) ",
                      numericInput("ssdiPIA2", label="Amount ($) of SSDI received per month for second adult's disability",value=0, min=0, max=10000, step=100),
                      h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

     conditionalPanel(condition = "(input.disab3==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)')))",
                      numericInput("ssdiPIA3", label="Amount ($) of SSDI received per month for third adult's disability",value=0, min=0, max=10000, step=100),
                      h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

     conditionalPanel(condition = "(input.disab4==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)')))",
                     numericInput("ssdiPIA4", label="Amount ($) of SSDI received per month for fourth adult's disability",value=0, min=0, max=10000, step=100),
                     h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

     conditionalPanel(condition = "(input.disab5==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)')))",
                     numericInput("ssdiPIA5", label="Amount ($) of SSDI received per month for fifth adult's disability",value=0, min=0, max=10000, step=100),
                     h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

     conditionalPanel(condition = "(input.disab6==1 & (input.benefit1 == 'All programs' | input.benefit2.includes('Social Security Disability Insurance (SSDI)')))",
                     numericInput("ssdiPIA6", label="Amount ($) of SSDI received per month for sixth adult's disability",value=0, min=0, max=10000, step=100),
                     h6("Do not include SSDI amounts that are for children, spouses, or other adults with a disability")),

    
#ask about assets, but only for the states where there are asset tests for programs selected
#conditionalPanel(condition = "(input.benefit1 == 'All programs' | input.benefit2a.includes('Supplemental Nutrition Assistance Program (SNAP)')",
   hr(),
   h4(strong("Assets")),
   h6("Assets are the amount of money in your Checking and Savings Accounts. Do not include retirement accounts such as Roth, IRA, or 401K."),  
   hr(),
   h4("Current assets"),
   currencyInput("assets_current", label=NULL, value = 0, format = "dollar"),
   h4("Anticipated assets (while receiving GI)"),
   h6("Enter the maximum amount you could imagine having while receiving GI."),
   currencyInput("assets_change", label=NULL, value = 0, format = "dollar"),


    ) #end sidebar panel

  ,mainPanel(
    
    tabsetPanel(id="main",
      
      tabPanel("Results",  #icon = icon("angle-double-right", lib = "font-awesome"),
               
               tags$div(class = "container",
                        tags$head(tags$style(HTML(".shiny-output-error-validation{color: red; font-size:36px; }"))),
                        tags$head(tags$style(".shiny-notification{color: red; font-size:32px; position: fixed; top: 87% ;left: 2%  }")),
                      #  br(),
                     #   includeHTML("include_beta.html"), # Home page - written in HTML
                   
                        br(),
                        fluidRow(
                          column(10
                                 ,h3(strong("Step 1: UNDERSTAND LIMITATIONS"))
                                 ,h4(strong("Fill in the form to the left and read the limitations below."))
                                 
                                 ,h4("1) The GI Dashboard provides estimates; it does not provide financial advice. You should confirm any amounts shown on the calculator with your case manager or a representative from the agency you are receiving benefits from.")
                                 ,h4("2) The GI Dashboard does NOT include all government programs that may be affected by a change in income. See the 'FAQ' tab for a list of included programs.")
                                 ,h4("3) Any changes to your household characteristics will likely affect the results shown. For other tools which incorporate household changes and income changes over time, see the FAQ tab.")
                                 ,br()
                                 
                                 
                          )   
                        ),
                      
                    fluidRow(
                          column(10,h3(strong("Step 2: Click 'Calculate' to COMPARE FINANCIAL RESOURCES WITH AND WITHOUT GUARANTEED INCOME")))
                            ),
                    
                  
                br(),
                    tags$div(align = "center",
                             actionButton("getresults","Calculate Results", 
                                          style="background-color: rgb(20,68,104); border-color: rgb(20,68,104)",
                                          class="btn btn-primary btn-lg")),
                 br(),
  
                   conditionalPanel(condition = "input.getresults>0",
                            tags$div(align = "center",
                            br(),
                            
                             h4("To save these results hit CTRL+P on your keyboard (COMMAND+P on Mac) and print to PDF."),
                             br(),
                             tags$div(align = "center",
                                      h4(htmlOutput('effect_budget'))),
                            h5(strong("Note that some programs are not included in these calculations.  For example, the following programs are not included in this calculations and could be effected by receipt of GI: non-MAGI medicaid programs, FAFSA student aid, Energy Assistance (LIHEAP), and city-run programs such as metro and water discounts. Check with a representative from the appropriate agency if you are receiving any of these benefits. Further, special program rules for Veterans, pregnant women, children currently in or exiting foster care, and persons experiencing homelessness are NOT included in these calculations.")),
                            
                            br(),
                             h4(paste("The table below estimates the overall effect of guaranteed income on your household's monthly budget.")),
                             br(),
                             withSpinner(tableOutput('table.NetResources'),type=2),
                             h6(span(strong("*Take-home pay is after-tax wage income for the entire household, including all sources of income."))))
                    )
                    
                  ,conditionalPanel(condition = "input.getresults>0",
                                  tags$div(align = "center",
                                           br(),
                                           h4("The second table shows the impact on your household's monthly public assistance and tax liabilities."),
                                           br(),
                                           withSpinner(tableOutput('table.programloss'),type=2))
                  ),
                tags$div(align = "center",
                         h6(span(strong(textOutput("asset_limit"))))),
                
                # conditionalPanel(condition = "input.getresults>0",
                #  tags$div(align = "center",
                #           br(),
                #           h4("The last table shows the change in your household's monthly out-of-pocket expenses."),
                #           br(),
                #           withSpinner(tableOutput('table.expenses'),type=2)
                #  ))

        ) #closes tabPanel("results")
      )
  
      , tabPanel("FAQ"
                 
                 ,h3("To report errors or make suggestions about how to improve the tool please use our",strong(tags$u(tags$a(href="https://frb.co1.qualtrics.com/jfe/form/SV_8lc90nmNJZL709M",target="_blank", "suggestion box"))))
                 ,br()
                 ,h3(strong(tags$u(tags$a(href="http://eepurl.com/hMplE5",target="_blank","Subscribe"))),"to receive occasional email updates about new features or tools.")
                  ,br()
                 ,br()
                 ,h2("Frequently Asked Questions")   
                 ,br()
                 ,h4(strong("What is non-MAGI Medicaid?"))
                ,h5("Some medicaid programs do not use modified adjusted gross income to determine eligiblity. These programs tend to be for individuals over 65, with disabilities, or that are medically needy. The name of non-MAGI medicaid programs vary by state but will tend to have names like 'spend-down' or 'buy-in'.")
                ,br()
                ,h4(strong("Is there an asset test for Medicaid?"))
                ,h5("According to the Affordable Care Act (ACA) rules, no state can impose an asset or resource test on MAGI based Medicaid applicants or recipients beginning in 2014. Some states require asset tests for non-MAGI medicaid programs.")
                ,br()
                 ,h4(strong("Is the information I enter here stored in anyway?"))
                  ,h5("No. This tool is not capable of storing information.")
                 ,br()
                 ,h4(strong("How do I use this tool?"))
                 ,h5("Training for this tool is coming soon. You can find training on benefits cliffs and other FREE CLIFF tools at ",strong(tags$u(tags$a(href="https://academy-clifftool.thinkific.com/",target="_blank","CLIFF Academy"))))
                 ,br()
                 ,h4(strong("How do I learn more about your other CLIFF tools that incorporate career planning and career comparisons?"))
                 ,h5("Our other CLIFF tools, CLIFF Dashboard and CLIFF Planner, show how benefits phase out along a chosen career path. They can be used to determine which career will enable someone to meet a basic set of expenses in their area, to compare financial tradeoffs and financial returns of different careers, and to budget for a career move. Please contact us at cliff@atl.frb.org to learn more about these FREE TOOLS.")
                 ,br()
                 ,h4(strong("What are each of the public assistance programs shown in CLIFF?"))
                 ,h5("A brief summary of each program is provided in the 'Programs and Tax Credits' section below. To visually see how any of these programs vary by state, income level, and household composition view the", strong(tags$u(tags$a(href='https://emar-data-tools.shinyapps.io/prd_dashboard',target="_blank",'Policy Rules Database Dashboard.'))))
                 ,br()
                 ,h4(strong("I'm receiving a program that is not on the list. What should I do?"))
                 ,h5("Unfortunately, CLIFF tools does not include all programs. For example, Social Security Disability Insurance (SSDI), unemployment insurance, non-MAGI medicaid programs, city-run programs and programs for specific populations (such as Veterans) are not currently included (see full list  in 'Programs not included in the CLIFF tools' below). If a program you are receiving is not on the list of programs in the list of public assistance you should check with your case manager to learn about the eligibility rules and do your own calculations to learn how it will impact your budget.")
                 ,br()
                 ,h4(strong("Who should I include for my 'household'?"))
                 ,h5("Include anyone that shares financial resources with you. This could be someone they financially support or that financially supports them.")
                 ,br()
                 ,h4(strong("Are eligibility redetermination periods factored in to these calculations?")) 
                 ,h5("Some programs have probational periods where participants are able to keep a benefit for some period after they are no longer eligible - for example, if their income passes a threshold. Probational periods vary by state and program and are not factored in to the PRD.")
                 ,br()
                 #for employer tool only
                 # ,h4(strong("I do not know whether the stipend is taxable. What should I do?"))
                 # ,h5("See IRS rules on how stipends are treated for the taxation purposes", strong(tags$u(tags$a(href="https://www.irs.gov/pub/irs-pdf/p5137.pdf",target="_blank","here."))))
                 #,br()
                 ,h4(strong("What types of income are assumed to be $0?"))
                 ,h5("The following types of income affect eligibility for some programs and are assumed to be $0 for the purposes of generating calculations in this tool: unemployment insurance, pensions, and state disability payments.")
                 ,br()
                 ,h4(strong("Are the effects of income changes on wage garnishments or child support payments factored in?"))
                 ,h5("No. The effects of income changes on wage garnishments, child support payments, etc. are not included in these calculations")
                 ,br()
                 ,h4(strong("How is out-of-pocket health insurance cost determined?"))
                 ,h5("For purposes of calculating net financial resources, CLIFF automatically calculates the out-of-pocket cost of health insurance based on the lowest cost option the household is eligible for. To determine the potential set of health insurance options CLIFF takes into consideration the programs you select from the list and the cost of purchasing health insurance through an employer. The cost of health insurance (both the total cost and the unsubsidized out-of-pocket cost) varies by source. For example, the premium paid each month will depend on whether the person is receiving Employer Sponsored Health Insurance, Medicaid/CHIP, or health insurance through the Health Exchange Marketplace.")
                 ,br()
                 ,h4(strong("How do the results differ for persons with disabilities?"))
                 ,h5("This tool incorporates special rules for individuals with disabilities for the following programs: SSDI, SSI, Medicaid, SNAP, Section 8 and Federal Income Taxes. In some states, children with disabilities may be subject to different rules for the CCDF program; these state-specific rules are not incorporated.   The following programs count SSI and SSDI income towards eligibility: SNAP, WIC, School Meals, Head Start/Early Head Start, and Section 8.")

                ,br()
               # ,conditionalPanel(condition = "input.organization == 'NYS Direct Cash Transfer'",
               # )
                 , h2("Methodology")
                 , br()
                 , h5("The values of basic expenses are based on the ", strong(tags$u(tags$a(href="https://www.atlantafed.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/cost-of-living-database.aspx", target="_blank", "Federal Reserve Bank of Atlanta Cost-of-Living Database."))))
                 , br()
                 , h5("The calculations for public assistance programs, taxes, and tax credits are produced using the ", strong(tags$u(tags$a(href="https://www.frbatlanta.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/policy-rules-database.aspx", target="_blank", "Policy Rules Database."))))
                 , br()
                 
                 ,h2("Programs and Tax Credits")
                 ,br()
                 ,h5(strong("Subsidized Child Care: "),"Subsidized child care programs are funded by the child care development fund grant (CCDF). This program has a different name and different eligibility rules in each state. The Subsidized Child Care program provides subsidized daycare services to families below state-specified income levels. In almost all states, there is a different income threshold for initial enrollment and continuous enrollment. This lets families stay on the program as their income increases beyond the initial enrollment income threshold. Families pay a copay based on a sliding scale fee schedule, which varies at the county or state level. Note that Subsidized Child Care programs have a waitlist in many cities.")
                 
                 ,h5(strong("Head Start and Early Head Start: "),"Children from birth to age five and pregnant women from families with incomes below the poverty guidelines are eligible for Head Start and Early Head Start services. Children younger than three are eligible for Early Head Start, and children at least three years old up to school age (typically five years in most states) are eligible for Head Start. Children from families receiving Temporary Assistance for Needy Families (TANF) or SSI are categorically eligible. Children who are homeless or in foster care are eligible as well. Additionally, programs are allowed to enroll families whose incomes are below 130 percent of the poverty line, although the enrollment of these families are capped at 35 percent of participants.")
                 
                 ,h5(strong("Supplemental Nutrition Assistance Program (SNAP): "),"This program is also known as food stamps and provides vouchers that can be exchanged for food. SNAP is available to individuals below state-specified income levels. Eligibility for SNAP depends on household income, housing expenses, and child care expenses. Some states also have asset tests.Note that the income eligibility rules described on the federal government's benefits website are not correct because they do not account for the fact that states sometimes use broad based categorical eligibility rules to extend eligibility beyond federal limits.")
                 
                 ,h5(strong("Free or Reduced Price School Meals: "), "This program is also known as the School Breakfast Program and National School Lunch Program. Both programs are federally assisted meal programs operating in public and nonprofit private schools and residential child care institutions. They provide low-cost or no-cost breakfasts and lunches to children every school day.  The maximum income for eligibility is 185 percent of the FPL. Children also gain categorical eligibility for free school lunch and breakfast if they are in a household that receives SNAP benefits or TANF cash assistance. In addition, all children at high-poverty schools ('CEP') designated by the U.S. The Department of Agriculture receives school meals at no charge. Note that CLIFF does not account for CEP schools when determining eligibility for SLP and NSLP.")
                 
                 ,h5(strong("The Special Supplemental Nutrition Program for Women, Infants, and Children (WIC): "), "WIC provides supplemental foods, health care referrals, and nutrition education for low-income pregnant, breastfeeding, and non-breastfeeding postpartum women, and to infants and children up to age five who are found to be at nutritional risk. WIC provides vouchers for specific types of foods - such as whole-grain bread, baby food, infant formula, and milk - as well as separate 'cash value vouchers' that can be used to buy fruits and vegetables. WIC also provides infant formula to mothers who do not breastfeed. Note that only the value of food is estimated in CLIFF (not the other services provided). To be eligible for WIC,  applicants must either have gross household income at or below 185 percent of the federal poverty level or be categorically eligible. An applicant who already receives SNAP (formerly food stamps), Medicaid, or Temporary Assistance for Needy Families cash assistance is categorically eligible for WIC. In addition to passing income tests, women are eligible while they are pregnant and for six months after the birth of the infant. Women who continue to breastfeed their infants beyond six months are eligible for WIC benefits for up to a year after childbirth. Children may be eligible up to their fifth birthday. ")
                 
                 ,h5(strong("Housing Voucher: "), "The Housing Choice Voucher Program (Section 8). Section 8 provides vouchers which reduce the cost of rent. The voucher is paid to the landlord directly by the local public housing agency on behalf of the participating household. The household then pays the difference between the actual rent charged by the landlord and the amount subsidized by the program. To be income-eligible, a household must have adjusted income below 80 percent of the median income for the county or metropolitan area where the household lives. Adjusted income reduces a household's gross income by (1) dependent deduction, (2) elderly/disabled deduction, (3) unreimbursed medical expenses of any elderly household or disabled household, and (4) any reasonable child care expenses necessary to enable a member of the household to work or to further his or her education. The household is 'continuously assisted' under the 1937 Housing Act, meaning the household is already receiving assistance under any 1937 Housing Act program-for example, public housing-when the household is admitted to the voucher program. Once admitted to the program, the household does not undergo any further income eligibility tests. Note that the Section 8 Housing Voucher program has a waitlist in many cities. In Connecticut the value of the housing voucher will include the Rental Assistance Program if that program is checked.")
                 
                 ,h5(strong("Health Insurance Marketplace Subsidies/ACA: "),"Health insurance marketplace subsidies (also referred to as 'ACA' in CLIFF) provide subsidies for health insurance for those with incomes between 100 percent and 400 percent of the FPL. People pay a portion of their income toward health insurance. The amount of the individual responsibility portion varies by income level, age, and household size. The difference between the individual responsibility portion and the full cost of health insurance purchased on the health exchange is the estimated value of the program.")
                 
                 ,h5(strong("Medicaid and Children's Health Insurance Program: "),"Medicaid is a federal and state program that helps with medical costs for some people with limited income and resources. CHIP provides health coverage to eligible children, through both Medicaid and separate CHIP programs. Income-eligibility thresholds vary by state and depend on whether adults have dependents. There are no work requirements for Medicaid.")
                 
                 #uncomment once these programs are added (and remove ssi and ssdi from the list of  programs not included)
                 
                 #,h5(strong("Supplemental Security Income (SSI): "), "SSI is a federally funded program that provides cash assistance to low-income individuals with disabilities as diagnosed by medical professionals that prevent them from engaging in" ,strong(tags$a(href="https://www.ssa.gov/oact/cola/sga.html#:~:text=To%20be%20eligible%20for%20disability,to%20be%20engaging%20in%20SGA",target="_blank","'substantial gainful activity' (SGA).")), "Any earnings that SSI recipients receive lowers their SSI benefit amount.")
                 
                 ,h5(strong("Social Security Disability Insurance (SSDI): "), "SSDI is a federally funded program that provides cash assistance to low-income individuals with disabilities as diagnosed by medical professionals that prevent them from engaging in", strong(tags$a(href="https://www.ssa.gov/oact/cola/sga.html",target="_blank","'substantial gainful activity' (SGA).")), "The Policy Rules Database does not model the determination of initial value of SSDI. Instead, PRD can be used to determine how the initial value of SSDI changes as employment income increases.")
                 
                 ,h5(strong("Earned Income Tax Credit (EITC): "), "Both federal and state EITC calculations are included in CLIFF. The Federal EITC is a benefit for working people with low to moderate income. Workers receive a credit equal to a percentage of their earned income up to a maximum. After the credit reaches the maximum, it remains flat until earnings reach the phaseout point. Both the credit rate and the credit maximum vary by household size. To qualify, workers must meet certain requirements and file a tax return, even if they do not owe any taxes or are not required to file. The Federal EITC is a refundable tax credit; it reduces the amount of tax owed and may result in a refund.  The income eligibility threshold varies according to the number of dependents and tax filing status. Workers who do not claim eligible children must be at least age 25 but under age 65. If a worker is married filing a joint return, either the worker or spouse must meet this age requirement. Those who are married and filing separately are not eligible. In 2020, 28 states and the District of Columbia offered an additional State EITC. States typically set their credits as a percentage of the Federal EITC. However, unlike the federal credit, some State EITCs are not refundable")
                 
                 ,h5(strong("Child Tax Credit (CTC): "),"Both federal and state CTC calculations are included in CLIFF.  The Federal CTC is a partially refundable tax credit available to parents with qualifying dependents under the age of 17. In 2020, a household that earned less than $2,500 was ineligible for the credit. Those with incomes above $480,000 ($280,000 for singles and household heads) receive no CTC. Working families can receive a refund equal to 15 percent of their earnings above $2,500. This refund can be worth up to $1,400 per child. Families can claim a maximum tax credit of $2,000. The Federal CTC starts to phase out at income levels of $400,000 ($200,000 for single or head-of-household filers). The America Rescue Plan significantly temporarily expanded the Federal CTC credit in multiple ways. See the Policy Rules Database Technical Manual for more information. As of 2021, six U.S. states-California, Colorado, Idaho, New York, North Carolina, and Oklahoma-have their own child tax credits. States' CTC are typically structured either as a lump-sum payment for each eligible child or as a fixed percent of the federal credit. New York has a combination of both approaches. Some states' CTCs are nonrefundable. ")
                 
                 ,h5(strong("Federal Child and Dependent Care Tax Credit (CDCTC): "),"The Federal CDCTC is a nonrefundable tax credit that reduces a taxpayer's federal income tax liability based on child- and dependent-care expenses incurred. Taxpayers must have earned income and meet a variety of eligibility criteria including incurring qualifying child- and dependent-care expenses for a qualifying individual. A qualifying individual for the Federal CDCTC is either (1) the taxpayer's dependent child under 13 years of age, or (2) the taxpayer's spouse or dependent who is incapable of caring for himself or herself. A taxpayer must have earned income to claim the credit. For married couples, both spouses must have earnings unless one is a student or incapable of self-care. There is no upper income eligibility threshold-taxpayers at all income levels can claim the Federal CDCTC. Many lower-income taxpayers receive little or no credit since the credit is nonrefundable. As of 2020, 28 states and the District of Columbia had enacted their own State CDCTCs, the structure of which varies significantly by state. Some states have set their credits as a share of the federal credit while other states calculate it as a share of expenses. In some states, the credit is fully refundable, while in others it is nonrefundable.")
                 
                 ,h5(strong("Programs not included in the CLIFF: "),"The following programs are not currently included: Temporary Assistance for Needy Families (TANF) for most states, Supplemental Security Income (SSI), Social Security Disability Insurance (SSDI),  Medicare, city-funded programs, and programs available to specific populations (such as Veterans).")
                 
                 
      ) #closes tab panel FAQ    
      
      
    ) #closes tabsetpanel 
  ) #closes sidebarlayout 
  ) #closes mainpanel
 
 ,div(class = "footer",
       includeHTML("www/include_footer.html"))
 
) #closes fluidpage
 


