# Run this file to copy the most recent files

#user<-"ilin_/Dropbox"
user<-"ellie/Dropbox" #Ellie
user<-"edgar/Dropbox (ATL FRB)"
#user<-"f1dat01/Dropbox (ATL FRB)/"

setwd(paste0("C:/Users/", user,"/WorkForceDevProj/CareerLadderTool/Tools for Shinyapps.io/gi_dashboard"))

# Database files
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/prd_parameters/tables.rdata"), paste0(getwd(),"/mainFiles/Database/tables.rdata"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/prd_parameters/parameters.defaults.rdata"), paste0(getwd(),"/mainFiles/Database/parameters.defaults.rdata"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/prd_parameters/expenses.rdata"), paste0(getwd(),"/mainFiles/Database/expenses.rdata"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/prd_parameters/benefit.parameters.rdata"), paste0(getwd(),"/mainFiles/Database/benefit.parameters.rdata"), overwrite = TRUE)

# Function files
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/functions/benefits_functions.R"), paste0(getwd(),"/mainFiles/functions/benefits_functions.R"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/functions/expense_functions.R"), paste0(getwd(),"/mainFiles/functions/expense_functions.R"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/functions/BenefitsCalculator_functions.R"), paste0(getwd(),"/mainFiles/functions/BenefitsCalculator_functions.R"), overwrite = TRUE)
file.copy(paste0("C:/Users/",user,"/WorkForceDevProj/Documentation/Benefits & Expenses Database/programs/libraries.R"), paste0(getwd(),"/mainFiles/libraries.R"), overwrite = TRUE)

