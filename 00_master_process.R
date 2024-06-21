

library(tidyverse)
library(quarto)

#step 0 set up ---------------------
# setup variables used throughout----
farm1_name <- "Template" # enter Farm name for reports
filename1 <- "temp" # enter name used for farm in DC export

# start month 5 years ago 
startmon <- "Jan 2018"

# for variables defined below use last full month of data
endmon <- "Dec 2023" 
start_month_1y <- "Jan 2023"
end_month_1y <- "Dec 2023"
censorend <- c("12-31-23")

# trim date item name in dairycomp
ftdat_item <- "ftdat" 
breed_item <- "cbrd"
fresh_date_item <- "fdat"
birth_date_item <- "bdat"

# timing of mid lactation trims
# splits DIM of trimming in categories 
# for early enter the DIM that mid lactation trim start
# for mid enter the DIM that mid lacations trims are supposed to end
early <- 115
mid <- 131

#------------------------------
# setup data section----
# enter dates last 12 months of full data
startdat <- "2023-01-01"
enddat <- "2023-12-31"
startym <- 2023+0/12   #to get 12 months yearmon format year + month#-1/12
endym <- 2023+11/12
start_month_1y <- "Jan 2023"
end_month_1y <- "Dec 2023"
regfarm1 <- "Template"

#write out parameters for use later------------
#these are written out so that the render function of quarto can access them.  They may not all be necessary?------------
write_rds(startdat, 'reportparameters/startdat.rds')
write_rds(enddat, 'reportparameters/enddat.rds')
write_rds(startym, 'reportparameters/startym.rds')
write_rds(endym, 'reportparameters/endym.rds')
write_rds(start_month_1y, 'reportparameters/start_month_1y.rds')
write_rds(end_month_1y, 'reportparameters/end_month_1y.rds')
write_rds(regfarm1, 'reportparameters/regfarm1.rds')

#run the first step---------------
source('01_single_import.R')

#run setup step 2 -------------------
source('02_setup_for_analysis_single.R')

#generate report ----------------- 
##NOTE: 06_needs finalized before this will run-------------
quarto::quarto_render("Template Lameness Report.qmd", output_format = "pdf") #this has an error that needs fixed for 017 chunk
