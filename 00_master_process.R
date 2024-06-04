

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


#these are written out so that the render function of quarto can access them.  They may not all be necessary?
write_rds(startdat, 'ReportParameters/startdat.rds')
write_rds(enddat, 'ReportParameters/enddat.rds')
write_rds(startym, 'ReportParameters/startym.rds')
write_rds(endym, 'ReportParameters/endym.rds')
write_rds(start_month_1y, 'ReportParameters/start_month_1y.rds')
write_rds(end_month_1y, 'ReportParameters/end_month_1y.rds')
write_rds(regfarm1, 'ReportParameters/regfarm1.rds')


source('01_single_import.R')
source('02_setup_for_analysis_single.R')
quarto::quarto_render("document.qmd", output_format = "pdf")
