# Set Up file for analysis

#Load Packages----

# rio tips comes from https://epirhandbook.com/suggested-packages-1.html

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  janitor,       # tables
  tidyverse,     # data mgmt and viz
  readr,         # import 
  zoo,           # dates
  flextable,      # pretty tables
  #broom,         # model output
  #scales,        # for tables %
  gtsummary,     # tables
  gt, 
  broom,         # tid models
  #rstatix,
  #meantables,
  RColorBrewer,
  survminer,     # survival
  Hmisc,        # for CI
  ggtext,       # dim graph
  ragg,         # dim graph
  knitr,         # tables
  epiR,          # Conf Inf
  ggridges       # for ridgeplots
)

# load data----
lame4 <-import("datafiles/lame4.rds")
lameleg <- import("datafiles/lameleg.rds")
mall <- import("datafiles/mall.rds") 
lamecull <- import("datafiles/lamecull.rds")

# setup data section----
# enter dates last 12 months of full data
startdat <- "2023-01-01"
enddat <- "2023-12-31"
startym <- 2023+0/12   #to get 12 months yearmon format year + month#-1/12
endym <- 2023+11/12
start_month_1y <- "Jan 2023"
end_month_1y <- "Dec 2023"
regfarm1 <- "Template"

# load data----
lame4 <- lame4 |> 
  filter(ftdat>= startdat) |> 
  filter(ftdat<=enddat)

lamecull <- lamecull |> 
  filter(ftdat>= startdat) |> 
  filter(ftdat<=enddat)

# set up repeated variables
les_variables <- c("lesion", "dd", "footrot", "wld",
                   "soleulcer", "solefract", "hem", "cork", "other", "axial",
                   "toeulcer", "thin", "inf", "noninf", "toe", "injury")
