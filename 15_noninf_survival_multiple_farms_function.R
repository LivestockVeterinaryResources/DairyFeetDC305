# Survival post lesion Dx

#Survival after lesion dx:----

## function to create data set for further use
survival_data <- function(data = lamecull, censor_days = censordat,
                          disease_date = ftdat, culled = culled,
                          control = lifexlame, life_x_disease) {
  # need to do this to use as.numeric to convert duration
  #censor_days <- ensym(censor_var)
  data |> 
    # reduce data
    select(farm, cowid, {{disease_date}}, {{censor_days}}, {{culled}},
           {{control}}, {{life_x_disease}}
           ) |>  
    ## this as.numeric creates NA's not sure why as it works without function
  mutate (censor_time = as.numeric({{censor_days}}),
         # create variables to condition on
         life_x_disease = case_when({{ control }} == 0 ~ 0,
                                    {{ life_x_disease }} == 1 ~ 1,
                                    {{ life_x_disease }} == 2 ~ 2,
                                    {{ life_x_disease }} > 2 ~ 3,
                                    TRUE ~ NA),
         life_x_disease_cat = case_when(life_x_disease == 0 ~
                                          "Never any lesion",
                                        life_x_disease == 1 ~ 
                                          "Once",
                                        life_x_disease == 2 ~ 
                                          "Twice",
                                        life_x_disease == 3 ~ 
                                          "3 or more times",
                                        TRUE ~ NA),
         life_x_disease_cat = fct_reorder(life_x_disease_cat, 
                                       life_x_disease, .na_rm = TRUE)
         )
}

# km function to create graph
km_fit <- function(data, time = censor_time, event = culled) {
  data_surv <- data |> 
    mutate(surv_object = Surv(time = censor_time, event = culled))
  
  fit_km <- survfit(surv_object ~ life_x_disease_cat, data = data_surv)
  fit_km |> 
    ggsurvplot(
      # needed data statement as extracts see help and without it doesn't work
      data = data_surv, 
      pval = FALSE, 
      conf.int = TRUE,
      censor = FALSE,
      fun = "pct",
      size = 1,
      linetype = c(1, 2, 3, 4), # Specify dashed lines for each group
      palette = c("#fde725ff", "#440154ff", "#3b528bff",
                  "#21908cff"), 
      legend = "bottom",
      legend.title = "Lifetime Lesion #",
      legend.labs = c("Never any lesion",
                      "First",
                      "Second",
                      "3 or more"),
      xlab = "Days to cull after trimming/diagnosis",
      ylab = "% of Cows Alive",
      xlim = c(0,180),
      ylim = c(50,100),
      break.time.by = 30
    )
}

# function for table
km_fit_table <- function(data, time = censor_time, event = culled) {
  data_surv <-data |> 
    mutate(surv_object = Surv(time = censor_time, event = culled))
  
  fit_table <- survfit(surv_object ~ life_x_disease_cat, data = data_surv)
  fit_table |> 
    tbl_survfit(times = c(30,120, 180),
                label = " ",
                reverse = TRUE, 
                label_header = "{time} Days") %>% 
    as_flex_table() %>% # makes formatting and pdf better
    add_header_row(top = TRUE,
                   values = c("Lifetime Lesion History",
                              "% Culled (Confidence Interval) at",
                              "", "")) %>% 
    bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
    bold(i = 2, bold = TRUE, part = "header") %>% 
    merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
    merge_at(i = 1, j = 2:4, part = "header") %>% # merges top columns
    delete_rows(i = 1, part = "body")|>
    fit_to_width(max_width= 6.5) |> 
    hline_bottom(part = "body")
}

# commands to add into QMD----
# create single dataset
surval_inf <- survival_data(life_x_disease = lifexnoninf, 
                            disease_date = ftdat)

# create graph
km_graph <- km_fit(data = surval_inf, event = culled)

# km table
km_table <- km_fit_table(data = surval_inf, event = culled)

# or combo approach
km_graph2 <- survival_data(life_x_disease = lifexinf, disease_date = ftdat) |> 
  km_fit(event = culled)


# todo: create list of all datasets for lesions 
# then feed to graph function




