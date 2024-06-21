# Survival post lesion Dx

#Survival:Blocks----

## function

survival_data <- function(data = lamecull, censor_var = censordat,
                          disease_date = ftdat, culled = culled,
                          control = lifexlame, life_x_disease) {
  # need to do this to use as.numeric to convert duration
  #censor_date <- ensym(censor_var)
  data |> 
    # reduce data
    select(farm, cowid, {{disease_date}}, {{censor_var}}, {{culled}},
           {{control}}, {{life_x_disease}}
           ) |>  
    ## this as.numeric creates NA's not sure why as it works without function
  mutate (censor_time = as.numeric({{censor_var}}),
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

# create dataset
surval_inf <- survival_data(life_x_disease = lifexnoninf, 
                            disease_date = ftdat)

# fit KM data 
# not working yet
km_fit <- function(data, time = censor_time, event = culled){
  data_surv <- data |> 
    mutate(surv_object = Surv(time = {{time}}, event = {{culled}}))
  
  fit <- survfit(surv_object ~ life_x_disease, 
                 data = data)
}
  
km_test <- km_fit(data = surval_inf)

fitKM <- survfit(Surv(censordat, culled) ~ lifexnoninf, data = survall_1y)
km<- ggsurvplot(fitKM, 
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
                #   "Combined"), for add.all
                xlab = "Days to cull after lifetime lesion #",
                ylab = "% of Cows Alive",
                xlim = c(0,180),
                ylim = c(50,100),
                #add.all = TRUE,
                tables.theme = theme_cleantable(),
                break.time.by = 30
                ) 

# tables for survival

fit_farm1 <- survfit(Surv(censordat, culled) ~ lifexnoninf_cat, 
                     data = survall_1y)


tbl_surv_farm1 <- tbl_survfit(fit_farm1, 
                              times = c(30,120, 180),
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


