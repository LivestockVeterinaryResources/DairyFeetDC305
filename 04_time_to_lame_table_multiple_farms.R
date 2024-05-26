# time to lameness table----
# uses trimtolame from 03

fit_farm <- survfit(Surv(censordat, nextrmlame) ~ farm, 
                     data = trimtolame)

# farm
tbl_time_2_lame_farm <- tbl_survfit(fit_farm,
            times = c(90, 120, 150, 180),
            label = " ",
            reverse = TRUE,
            label_header = "{time} Days") %>% 
  as_flex_table() %>% # makes formatting and pdf better
  add_header_row(top = TRUE,
                 values = c("Farm",
                            "% Lame (Confidence Interval) at",
                            "",
                            "",
                            "")) %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
  bold(i = 2, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:5, part = "header") %>% # merges top columns
  autofit() %>% 
  fit_to_width(max_width= 6.5)