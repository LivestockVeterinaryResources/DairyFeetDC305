# Survival post lesion Dx

#Survival:Blocks----
survall <- lamecull %>%
  # filter(farm == regfarm) %>% 
  # filter out the specific lesion
  filter(ftdat >= startdat) %>% 
  mutate(censordat = as.numeric(censordat)) %>% 
  mutate(lifexnoninf = case_when(lifexlame == 0 ~ 0,
                                 lifexnoninf == 1 ~ 1,
                                 lifexnoninf == 2 ~ 2,
                                 lifexnoninf >2 ~3,
                                 TRUE ~ NA),
         lifexnoninf_cat = case_when(lifexlame == 0 ~ "Never any lesion",
                                     lifexnoninf == 1 ~ "Once",
                                     lifexnoninf == 2 ~ "Twice",
                                     lifexnoninf == 3 ~ "3 or more times",
                                     TRUE ~ NA) ) |> 
  mutate(lifexnoninf_cat = fct_reorder(lifexnoninf_cat, 
                                       lifexnoninf, .na_rm = TRUE))

fitKM <- survfit(Surv(censordat, culled) ~ lifexnoninf, data = survall)
km<- ggsurvplot(fitKM, 
                pval = FALSE, 
                facet.by = "farm",
                conf.int = TRUE,
                censor = FALSE,
                fun = "pct",
                size = 1,
                linetype = c(1, 1, 1, 1, 1,
                             2, 2, 2, 2, 2,
                             3, 3, 3, 3, 3,
                             4, 4, 4, 4, 4
                ),
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
                ylab = "Survival %",
                xlim = c(0,180),
                ylim = c(50,100),
                #add.all = TRUE,
                tables.theme = theme_cleantable(),
                break.time.by = 30,
                short.panel.labs = TRUE) +
# Set the fill colors and linetypes for each group in the legend and overide
# the default where lines and colours are separate
# to get hex for palette:  brewer.pal(n = 4, name = 'Set1')
scale_fill_manual(values = c("#fde725ff", "#440154ff", "#3b528bff",
                             "#21908cff"),
                  guide = guide_legend(override.aes = 
                                         list(linetype = c(1, 2, 3, 4),
                                              fill = c("#fde725ff", 
                                                       "#440154ff",
                                                       "#3b528bff",
                                                       "#21908cff")))) +
  guides(linetype = "none") # Remove the linetype legend


# tables for survival

# farm 1
survall_farm1 <- survall 

# recode/mutate lifeexnoninf to text so can have text in table.

fit_farm1 <- survfit(Surv(censordat, culled) ~ farm, 
                         data = survall_farm1)

tbl_surv_farm1 <- tbl_survfit(fit_farm1, 
            times = c(30, 60, 120, 180),
            label = " ",
            reverse = TRUE, 
            label_header = "{time} Days") %>% 
  as_flex_table() %>% # makes formatting and pdf better
  add_header_row(top = TRUE,
                 values = c("Lifetime Lesion History",
                            "Probability of being Culled (Confidence Interval) at",
                            "", "",
                            "")) %>% 
  bold(i = 1, bold = TRUE, part = "header") %>% # bolds headers
  bold(i = 2, bold = TRUE, part = "header") %>% 
  merge_at(i = 1:2, j = 1, part = "header") %>% #merges 1st row
  merge_at(i = 1, j = 2:5, part = "header") %>% # merges top columns
  autofit()%>% 
  fit_to_width(max_width= 6.5) |> 
  delete_rows(i = 1, part = "body") 


