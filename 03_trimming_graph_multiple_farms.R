# Graph Trimming data

# Trimming: Graph distribution of trims----
lact.labs <- c("1" = "1st Lactation", 
               "2" = "2nd Lactation",
               "3" = "3rd Lactation",
               "4" = "4+ Lactation")

# combined faceted by farm
trimdistgfarm <- lame4 %>% 
  filter(ftdat >= startdat) %>%
  select(farm, cowid, ftdim, lifexlame, lact) %>% 
  mutate(lifexlame = case_when(lifexlame == 0 ~ "0",
                               lifexlame == 1 ~ "1",
                               lifexlame == 2 ~ "2",
                               TRUE ~ "3"),
         lctgp = case_when(lact == 1 ~ "1",
                           lact == 2 ~ "2",
                           lact == 3 ~ "3",
                           TRUE ~ "4")) %>% 
  #ridgeline graph
  ggplot(aes(x = ftdim, y = lifexlame))+
  geom_density_ridges(aes( fill = as.factor(lifexlame)),
                      alpha = 0.7, color = "black", 
                      from = 0, to = 500,
                      scale = 2,
                      jittered_points = TRUE, point_alpha = 0.5,
                      position = position_points_jitter(width = 0.05,
                                                        height = 0),
                      point_shape = '|', point_size = 0.5)+
  scale_fill_viridis_d(option = "D",
                       breaks = c("0", "1", "2", "3"),
                       name = "Lameness History",
                       labels = c("0" = "Never",
                                  "1" = "Once",
                                  "2" = "Twice",
                                  "3" = "Chronic"),
                       # for legend positions to move needs to be horizontal
                       guide = guide_legend(direction = "horizontal",
                                            title.position = "top"))+ 
  # zooms into 0-500
  coord_cartesian(xlim = c(0,500))+
  # changes chow close labels are to axis
  scale_x_continuous(expand = c(0, 0))+
  scale_y_discrete(expand = c(0.01, 0.01),
                   labels = NULL)+
  labs(x = "Days in Milk at Trim Date", y = NULL)+
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)+
  # more moving of legend
  theme(legend.position = "bottom",
        legend.title.align = 0.5,
        legend.justification = "center")+
  facet_grid(cols = vars(lctgp), 
             rows = vars(farm), 
             labeller = labeller(lctgp = lact.labs))

# Time to Lameness Graph----

# set up variables
trimtolame <- lamecull %>% 
  filter (lifexlame == 0) %>%
  filter(ftdat > startdat) %>% 
  select (cowid, farm, ftdat, lact, birth, frsh, lifexlame,
          time2nextftdat, time2nextlame, censordat) %>% 
  mutate(nextrmlame = if_else(is.na(time2nextlame),0,1),
         lctgp = case_when(lact == 1 ~ "1",
                           lact == 2 ~ "2",
                           TRUE ~ "3+"))
# KM graph
# Set the name 'trim2lame' as an alias for 'trimtolame' dataset
trim2lame <- trimtolame

# Fit the survival model using the 'survfit' function
fitKM <- survfit(Surv(censordat, nextrmlame) ~ lctgp, data = trim2lame)

# Create a Kaplan-Meier survival plot using 'ggsurvplot'
km <- ggsurvplot(fitKM, 
                 pval = FALSE, 
                 facet.by = "farm",
                 conf.int = TRUE,
                 censor = FALSE,
                 fun = "pct",
                 size = 1,
                 linetype = c(1, 1, 1, 1, 1,
                              2, 2, 2, 2, 2,
                              3, 3, 3, 3, 3), # Specify dashed lines for each group
                 palette = c("#440154ff", "#3b528bff", "#fde725ff"),
                 legend = "bottom",
                 legend.title = "Lactation",
                 legend.labs = c("1st", "2nd", "3+"),
                 xlab = "Days to Lameness after Trim",
                 ylab = "Percentage of Healthy Cows",
                 xlim = c(0, 180),
                 ylim = c(50, 100),
                 break.time.by = 30,
                 short.panel.labs = TRUE) +
  # Set the fill colors and linetypes for each group in the legend and overide
  # the default where lines and colours are separate
  scale_fill_manual(values = c("#440154ff", "#3b528bff", "#fde725ff"),
                    guide = guide_legend(override.aes = 
                                           list(linetype = c(1, 2, 3),
                                                fill =c("#440154ff",
                                                        "#3b528bff",
                                                        "#fde725ff")))) +
  guides(linetype = "none") # Remove the linetype legend
