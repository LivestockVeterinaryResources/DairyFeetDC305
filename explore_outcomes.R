library(tidyverse)
library(janitor)


#read in data -------------
lame3<-read_rds('datafiles/lame3.rds')
lame4<-read_rds('datafiles/lame4.rds')#%>% #this is the lame4 from single import, not after the date filters in 02_
  #filter(cowid %in% '7')
  #filter(cowid %in% '257')
  #filter(cowid %in% '4329')
  #filter(cowid %in% '5797')


  

# lamedry<-read_rds("datafiles/lamedry.rds")
# lamecull<-read_rds("datafiles/lamecull.rds")

temp_dry_cull<-read_csv('sourcedata/temp_dry_cull.CSV')%>%
  clean_names()%>%
  select(id, bdat, fdat, lact, date, event)%>%
  mutate(bdat = lubridate::mdy(bdat))%>%
  rename(birth = bdat)%>%
  mutate(cowid = paste0(id), 
         date = lubridate::mdy(date),
         frsh = lubridate::mdy(fdat))

max_pull_date<-case_when(
  (range(lame4$ftdat)[2])>(range(temp_dry_cull$date)[2])~(range(lame4$ftdat)[2]),
  TRUE~(range(temp_dry_cull$date)[2])
   )
  

outcomes<-temp_dry_cull%>%
  group_by(cowid, birth, frsh, lact)%>%
  summarise(date_outcome_min = min(date), 
            date_outcome_max = max(date), 
            outcome = paste0(unique(event), collapse = ','), 
            ct_row = sum(n()))%>%
  ungroup()%>%
  mutate(outcome_clean = case_when(
    (str_detect(outcome, ','))~'Invalid Outcome', 
    TRUE~trimws(outcome))
  )

#lame 5 ------------

lame5<-lame4%>%
  mutate(cowid = paste0(cowid))%>%
  ungroup()%>%
  left_join(outcomes)%>%
  select(outcome_clean, 
         farm, cowid, birth, frsh, 
         lact, ftdat, ftdim, site, breed, trimonly,
         dd, footrot, wld , soleulcer#,
         # toe, axial, totxtrimmed, 
         # prevftdat, diffprevftdat, time2nextftdat, 
         # prevtrimtype, timefromtrim, 
         # time2nextlame
         )%>%
  mutate(outcome_clean = case_when(
    is.na(outcome_clean)~'Unknown', 
    TRUE~outcome_clean)
  )%>%
  pivot_longer(cols = c('dd', 'footrot', 'wld' , 'soleulcer'), 
               names_to = 'lesion', 
               values_to = 'lesion_present')%>%
  ungroup()%>%
  filter(lesion_present>0)%>%
  mutate(Date = ftdat, 
         cowid = paste0(cowid))%>%
  rowid_to_column()



# #Lag function--------------------------

#fucntion to rename variables used within lag function
mutfxn = function(x, vars, prefix){
  
  map_dfc(vars, ~
            x %>% 
            #mutate(key = as.character(NA))%>%
            transmute(!! paste(prefix,"lag",.x,sep = "_") := lag(!!as.name(.x))==!!as.name(.x))) %>% 
    bind_cols(x,.) 
  
}



#function to create grouping variables
test_fxn1 <- function(x,arrange_var, mutate_var, prefix, gap){
  arrange_var1 <- quos(!!! arrange_var) # quos returns a list of quosures - can handle (...), !!! allows more than 1
  var_ct<-length(arrange_var1) # Calculate the number of variables
  
  x %>%
    arrange(!!!arrange_var1) %>% 
    
    mutfxn(mutate_var, prefix) %>%   # call mutfxn
    
    mutate(date_gap = (difftime(Date, lag(Date), units = 'days')),  # for long format data: data_last_admin is replaced by Date
           lag_date = ((date_gap > (gap*(-1))) & (date_gap < gap))) %>%
    
    mutate(count_lags = reduce(select(., contains("lag")), `+`)) %>%  #Identify rows where a new lag group starts
    
    mutate(lag_ct = (count_lags) < (var_ct))%>%
    
    mutate(lag_ct = case_when(is.na(lag_ct)~TRUE,
                              TRUE~lag_ct)) %>% #this is necessary to fill the first row in the dataframe, which cannot be NA 
    
    mutate(key = case_when(is.na(lag_ct)>0~paste0(prefix, rowid),
                           lag_ct>0~paste0(prefix, rowid),
                           TRUE~as.character(NA))) %>%
    fill(key)
}



#use lag function to greate construct groupings-----------------

#set parameters ---------------
eval_outcomes_at_x_days<-60
therapy_duration_days<-14

#Therapy
arrange_vars_t <- alist(farm, site, cowid, lesion, Date)  

sort_vars_t <- c("farm", 'site', "cowid", "lesion") 

df_t<-test_fxn1(x = lame5,
                arrange_var = arrange_vars_t, #does inclued date
                mutate_var = sort_vars_t, #does NOT include date
                prefix = "t", #useful if multiple sequences are run on the same data
                gap = therapy_duration_days)%>% #gap set to identify theraputic event sequences
  rename(t_key = key, 
         t_date_gap = date_gap, 
         t_ct = lag_ct)%>%
  select(-(contains('lag')))

head(df_t%>%select(t_key, t_date_gap, t_ct, everything()))


#identify retreats-----------------
#outcome
arrange_vars_o <- alist(farm, cowid, lesion, Date)  

sort_vars_o <- c("farm", "cowid", "lesion") 

df_o<-test_fxn1(x = df_t,
                arrange_var = arrange_vars_o,
                mutate_var = sort_vars_o, 
                prefix = "o", 
                gap = eval_outcomes_at_x_days)%>% #gap set for outcome on day 90
  rename(o_key = key, 
         o_date_gap = date_gap, 
         o_ct = lag_ct)%>%
  select(-(contains('lag')))

head(df_o%>%select(o_key, o_date_gap, o_ct, everything()))



#group by unique therapy----------------

therapy<-df_o%>%
  group_by(farm, site, cowid, birth, frsh, lact, lesion, 
           o_key, o_ct, t_key, t_ct)%>%
  summarise(lesion_unique_ct = n_distinct(lesion),
            
            lesion = paste0(unique(lesion), collapse = ';'),  
            
            #Treatment = paste0(unique(Treatment), collapse = ','),  
            date_min = min(Date), 
            date_max = max(Date), 
            
            therapy_ct = n_distinct(t_key)
            
  )%>%
  ungroup()%>%
  mutate(t_timeframe = as.numeric(difftime(date_max, date_min, units = 'days')))%>%
  arrange(cowid, lesion, date_min)%>%
  group_by(o_key)%>%
  mutate(pull_ct = 1:n(), 
         pull_max = sum(n()))%>%
  ungroup()

head(therapy)



#put it all together -----------
final_outcomes<-therapy%>%
  left_join(outcomes%>%filter(!(outcome_clean %in% 'DRY')))%>%
    mutate(final_outcome = case_when(
      (pull_ct<pull_max)~paste0('Retreat <', eval_outcomes_at_x_days, ' days'), 
      date_max>(max_pull_date-eval_outcomes_at_x_days)~paste0('Unknown (<', eval_outcomes_at_x_days, ' days from tx)'),
      (is.na(date_outcome_min))~paste0('Success at ', eval_outcomes_at_x_days, ' days'),
      (date_min+eval_outcomes_at_x_days)<(date_outcome_min)~paste0('Success at ', eval_outcomes_at_x_days, ' days'),
      (!(is.na(outcome_clean)))~outcome_clean,
      TRUE~'Unknown')
    )%>%
  arrange(cowid, birth, frsh, lact, lesion, date_min)%>%
  group_by(cowid, birth, frsh, lact, lesion)%>%
  mutate(times_treated = 1:n())%>%
  ungroup()%>%
  mutate(floordate_month = floor_date(date_min, unit = 'month'), 
         floordate_season = floor_date(date_min, unit = 'season'), 
         floordate_year = floor_date(date_min, unit = 'year')
  )


#cows with lots of treatments------------
tcows<-final_outcomes%>%
  group_by(cowid)%>%
  summarise(ct = sum(n()))%>%
  ungroup()%>%
  filter(ct>7)

#picture of therapies ------------
#ggplot(final_outcomes)+
ggplot(final_outcomes%>%filter(cowid %in% tcows$cowid))+
  geom_point(aes(x = date_min, y = lesion, color = final_outcome))+
  facet_grid(cowid~.)



#percentages by pull count----------
finaloutcomes_summary<-final_outcomes%>%
  group_by(floordate_year, lesion, pull_ct)%>%
  mutate(t_total = n_distinct(t_key))%>%
  ungroup()%>%
  group_by(floordate_year, lesion, pull_ct, final_outcome, t_total)%>%
  summarise(#ct_cows = n_distinct(cowid), 
            #ct_rows = sum(n()), 
            ct_t_keys = n_distinct(t_key))%>%
  ungroup()%>%
  mutate(pct = round((ct_t_keys/t_total)*100, digits = 1))


#plot summary all-----------
ggplot(finaloutcomes_summary%>%
         #filter(lesion %in% 'dd')%>%
         filter(pull_ct ==1))+
  geom_point(aes(x = floordate_year, y = pct, color = lesion), size = 2, alpha = .5)+
  geom_line(aes(x = floordate_year, y = pct, color = lesion, group = lesion))+
  facet_wrap(final_outcome~.)+
  theme_bw()+
  theme(axis.text.x = element_text(angle =45, hjust =1))+
  #coord_cartesian(ylim = c(0, 1))+
  labs(title = '1st Treatment Outcomes')

