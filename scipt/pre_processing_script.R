#load necessary packages

library(tidyverse)
library(tidyverse)
library(readxl)

#laod data

data<-read_excel("raw_data/QOL_Raw.xlsx")


# missing valu identification code
is.na(data)- #count number of missing value
sum(is.na(data))  


#Removing the missing value
data<-na.omit(data)
Physical_func<-na.omit(Physical_func)

#data cutting

sf_data<-data |> 
  
  select(20:55)

#column name change code

colnames(sf_data)<- paste0("Q",1:36)

#1.physical functioning

Physical_func<- sf_data |> 
  select(Q3:Q12) |> 
  mutate(across(Q3:Q12,~ case_when(
    .=="Yes, Limited a Little"~0,
    .=="No, Not Limited at all"~50,
    .=="Yes, Limited a lot"~100,
    TRUE~NA_real_
    
  ))) |> 
  
  
  rowwise() |> 
  mutate(Physical_func=mean(c_across(Q3:Q12),na.rm = TRUE))

na.omit(Physical_func$Q3)
unique(Physical_func$Q3)

#2.physical role 

role_physical<-sf_data |> 
  select(Q13:Q16) |> 
  mutate(across(Q13:Q16,~ case_when(
    .=="Yes"~0,
    .=="No"~100,
    
    TRUE~NA_real_
    
  ))) |> 
  
  rowwise() |> 
  mutate(role_physical=mean(c_across(Q13:Q16),na.rm = TRUE))

unique(role_physical$Q13)
sum(is.na(role_physical))
na.omit(role_physical)


  
#3.bodily pain 


bodily_pain<-sf_data |> 
  select(Q17:Q19)|> 
  mutate(across(Q17:Q19,~ case_when(
    .=="Yes"~0,
    .=="No"~100,
    
    TRUE~NA_real_
    
  ))) |> 
  
  rowwise() |> 
  mutate(role_physical=mean(c_across(Q17:Q19),na.rm = TRUE))

#4.General health



general_health <- sf_data |> 
  select(Q1, Q33:Q36) |> 
  mutate(Q1 = case_when(
    Q1 == "Excellent" ~ 100,
    Q1 == "Very Good" ~ 75,
    Q1 == "Good" ~ 50,
    Q1 == "Fair" ~ 25,
    Q1 == "Poor" ~ 0,
    
    TRUE ~ NA_real_
  )) |> 
  mutate(across(Q33:Q35, ~ case_when(
    . == "Definitely true" ~ 0,
    . == "Mostly true" ~ 25,
    . == "Don't know" ~ 50,
    . == "Mostly false" ~ 75,
    . == "Definitely false" ~ 100,
    TRUE ~ NA_real_
  ))) |> 
  mutate(Q36 = case_when(
    Q36 == "Definitely true" ~ 100,
    Q36 == "Mostly true" ~ 75,
    Q36 == "Don't know" ~ 50,
    Q36 == "Mostly false" ~ 25,
    Q36 == "Definitely false" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(general_health = rowMeans(across(c(Q1, Q33:Q36)), na.rm = TRUE))




 unique(general_health$Q33)

 #5.Vitality

 vitality <- sf_data |> 
   select(Q23, Q27, Q29, Q31) |> 
   mutate(Q23 = case_when(
     Q23 == "All of the time" ~ 100,
     Q23 == "Most of the time" ~ 80,
     Q23 == "A good Bit of the time" ~ 60,
     Q23 == "Some of the time" ~ 40,
     Q23 == "A little bit of the time" ~ 20,
     Q23 == "None of the Time" ~ 0,
     TRUE ~ NA_real_
   )) |> 
   mutate(Q27 = case_when(
     Q27 == "All of the time" ~ 100,
     Q27 == "Most of the time" ~ 80,
     Q27 == "A good Bit of the time" ~ 60,
     Q27 == "Some of the time" ~ 40,
     Q27 == "A little bit of the time" ~ 20,
     Q27 == "None of the Time" ~ 0,
     TRUE ~ NA_real_
   )) |> 
   mutate(Q29 = case_when(
     Q29 == "All of the time" ~ 0,
     Q29 == "Most of the time" ~ 20,
     Q29 == "A good Bit of the time" ~ 40,
     Q29 == "Some of the time" ~ 60,
     Q29 == "A little bit of the time" ~ 80,
     Q29 == "None of the Time" ~ 100,
     TRUE ~ NA_real_
   )) |> 
   mutate(Q31 = case_when(
     Q31 == "All of the time" ~ 0,
     Q31 == "Most of the time" ~ 20,
     Q31 == "A good Bit of the time" ~ 40,
     Q31 == "Some of the time" ~ 60,
     Q31 == "A little bit of the time" ~ 80,
     Q31 == "None of the Time" ~ 100,
     TRUE ~ NA_real_
   )) |> 
   rowwise() |> 
   mutate(vitality = mean(c_across(Q23:Q31), na.rm = TRUE))
 



#6.social functioning
 
socila_fn<-sf_data |> 
  
  select(Q20,Q32) |> 
  
  mutate(Q20=case_when(
    
    Q20 == "None" ~ 100,
    Q20 == "Very mild"~ 75,
    Q20 == "Mild" ~ 75,
    Q20 == "Moderate" ~ 50,
    Q20 == "Severe" ~ 25,
    Q20 == "Very Severe" ~0,
    
    TRUE ~ NA_real_
    
  )) |> 
  
  mutate(Q32 = case_when(
    Q32 == "All of the time" ~ 0,
    Q32 == "Most of the time" ~ 25,
    Q32 == "Some of the time" ~ 50,
    Q32 == "A good Bit of the time" ~ 50,
    Q32 == "A little bit of the time" ~ 75,
    Q32 == "None of the Time" ~ 100,
    
    TRUE ~ NA_real_
    
  )) |> 
  
  rowwise() |> 
  mutate(social_fn= mean(c(Q20,Q32), na.rm = TRUE))


  
  
  
  
#7Emotional Role

emotional_role <- sf_data |> 
  select(Q24:Q26, Q28, Q30) |> 
  mutate(across(Q24:Q25, ~case_when(
    . == "All of the time" ~ 0,  
    . == "Most of the time" ~ 20,
    . == "A good Bit of the time" ~ 40,
    . == "Some of the time" ~ 60, 
    . == "A little bit of the time" ~ 80,
    . == "None of the Time" ~ 100,
    TRUE ~ NA_real_
  ))) |> 
 
  mutate(Q26 = case_when(
    Q26 == "All of the time" ~ 100,
    Q26 == "Most of the time" ~ 80,
    Q26 == "A good Bit of the time" ~ 60,
    Q26 == "Some of the time" ~ 40,
    Q26 == "A little bit of the time" ~ 20,
    Q26 == "None of the Time" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q28 = case_when(
    Q28 == "All of the time" ~ 0,
    Q28 == "Most of the time" ~ 20,
    Q28 == "A good Bit of the time" ~ 40,
    Q28 == "Some of the time" ~ 60,
    Q28 == "A little bit of the time" ~ 80,
    Q28 == "None of the Time" ~ 100,
    TRUE ~ NA_real_
  )) |> 
  mutate(Q30 = case_when(
    Q30 == "All of the time" ~ 0,    #need to reverse to number 100 to 0
    Q30 == "Most of the time" ~ 20,
    Q30 == "A good Bit of the time" ~ 60,
    Q30 == "Some of the time" ~ 40,
    Q30 == "A little bit of the time" ~ 20,
    Q30 == "None of the Time" ~ 0,
    TRUE ~ NA_real_
  )) |> 
  
  
  rowwise() |> 
  mutate(emotional_role = mean(c_across(c(Q24,Q25, Q26, Q28, Q30)), na.rm = TRUE))




sum(is.na(sf_data$Q21))  # Check for missing values in Q21
sum(is.na(sf_data$Q22))  # Check for missing values in Q22

#8.pain


pain <- sf_data |> 
  select(Q21, Q22) |> 
  mutate(
    Q21 = case_when(
      Q21 == "None" ~ 100,
      Q21 == "Very Mild" ~ 80,
      Q21 == "Mild" ~ 60,
      Q21 == "Moderate" ~ 40,
      Q21 == "Severe" ~ 20,
      Q21 == "Very Severe" ~ 0,
      TRUE ~ NA_real_  # Handles unmatched cases
    ))|> 
    mutate(Q22 = case_when(
      Q22 == "Not at all" ~ 100,
      Q22 == "A little bit" ~ 75,
      Q22 == "Moderately" ~ 50,
      Q22 == "Quite a bit" ~ 20,
      Q22 == "Extremely" ~ 0,
      TRUE ~ NA_real_  # Handles unmatched cases
  
  ))|> 

  mutate(pain =mean(c(Q21,Q22),na.rm = TRUE))  # Calculate row-wise mean with na.rm = TRUE
 


# Select the demographic data
demographic_data <- data |> 
  select(1:19)


sf_dataa<-cbind(demographic_data,Physical_func,bodily_pain,
                general_health,vitality,socila_fn,
                emotional_role,pain)

   qol<-cbind(demographic_data,sf_data)
   
 

