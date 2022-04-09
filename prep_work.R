library(readr)
library(tidyverse)
library(VIM)
wnba <- read_csv("wnba-player-stats.csv")
wnba_filtered <- wnba %>% filter(year_ID >= 2000, MP > 100) %>% 
  mutate(decade = case_when(
           year_ID >=2000 & year_ID < 2010 ~ "2000s",
           year_ID >=2010 ~ "2010s"), 
         court_position = case_when(
           Pos == "G" ~ "Back",
           Pos == "G-F" ~ "Back",
           Pos == "F" ~ "Front",
           Pos == "C" ~ "Front",
           Pos == "F-G" ~ "Front",
           Pos == "F-C" ~ "Front",
           Pos == "C-F" ~ "Front"),
         TRB_pct_scaled = scale(TRB_pct),
         AST_pct_scaled = scale(AST_pct),
         BLK_pct_scaled = scale(BLK_pct),
         ThrPAr_scaled = scale(ThrPAr))
wnba_imputed <- kNN(wnba_filtered, variable = "court_position", dist_var = c("TRB_pct_scaled", "AST_pct_scaled", "BLK_pct_scaled", "ThrPAr_scaled"))
wnba_final <- wnba_imputed %>% select(PER, decade, court_position, Tm_Net_Rtg, Age)
wnba_final %>% is.na() %>% colSums()
wnba_full_mod <- lm(PER ~ decade*court_position + Tm_Net_Rtg + Age, data = wnba_final)
summary(wnba_full_mod)
wnba_reduced_mod1 <- lm(PER ~ decade + court_position + Tm_Net_Rtg + Age, data = wnba_final)
summary(wnba_reduced_mod1)
