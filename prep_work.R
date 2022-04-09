library(readr)
library(tidyverse)
library(VIM)
library(moderndive)
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
wnba_final <- wnba_imputed %>% select(TS_pct, decade, court_position, Tm_Net_Rtg, Age)
wnba_final %>% is.na() %>% colSums()
wnba_full_mod <- lm(TS_pct ~ decade*court_position + Tm_Net_Rtg + Age, data = wnba_final)
summary(wnba_full_mod)
wnba_reduced_mod1 <- lm(TS_pct ~ decade + court_position + Tm_Net_Rtg + Age, data = wnba_final)
summary(wnba_reduced_mod1)
anova(wnba_reduced_mod1, wnba_full_mod)
wnba_reduced_mod2 <- lm(TS_pct ~ decade + court_position, data = wnba_final)
summary(wnba_reduced_mod2)
anova(wnba_reduced_mod2, wnba_reduced_mod1)
wnba_reduced_mod3 <- lm(TS_pct ~ decade + court_position + Tm_Net_Rtg, data = wnba_final)
summary(wnba_reduced_mod3)
anova(wnba_reduced_mod2, wnba_reduced_mod3, wnba_reduced_mod1)
final_mod <- lm(TS_pct ~ decade + court_position + Tm_Net_Rtg, data = wnba_final)
summary(final_mod)
wnba_final_graph <- wnba_final %>% mutate(court_position_decade = paste(court_position, decade, sep = "_"))

ggplot(wnba_final_graph, aes(x = Tm_Net_Rtg, y = TS_pct, color = court_position_decade)) +
  geom_point(alpha = 0.5) + 
  geom_abline(slope = final_mod$coefficients[4], intercept = final_mod$coefficients[1], color = "red", lwd = 1.25) +
  geom_abline(slope = final_mod$coefficients[4], intercept = final_mod$coefficients[1] + final_mod$coefficients[2], color = "darkgreen", lwd = 1.25) + 
  geom_abline(slope = final_mod$coefficients[4], intercept = final_mod$coefficients[1] + final_mod$coefficients[3], color = "cadetblue1", lwd = 1.25) +
  geom_abline(slope = final_mod$coefficients[4], intercept = final_mod$coefficients[1] + final_mod$coefficients[2] + final_mod$coefficients[3], color = "purple", lwd = 1.25) +
  scale_color_discrete(name = "Court Position / Decade", labels = c("Back Court / 2000s", "Back Court / 2010s", "Front Court / 2000s", "Front Court / 2010s")) +
  xlab("Team Net Efficiency Rating") + 
  ylab("Player True Shooting Percentage")
