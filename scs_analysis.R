#Herd-level SCS summary table (a standard udder health KPI report), 
#Mixed model of SCS trend across DIM.
suppressMessages({ 
library(dplyr); library(lme4); library(ggplot2); library(broom.mixed) 
})
getwd ()
df <- read.csv("outputs/tables/testday_clean_long.csv", stringsAsFactors = FALSE)
dir()
## ---- 1. Herd-level udder health KPI table ----
herd_scs <- df %>%
  group_by(herd_id, region, system) %>%
  summarise(
    n_test_days = n(),
    mean_scc = mean(scc_cells_ml),
    median_scc = median(scc_cells_ml),
    mean_scs = mean(scs),
    pct_high_scc_tests = mean(high_scc_flag) * 100,   # % test-days >=200,000 cells/mL
    .groups = "drop"
  ) %>%
  arrange(desc(pct_high_scc_tests))

write.csv(herd_scs, "outputs/tables/herd_udder_health_KPI_summary.csv", row.names = FALSE)

#Mixed model: does SCS rise with days in milk / parity? ----
df$parity_group <- factor(ifelse(df$parity == 1, "Primiparous","Multiparous"),
                           levels = c("Primiparous","Multiparous"))
scs_model <- lmer(scs ~ dim + parity_group + (1 | herd_id) + (1 | cow_id), data = df)
print(summary(scs_model))

scs_fixed <- broom.mixed::tidy(scs_model, effects = "fixed", conf.int = TRUE)
write.csv(scs_fixed, "outputs/tables/scs_trend_model_fixed_effects.csv", row.names = FALSE)


