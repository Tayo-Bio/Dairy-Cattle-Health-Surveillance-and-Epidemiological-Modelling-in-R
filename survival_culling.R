# survival_culling
#Time-to-event analysis of culling risk during lactation.
suppressMessages({
  library(survival)
  library(survminer)
  library(dplyr)
})

lact <- read.csv("C:/Users/HP/Documents/Cow_control_project/cowcontrol_pipeline/
                 cowcontrol/outputs/tables/lactation_summary.csv", 
                 stringsAsFactors = FALSE)


lact <- lact %>%
  mutate(
    event = as.integer(culled == "True" | culled == TRUE),
    parity_group = ifelse(parity == 1, "Primiparous", "Multiparous")
  ) %>%
  filter(!is.na(days_in_lactation), days_in_lactation > 0)

surv_obj <- Surv(time = lact$days_in_lactation, event = lact$event)

cox_fit <- coxph(surv_obj ~ parity_group + breed + mean_scs + system, data = lact)
print(summary(cox_fit))

cox_tab <- broom::tidy(cox_fit, exponentiate = TRUE, conf.int = TRUE)

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)
write.csv(cox_tab, "outputs/tables/coxph_culling_hazard_ratios.csv", row.names = FALSE)

# Proportional-hazards assumption check
ph_test <- cox.zph(cox_fit)
write.csv(as.data.frame(ph_test$table), "outputs/tables/coxph_proportional_hazards_test.csv")

## Kaplan-Meier curve by parity group
km_fit <- survfit(Surv(days_in_lactation, event) ~ parity_group, data = lact)
km_plot <- ggsurvplot(km_fit, data = lact, risk.table = TRUE, pval = TRUE,
                       conf.int = TRUE,
                       title = "Kaplan-Meier: probability of remaining in herd by parity group",
                       xlab = "Days since calving", legend.title = "Parity group")

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
png("outputs/figures/kaplan_meier_culling_by_parity.png", width = 1600, height = 1400, res = 200)
print(km_plot)
dev.off()
