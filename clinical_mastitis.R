##Generalized linear mixed model (binomial) for clinical mastitis at test-day level, with a lagged SCS predictor
suppressMessages({
  library(lme4)
  library(broom.mixed)
  library(dplyr)
})

df <- read.csv("outputs/tables/testday_clean_long.csv", stringsAsFactors = FALSE)
df$clinical_mastitis_flag <- as.integer(df$clinical_mastitis_flag == "True" |
                                         df$clinical_mastitis_flag == TRUE)

df <- df %>%
  arrange(cow_id, lact_id, dim) %>%
  group_by(lact_id) %>%
  mutate(scs_lag1 = lag(scs)) %>%
  ungroup() %>%
  filter(!is.na(scs_lag1))

df$parity_group <- factor(ifelse(df$parity == 1, "Primiparous", "Multiparous"),
                           levels = c("Primiparous","Multiparous"))
df$season <- factor(df$season, levels = c("Winter","Spring","Summer","Autumn"))

glmm_fit <- glmer(clinical_mastitis_flag ~ scs_lag1 + parity_group + season +
                     (1 | herd_id) + (1 | cow_id),
                   data = df, family = binomial,
                   control = glmerControl(optimizer = "bobyqa"))

print(summary(glmm_fit))

or_table <- broom.mixed::tidy(glmm_fit, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
