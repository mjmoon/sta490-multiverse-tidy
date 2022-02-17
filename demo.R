library(tidyverse)
soccer <- read.csv("https://raw.githubusercontent.com/mjmoon/sta490-soccer/main/data/soccer.csv")
soccer["skintone"] <- (soccer["rater1"] + soccer["rater2"]) / 2
# 02-logistic-regression
model <- formula(cbind(redCards, games - redCards) ~ skintone)
fit <- glm(model, family = binomial(link = "logit"), data = soccer)
res <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE, conf.level = .95)
save(fit, res, file = "main/02-logistic-regression.RData")
# 03-multiverse-anlaysis
soccer <- soccer %>%
  mutate(
    position_common = forcats::fct_collapse( 
      # forcats::fct_collapse() is a convenient helper function for regrouping factors
      position, 
      Forward = c("Center Forward", "Right Winger", "Left Winger"), 
      Midfielder = c("Defensive Midfielder", "Attacking Midfielder", 
                     "Right Midfielder", "Center Midfielder", "Left Midfielder"), 
      Defender = c("Center Back", "Right Fullback", "Left Fullback"), 
      Goalkeeper = c("Goalkeeper")), # groups position into 4 groups
    position_fld_vs_gk = if_else(position == "Goalkeeper", "Goalkeeper", "Fielder") # group all fielders
  )
model_1 <- formula(cbind(redCards, games - redCards) ~ skintone + position)
model_2 <- formula(cbind(redCards, games - redCards) ~ skintone + position_common)
model_3 <- formula(cbind(redCards, games - redCards) ~ skintone + position_fld_vs_gk)
model_4 <- formula(cbind(redCards, games - redCards) ~ skintone)
models <- list(model_1, model_2, model_3, model_4)
multiverse_fit <- lapply(models, function(x) {
  glm(x, family = binomial(link = "logit"), data = soccer)
})
multires <- lapply(multiverse_fit, function(x) {
  broom::tidy(x, exponentiate = TRUE, conf.int = TRUE, conf.level = .95) %>%
    filter(term == "skintone") # filter for skintone
})
multiverse_res <- bind_rows(multires) # join the results
save(multiverse_fit, multiverse_res, 
     file = "main/03-multiverse-analysis.RData")
