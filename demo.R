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
# plot
tmp <- soccer %>%
  mutate(position = forcats::fct_infreq(position)) %>%
  mutate(position = factor(position, rev(levels(position)))) %>%
  mutate(position_group = forcats::fct_collapse(position,
                                                Forward = c("Center Forward", "Right Winger", "Left Winger"),
                                                Midfielder = c("Defensive Midfielder", "Attacking Midfielder", 
                                                               "Right Midfielder", "Center Midfielder", "Left Midfielder"),
                                                Defender = c("Center Back", "Right Fullback", "Left Fullback"),
                                                Goalkeeper = c("Goalkeeper")
  )) %>%
  group_by(playerShort, position, position_group) %>%
  summarise(redCards = sum(redCards), games = sum(games), .groups = "keep")
tmp_group <- tmp %>%
  group_by(position, position_group) %>%
  summarise(medianp = median(redCards / games), n = n(), .groups = "keep")
ggplot(tmp, aes(y = position, x =redCards / games, colour = position_group)) +
  theme_minimal() +
  geom_violin(alpha = 0.25, show.legend = FALSE) +
  geom_point(aes(x = medianp), data = tmp_group, size = 2, shape = 15, show.legend = FALSE) +
  geom_text(aes(x = 0.04, label = paste0(position, " (n=", format(n, big.mark = ",") , ")")),
            size = 3, hjust = 1, vjust = 0.5, nudge_y = 0, nudge_x =- 0.001, show.legend = FALSE,
            data = tmp_group) +
  scale_x_continuous(position = "top", limits = c(0, 0.04)) +
  scale_colour_brewer(palette = "Set2") +
  facet_grid("position_group", scales = "free_y", space = "free_y", switch = "y",
             labeller = as_labeller(~ paste0('<span style="color:',
                                             RColorBrewer::brewer.pal(4, "Set2")[1:4], ';">',.x, '</span>'))) +
  labs(y = NULL, x = "Proportion of games given a red card", 
       caption = stringr::str_wrap(paste0(
         "Figure 1. The violins display the distributions of proportions of games",
         " where the player received a red card and the squares indicate the median",
         " value for each position. They suggest that the likelihood of receiving",
         " a red card in a soccer match may vary by position."), width = 95)) +
  theme(axis.text.y = element_blank(),
        axis.title.x.top = element_text(color = "#555555", size = 10),
        axis.text.x.top = element_text(color = "#555555", size = 10),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(color = "#555555", size = 10),
        plot.caption.position = "plot",
        strip.text.y.left = ggtext::element_markdown(angle = 0, size = 12))
ggsave("main/www/positions.png", width = 2160, height = 1620, units = "px",
       dpi = 300)