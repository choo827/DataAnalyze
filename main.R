library(tidyverse)
library(ggchicklet)
library(cowplot)
library(waffle)
library(hrbrthemes)
library(ggalt)
library(GGally)
library(rwa)


df_2021 <- read.csv("/Users/choo827/DataspellProjects/Test/world-happiness-report-2021.csv")
df_2023 <- read.csv("/Users/choo827/DataspellProjects/Test/WHR2023.csv")

ifelse(df_2021$Country.name %in% df_2023$Country.name, "Yes", "❌")


id <- (1:5)
name <- c("Jung", "Lee", "Kim", "Hwang", "Hong")
df1 <- data.frame(id, name)

id <- (1:4)
name <- c("Jung", "Kim", "Hwang", "Hong")
df2 <- data.frame(id, name)

ifelse(df1$name %in% df2$name, "Yes", "No")

ifelse(df_2023$Country.name %in% df_2021$Country.name, "Yes", "❌")


# Turkey -> 106 Turkiye

df_new2023 <- read.csv("/Users/choo827/DataspellProjects/Test/new_WHR2023.csv")
head(df_new2023)

fig <- function(x, y) {
  options(repr.plot.width = x, repr.plot.height = y)
}

dimensions <- c('Ladder.score', 'Logged.GDP.per.capita', 'Social.support', 'Healthy.life.expectancy', 'Freedom.to.make.life.choices', 'Generosity', 'Perceptions.of.corruption')

# map country to regions
country_region_dict <- df_new2023 %>%
  select(country = Country.name, region = Regional.indicator) %>%
  unique()

df_new2023_long <- df_new2023 %>%
  select(country = Country.name, all_of(dimensions)) %>%
  mutate(absence_of_corruption = 1 - Perceptions.of.corruption) %>%
  pivot_longer(cols = c(all_of(dimensions), 'absence_of_corruption'), names_to = 'dimension', values_to = 'score') %>%
  filter(dimension != "Perceptions.of.corruption")

df_new2023_tranformed <- df_new2023_long %>%
  group_by(dimension) %>%
  mutate(min_value = min(score),
         max_value = max(score)) %>%
  mutate(score_pct = (score - min_value) / (max_value - min_value)) %>%
  ungroup()

# get top 10
df_new2023_top10 <- df_new2023_tranformed %>%
  filter(dimension == "Ladder.score") %>%
  slice_max(score, n = 10) %>%
  mutate(cat = 'top_10',
         country_rank = rank(-score),
         country_label = paste0(country, ' (', country_rank, ')'))

head(df_new2023, 10)


# get bottom 10
df_new2023_bottom10 <- df_new2023_tranformed %>%
  filter(dimension == "Ladder.score") %>%
  mutate(country_rank = rank(score),
         country_label = paste0(country, ' (', country_rank, ')')) %>%
  slice_min(score, n = 10) %>%
  mutate(cat = 'bottom_10')

fig(12, 8)

ggplot(df_new2023_top10, aes(x = reorder(country_label, score))) +
  geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_text(aes(y = score), label = round(df_new2023_top10$score, 2), nudge_y = 0.4, size = 6) +
  scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
  scale_fill_gradient2(low = 'black', high = '#7FB185', mid = 'white', midpoint = 5) +
  coord_flip() +
  labs(y = "Best possible life = 10", x = '',
       title = "10 Happiest Countries in the World",
       subtitle = "Nine of the happinest countries are in Europe",
       caption = "Source: The World Happiness Report 2023") +
  theme_ipsum(grid = '') +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size = 15, color = '#555955'),
        axis.text.y = element_text(size = 19, color = 'black'),
        axis.text.x = element_blank(),
        legend.position = 'None')


fig(12, 8)

ggplot(df_new2023_bottom10, aes(x = reorder(country_label, score))) +
  geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_text(aes(y = score), label = round(df_new2023_bottom10$score, 2), nudge_y = 0.4, size = 6) +
  scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
  scale_fill_gradient2(low = 'black', high = '#7FB185', mid = 'white', midpoint = 5) +
  coord_flip() +
  labs(y = "Best possible life = 10", x = '',
       title = "10 Saddest Countries in the World",
       subtitle = "Countries torn by poverty and war",
       caption = "Source: The World Happiness Report 2023") +
  theme_ipsum(grid = '') +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size = 15, color = '#555955'),
        axis.text.y = element_text(size = 19, color = 'black'),
        axis.text.x = element_blank(),
        legend.position = 'None')


df_waffle <- df_new2023_tranformed %>%
  filter(dimension == 'Ladder.score') %>%
  left_join(country_region_dict, by = 'country') %>%
  mutate(score_bin = cut(score, seq(2, 8, 1), right = FALSE)) %>%
  group_by(region) %>%
  mutate(region_avg = mean(score)) %>%
  ungroup() %>%
  mutate(region = reorder(region, region_avg)) %>%
  count(region, score_bin) %>%
  arrange(score_bin, n)

score_levels <- levels(df_waffle$score_bin)

fig(18, 12)

pal <- colorRampPalette(c("black", "white", "#7FB185"))

ggplot(df_waffle, aes(fill = score_bin, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 6, flip = TRUE, na.rm = FALSE) +
  facet_wrap(~region, nrow = 2, strip.position = "bottom", labeller = label_wrap_gen()) +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 6, # make this multiplyer the same as n_rows
                     expand = c(0, 0.2),
                     limits = c(0, 7)) +
  scale_fill_manual(
    name = "Green indicates more happiness",
    values = pal(6),
    labels = c('', '', '', '', '', '')
  ) +
  coord_equal() +
  labs(
    title = "Happiness by World Region",
    subtitle = "Happiest regions: North America and Western Europe.",
    caption = "1 square = a country\n\nSource: The World Happiness Report 2021",
    y = "Country Count"
  ) +
  theme_ipsum(grid = '') +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 18, hjust = 0.5),
        legend.position = c(0.85, 1),
        legend.box = 'horizontal',
        legend.key.size = unit(1, 'cm'),
        legend.title = element_text(size = 18, hjust = 1),
        legend.text = element_text(size = 16)) +
  guides(fill = guide_legend(reverse = FALSE, nrow = 1, byrow = TRUE))


df_cor <- df_new2023 %>%
  select(corruption = Perceptions.of.corruption,
         generosity = Generosity,
         freedom = Freedom.to.make.life.choices,
         life_expectancy = Healthy.life.expectancy,
         social_support = Social.support,
         GDP_per_capita = Logged.GDP.per.capita,
         happiness = Ladder.score
  )

fig(9, 8)
ggcorr(df_cor,
       method = c("everything", "pearson"),
       size = 6, hjust = 0.77,
       low = '#edae52', mid = 'white', high = "#7FB185",
       label = TRUE, label_size = 6,
       layout.exp = 1) +
  labs(title = 'Correlation Matrix',
       subtitle = 'Happiness most strongly correlates with (1) wealth (GDP)\n(2) health, (3) social support, and (4) freedom') +
  theme_ipsum() +
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size = 18),
        legend.text = element_text(size = 18))


predictors <- colnames(df_cor)[1:6]
outcome <- 'happiness'
rwa <- rwa(df_cor,
           outcome = outcome,
           predictors = predictors,
           applysigns = FALSE)

rsquare <- rwa$rsquare
(relative_weight <- rwa$result)
n <- rwa$n

fig(12,6)

ggplot(relative_weight, aes(x = reorder(Variables, Rescaled.RelWeight), y = Rescaled.RelWeight, fill = Rescaled.RelWeight)) +
  geom_chicklet(width = 0.618, radius = grid::unit(12, "pt")) +
  geom_text(label = paste0(round(relative_weight$Rescaled.RelWeight,0),"%"), nudge_y = 1, size = 6) +
  scale_y_continuous(expand = c(0, 0.2), limits= c(0, 30)) +
  scale_fill_gradient(low = 'white', high = '#7FB185') +
  coord_flip() +
  labs(y="Rescaled Relative Weights", x = '',
       title="Variable importance estimates",
       subtitle="Top 3 important factors: (1) GDP, (2) Social support (3) Life expectancy",
       caption=paste0("Note: Recaled Relative Weights sum to 100%. n = ",n, '. R-squared:', round(rsquare,2))) +
  theme_ipsum(grid = '') +
  theme(plot.title = element_text(size=24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size= 18),
        axis.text.y = element_text(size = 18, color = 'black'),
        axis.text.x = element_blank(),
        legend.position = 'None')


