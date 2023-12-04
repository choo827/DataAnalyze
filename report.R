df <- read.csv("/Users/choo827/DataspellProjects/DataAnalyze/new_WHR2023.csv")
glimpse(dataset)
head(dataset)

# 선형회귀

# 그래프 분할
par(mfrow = c(3, 2))

# 행복과 1인당 GDP의 관계
lm_fit1 <- lm(df$Ladder.score ~ df$Logged.GDP.per.capita)
plot(df$Logged.GDP.per.capita, df$Ladder.score)
abline(lm_fit1)

# 행복과 사회적 지원의 관계
lm_fit2 <- lm(df$Ladder.score ~ df$Social.support)
plot(df$Social.support, df$Ladder.score)
abline(lm_fit2)

# 행복과 기대수명의 관계
lm_fit3 <- lm(df$Ladder.score ~ df$Healthy.life.expectancy)
plot(df$Healthy.life.expectancy, df$Ladder.score)
abline(lm_fit3)

# 행복과 자유의 관계
lm_fit4 <- lm(df$Ladder.score ~ df$Freedom.to.make.life.choices)
plot(df$Freedom.to.make.life.choices, df$Ladder.score)
abline(lm_fit4)

# 행복과 관대함의 관계
lm_fit5 <- lm(df$Ladder.score ~ df$Generosity)
plot(df$Generosity, df$Ladder.score)
abline(lm_fit5)

# 행복과 부패지수의 관계
lm_fit6 <- lm(df$Ladder.score ~ df$Perceptions.of.corruption)
plot(df$Perceptions.of.corruption, df$Ladder.score)
abline(lm_fit6)


lm_fit7 <- lm(df$Ladder.score ~ df$Logged.GDP.per.capita +
  df$Social.support +
  df$Healthy.life.expectancy +
  df$Freedom.to.make.life.choices +
  df$Generosity +
  df$Perceptions.of.corruption)
summary(lm_fit7)
plot(lm_fit7)

cor(df$Logged.GDP.per.capita, df$Ladder.score)
cor(df$Social.support, df$Ladder.score)
cor(df$Healthy.life.expectancy, df$Ladder.score)
cor(df$Freedom.to.make.life.choices, df$Ladder.score)
cor(df$Generosity, df$Ladder.score)
cor(df$Perceptions.of.corruption, df$Ladder.score)


data <- df[c(3, 7:11)]
cor(data)

install.packages('corrplot')
library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(data), method = 'color',
         tl.srt = 45, type = "lower",
         col = col(200), addCoef.col = "black", diag = FALSE)