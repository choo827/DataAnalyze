df <- read.csv("/Users/choo827/DataspellProjects/DataAnalyze/new_WHR2023.csv")
glimpse(dataset)
head(dataset)

# 패키지 설치
library(corrplot)
library(ggplot2)

df_order <- df[order(df$Ladder.score),]
ggplot(data = df_order, aes(x = reorder(Country.name, Ladder.score), y = Ladder.score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab('Country') +
  ylab('Happiness')


# boxplot
ggplot(data = df_order, aes(x = reorder(Regional.indicator, Ladder.score), y = Ladder.score)) +
  geom_boxplot(aes(fill = Regional.indicator)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  xlab('Region') +
  ylab('Happiness')

# 선형회귀
# 그래프 분할
par(mfrow = c(3, 2))

# 행복과 1인당 GDP의 관계
lm_fit1 <- lm(df$Ladder.score ~ df$Logged.GDP.per.capita)
plot(df$Logged.GDP.per.capita, df$Ladder.score, xlab = "GDP per capita", ylab = "Happiness")
abline(lm_fit1)

# 행복과 사회적 지원의 관계
lm_fit2 <- lm(df$Ladder.score ~ df$Social.support)
plot(df$Social.support, df$Ladder.score, xlab = "Social support", ylab = "Happiness")
abline(lm_fit2)

# 행복과 기대수명의 관계
lm_fit3 <- lm(df$Ladder.score ~ df$Healthy.life.expectancy)
plot(df$Healthy.life.expectancy, df$Ladder.score, xlab = "Life expectancy", ylab = "Happiness")
abline(lm_fit3)

# 행복과 자유의 관계
lm_fit4 <- lm(df$Ladder.score ~ df$Freedom.to.make.life.choices)
plot(df$Freedom.to.make.life.choices, df$Ladder.score, xlab = "Freedom", ylab = "Happiness")
abline(lm_fit4)

# 행복과 관대함의 관계
lm_fit5 <- lm(df$Ladder.score ~ df$Generosity)
plot(df$Generosity, df$Ladder.score, xlab = "Generosity", ylab = "Happiness")
abline(lm_fit5)

# 행복과 부패지수의 관계
lm_fit6 <- lm(df$Ladder.score ~ df$Perceptions.of.corruption)
plot(df$Perceptions.of.corruption, df$Ladder.score, xlab = "Corruption", ylab = "Happiness")
abline(lm_fit6)

# 중회귀 분석
lm_fit7 <- lm(df$Ladder.score ~ df$Logged.GDP.per.capita +
  df$Social.support +
  df$Healthy.life.expectancy +
  df$Freedom.to.make.life.choices +
  df$Generosity +
  df$Perceptions.of.corruption)
summary(lm_fit7)

par(mfrow = c(2, 2))
plot(lm_fit7)

# 상관계수
# cor(df$Logged.GDP.per.capita, df$Ladder.score)
# cor(df$Social.support, df$Ladder.score)
# cor(df$Healthy.life.expectancy, df$Ladder.score)
# cor(df$Freedom.to.make.life.choices, df$Ladder.score)
# cor(df$Generosity, df$Ladder.score)
# cor(df$Perceptions.of.corruption, df$Ladder.score)

# 상관계수 분석
data <- cor(df[c(3, 7:12)])
cor_label <- c("Happiness", "GDP per capita", "Social support", "Life expectancy", "Freedom", "Generosity", "Corruption")
colnames(data) <- cor_label
rownames(data) <- cor_label
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method = 'color',
         tl.srt = 45, type = "lower",
         col = col(200), addCoef.col = "black", diag = FALSE)


cor.test(df$Ladder.score, df$Logged.GDP.per.capita +
  df$Social.support +
  df$Healthy.life.expectancy +
  df$Freedom.to.make.life.choices +
  df$Generosity +
  df$Perceptions.of.corruption)경

cor.test(df$Ladder.score, df$Social.support)




