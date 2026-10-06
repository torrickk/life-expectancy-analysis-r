here::i_am("scripts/analysis.R")


library(rpart)
library(rpart.plot)
library(randomForest)
library(tidyverse)
library(here)
library(gapminder)
library(modelsummary)
library(ggbeeswarm)
library(patchwork)
library(plotly)
library(tsibble)
library(fable)


### ----------------------------
### Task 1

df <- gapminder |>
  filter(year == 1997) |>
  mutate(gdp = gdpPercap * pop)
glimpse(df)

df2 <- gapminder |> 
  filter(year == 2002) |>
  mutate(gdp = gdpPercap * pop)
glimpse(df2)


### ----------------------------
### Task 2

summary(df)
df |>
  select(lifeExp, gdpPercap, pop, gdp) |>
  rename(
    `Life expectancy` = lifeExp,
    `GDP per capita`  = gdpPercap,
    Population        = pop,
    GDP               = gdp
  ) |>
  datasummary_correlation(
    fmt    = 2,
    escape = FALSE,
    title  = "Pairwise Pearson correlations, 1997 (142 countries) \\label{tab:correlations}",
    output = here("output", "var_cor_matrix.tex")
  )

datasummary(
  (`Life expectancy (years)` = lifeExp) +
    (`GDP per capita (USD)`  = gdpPercap) +
    (`Population (millions)` = pop / 1e6) +
    (`GDP (billion USD)`     = gdp / 1e9) ~
    N + Mean + SD + Min + Median + Max,
  data   = df,
  fmt    = 1,
  escape = FALSE,
  title  = "Summary statistics, 1997 (142 countries) \\label{tab:summary}",
  output = here("output", "summary_stats.tex")
)

df |>
  filter(lifeExp >= 70)

df |> 
  arrange(lifeExp) |> 
  select(country, lifeExp) |> 
  slice(c(1, n()))

df |> 
  arrange(gdpPercap) |> 
  select(country, gdpPercap) |> 
  slice(c(1, n()))

df |>
  arrange(pop) |>
  select(country, pop) |>
  slice(c(n() - 2, n() - 1, n()))

df |> 
  arrange(gdp) |> 
  select(country, gdp) |> 
  slice(c(1, n()))



# Life expectancy (target): mean 65.0 years is below the median 69.4, so the
# distribution is skewed left - most countries are clustered around 70+ years
# (65 of 142), while a long tail of low values (21 countries below 50) pulls
# the mean down. The spread is large: SD 11.6 years and a range of 44.6 years,
# from Rwanda (36.1) to Japan (80.7).
#
# GDP per capita: strongly skewed right - mean 9090 USD is almost twice the
# median 4782 USD, and SD (10172) exceeds the mean; 68% of countries are below
# the mean. Range from 312 USD (Congo, Dem. Rep.) to 41283 USD (Norway), i.e.
# a factor of ~130 -> a log scale is natural for this variable.
#
# Population: extremely skewed right - mean 38.8 million vs median 9.7 million,
# driven by a few giants (China 1230 mln, India 959 mln). Smallest country is
# Sao Tome and Principe (0.15 mln).
#
# GDP: the most skewed variable - mean 288.8 bn USD vs median 37.5 bn USD,
# from Sao Tome and Principe (0.2 bn) to the United States (9761 bn). As the
# product of GDP per capita and population it inherits the skewness of both.
#
# Correlations: life expectancy is strongly positively correlated with GDP per
# capita (0.70) - richer countries tend to live longer. It is only weakly
# correlated with total GDP (0.25) and essentially uncorrelated with population
# (0.05), so country size itself does not matter. GDP is correlated with both
# population (0.43) and GDP per capita (0.37) since it is their product, while
# population and GDP per capita are unrelated (-0.05).


### ----------------------------
### Task 3

fig1 <- ggplot(df, aes(x = log(gdpPercap), y = lifeExp)) +
  geom_point(alpha = 0.7, colour = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "log(GDP per capita)",
    y = "Life expectancy (years)",
    title = "Life expectancy and log GDP per capita, 1997"
  ) +
  theme_minimal()
fig1

ggsave(here("output", "lifeExp_and_logGDPperCapita.pdf"), fig1, width = 9, height = 5)


# Explore the countries under/above the regression line
fig1_explore <- ggplot(df, aes(x = log(gdpPercap), y = lifeExp, text = country)) +
  geom_point(alpha = 0.7, colour = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = "log(GDP per capita)",
    y = "Life expectancy (years)",
    title = "Life expectancy and log GDP per capita, 1997"
  ) +
  theme_minimal()
fig1_explore
ggplotly(fig1_explore, tooltip = "text")



fig2 <- ggplot(df, aes(x = reorder(continent, lifeExp, FUN = median), 
                       y = lifeExp,
                       fill = continent)) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE, outlier.shape = NA) +
  geom_beeswarm(alpha = 0.6, size = 0.7, colour = "black", cex = 2) +
  labs(
    x = NULL,
    y = "Life expectancy (years)",
    title = "Distribution of life expectancy by continent, 1997"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
fig2

ggsave(here("output", "distr_lifeExp_by_continent.pdf"), fig2, width = 9, height = 5)


df |> 
  arrange(lifeExp) |> 
  filter(continent == "Asia") |>
  select(country, lifeExp) |> 
  slice(c(1, n()))

df |>
  summarise(
    median = median(lifeExp),
    q25    = quantile(lifeExp, 0.25),
    q75    = quantile(lifeExp, 0.75),
    iqr    = IQR(lifeExp),
    sd     = sd(lifeExp),
    .by    = continent
  )


fig3 <- ggplot(df, aes(x = log(gdpPercap), y = lifeExp, colour = continent)) +
  geom_point(aes(size = pop / 1e6), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  scale_size_area(max_size = 12, breaks = c(10, 100, 500, 1000)) +
  labs(
    x = "log(GDP per capita)",
    y = "Life expectancy (years)",
    colour = "Continent",
    size = "Population (mln)",
    title = "Life expectancy and log GDP per capita by continent, 1997"
  ) +
  theme_minimal()
fig3

ggsave(here("output", "lifeExp_and_logGDPperCapita_by_continent.pdf"), 
       fig3, width = 9, height = 5.5)


p_pop <- ggplot(df, aes(x = log(pop), y = lifeExp)) +
  geom_point(alpha = 0.7, colour = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "black") +
  coord_cartesian(ylim = c(35, 90)) +
  labs(x = "log(population)", y = "Life expectancy (years)") +
  theme_minimal()

p_gdp <- ggplot(df, aes(x = log(gdp), y = lifeExp)) +
  geom_point(alpha = 0.7, colour = "steelblue") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "black") +
  coord_cartesian(ylim = c(35, 90)) +
  labs(x = "log(GDP)", y = NULL) +
  theme_minimal()

fig4 <- (p_gdp | p_pop) +
  plot_annotation(
    title = "Life expectancy against log GDP and log population, 1997",
    tag_levels = "A"
  )
fig4

ggsave(here("output", "lifeExp_and_logPop_logGDP.pdf"), fig4, width = 9, height = 4.5)


lm(lifeExp ~ log(gdp), data = df) |>
  summary()

lm(lifeExp ~ log(gdpPercap), data = df) |>
  summary()

cor(df$lifeExp, log(df$gdpPercap))


### ----------------------------
### Task 4

df |> 
  filter(continent == "Oceania") |>
  arrange(lifeExp) |> 
  select(country, lifeExp) |> 
  slice(c(1, n()))

### Discussion
# Fig 1: strong positive, roughly linear relation between log(GDP per capita)
# and life expectancy (correlation 0.86 on the log scale). Richer countries can
# afford better healthcare, nutrition and sanitation. Because the relation is
# linear in logs, each 1% increase in income adds about the same number of
# years, so in absolute dollars the gains diminish: an extra 1000 USD matters
# far more in a poor country than in a rich one. The countries furthest below
# the line are in Southern Africa (Angola, Botswana, Zambia), hit by HIV/AIDS.

# Fig 2: Africa has by far the lowest median (52.8 years) and the widest spread
# (IQR 11.9 years), consistent with Fig 1, as most low-income countries are
# there. Asia, the Americas and Europe have medians between 70 and 76. Asia is
# the most dispersed of the three (range 38.9 years, similar to Africa's), from
# Japan (80.7, the highest in the data) to Afghanistan (41.8), Yemen and Iraq.
# The Americas' low outliers are Haiti (56.7) and Bolivia (62.0). Europe is the
# most compact (SD 3.1) with no outliers; its 25th percentile (73.0) is above
# both the Americas' median (72.1) and Asia's 75th percentile (72.5). Oceania
# has only 2 countries (Australia, New Zealand) and should be read with caution.

# Fig 3: combines both determinants. The income gradient is positive within
# every continent, so income matters regardless of region. But at the same
# income level African countries lie well below the others (the African line
# is 7-9 years lower at low and middle incomes, the gap narrowing as income
# rises), so continent carries information beyond income,
# e.g. disease burden. The slopes are fairly similar across continents, which
# supports a model with one common slope plus continent dummies (Task 5). Point size shows that population does not line up
# with life expectancy: China and India sit where their income predicts.

# Fig 4: (A) log(GDP) has a moderate positive relation (correlation 0.62), but
# with much more scatter than log(GDP per capita) in Fig 1 (R^2 0.38 vs 0.75).
# (B) the line for log(population) is almost flat (slope 0.48, p = 0.45) and
# the 95% band easily contains a horizontal line, so country size is not
# related to life expectancy. Total GDP mixes income with country size, and only
# the income part matters, so log(GDP) should be the weaker predictor (Task 6).


### ----------------------------
### Task 5

# Goodness-of-fit rows with readable names for the LaTeX tables
gof_rows <- data.frame(
  raw   = c("nobs", "r.squared", "adj.r.squared"),
  clean = c("Observations", "R²", "Adjusted R²"),
  fmt   = c(0, 3, 3)
)

model_1 <- lm(lifeExp ~ log(gdpPercap) + continent, data = df)
summary(model_1)

modelsummary(
  list("Life expectancy" = model_1),
  stars = TRUE,
  coef_rename = c(
    `(Intercept)`       = "Constant",
    `log(gdpPercap)`    = "log(GDP per capita)",
    continentAmericas   = "Americas",
    continentAsia       = "Asia",
    continentEurope     = "Europe",
    continentOceania    = "Oceania"
  ),
  gof_map = gof_rows,
  notes   = "Reference continent: Africa. Standard errors in parentheses.",
  width   = 0.7,
  escape  = FALSE,
  title   = "OLS regression of life expectancy on log GDP per capita and continent, 1997 \\label{tab:model1}",
  output  = here("output", "lifeExp_logGDPperCapita_modelsummary.tex")
)

### Interpretation
# Significance: all coefficients are statistically significant. log(GDP per
# capita) and the Americas, Asia and Europe dummies at the 0.1% level
# (p < 0.001), Oceania only at the 5% level (p = 0.018) - with just 2 countries
# its estimate is imprecise (SE 3.8).
#
# log(GDP per capita): 5.60. Since GDP per capita is logged, a 1% increase in
# GDP per capita is associated with 5.60/100 = 0.056 more years of life
# expectancy, holding continent fixed.
#
# Continent: Africa is the reference category, so each coefficient is the
# difference in life expectancy relative to an African country with the SAME
# GDP per capita:
#   Americas +9.1 years, Asia +8.0 years, Europe +8.7 years, Oceania +9.1 years.
# All are positive: at equal income, Africa lags every other continent by about
# 8-9 years (e.g. HIV/AIDS, disease burden). The non-African continents differ
# little from each other once income is controlled for - the large raw gaps in
# Fig 2 (e.g. Europe vs Africa 23 years in medians) are mostly explained by
# income.
#
# Constant: 12.6 = predicted life expectancy of an African country with
# log(GDP per capita) = 0, i.e. 1 USD per person - far outside the data, so it
# has no meaningful interpretation.
#
# Fit: R^2 = 0.82, the model explains 82% of the cross-country variation in
# life expectancy (vs 0.75 with log(GDP per capita) alone).
# Results are associations from one cross-section, not causal effects.


### ----------------------------
### Task 6

model_2 <- lm(lifeExp ~ log(gdp) + continent, data = df)
summary(model_2)

modelsummary(
  list("Life expectancy" = model_2),
  stars = TRUE,
  coef_rename = c(
    `(Intercept)`       = "Constant",
    `log(gdp)`          = "log(GDP)",
    continentAmericas   = "Americas",
    continentAsia       = "Asia",
    continentEurope     = "Europe",
    continentOceania    = "Oceania"
  ),
  gof_map = gof_rows,
  notes   = "Reference continent: Africa. Standard errors in parentheses.",
  width   = 0.7,
  escape  = FALSE,
  title   = "OLS regression of life expectancy on log GDP and continent, 1997 \\label{tab:model2}",
  output  = here("output", "lifeExp_logGDP_modelsummary.tex")
)

### Interpretation
# Significance: all coefficients are statistically significant. log(GDP) and
# all four continent dummies at the 0.1% level (p < 0.001), the constant at the
# 1% level (p = 0.009).
#
# log(GDP): 1.35. A 1% increase in total GDP is associated with 1.35/100 =
# 0.0135 more years of life expectancy, holding continent fixed.
# The effect is positive but only about a quarter of the GDP per capita effect
# (5.60). Reason: log(GDP) = log(GDP per capita) + log(population), so total
# GDP mixes income (which matters) with country size (which does not, Fig 4B).
# A large GDP can mean a rich country or just a big one (China, India), so the
# coefficient is diluted.
#
# Continent: Africa is again the reference. Relative to an African country with
# the SAME total GDP: Americas +14.7 years, Asia +11.2, Europe +18.1, Oceania
# +20.4. These gaps are much larger than in model 1 (8-9 years), because
# log(GDP) is a poor measure of living standards: the dummies now absorb income
# differences that log(GDP) fails to capture (e.g. Europe is rich per person).
# Oceania becomes significant at the 0.1% level for the same reason: its two
# countries are very rich per person, which log(GDP) does not reflect.
#
# Constant: 22.5 = predicted life expectancy of an African country with a total
# GDP of 1 USD - meaningless, far outside the data.
#
# Fit: R^2 = 0.65 vs 0.82 in model 1 (adjusted 0.64 vs 0.82). Replacing GDP per
# capita by total GDP clearly worsens the fit: GDP per capita is the better
# explanatory variable for life expectancy.


### ----------------------------
### Task 7

pred_1 <- predict(model_1, newdata = df2)

mse_1 <- mean((df2$lifeExp - pred_1)^2)
mse_1
sqrt(mse_1)

df2 |>
  mutate(prediction = pred_1, error = lifeExp - prediction,
         change = lifeExp - df$lifeExp) |>
  slice_max(abs(error), n = 5) |>
  select(country, lifeExp, prediction, error, change)


pred_2 <- predict(model_2, newdata = df2)

mse_2 <- mean((df2$lifeExp - pred_2)^2)
mse_2
sqrt(mse_2)

df2 |>
  mutate(prediction = pred_2, error = lifeExp - prediction,
         change = lifeExp - df$lifeExp) |>
  slice_max(abs(error), n = 5) |>
  select(country, lifeExp, prediction, error, change)


### Extra:
tree_model <- rpart(lifeExp ~ gdpPercap + continent, data = df)
pdf(here("output", "decision_tree.pdf"), width = 9, height = 5)
rpart.plot(tree_model, digits = -3)
dev.off()
pred_tree <- predict(tree_model, newdata = df2)
mse_tree <- mean((df2$lifeExp - pred_tree)^2)
mse_tree
sqrt(mse_tree)


set.seed(256)
rf_model <- randomForest(lifeExp ~ gdpPercap + continent, data = df)
pred_rf <- predict(rf_model, newdata = df2)
mse_rf <- mean((df2$lifeExp - pred_rf)^2)
mse_rf
sqrt(mse_rf)


mse_table <- tibble(
  Model = c("(1) OLS: log(GDP per capita) + continent",
            "(2) OLS: log(GDP) + continent",
            "Decision tree",
            "Random forest"),
  MSE   = c(mse_1, mse_2, mse_tree, mse_rf)
) |>
  mutate(RMSE = sqrt(MSE))

mse_table

datasummary_df(
  mse_table,
  fmt    = 2,
  align  = "lrr",
  escape = FALSE,
  title  = "Test MSE: models trained on 1997, evaluated on 2002 \\label{tab:mse}",
  output = here("output", "mse_comparison.tex")
)

### Discussion
# Setup: the models from Tasks 5 and 6 are trained on the 1997 data (df) and
# used to predict life expectancy in 2002 (df2) from each country's 2002 GDP
# (per capita or total) and continent. Accuracy is measured by the mean squared
# error, MSE = mean((actual - predicted)^2).
#
# Comparison: model 1 (log GDP per capita) predicts considerably better, with a
# test MSE of 32.5 vs 50.8 for model 2 (log GDP), i.e. about 36% lower. In
# years, its typical error (RMSE) is 5.7 vs 7.1 years. This matches the
# in-sample results (R^2 0.82 vs 0.65): GDP per capita is the better predictor.
#
# The largest errors come from Southern African countries whose life
# expectancy fell between 1997 and 2002 (HIV/AIDS), e.g. Botswana and
# Swaziland.
#
# Extra - tree-based models. As a benchmark for model 1, a decision tree and a
# random forest (500 trees) are trained on the same data and the same
# information (lifeExp ~ gdpPercap + continent; no log needed, as tree splits
# do not depend on the scale of a variable). Results are in mse_table.
#
# - The single tree (6 leaves, see rpart.plot) is slightly worse than model 1:
#   it predicts only 6 distinct values, a coarse step function of income. Its
#   first split is GDP per capita at ~2300 USD, and continent = Africa appears
#   in two later splits - the same two determinants as in the regression.
# - The random forest is the best model, ~6% lower MSE than model 1. Averaging
#   many trees smooths the steps, and the forest can capture non-linearities
#   and interactions (e.g. different income effects by continent, Fig 3)
#   without specifying them.
# - The gain is small: model 1 already captures the main pattern, and with 142
#   countries there is little more to learn. The forest's largest errors are
#   the same countries (Swaziland, Botswana, Afghanistan, Angola, Zambia) -
#   no model trained on 1997 can foresee the 2002 HIV/AIDS declines.
# - Trade-off: the forest predicts slightly better but has no coefficients,
#   so it cannot be interpreted like the regression (e.g. "1% more income ->
#   0.056 more years").


### ----------------------------
### Task 8

ts_df <- gapminder |>
  filter(country %in% c("Austria", "United States")) |>
  select(country, year, lifeExp) |>
  as_tsibble(index = year, key = country)

print(ts_df, n = 24)

fig5 <- ggplot(ts_df, aes(x = year, y = lifeExp, colour = country)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1952, 2007, 5)) +
  labs(
    x = "Year",
    y = "Life expectancy (years)",
    colour = NULL,
    title = "Life expectancy in Austria and the United States, 1952-2007"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
fig5

ggsave(here("output", "lifeExp_Austria_US.pdf"), fig5, width = 9, height = 5)

# Comparison:
# Both series rise steadily over the whole period. The US starts higher
# (68.4 vs 66.8 years in 1952, a gap of 1.6 years) and stays ahead for four
# decades, but Austria improves faster: +13.0 years over 1952-2007 vs +9.8 for
# the US. The gap narrows to almost zero in 1987-1992 (0.1 years), Austria
# overtakes the US between 1992 and 1997, and the gap widens afterwards.
# In 2007 Austria is 1.6 years ahead (79.8 vs 78.2) - a full reversal of the
# 1952 position. US growth slows noticeably after 1980.


austria_full  <- ts_df |> filter(country == "Austria")
austria_train <- austria_full |> filter(year <= 2002)

fit <- austria_train |>
  model(
    drift = RW(lifeExp ~ drift())
  )

fc <- forecast(fit, h = 1)

predicted_2007 <- fc$.mean
actual_2007    <- austria_full |> filter(year == 2007) |> pull(lifeExp)
predicted_2007
actual_2007
actual_2007 - predicted_2007

fabletools::accuracy(fc, ts_df)

austria_recent <- austria_full |> filter(year >= 1982)

fig6 <- fc |>
  autoplot(austria_recent) +
  geom_point(data = austria_recent, aes(x = year, y = lifeExp), size = 2) +
  scale_x_continuous(breaks = seq(1982, 2007, 5)) +
  labs(
    x = "Year",
    y = "Life expectancy (years)",
    title = "Austria: random walk with drift forecast for 2007 vs actual"
  ) +
  guides(
    colour_ramp = guide_legend(title = "Prediction interval"),
    fill_ramp   = guide_legend(title = "Prediction interval")
  ) +
  theme_minimal()
fig6

ggsave(here("output", "forecast_Austria_2007.pdf"), fig6, width = 9, height = 5)

# Forecast discussion:
# Austria's series has a steady upward trend and no seasonality, so a simple
# trend method is appropriate. I use a random walk with drift, trained on the
# 11 observations from 1952 to 2002: the forecast for 2007 is the last value
# (78.98 in 2002) plus the average 5-year change over 1952-2002
# ((78.98 - 66.80) / 10 = +1.22 years).
# It predicts 80.20 years vs the actual 79.83: an error of -0.37 years, i.e.
# the forecast is 0.5% too high. The actual value lies well inside the 95%
# prediction interval (see figure).
# The forecast slightly overshoots because the last increase (1997-2002) was
# unusually large.
# Overall, because Austrian life expectancy grows so regularly, even this
# simple method forecasts it within less than half a year.

