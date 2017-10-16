#' ---
#' title: "Quickstart Guide to Models"
#' output:
#'   html_document:
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 2
#'     toc_float:
#'       collapsed: no
#' ---
#' 
#' <style>
#' h1{font-weight: 400;}
#' </style>
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE)
set.seed(76)
options(digits = 4)

#' 
#' 
#' 
#' 
#' # Binary outcomes
#' 
## ------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(okcupiddata)

#' 
## ---- cache=TRUE---------------------------------------------------------
profiles <- profiles %>%
  as_tibble() %>%
  # Create binary outcome variable y:
  mutate(y = ifelse(sex=="f", 1, 0)) %>%
  # Remove heights below 50 inches:
  filter(height>50) %>%
  # Add ID column:
  mutate(ID = 1:n()) %>%
  select(ID, sex, y, height) %>%
  # Remove all rows with NA missing values:
  na.omit()
profiles_train <- profiles %>% 
  sample_frac(0.5)
profiles_test <- profiles %>% 
  anti_join(profiles_train, by="ID")

#' 
#' ## Logistic regression via `glm`
#' 
#' ### Fit/train model
#' 
## ---- cache=TRUE---------------------------------------------------------
model_formula <- as.formula(y~height)
model_logistic <- glm(model_formula, data=profiles_train, family="binomial")

# 1.a) Extract regression table in tidy format
model_logistic %>% 
  broom::tidy(conf.int=TRUE)

# 1.b) Extract point-by-point info in tidy format
model_logistic %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  sample_n(5)

# 1.c) Extract summary stats info in tidy format
model_logistic %>% 
  broom::glance()

#' 
#' ### Predict outcomes for test data
#' 
## ---- cache=TRUE---------------------------------------------------------
# 2. Make predictions on test data
# Method 1:
# -input: profiles_test is a data frame
# -output: log_odds_hat is a vector of log odds
log_odds_hat <- predict(model_logistic, newdata=profiles_test)
p_hat <- 1/(1 + exp(-log_odds_hat))

# Method 2: All new variables start with a period
model_logistic %>% 
  broom::augment(newdata=profiles_test) %>% 
  as_tibble() %>% 
  mutate(p_hat = 1/(1 + exp(-.fitted))) %>% 
  sample_n(5)

#' 
#' ### Plot
#' 
## ---- cache=TRUE---------------------------------------------------------
fitted_model <- model_logistic %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  mutate(p_hat = 1/(1 + exp(-.fitted)))
predictions <- model_logistic %>% 
  broom::augment(newdata=profiles_test) %>% 
  mutate(p_hat = 1/(1 + exp(-.fitted)))

# Logistic regression is fitted in log-odds(p) space
ggplot(NULL) +
  geom_line(data=fitted_model, aes(x=height, y=.fitted), col="blue") +
  geom_point(data=predictions, aes(x=height, y=.fitted), col="red") +
  labs(x="height (in inches)", y="Fitted log-odds of p_hat", title="Fitted log-odds of probability of being female vs height")

# Convert back to probability space
ggplot(NULL) +
  geom_line(data=fitted_model, aes(x=height, y=p_hat), col="blue") +
  geom_point(data=predictions, aes(x=height, y=p_hat), col="red") +
  labs(x="height (in inches)", y="p_hat", title="Fitted probability of being female vs height") +
  # Add observed binary y's, and put a little random jitter to the points
  geom_jitter(data=fitted_model, aes(x=height, y=y), height=0.05, alpha=0.05)

#' 
#' 
#' 
#' # Continuous outcomes
#' 
## ------------------------------------------------------------------------
library(tidyverse)
library(broom)

# Continuous outcome:
mtcars <- mtcars %>% 
  mutate(ID = 1:n()) %>% 
  select(ID, mpg, hp) %>% 
  as_tibble()
mtcars_train <- mtcars %>% 
  sample_frac(0.5)
mtcars_test <- mtcars %>% 
  anti_join(mtcars_train, by="ID")

#' 
#' 
#' ## Regression via `lm`
#' 
#' ### Fit/train model
#' 
## ------------------------------------------------------------------------
model_formula <- as.formula("mpg ~ hp")
model_lm <- lm(model_formula, data=mtcars_train)

# 1.a) Extract regression table in tidy format
model_lm %>% 
  broom::tidy(conf.int=TRUE)

# 1.b) Extract point-by-point info in tidy format
model_lm %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  sample_n(5)

# 1.c) Extract summary stats info in tidy format
model_lm %>% 
  broom::glance()

#' 
#' ### Predict outcomes for test data
#' 
## ------------------------------------------------------------------------
# 2. Make predictions on test data
# Method 1:
# -input: mtcars_test is a data frame
# -output: y_hat is a vector
y_hat <- predict(model_lm, newdata=mtcars_test)

# Method 2: All new variables start with a period
model_lm %>% 
  broom::augment(newdata=mtcars_test) %>% 
  as_tibble() %>% 
  sample_n(5)

#' 
#' ### Plot
#' 
## ------------------------------------------------------------------------
fitted_model <- model_lm %>% 
  broom::augment() %>% 
  as_tibble()
predictions <- model_lm %>% 
  broom::augment(newdata=mtcars_test)

ggplot(NULL) +
  geom_point(data=fitted_model, aes(x=hp, y=mpg)) +
  geom_line(data=fitted_model, aes(x=hp, y=.fitted), col="blue") +
  geom_point(data=predictions, aes(x=hp, y=.fitted), col="red") +
  labs(x="Horse power", y="Miles per gallon")

#' 
#' 
#' 
#' ## LOESS {#loess}
#' 
#' ### Fit/train model
#' 
## ------------------------------------------------------------------------
model_formula <- as.formula("mpg ~ hp")
model_loess <- loess(model_formula, data=mtcars_train, span=0.9)

# 1.a) Extract point-by-point info in tidy format
model_loess %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  sample_n(5)

#' 
#' ### Predict outcomes for test data
#' 
## ------------------------------------------------------------------------
# 2. Make predictions on test data
# Method 1:
# -input: mtcars_test is a data frame
# -output: y_hat is a vector
y_hat <- predict(model_loess, newdata=mtcars_test)

# Method 2: All new variables start with a period
model_loess %>% 
  broom::augment(newdata=mtcars_test) %>% 
  sample_n(5)

#' 
#' ### Plot
#' 
## ------------------------------------------------------------------------
fitted_model <- model_loess %>% 
  broom::augment() %>% 
  as_tibble()
predictions <- model_loess %>% 
  broom::augment(newdata=mtcars_test) %>% 
  as_tibble()

ggplot(NULL) +
  geom_point(data=fitted_model, aes(x=hp, y=mpg)) +
  geom_line(data=fitted_model, aes(x=hp, y=.fitted), col="blue") +
  geom_point(data=predictions, aes(x=hp, y=.fitted), col="red") +
  labs(x="Horse power", y="Miles per gallon")

#' 
#' 
#' 
#' 
#' 
#' ## Splines {#splines}
#' 
#' ### Fit/train model
#' 
## ------------------------------------------------------------------------
model_spline <- smooth.spline(x=mtcars_train$hp, y=mtcars_train$mpg, df = 4)

# 1.a) Extract point-by-point info in tidy format
model_spline %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  sample_n(5)

# 1.b) Extract summary stats info in tidy format
model_spline %>% 
  broom::glance()

#' 
#' ### Predict outcomes for test data
#' 
## ------------------------------------------------------------------------
# 2. Make predictions on test data
# Method 1:
# -input: mtcars_test$hp is a vector
# -output: is a list with two slots: x & y
spline_fitted <- predict(model_spline, x=mtcars_test$hp)

# Convert y_hat to tibble data frame with x, y columns
spline_fitted <- spline_fitted %>% 
  as_tibble() %>% 
  rename(hp = x, .fitted = y)

y_hat <- spline_fitted$.fitted

#' 
#' ### Plot
#' 
## ------------------------------------------------------------------------
fitted_model <- model_spline %>% 
  broom::augment() %>% 
  as_tibble() %>% 
  rename(hp = x, mpg = y)
predictions <- mtcars_test %>% 
  mutate(.fitted = y_hat)

ggplot(NULL) +
  geom_point(data=fitted_model, aes(x=hp, y=mpg)) +
  geom_line(data=fitted_model, aes(x=hp, y=.fitted), col="blue") +
  geom_point(data=predictions, aes(x=hp, y=.fitted), col="red") +
  labs(x="Horse power", y="Miles per gallon")

#' 
#' 
