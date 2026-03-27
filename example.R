## Example feature development in Tidymodels

library(tidyverse)
library(tidymodels)
library(ranger)

dat0 <- read_csv("pmFRM.csv.gz")
dat0

## Take a look at the data in a scatterplot
dat0 |>
    ggplot(aes(date, pmFRM)) +
    geom_point()

## Add features based on date
dat0 <- dat0 |> 
  mutate(month = month(date),
         year = year(date)) 

## Split data into training and testing
dat_split <- initial_split(dat0)
dat_split

## Get the training data
dat <- training(dat_split)

## Basic linear model
rec <- dat |>
    recipe(pmFRM ~ date)

model <- linear_reg() |>
    set_engine("lm") |>
    set_mode("regression")

wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- dat |>
    vfold_cv(v = 5)

result <- wf |>
    fit_resamples(resamples = folds)

result

## Look at performance metrics
result |>
    collect_metrics()

################################################################################
## Add new features based on date

rec <- dat |> 
    select(pmFRM, month, year) |> 
    recipe(pmFRM ~ .) 

rec

## Look at what new features look like
rec |>
    prep(dat) |>
    bake(new_data = NULL)

## Update workflow
wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

folds <- dat |>
    vfold_cv(v = 5)
result <- wf |>
    fit_resamples(resamples = folds)

## Look at performance metrics
result |>
    collect_metrics()

################################################################################
## Change methods

## Use random forest instead
model <- rand_forest() |>
    set_mode("regression") |>
    set_engine("ranger")

## Update workflow
wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model)

wf

folds <- dat |>
    vfold_cv(v = 5)
result <- wf |>
    fit_resamples(resamples = folds)

## Look at performance metrics
result |>
    collect_metrics()


################################################################################

## Fit random forest model to the entire training data
model_fit <- wf |>
    fit(data = dat)

## Preparing trainig
dat_prep <- rec |>
    prep(dat) |>
    bake(new_data = NULL)

## Get predicted values on the training data
model_fit |>
    extract_fit_parsnip() |>
    augment(dat_prep)

## Plot predicted values vs. true values
model_fit |>
    extract_fit_parsnip() |>
    augment(dat_prep) |>
    ggplot(aes(.pred, pmFRM)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)


testing <- testing(dat_split) |> 
  mutate(month = month(date),
         year = year(date))

test_prep <- rec |>
    prep(dat) |>
    bake(new_data = testing)
test_prep

## Predicted values in the testing data
model_fit |>
    extract_fit_parsnip() |>
    augment(test_prep)

## Plot predicted values vs. true values in the testing data
model_fit |>
    extract_fit_parsnip() |>
    augment(test_prep) |>
    ggplot(aes(.pred, pmFRM)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1)


## Final fit
wf |>
    last_fit(dat_split) |>
    collect_metrics()
