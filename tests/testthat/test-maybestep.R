test_that("Can prepare and bake a recipe", {

  df <- data.frame(x = letters, y = runif(length(letters)))
  rec <- recipe(y ~ x, data = df) %>%
    step_maybe(step_dummy, use = TRUE, all_nominal_predictors())

  df_t <- rec %>% prep() %>% bake(df)
  expect_equal(ncol(df_t), 26)

  rec <- recipe(y ~ x, data = df) %>%
    step_maybe(step_dummy, use = FALSE, all_nominal_predictors())

  df_t <- rec %>% prep() %>% bake(df)
  expect_equal(ncol(df_t), 2)

})

test_that("Can tune a recipe that in includes maybestep", {

  data(concrete)

  model <- linear_reg(penalty = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

  rec <- recipe(compressive_strength ~ ., data = concrete) %>%
    step_maybe(step_poly, use = tune("use_poly"), all_numeric_predictors(),
                   degree = tune("degree"))

  wflow <- workflow() %>% add_recipe(rec) %>% add_model(model)

  tune_args(wflow)

  grid <- bind_rows(
    grid_regular(
      use_poly = use_step(TRUE),
      penalty(),
      degree = degree_int(c(2,9)),
      levels = 30
    ),
    grid_regular(
      use_poly = use_step(FALSE),
      penalty(),
      degree = degree_int(range = c(1,1)),
      levels = 30
    )
  )

  res <- tune_grid(
    wflow,
    resamples = vfold_cv(concrete, v = 5),
    grid = grid
  )

  expect_true(all(!is.na(collect_metrics(res)$mean)))
})

test_that("can use mold", {
  df <- data.frame(x = letters, y = runif(length(letters)))
  rec <- recipe(y ~ x, data = df) %>%
    step_maybe(step_dummy, use = TRUE, all_nominal_predictors())

  mold <- hardhat::mold(rec, df)
  expect_equal(nrow(mold$predictors), nrow(df))
})
