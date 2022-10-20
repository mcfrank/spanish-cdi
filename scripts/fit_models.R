fit_bglm <- function(df, max_steps = 200) {
  model <- arm::bayesglm(cbind(num_true, num_false) ~ age,
                         family = "binomial",
                         prior.mean = .3,
                         prior.scale = c(.01),
                         prior.mean.for.intercept = 0,
                         prior.scale.for.intercept = 2.5,
                         prior.df = 1,
                         data = df,
                         maxit = max_steps)
  intercept <- model$coefficients[["(Intercept)"]]
  slope <- model$coefficients[["age"]]
  tibble(intercept = intercept, slope = slope, aoa = -intercept / slope)
}

fit_aoas <- function(wb_data, max_steps = 200, min_aoa = 0, max_aoa = 72) {
  aoas <- wb_data |>
    mutate(num_false = total - num_true) |>
    nest(data = -c(language, uni_lemma)) |>
    mutate(aoas = map(data, fit_bglm)) |>
    dplyr::select(-data) |>
    unnest(aoas) |>
    filter(aoa >= min_aoa, aoa <= max_aoa)
}
