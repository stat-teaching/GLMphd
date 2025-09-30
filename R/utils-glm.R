#' Add predictions to a data frame
#'
#' A wrapper around [predict()] that adds model predictions as a new column to
#' a data frame. This function is designed to work smoothly within a
#' pipe (`%>%` or `|>`) workflow.
#'
#' @param data A `data.frame` containing the variables used in the model.
#' @param fit A fitted model object (e.g., from [lm()], [glm()], or other
#'   modeling functions that support [predict()]).
#' @param ... Additional arguments passed to [predict()].
#'
#' @return A `data.frame` containing the original variables in `data` and an
#'   additional column `pr` with the predictions.
#'
#' @examples
#' # Fit a simple linear model
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#'
#' # Add predictions to the original data
#' head(add_predict(mtcars, fit))
#'
#' # Use with pipes
#' mtcars |>
#'   add_predict(fit) |>
#'   head()
#'
#' @export

add_predict <- function(data, fit, ...) {
  pr <- predict(fit, newdata = data, ...)
  data.frame(cbind(data, data.frame(pr)))
}

#' Compute odds from probabilities
#'
#' Converts probabilities to odds, using the formula
#' \deqn{\text{odds}(p) = \frac{p}{1 - p}}
#'
#' @param p A numeric vector of probabilities in the range \[0, 1\].
#'
#' @return A numeric vector of odds corresponding to the input probabilities.
#'
#' @examples
#' odds(0.5)   # returns 1
#' odds(0.8)   # returns 4
#' odds(c(0.2, 0.5, 0.8))
#'
#' @export

odds <- function(p) {
  if(p < 0 | p > 1){
    stop("p need to be bounded between 0 and 1!")
  }
  p / (1 - p)
}

#' Compute the odds ratio of two probabilities
#'
#' Given two probabilities, computes the odds ratio (OR), defined as the ratio
#' of their odds:
#' \deqn{\text{OR} = \frac{pn / (1 - pn)}{pd / (1 - pd)}}
#'
#' @param pn A numeric probability (numerator) in the range \[0, 1\].
#' @param pd A numeric probability (denominator) in the range \[0, 1\].
#'
#' @return A numeric value representing the odds ratio of `pn` relative to `pd`.
#'
#' @examples
#' # OR comparing 0.8 vs 0.5
#' odds_ratio(0.8, 0.5)
#'
#' # Vectorized input
#' or(c(0.8, 0.6), 0.5)
#'
#' @seealso [odds()] to compute odds from probabilities.
#'
#' @export

or <- function(pn, pd) {
  odds(pn) / odds(pd)
}

#' Compute numerator probability from odds ratio
#'
#' Given a probability at the denominator (`pd`) and a desired odds ratio (`or`),
#' computes the corresponding numerator probability (`pn`) using the formula:
#' \deqn{pn = \frac{or \cdot pd}{pd \cdot (or - 1) + 1}}.
#'
#' @param pd A numeric probability at the denominator, in the range \[0, 1\].
#' @param or A positive numeric value representing the odds ratio.
#'
#' @return A numeric probability (between 0 and 1) corresponding to the numerator.
#'
#' @examples
#' # Given denominator probability 0.5 and OR = 2
#' or2pn(0.5, 2)  # = 0.667
#'
#' # Vectorized input
#' or2pn(c(0.2, 0.5), 3)
#'
#' @seealso [odds_ratio()] to compute odds ratios from two probabilities.
#'
#' @export

or2pn <- function(pd, or) {
  (or * pd) / (pd * (or - 1) + 1)
}

#' Convert binomial data to binary format
#'
#' Expands a dataset in aggregated binomial form (successes and total trials)
#' into a dataset in binary form (0/1 outcomes).
#'
#' @param data A `data.frame` containing the binomial data.
#' @param nc A column in `data` giving the number of successes.
#' @param nt A column in `data` giving the total number of trials.
#'
#' @return A `data.frame` where each row corresponds to a single binary trial,
#'   with a new column `y` containing 1s (successes) and 0s (failures).
#'
#' @examples
#' df <- data.frame(
#'   x = c(1, 2)
#'   successes = c(2, 3),
#'   trials = c(5, 4)
#' )
#'
#' bin2binary(df, successes, trials)
#'
#' # Resulting dataset has 9 rows:
#' # - 2 successes and 3 failures from the first row
#' # - 3 successes and 1 failure from the second row
#'
#' @export

bin2binary <- function(data, nc, nt) {
  nc <- deparse(substitute(nc))
  nt <- deparse(substitute(nt))

  nts <- data[[nt]]
  ncs <- data[[nc]]

  drep <- data[rep(seq_len(nrow(data)), nts), ]

  y <- lapply(1:nrow(data), function(i) {
    rep(c(1, 0), c(ncs[i], nts[i] - ncs[i]))
  })

  drep$y <- unlist(y)
  rownames(drep) <- NULL
  drep
}

#' Convert binary dataset to binomial summary
#'
#' Aggregates a binary dataset (0/1 outcomes) into a binomial summary dataset
#' with counts of successes and total trials. Useful for converting individual
#' trial-level data into a summarized form for modeling.
#'
#' @param data A `data.frame` containing the binary outcome variable `y` and
#'   grouping variables.
#' @param formula A formula specifying the outcome and grouping variable(s),
#'   e.g., `y ~ x` where `y` is the 0/1 variable and `x` is the grouping variable.
#'
#' @return A `data.frame` containing:
#' \itemize{
#'   \item `nc`: Number of successes per group.
#'   \item `nt`: Total trials per group.
#'   \item `nf`: Number of failures per group (`nt - nc`).
#'   \item The grouping variables.
#' }
#'
#' @examples
#' df <- data.frame(
#'   y = c(1,0,1,1,0,0,1),
#'   group = c("A","A","B","B","B","C","C")
#' )
#' binary2bin(df, y ~ group)
#'
#' @export

binary2bin <- function(data, formula){
  formula_s <- as.character(formula)
  res <- aggregate(formula, data = data, function(x) c(nc = sum(x), nt = length(x)))
  res <- do.call(data.frame, res)
  names(res) <- gsub(paste0(formula_s[2], "\\."), "", names(res))
  res$nf <- res$nt - res$nc
  res
}

#' Compute influence measures for a model
#'
#' A wrapper around [stats::influence.measures()] that returns the influence
#' measures as a `data.frame` for easy inspection and further analysis.
#'
#' @param fit A fitted model object (e.g., from [lm()] or [glm()]).
#'
#' @return A `data.frame` containing the influence measures for each observation,
#'   including Cook's distance, hat values, DFBETAs, and other diagnostics.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' infl_measure(fit)
#'
#' @export

infl_measure <- function(fit) {
  data.frame(stats::influence.measures(fit)$infmat)
}

#' Compute classification error rate for a binomial model
#'
#' Computes the misclassification error rate for a fitted binomial model
#' (e.g., logistic regression). Predictions are converted to binary outcomes
#' using a threshold of 0.5.
#'
#' @param fit A fitted binomial model object (e.g., from [glm()] with
#'   `family = binomial`).
#'
#' @return A numeric value between 0 and 1 representing the classification error
#'   rate.
#'
#' @examples
#' fit <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
#' error_rate(fit)
#'
#' @export

error_rate <- function(fit) {
  if(fit$family$family != "binomial"){
    stop("the error_rate function is made for family = 'binomial') models!")
  }
  pi <- predict(fit, type = "response")
  yi <- fit$y
  cr <- mean((pi > 0.5 & yi == 1) | (pi < 0.5 & yi == 0))
  1 - cr
}


#' Plot and extract DFBETAs
#'
#' Computes and plots DFBETA values for a fitted regression model, with an
#' optional threshold to highlight influential observations. Returns the
#' underlying long-format data frame used for plotting.
#'
#' @param x A fitted regression model (e.g., from [lm()] or [glm()]).
#' @param b Optional. A character vector of coefficient names to include
#'   (defaults to all).
#' @param threshold Numeric. Cutoff for influence, defaults to \eqn{2/\sqrt{n}},
#'   where \eqn{n} is the number of observations.
#' @param intercept Logical. Whether to include the intercept term in the plot
#'   and results (default `FALSE`).
#'
#' @return A `data.frame` in long format containing:
#' \itemize{
#'   \item `obs`: Observation index.
#'   \item `param`: Coefficient name.
#'   \item `value`: DFBETA value.
#'   \item `obs_n`: Original row names of the data.
#'   \item `threshold`: The threshold used for outlier detection.
#'   \item `is_out`: Logical, whether the DFBETA exceeds the threshold.
#' }
#'
#' @details
#' The function produces a plot of DFBETA values by parameter. Observations
#' exceeding the threshold are flagged in the returned data frame.
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' dfbl <- plot_dfbeta(fit)
#'
#' # Only plot selected coefficient
#' dfbl_hp <- plot_dfbeta(fit, b = "hp")
#'
#' @seealso [stats::dfbeta()] for the underlying computation of DFBETAs.
#'
#' @export

plot_dfbeta <- function(x, b = NULL, threshold = NULL, intercept = FALSE){

  n <- nrow(x$model)

  if(is.null(threshold)) threshold <- 2/sqrt(n)

  dfb <- data.frame(stats::dfbeta(x))
  names(dfb) <- names(coef(x))

  if(!is.null(b)){
    dfb <- dfb[, b, drop = FALSE]
  }

  if(!intercept){
    dfb <- dfb[, !grepl("(Intercept)", names(dfb))]
  }

  dfbl <- reshape(
    dfb,
    direction = "long",
    varying = list(names(dfb)),  # all columns to melt
    v.names = "value",
    timevar = "param",
    times = names(dfb),
    idvar = "obs"
  )

  dfbl$obs_n <- rownames(dfb)[dfbl$obs]
  rownames(dfbl) <- NULL

  xlim <- c(
    -threshold * 1.5, threshold*1.5
  )

  out <- abs(dfbl$value) > threshold
  dfbl$threshold <- threshold

  tinyplot::tinyplot(
    obs ~ value,
    data = dfbl,
    facet = ~ param,
    xlim = xlim,
    pch = 19,
    xlab = "DFBeta",
    ylab = "Index"
  )

  dfbl$is_out <- out
  return(dfbl)

}

#' Set contrasts for all categorical variables in a data frame
#'
#' Applies a contrast coding scheme to all factor or character variables in
#' a data frame. By default, treatment contrasts are used, but any valid
#' contrast function (e.g., [stats::contr.sum], [stats::contr.helmert]) can be
#' specified. A list of contrasts can also be supplied to set variable-specific
#' contrast schemes.
#'
#' @param data A `data.frame` containing the variables to modify.
#' @param contrasts Either:
#'   \itemize{
#'     \item A contrast function (default: [stats::contr.treatment]), applied
#'       to all categorical variables.
#'     \item A named list of contrasts, where names correspond to column names
#'       in `data`. Each element can be a contrast function or a contrast matrix.
#'   }
#'
#' @return A `data.frame` where all factor or character variables have their
#'   contrasts set according to the specified scheme(s).
#'
#' @examples
#' df <- data.frame(
#'   group = factor(c("A", "B", "A", "C")),
#'   outcome = rnorm(4)
#' )
#'
#' # Apply treatment contrasts to all factors (default)
#' df_treat <- set_contrasts(df)
#'
#' # Apply sum contrasts to all factors
#' df_sum <- set_contrasts(df, contrasts = contr.sum)
#'
#' # Apply different contrasts per variable
#' df2 <- data.frame(
#'   f1 = factor(c("a", "b", "a", "c")),
#'   f2 = factor(c("x", "x", "y", "z"))
#' )
#' df_custom <- set_contrasts(df2, contrasts = list(
#'   f1 = contr.sum,
#'   f2 = contr.helmert
#' ))
#'
#' @seealso [stats::contr.treatment], [stats::contr.sum], [stats::contr.helmert]
#'
#' @export

set_contrasts <- function(data,
                          contrasts = contr.treatment){

  if(!is.list(contrasts)){
    contrasts <- lapply(1:ncol(data), function(x) contrasts)
    names(contrasts) <- names(data)
  }

  for(i in 1:length(contrasts)){
    cvar <- data[[names(contrasts)[i]]]
    if(is.character(cvar) | is.factor(cvar)){
      contrasts(cvar) <- contrasts[[i]]
      data[[names(contrasts)[i]]] <- cvar
    }
  }

  return(data)

}

#' Simulate experimental design data
#'
#' Generates a dataset representing a factorial experimental design with
#' optional within-subject factors, between-subject factors, and covariates.
#' Useful for creating synthetic data structures for simulations.
#'
#' @param n Integer. Number of subjects (or experimental units).
#' @param within Optional. A named list of within-subject factors and their
#'   levels, e.g. `list(time = c("t1", "t2"))`.
#' @param between Optional. A named list of between-subject factors and their
#'   levels, e.g. `list(group = c("A", "B"))`.
#' @param covariates Optional. A named list of covariates to simulate. Each
#'   element should be a specification accepted by [`sim_n_x()`] (not included here).
#'
#' @return A `data.frame` containing:
#' \itemize{
#'   \item `id`: Subject identifier.
#'   \item Columns for within-subject factors (if any).
#'   \item Columns for between-subject factors (if any).
#'   \item Columns for simulated covariates (if any).
#' }
#'
#' @details
#' - At least one of `within`, `between`, or `covariates` must be provided.
#' - Within-subject factors are fully crossed using [expand.grid()].
#' - Between-subject factors are merged with within-subject factors to create
#'   all combinations.
#' - Covariates are simulated per subject using `sim_n_x()`.
#'
#' @examples
#' # Only between-subject factor
#' sim_design(n = 4, between = list(group = c("A", "B")))
#'
#' # Within and between
#' sim_design(
#'   n = 2,
#'   within = list(time = c("t1", "t2")),
#'   between = list(group = c("A", "B"))
#' )
#'
#' # With covariates (assuming [`sim_n_x`] is defined)
#' # sim_design(n = 5, between = list(group = c("A","B")),
#' #            covariates = list(age = "rnorm"))
#'
#' @export

sim_design <- function(n,
                       within = NULL,
                       between = NULL,
                       covariates = NULL){

  id <- 1:n

  if(is.null(within) & is.null(between) & is.null(covariates)){
    data <- data.frame(1:n)
  }

  # within

  if(!is.null(within)){
    within$id <- id
    data <- expand.grid(within, stringsAsFactors = FALSE)
  } else{
    data <- data.frame(id = id)
  }

  # between

  if(!is.null(between)){
    data_between <- expand.grid(between)
    data <- merge(data, data_between, by = NULL)
    nid <- tapply(data, c(list(data$id), list(data[, names(between)])))
    data$id <- nid
  }

  # covariates

  if(!is.null(covariates)){
    covs <- lapply(covariates, function(x) sim_n_x(data, x))
    data <- cbind(data, covs)
  }

  data

}

#' Simulate a covariate
#'
#' Helper function to generate covariates at either the observation level
#' or the subject level. Typically used inside [sim_design()].
#'
#' @param data A `data.frame` containing at least an `id` column.
#' @param covariate A list describing the covariate, with elements:
#'   \itemize{
#'     \item `level`: Either `"obs"` (generate a value per observation) or `"id"`
#'       (generate a value per subject, recycled to observations).
#'     \item `f`: A function taking an integer `n` and returning `n` simulated
#'       values (e.g. `rnorm`, `runif`).
#'   }
#'
#' @return A numeric (or other type) vector of length `nrow(data)` representing
#'   the simulated covariate.
#'
#' @examples
#' df <- data.frame(id = rep(1:3, each = 2))
#'
#' # Observation-level covariate
#' sim_n_x(df, list(level = "obs", f = rnorm))
#'
#' # Subject-level covariate (same value for each subject's observations)
#' sim_n_x(df, list(level = "id", f = function(n) rnorm(n, mean = 50, sd = 10)))
#'
#' @seealso [sim_design()] for generating full designs with covariates.
#'
#' @export

sim_n_x <- function(data, covariate){
  if(covariate$level == "obs"){
    n <- nrow(data)
    x <- covariate$f(n)
  } else{
    n <- length(unique(data$id))
    x <- covariate$f(n)
    x <- x[data$id]
  }
  x
}

#' Create a covariate specification
#'
#' Helper to create a covariate object that can be passed to
#' [sim_design()] or [sim_n_x()]. Wraps a generating function
#' together with the level at which the covariate varies.
#'
#' @param fun A function that takes an integer `n` and returns `n`
#'   simulated values (e.g., `rnorm`, `runif`).
#' @param level A character string, either `"obs"` for observation-level
#'   covariates (different value per row), or `"id"` for subject-level
#'   covariates (same value repeated for all observations of the same subject).
#'
#' @return A list with elements `f` (the generator function) and
#'   `level` (the specified level).
#'
#' @examples
#' # Observation-level covariate
#' new_cov(rnorm, level = "obs")
#'
#' # Subject-level covariate
#' new_cov(function(n) rnorm(n, mean = 50, sd = 10), level = "id")
#'
#' @seealso [sim_design()], [sim_n_x()]
#'
#' @export

new_cov <- function(fun, level = "obs"){
  list(
    f = function(n) fun(n),
    level = level
  )
}

#' Attach a random generator function to a family object
#'
#' Adds (or overrides) a random number generator (RNG) function for
#' a given model family. By default, common distributions are mapped
#' to their standard RNG functions:
#'
#' - `"gaussian"` → [stats::rnorm()]
#' - `"binomial"` → [stats::rbinom()]
#' - `"poisson"`  → [stats::rpois()]
#' - `"gamma"`    → [stats::rgamma()]
#'
#' @param x A family object (as returned by [stats::family()]) or a
#'   list-like object with a `family` element.
#' @param rng An optional RNG function. If provided, it overrides the
#'   default mapping.
#'
#' @return The same object `x`, augmented with an element `rng`
#'   containing the chosen random generator function.
#'
#' @examples
#' f <- stats::binomial()
#' f <- add_rng(f)
#' f$rng(5, size = 1, prob = 0.5)  # simulate 5 Bernoulli trials
#'
#' g <- stats::gaussian()
#' g <- add_rng(g, rng = function(n) rep(1, n))  # override with custom rng
#' g$rng(5)
#'
#' @export
add_rng <- function(x, rng = NULL) {
  fam <- tolower(x$family)

  rng.default <- switch(
    fam,
    gaussian = stats::rnorm,
    binomial = stats::rbinom,
    poisson  = stats::rpois,
    gamma    = stats::rgamma,
    NULL
  )

  if (is.null(rng) & is.null(rng.default) & is.null(x$rng)) {
    stop(sprintf("rng function for family '%s' not included yet!\nYou can provide a custom rng() function within the family object!", fam))
  }

  if (!is.null(rng)) {
    x$rng <- rng
  } else if (is.null(x$rng)) {
    x$rng <- rng.default
  }

  x
}

#' Simulate outcome data from a GLM design
#'
#' Given a dataset, a model formula, regression coefficients, and a GLM
#' family, this function simulates response values `y` by generating data
#' from the corresponding distribution.
#'
#' @param data A data frame containing predictors.
#' @param formula A model formula specifying the linear predictor.
#' @param beta A numeric vector of regression coefficients (must match
#'   the design matrix from `formula`).
#' @param family A GLM family object (e.g., [stats::gaussian()],
#'   [stats::binomial()], [stats::poisson()]) or a list with elements
#'   `family` and `linkinv`. Defaults to [stats::gaussian()].
#' @param which.mu Character string naming the argument of the RNG that
#'   corresponds to the mean (`"mean"` for Gaussian, `"lambda"` for
#'   Poisson, `"prob"` for Binomial, etc.).
#' @param rng An optional custom random generator function. If `NULL`,
#'   the default is inferred from the family via [add_rng()].
#' @param rng.arg A named list of additional arguments to pass to the RNG
#'   (e.g., `list(sd = 2)` for Gaussian).
#'
#' @return A copy of `data` with an additional column `y` containing the
#'   simulated outcome values.
#'
#' @details
#' The function computes the linear predictor \eqn{\eta = X \beta}, applies
#' the inverse link function to obtain the mean \eqn{\mu}, and then simulates
#' outcomes using the appropriate RNG. The argument `which.mu` is used to map
#' \eqn{\mu} to the correct argument of the RNG function.
#'
#' The family object (for the simulation) is not necessary a full family() implementation.
#' Can be a list with: `family = "name"`, `linkinv = fun(x)` (the link function),
#' `linkinv = fun(x)` and `rng = fun` with the random number generator function.
#'
#' @examples
#' set.seed(123)
#'
#' d <- data.frame(x = rnorm(100))
#' beta <- c(0, 1)  # intercept = 0, slope = 1
#'
#' # Gaussian simulation
#' sim_data(d, y ~ x, beta, family = gaussian(), rng.arg = list(sd = 1))
#'
#' # Poisson simulation
#' sim_data(d, y ~ x, beta, family = poisson(), which.mu = "lambda")
#'
#' # Binomial simulation (Bernoulli trials)
#' sim_data(d, y ~ x, beta, family = binomial(), which.mu = "prob", rng.arg = list(size = 1))
#'
#' fam <- list(
#'   family = "negative.binomial",
#'   linkfun = function(x) log(x),
#'   linkinv = function(x) exp(x),
#'   rng = MASS::rnegbin
#'   )
#' sim_data(d, ~ x, beta = c(0.1, 0.5), family = fam, which.mu = "mu", rng.arg = list(theta = 5))
#'
#'
#' @export


sim_data <- function(data,
                     formula,
                     beta,
                     family = gaussian(),
                     which.mu = "mean",
                     rng = NULL,
                     rng.arg = NULL,
                     simulate = TRUE,
                     get.lp = TRUE){
  X  <- model.matrix(formula, data = data)
  n  <- nrow(X)
  # put column vector
  beta <- matrix(beta, ncol = 1)
  lp <- c(X %*% beta)
  family <- add_rng(family, rng)
  rng.default <- formals(family$rng)
  mu <- family$linkinv(lp)
  rng.default[names(rng.arg)] <- rng.arg
  rng.default$n <- n
  rng.default[[which.mu]] <- mu

  if(simulate){
    data$y <- do.call(family$rng, rng.default)
  } else{
    get.lp <- TRUE
  }

  if(get.lp){
    data$lp <- lp
    data$mu <- mu
  }
  data
}


#' Simulate a Signal Detection Theory (SDT) experiment
#'
#' Generates simulated data for a basic Signal Detection Theory (SDT) experiment,
#' where each trial contains either a signal or noise, and the participant
#' responds based on a decision criterion.
#'
#' @param n Integer. Total number of trials.
#' @param d Numeric. Sensitivity (\eqn{d'}) of the participant, representing
#'   the standardized separation between the signal and noise distributions.
#' @param c Numeric. Decision criterion (\eqn{c}). A positive `c` means a conservative
#'   criterion (biased towards responding "noise"), while a negative `c` means a liberal
#'   criterion (biased towards responding "signal").
#' @param p.signal Numeric. Probability that a trial contains a signal
#'   (between 0 and 1).
#' @param vr Numeric. Standard deviation of the signal distribution.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{say_signal}{Participant response (1 = "signal", 0 = "noise").}
#'   \item{is_signal}{True trial type (1 = signal, 0 = noise).}
#'   \item{x}{Internal decision variable sampled from the noise or signal distribution.}
#' }
#'
#' @details
#' This function implements a standard SDT model:
#'
#' \deqn{x \sim \mathcal{N}(-d/2, 1), \quad \text{if trial is noise}}
#' \deqn{x \sim \mathcal{N}(d/2, vr^2), \quad \text{if trial is signal}}
#'
#' The participant responds "signal" if the internal decision variable exceeds
#' the decision criterion:
#'
#' \deqn{\text{say\_signal} = \begin{cases}
#' 1 & \text{if } x > c \\
#' 0 & \text{if } x \le c
#' \end{cases}}
#'
#' The criterion `c` reflects response bias:
#' - `c > 0`: conservative, more likely to respond "noise"
#' - `c < 0`: liberal, more likely to respond "signal"
#'
#' Sensitivity `d` (d-prime) represents the distance between the means of
#' the signal and noise distributions in standard deviation units.
#'
#' @examples
#' set.seed(123)
#' # Simulate 100 trials with moderate sensitivity and neutral criterion
#' sim_sdt(n = 100, d = 1, c = 0, p.signal = 0.5, vr = 1)
#'
#' # Conservative criterion (c > 0)
#' sim_sdt(n = 100, d = 1, c = 0.5, p.signal = 0.5, vr = 1)
#'
#' # Liberal criterion (c < 0)
#' sim_sdt(n = 100, d = 1, c = -0.5, p.signal = 0.5, vr = 1)
#'
#' @export
sim_sdt <- function(n,
                    d = 0,
                    c = 0,
                    p.signal = 0.5,
                    vr = 1) {
  # trial types
  is_signal <- rbinom(n = n, size = 1, prob = p.signal)

  # internal decision variable sampled per trial
  x <- rnorm(n, mean = ifelse(is_signal == 1, d / 2, -d / 2),
             sd = ifelse(is_signal == 1, vr, 1))

  # responses based on criterion
  say_signal <- as.integer(x > c)

  data.frame(say_signal, is_signal, x)
}

#' Classify responses in a Signal Detection Theory (SDT) task
#'
#' Computes the number of hits, misses, false alarms, and correct rejections
#' given observed responses and true trial types, along with their proportions.
#'
#' @param say_signal Integer vector (0/1). Participant responses
#'   (1 = "signal", 0 = "noise").
#' @param is_signal Integer vector (0/1). True trial type
#'   (1 = signal, 0 = noise).
#'
#' @return A list with counts and proportions:
#' \describe{
#'   \item{hit}{Number of hits (\eqn{is\_signal = 1}, \eqn{say\_signal = 1}).}
#'   \item{fa}{Number of false alarms (\eqn{is\_signal = 0}, \eqn{say\_signal = 1}).}
#'   \item{miss}{Number of misses (\eqn{is\_signal = 1}, \eqn{say\_signal = 0}).}
#'   \item{cr}{Number of correct rejections (\eqn{is\_signal = 0}, \eqn{say\_signal = 0}).}
#'   \item{phit}{Proportion of hits: \eqn{hit / (hit + miss)}.}
#'   \item{pfa}{Proportion of false alarms: \eqn{fa / (fa + cr)}.}
#'   \item{pmiss}{Proportion of misses: \eqn{miss / (hit + miss)}.}
#'   \item{pcr}{Proportion of correct rejections: \eqn{cr / (cr + fa)}.}
#' }
#'
#' @details
#' Signal Detection Theory (SDT) classifies each trial outcome into
#' four categories:
#' - **Hit**: correctly saying "signal" when a signal is present.
#' - **Miss**: saying "noise" when a signal is present.
#' - **False alarm**: saying "signal" when only noise is present.
#' - **Correct rejection**: correctly saying "noise" when no signal is present.
#'
#' The proportions (`phit`, `pfa`, `pmiss`, `pcr`) are useful for
#' computing SDT measures such as \eqn{d'} and criterion.
#'
#' @examples
#' set.seed(123)
#' dat <- sim_sdt(100, d = 1, c = 0)
#' sdt_classify(dat$say_signal, dat$is_signal)
#' @keywords internal

sdt_classify <- function(say_signal, is_signal){
  hit  <- sum(is_signal == 1 & say_signal == 1)
  miss <- sum(is_signal == 1 & say_signal == 0)
  fa   <- sum(is_signal == 0 & say_signal == 1)
  cr   <- sum(is_signal == 0 & say_signal == 0)

  list(
    hit = hit,
    fa = fa,
    miss = miss,
    cr = cr,
    phit = hit / (hit + miss),
    pfa = fa / (fa + cr),
    pmiss = miss / (hit + miss),
    pcr = cr / (cr + fa)
  )
}

#' Compute Signal Detection Theory (SDT) outcomes
#'
#' Applies Signal Detection Theory (SDT) classification to a dataset given
#' a response variable and a signal/noise indicator. The response can be
#' binary (already "signal"/"noise") or continuous, in which case thresholds
#' (`c`) are applied to classify responses.
#'
#' @param data A data.frame containing responses and signal indicators.
#' @param formula A formula of the form \code{response ~ is_signal}, where:
#'   - \code{response} is either a binary decision (0 = "noise", 1 = "signal"),
#'     or a continuous decision variable (e.g., evidence strength).
#'   - \code{is_signal} is the true trial type (0 = noise, 1 = signal).
#' @param c Optional numeric vector of decision criteria (cutoffs). If \code{NULL}
#'   (default), the \code{response} is assumed to be binary and is used as-is.
#'   If not \code{NULL}, each cutoff defines a classification rule:
#'   \deqn{\text{say\_signal} = \mathbb{1}[\text{response} > c]}
#'
#' @return A data.frame with one row per criterion, containing:
#' \describe{
#'   \item{hit}{Number of hits (\eqn{is\_signal = 1, say\_signal = 1}).}
#'   \item{fa}{Number of false alarms (\eqn{is\_signal = 0, say\_signal = 1}).}
#'   \item{miss}{Number of misses (\eqn{is\_signal = 1, say\_signal = 0}).}
#'   \item{cr}{Number of correct rejections (\eqn{is\_signal = 0, say\_signal = 0}).}
#'   \item{phit}{Proportion of hits (\eqn{hit / (hit + miss)}).}
#'   \item{pfa}{Proportion of false alarms (\eqn{fa / (fa + cr)}).}
#'   \item{pmiss}{Proportion of misses (\eqn{miss / (hit + miss)}).}
#'   \item{pcr}{Proportion of correct rejections (\eqn{cr / (cr + fa)}).}
#'   \item{c}{The decision criterion (if provided).}
#' }
#'
#' @details
#' In Signal Detection Theory (SDT), a continuous internal response variable
#' \eqn{X} is compared against a criterion \eqn{c}:
#' \deqn{\text{say\_signal} = \mathbb{1}[X > c]}
#'
#' - If \eqn{X > c}, the observer says "signal".
#' - If \eqn{X \leq c}, the observer says "noise".
#'
#' Interpretation of the criterion:
#' - A **liberal** criterion (\eqn{c < 0}) increases hits but also false alarms.
#' - A **conservative** criterion (\eqn{c > 0}) reduces false alarms but also misses.
#' - A **neutral** criterion (\eqn{c \approx 0}) balances both.
#'
#' These outcomes can be used to compute SDT sensitivity and bias measures,
#' such as \eqn{d'} and criterion \eqn{c} in the classical sense:
#' \deqn{d' = \Phi^{-1}(p_\text{hit}) - \Phi^{-1}(p_\text{fa})}
#' \deqn{c = -\tfrac{1}{2}\left(\Phi^{-1}(p_\text{hit}) + \Phi^{-1}(p_\text{fa})\right)}
#' where \eqn{\Phi^{-1}} is the inverse standard normal CDF.
#'
#' @examples
#' set.seed(123)
#' dat <- sim_sdt(200, d = 1, c = 0)
#'
#' # case 1: response already binary
#' sdt(dat, say_signal ~ is_signal)
#'
#' # case 2: response is continuous, apply thresholds
#' sdt(dat, x ~ is_signal, c = c(-1, 0, 1))
#'
#' @export
#'

sdt <- function(data, formula, c = NULL){
  is_signal <- formula.tools::rhs.vars(formula)
  left <- formula.tools::lhs.vars(formula)

  complete <- complete.cases(data[, c(is_signal, left)])
  data <- data[complete, ]

  if(is.null(c)){
    say_signal <- list(data[[left]])
  } else{
    x <- left
    say_signal <- lapply(c, function(ci) as.integer(data[[x]] > ci))
  }

  is_signal <- data[[is_signal]]
  res <- lapply(say_signal, function(ss) data.frame(sdt_classify(is_signal, ss)))
  res <- do.call(rbind, res)
  res$c <- c
  res
}

#' Convert factors into numeric contrasts
#'
#' This function converts all factor variables in a data frame into their
#' corresponding numeric contrasts using \code{\link[stats]{model.matrix}}.
#' By default, the numeric contrasts are appended to the original data frame,
#' but they can also be returned alone.
#'
#' @param data A \code{data.frame} containing factor variables to be converted.
#' @param append Logical. If \code{TRUE} (default), the numeric contrasts are
#'   appended to the original data. If \code{FALSE}, only the numeric contrasts
#'   are returned.
#'
#' @details
#' The function uses \code{model.matrix} with all factor variables in the data.
#' The reference levels are determined by the contrast settings in
#' \code{options("contrasts")}. By default, this is usually
#' \code{contr.treatment}.
#'
#' If no factors are present in \code{data}, the function may return an error
#' unless additional checks are added.
#'
#' @return A \code{data.frame}. If \code{append = TRUE}, the original data
#'   with the numeric contrast variables appended. If \code{append = FALSE},
#'   only the numeric contrast variables.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   group = factor(c("A", "B", "A", "C")),
#'   sex = factor(c("M", "F", "M", "F"))
#' )
#'
#' # Append contrasts to original data
#' contr2num(df, append = TRUE)
#'
#' # Only return the numeric contrasts
#' contr2num(df, append = FALSE)
#'
#' @seealso \code{\link[stats]{model.matrix}}, \code{\link[stats]{contr.treatment}}
#'
#' @export

contr2num <- function(data, append = TRUE){
  fcts <- data[, sapply(data, function(x) is.factor(x) | is.character(x)), drop = FALSE]
  XF <- model.matrix(~ ., data = fcts)[, -1, drop = FALSE]
  XF <- data.frame(XF)
  if (append) {
    cbind(data, XF)
  } else {
    XF
  }
}

#' Extract model coefficients with confidence intervals
#'
#' This function returns a tidy data frame of model coefficients, standard errors,
#' test statistics, p-values, and confidence intervals for a fitted model.
#'
#' @param x A fitted model object (e.g., from \code{\link[stats]{lm}} or \code{\link[stats]{glm}}).
#' @param level The confidence level for intervals (default \code{0.95}).
#' @param ci.type The type of confidence interval to compute.
#'   One of \code{"profile"} (default, uses \code{\link[stats]{confint.default}})
#'   or \code{"wald"} (uses \code{\link[stats]{confint}}).
#'
#' @details
#' - \code{"profile"} computes intervals based on the profile likelihood method.
#' - \code{"wald"} computes Wald-type confidence intervals.
#'
#' The returned data frame contains one row per model parameter.
#'
#' @return A \code{data.frame} with the following columns:
#' \itemize{
#'   \item \code{param} - parameter name
#'   \item \code{beta} - estimated coefficient
#'   \item \code{se} - standard error
#'   \item \code{stat} - test statistic
#'   \item \code{p} - p-value
#'   \item \code{ci.lb} - lower bound of the confidence interval
#'   \item \code{ci.ub} - upper bound of the confidence interval
#' }
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' mcoefs(fit, level = 0.95, ci.type = "wald")
#'
#' @seealso \code{\link[stats]{summary.lm}}, \code{\link[stats]{confint}},
#'   \code{\link[stats]{confint.default}}
#'
#' @export

mcoefs <- function(x, level = 0.95, ci.type = "profile"){
  ci.type <- match.arg(ci.type, choices = c("profile", "wald"))
  xs <- data.frame(summary(x)$coefficients)
  if(ci.type == "profile"){
    ci <- confint(x, level = level)
  } else{
    ci <- confint.default(x, level = level)
  }
  ci <- data.frame(ci)
  xs <- cbind(param = rownames(xs), xs, ci)
  colnames(xs) <- c("param", "beta", "se", "stat", "p", "ci.lb", "ci.ub")
  rownames(xs) <- NULL
  xs
}
