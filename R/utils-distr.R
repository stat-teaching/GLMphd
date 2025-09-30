rnb <- function(n, mu, vmr = NULL, theta = NULL, message = FALSE) {
  if (is.null(theta) & is.null(vmr)) {
    stop("theta or vmr need to be specified!")
  }
  if (!is.null(vmr)) {
    if (vmr == 1) {
      msg <- sprintf("y ~ Poisson(mu = %2.f), vmr = %.2f", mu, vmr)
      y <- rpois(n, mu)
    } else {
      res <- theta_from_vmr(mu, vmr)
      y <- MASS::rnegbin(n, mu, res$theta)
      msg <- sprintf(
        "y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f",
        mu,
        res$theta,
        res$v,
        vmr
      )
    }
  } else {
    res <- vmr_from_theta(mu, theta)
    y <- MASS::rnegbin(n, mu, res$theta)
    msg <- sprintf(
      "y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f",
      mu,
      theta,
      res$v,
      res$vmr
    )
  }
  if (message) {
    message(msg)
  }
  return(y)
}

#' Compute theta from mean and variance-to-mean ratio
#'
#' Given a mean `mu` and a variance-to-mean ratio `vmr`, this function
#' computes the overdispersion parameter `theta` assuming a negative
#' binomial-like parameterization:
#' \deqn{v = \mu + \mu^2 / \theta, \quad \text{where } v = \text{variance}}.
#'
#' @param mu Numeric. The mean of the distribution.
#' @param vmr Numeric. Variance-to-mean ratio (\(v / \mu\)).
#'
#' @return A list with elements:
#' \describe{
#'   \item{theta}{The dispersion parameter.}
#'   \item{v}{The variance computed as `mu * vmr`.}
#'   \item{vmr}{The input variance-to-mean ratio.}
#' }
#'
#' @details
#' The function assumes the relationship \eqn{v = \mu + \mu^2 / \theta}.
#' If `vmr = 1`, then `theta` is \(-\infty\), and a warning is issued.
#'
#' @examples
#' theta_from_vmr(mu = 10, vmr = 2)
#' theta_from_vmr(mu = 5, vmr = 1)  # triggers warning
#'
#' @export

theta_from_vmr <- function(mu, vmr) {
  # vmr = v / mu
  v <- mu * vmr
  # v = mu + mu^2 / theta
  theta <- -(mu^2 / (mu - v))
  if (vmr == 1) {
    warning("when vmr = 1, theta = -Inf")
  }
  list(theta = theta, v = v, vmr = vmr)
}


#' Compute variance-to-mean ratio from mean and theta
#'
#' Given mean `mu` and dispersion parameter `theta`, computes the variance `v`
#' and the variance-to-mean ratio `vmr` assuming the relationship
#' \eqn{v = \mu + \mu^2 / \theta}.
#'
#' @param mu Numeric. The mean of the distribution.
#' @param theta Numeric. The dispersion parameter.
#'
#' @return A list with elements:
#' \describe{
#'   \item{theta}{Input theta value.}
#'   \item{v}{Variance computed as `mu + mu^2 / theta`.}
#'   \item{vmr}{Variance-to-mean ratio, `v / mu`.}
#' }
#'
#' @examples
#' vmr_from_theta(mu = 5, theta = 2)
#' vmr_from_theta(mu = 10, theta = 5)
#'
#' @export

vmr_from_theta <- function(mu, theta) {
  v <- mu + mu^2 / theta
  vmr <- v / mu
  list(theta = theta, v = v, vmr = vmr)
}

#' Variance of a Negative Binomial Distribution
#'
#' Computes the variance of a negative binomial distribution given the mean
#' \eqn{\mu} and the dispersion parameter \eqn{\theta}.
#'
#' @param mu Numeric. The mean(s) of the distribution.
#' @param theta Numeric. The dispersion parameter(s).
#'
#' @details
#' For a negative binomial distribution parameterized by mean \eqn{\mu} and
#' dispersion \eqn{\theta}, the variance is
#' \deqn{Var(Y) = \mu + \frac{\mu^2}{\theta}}{}
#'
#' - Larger \eqn{\theta} corresponds to less overdispersion (approaching the
#'   Poisson case as \eqn{\theta \to \infty}).
#' - Smaller \eqn{\theta} corresponds to greater overdispersion.
#'
#' @return A numeric value (or vector) giving the variance.
#'
#' @examples
#' nb.theta2var(mu = 5, theta = 2)
#' nb.theta2var(mu = 10, theta = 100)  # approaches Poisson variance
#'
#' @seealso \code{\link{theta_from_vmr}}, \code{\link{vmr_from_theta}}
#'
#' @export

nb.theta2var <- function(mu, theta) {
  mu + mu^2 / theta
}


#' Compute Gamma Distribution Parameters
#'
#' This function computes all common parameters of a gamma distribution
#' given **exactly two** known parameters. It supports multiple parameterizations,
#' including mean, standard deviation, shape, scale, rate, coefficient of variation (CV), and skewness.
#'
#' @param mean Numeric. The mean of the distribution.
#' @param sd Numeric. The standard deviation of the distribution.
#' @param shape Numeric. The shape parameter (\eqn{\alpha}) of the gamma distribution.
#' @param scale Numeric. The scale parameter (\eqn{\theta}) of the gamma distribution.
#' @param rate Numeric. The rate parameter (\eqn{\lambda = 1/\theta}) of the gamma distribution.
#' @param skew Numeric. The skewness of the gamma distribution (\eqn{2 / \sqrt{\alpha}}).
#' @param cv Numeric. The coefficient of variation (\eqn{1 / \sqrt{\alpha}}).
#'
#' @details
#' The gamma distribution has the following relationships:
#' \deqn{\text{mean} = \alpha \theta}{mean = alpha * theta}
#' \deqn{\text{sd} = \sqrt{\alpha} \theta}{sd = sqrt(alpha) * theta}
#' \deqn{\text{cv} = 1 / \sqrt{\alpha}}{cv = 1 / sqrt(alpha)}
#' \deqn{\text{skew} = 2 / \sqrt{\alpha}}{skew = 2 / sqrt(alpha)}
#' \deqn{\text{rate} = 1 / \theta}{rate = 1 / theta}
#'
#' Supported input pairs:
#' \itemize{
#'   \item \code{mean} + \code{sd}
#'   \item \code{mean} + \code{shape}
#'   \item \code{mean} + \code{cv}
#'   \item \code{shape} + \code{rate}
#'   \item \code{shape} + \code{scale}
#'   \item \code{mean} + \code{skew}
#' }
#'
#' The function calculates the remaining parameters and returns a complete list
#' containing:
#' \itemize{
#'   \item \code{shape} - shape parameter \eqn{\alpha}
#'   \item \code{rate} - rate parameter \eqn{\lambda = 1/\theta}
#'   \item \code{scale} - scale parameter \eqn{\theta}
#'   \item \code{cv} - coefficient of variation
#'   \item \code{skew} - skewness
#'   \item \code{mean} - mean
#'   \item \code{sd} - standard deviation
#' }
#'
#' @return A named list with all the gamma distribution parameters.
#'
#' @examples
#' # Using mean and sd
#' gamma_params(mean = 10, sd = 5)
#'
#' # Using shape and rate
#' gamma_params(shape = 4, rate = 2)
#'
#' # Using mean and skew
#' gamma_params(mean = 10, skew = 1)
#'
#' @seealso \code{\link[stats]{dgamma}}, \code{\link[stats]{rgamma}}, \code{\link[stats]{pgamma}}
#'
#' @export

gamma_params <- function(mean = NULL,
                         sd = NULL,
                         shape = NULL,
                         scale = NULL,
                         rate = NULL,
                         skew = NULL,
                         cv = NULL){
  pars <- as.list(environment())
  spars <- pars[!sapply(pars, is.null)]

  if(length(spars) != 2) stop("Please provide exactly two parameters from a supported pair.")

  is_pair <- function(x, pair) identical(sort(x), sort(pair))

  compute <- function(shape, scale) {
    rate <- 1 / scale
    mean <- shape * scale
    sd <- sqrt(shape * scale^2)
    cv <- 1 / sqrt(shape)
    skew <- 2 / sqrt(shape)
    list(shape = shape, rate = rate, scale = scale, cv = cv, skew = skew, mean = mean, sd = sd)
  }

  if(is_pair(names(spars), c("mean", "sd"))){
    mean <- spars$mean
    sd <- spars$sd
    cv <- sd/mean
    shape <- 1 / cv^2
    scale <- mean / shape
  } else if(is_pair(names(spars), c("mean", "shape"))){
    mean <- spars$mean
    shape <- spars$shape
    scale <- mean / shape
  } else if(is_pair(names(spars), c("mean", "cv"))){
    mean <- spars$mean
    cv <- spars$cv
    shape <- 1 / cv^2
    scale <- mean * cv^2
  } else if(is_pair(names(spars), c("shape", "rate"))){
    shape <- spars$shape
    scale <- 1/spars$rate
  }else if(is_pair(names(spars), c("shape", "scale"))){
    shape <- spars$shape
    scale <- spars$scale
  }else if(is_pair(names(spars), c("mean", "skew"))){
    mean <- spars$mean
    skew <- spars$skew
    shape <- 4 / skew^2
    scale <- mean / shape
  } else{
    stop("combination not implemented!")
  }
  compute(shape, scale)
}

#' Plot Probability Distributions for Multiple Parameter Sets
#'
#' `distplot()` generates and plots probability distributions (Normal, Gamma, Poisson)
#' for one or more sets of distribution parameters. Each parameter set is visualized
#' as a separate curve with an automatically generated legend.
#'
#' @param dist Character. Distribution name. Supported: `"normal"`, `"gamma"`, `"poisson"`.
#' @param dist.args Named list or data.frame. Distribution parameters. Each row (if `data.frame`)
#'   corresponds to one curve. Examples:
#'   \itemize{
#'     \item Normal: `list(mean = c(0, 2), sd = c(1, 0.5))`
#'     \item Gamma: `list(shape = c(2, 5), scale = 1)`
#'     \item Poisson: `list(lambda = c(1, 5, 10))`
#'   }
#' @param n Integer. Number of random samples drawn per parameter set (default: `1000`).
#' @param ... Additional arguments passed to `tinyplot::tinyplot()`.
#'
#' @details
#' For each parameter set, `n` random samples are generated and their density
#' (or probability for discrete distributions) is computed.
#' Curves are drawn using `tinyplot::tinyplot()`, with conditioning on the
#' parameter combination (shown in the legend).
#'
#' Internally:
#' \itemize{
#'   \item `dnorm`, `dgamma`, `dpois` are used for densities.
#'   \item `rnorm`, `rgamma`, `rpois` are used for random draws.
#'   \item Parameter values are automatically expanded across multiple sets.
#' }
#'
#' @return A plot of the distribution curves, faceted/colored by parameter sets.
#' Also invisibly returns the data frame used for plotting.
#'
#' @examples
#' # Normal distributions
#' distplot("normal", dist.args = list(mean = c(0, 2), sd = c(1, 0.5)), n = 2000)
#'
#' # Gamma distributions
#' distplot("gamma", dist.args = list(shape = c(2, 5), scale = 1))
#'
#' # Poisson distributions
#' distplot("poisson", dist.args = list(lambda = c(2, 5, 10)), n = 500)
#'
#' @importFrom tinyplot tinyplot
#' @export
distplot <- function(dist, dist.args = NULL, n = 1e3, ...){

  if(dist == "normal"){
    dens <- dnorm
    rng <- rnorm
    name <- "dnorm"
  } else if(dist == "gamma"){
    dens <- dgamma
    rng <- rgamma
    name = "dgamma"
  } else if(dist == "poisson"){
    dens <- dpois
    rng <- rpois
    name = "dpois"
  } else{
    stop("distribution not implemented yet!")
  }

  dist.args.dens <- formals(dens)
  dist.args.rng <- formals(rng)
  dist.args.dens <- dist.args.dens[!names(dist.args.dens) %in% c("x", "n")]
  dist.args.rng <- dist.args.rng[!names(dist.args.rng) %in% c("x", "n")]

  if(!is.null(dist.args)){
    dist.args.dens[names(dist.args.dens) %in% names(dist.args)] <- dist.args
    dist.args.rng[names(dist.args.rng) %in% names(dist.args)] <- dist.args
  }

  dist.args.densd <- data.frame(dist.args.dens)
  dist.args.rngd <- data.frame(dist.args.rng)

  x <- do.call(mapply, c(rng, cbind(dist.args.rngd, n = n), SIMPLIFY = FALSE))
  y <- do.call(mapply, c(dens, c(dist.args.rngd, x = list(x)), SIMPLIFY = FALSE))

  dist.args.rngd$cond <- .paste_arg_value(dist.args.rngd)
  dd <- dist.args.rngd[rep(1:nrow(dist.args.rngd), each = n), ]

  dd <- cbind(
    dd,
    x = unlist(x),
    y = unlist(y)
  )

  dd <- dd[order(dd$cond, dd$x), ]
  tinyplot::tinyplot(y ~ x | cond,
                     data = dd,
                     type = "l",
                     legend = list("topright", title = ""),
                     xlab = "x",
                     ylab = name,
                     lwd = 2,
                     ...)
  invisible(dd)
}

distplot <- function(dist, dist.args = NULL, n = 1e3, ...){

  if(dist == "normal"){
    dens <- dnorm
    rng <- rnorm
    name <- "dnorm"
  } else if(dist == "gamma"){
    dens <- dgamma
    rng <- rgamma
    name = "dgamma"
  } else if(dist == "poisson"){
    dens <- dpois
    rng <- rpois
    name = "dpois"
  } else{
    stop("distribution not implemented yet!")
  }

  dist.args.dens <- formals(dens)
  dist.args.rng <- formals(rng)
  dist.args.dens <- dist.args.dens[!names(dist.args.dens) %in% c("x", "n")]
  dist.args.rng <- dist.args.rng[!names(dist.args.rng) %in% c("x", "n")]

  if(!is.null(dist.args)){
    dist.args.dens[names(dist.args.dens) %in% names(dist.args)] <- dist.args
    dist.args.rng[names(dist.args.rng) %in% names(dist.args)] <- dist.args
  }

  dist.args.densd <- data.frame(dist.args.dens)
  dist.args.rngd <- data.frame(dist.args.rng)

  x <- do.call(mapply, c(rng, cbind(dist.args.rngd, n = n), SIMPLIFY = FALSE))
  y <- do.call(mapply, c(dens, c(dist.args.rngd, x = list(x)), SIMPLIFY = FALSE))

  dist.args.rngd$cond <- .paste_arg_value(dist.args.rngd)
  dd <- dist.args.rngd[rep(1:nrow(dist.args.rngd), each = n), ]

  dd <- cbind(
    dd,
    x = unlist(x),
    y = unlist(y)
  )

  dd <- dd[order(dd$cond, dd$x), ]
  tinyplot::tinyplot(y ~ x | cond,
           data = dd,
           type = "l",
           legend = list("topright", title = ""),
           xlab = "x",
           ylab = name,
           lwd = 2,
           ...)
  invisible(dd)
}










