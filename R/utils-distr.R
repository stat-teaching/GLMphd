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

gamma_params <- function(
  mean = NULL,
  sd = NULL,
  shape = NULL,
  scale = NULL,
  rate = NULL,
  skew = NULL,
  cv = NULL
) {
  pars <- as.list(environment())
  spars <- pars[!sapply(pars, is.null)]

  if (length(spars) != 2) {
    stop("Please provide exactly two parameters from a supported pair.")
  }

  is_pair <- function(x, pair) identical(sort(x), sort(pair))

  compute <- function(shape, scale) {
    rate <- 1 / scale
    mean <- shape * scale
    sd <- sqrt(shape * scale^2)
    cv <- 1 / sqrt(shape)
    skew <- 2 / sqrt(shape)
    list(
      shape = shape,
      rate = rate,
      scale = scale,
      cv = cv,
      skew = skew,
      mean = mean,
      sd = sd
    )
  }

  if (is_pair(names(spars), c("mean", "sd"))) {
    mean <- spars$mean
    sd <- spars$sd
    cv <- sd / mean
    shape <- 1 / cv^2
    scale <- mean / shape
  } else if (is_pair(names(spars), c("mean", "shape"))) {
    mean <- spars$mean
    shape <- spars$shape
    scale <- mean / shape
  } else if (is_pair(names(spars), c("mean", "cv"))) {
    mean <- spars$mean
    cv <- spars$cv
    shape <- 1 / cv^2
    scale <- mean * cv^2
  } else if (is_pair(names(spars), c("shape", "rate"))) {
    shape <- spars$shape
    scale <- 1 / spars$rate
  } else if (is_pair(names(spars), c("shape", "scale"))) {
    shape <- spars$shape
    scale <- spars$scale
  } else if (is_pair(names(spars), c("mean", "skew"))) {
    mean <- spars$mean
    skew <- spars$skew
    shape <- 4 / skew^2
    scale <- mean / shape
  } else {
    stop("combination not implemented!")
  }
  compute(shape, scale)
}

ggnorm <- function(
  mean = 0,
  sd = 1,
  lf = 4,
  ns = 1e4,
  annotate = FALSE,
  theme = NULL,
  size = 15
) {
  require(ggplot2)
  if (length(mean) != length(sd)) {
    stop("mean and sd need to be of the same length!")
  }
  n <- length(mean)
  range <- c(min(mean) - lf * max(sd), max(mean) + lf * max(sd))
  x <- seq(range[1], range[2], length.out = ns)
  d <- mapply(function(m, s) dnorm(x, m, s), mean, sd, SIMPLIFY = FALSE)
  D <- data.frame(
    x = rep(x, n),
    d = unlist(d),
    mean = rep(mean, each = length(x)),
    sd = rep(sd, each = length(x))
  )
  D$msd <- sprintf("m = %s, s = %s", D$mean, D$sd)
  labs <- sprintf("$\\mu = %s$, $\\sigma = %s$", mean, sd)

  p <- ggplot(D, aes(x = x, y = d, color = msd)) +
    geom_line(lwd = 1) +
    scale_color_discrete(labels = lapply(labs, latex2exp::TeX)) +
    ylab(latex2exp::TeX("$dnorm(x, \\mu, \\sigma)$")) +
    xlim(range) +
    theme_minimal(size) +
    theme(legend.position = "bottom", legend.title = element_blank())
  if (annotate) {
    title <- sprintf("$\\mu = %.3f$, $\\sigma = %.3f$", mean, sd)
    title <- paste(title, collapse = " ")
    p <- p + ggtitle(latex2exp::TeX(title))
  }
  if (!is.null(theme)) {
    p + theme(size)
  }
  return(p)
}

ggpois <- function(
  lambda,
  lf = 3,
  type = c("h", "pl"),
  annotate = FALSE,
  theme = NULL,
  size = 15
) {
  require(ggplot2)
  type <- match.arg(type)
  range <- c(0, max(lambda) * lf)
  if (-min(lambda) * lf < 0) {
    range[1] <- 0
  } else {
    range[1] <- -min(lambda) * lf
  }
  x <- seq(range[1], range[2], 1)
  D <- expand.grid(x = x, lambda = lambda)
  D$d <- dpois(D$x, D$lambda)
  p <- ggplot(D, aes(x = x, y = d))
  if (type == "h") {
    p <- p + geom_col(aes(fill = factor(lambda)), width = 0.3)
  } else {
    p <- p +
      geom_point(aes(color = factor(lambda)), size = 3) +
      geom_line(aes(group = factor(lambda), color = factor(lambda)))
  }
  p <- p +
    xlab("x") +
    ylab(latex2exp::TeX("$dpois(x, \\lambda)$")) +
    labs(
      fill = latex2exp::TeX("$\\lambda$"),
      color = latex2exp::TeX("$\\lambda$")
    ) +
    theme_minimal(size) +
    theme(legend.position = "bottom") +
    if (!is.null(theme)) p <- p + theme(size)
  if (annotate) {
    title <- sprintf("$\\lambda_{%s} = %s$", 1:length(lambda), lambda)
    title <- paste(title, collapse = ", ")
    p <- p + ggtitle(latex2exp::TeX(title))
  }
  return(p)
}

eqs <- function() {
  list(
    gamma = "mean  = shape * scale   mean  = shape/rate \nvar   = shape * rate^2  var   = shape/rate^2"
  )
}


gamma_params <- function(
  shape = NULL,
  scale = 1 / rate,
  rate = 1,
  mean = NULL,
  sd = NULL,
  eqs = FALSE
) {
  if (eqs) {
    cat(rep("=", 25), "\n")
    cat(eqs()$gamma, "\n")
    cat(rep("=", 25), "\n")
  } else {
    if (is.null(shape)) {
      var <- sd^2
      shape <- mean^2 / var
      scale <- mean / shape
      rate <- 1 / scale
    } else if (is.null(mean) & is.null(sd)) {
      if (is.null(rate)) {
        scale <- 1 / rate
      } else {
        rate <- 1 / scale
      }
      mean <- shape * scale
      var <- shape * scale^2
      sd <- sqrt(var)
    } else {
      stop(
        "when shape and scale are provided, mean and sd need to be NULL (and viceversa)"
      )
    }
    out <- list(
      shape = shape,
      scale = scale,
      rate = rate,
      mean = mean,
      var = var,
      sd = sd
    )
    # coefficient of variation
    out$cv <- 1 / sqrt(shape)
    return(out)
  }
}

ggamma <- function(
  shape = NULL,
  scale = 1 / rate,
  rate = 1,
  mean = NULL,
  sd = NULL,
  show = c("msd", "sr", "ss"),
  lf = 5,
  size = 15,
  ns = 1e4
) {
  show <- match.arg(show)
  argg <- as.list(environment())[1:5]
  gm <- do.call(gamma_params, argg)
  if (length(unique(sapply(gm, length))) != 1) {
    stop("All vectors need to be of the same length!")
  }

  n <- length(gm$mean)
  range <- c(0, max(gm$mean) + lf * max(gm$sd))
  x <- seq(range[1], range[2], length.out = ns)
  d <- mapply(
    function(sh, sc) dgamma(x, shape = sh, scale = sc),
    gm$shape,
    gm$scale,
    SIMPLIFY = FALSE
  )
  D <- data.frame(
    x = rep(x, n),
    d = unlist(d),
    shape = rep(gm$shape, each = length(x)),
    scale = rep(gm$scale, each = length(x)),
    rate = rep(gm$rate, each = length(x)),
    mean = rep(gm$mean, each = length(x)),
    sd = rep(gm$sd, each = length(x))
  )

  if (show == "msd") {
    D$cond <- factor(sprintf("$\\mu = %.3f$, $\\sigma = %.3f$", D$mean, D$sd))
  } else if (show == "ss") {
    D$cond <- factor(sprintf(
      "$k (shape) = %.3f$, $\\theta (scale) = %.3f$",
      D$shape,
      D$scale
    ))
  } else {
    D$cond <- factor(sprintf(
      "$\\alpha (shape) = %.3f$, $\\beta (rate) = %.3f$",
      D$shape,
      D$rate
    ))
  }

  ggplot(D, aes(x = x, y = d, color = cond)) +
    geom_line(lwd = 1) +
    scale_color_discrete(labels = lapply(levels(D$cond), latex2exp::TeX)) +
    theme_minimal(15) +
    theme(
      legend.title = element_blank(),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    ) +
    ylab(latex2exp::TeX("dgamma($x$, $\\alpha$/$k$, $\\theta$, $\\beta$)"))
}

gamma_shape <- function(x, method = c("glm", "invskew")) {
  method = match.arg(method)
  if (method == "glm") {
    formula <- paste(deparse(substitute(x)), "~ 1")
    fit <- glm(as.formula(formula), family = Gamma(link = "log"))
    shape <- MASS::gamma.shape(fit)$alpha
  } else {
    skew <- psych::skew(x)
    shape <- 4 / skew^2
  }
  return(shape)
}
