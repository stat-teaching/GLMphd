# Volunteering

sim_volunteering <- function(n){
  age <- round(runif(n, 30, 65))
  sconnection <- round(rnorm(n, 8, 2.5))
  sconnection <- cut_extreme(sconnection, 1, 10)
  ses <- sample(1:4, n, TRUE, c(0.1, 0.5, 0.4, 0.2))

  volunt <- data.frame(
    id = 1:n,
    age,
    sconnection,
    ses
  )

  volunt$ses <- factor(volunt$ses)
  volunt$age0 <- volunt$age - mean(volunt$age)
  volunt$sconnection0 <- volunt$sconnection - mean(volunt$sconnection)

  b0 <- qlogis(0.2) # p vol age mean, sconnection mean, ses 1
  b1 <- log(1.5) # age effect, ses 1
  b2 <- log(1.5) # sconnection 0 ses 1
  b3 <- log(1.2) # ses 2 vs 1, sconnection 0
  b4 <- log(2) # ses 3 vs 1, sconnection 0
  b5 <- log(2.5) # ses 4 vs 1, sconnection 0
  b6 <- log(1) # sconnection:ses2 vs sconnection:ses1
  b7 <- log(1) - b2 # sconnection:ses3 vs sconnection:ses1
  b8 <- log(1) - b2 # sconnection:ses4 vs sconnection:ses1

  B <- c(b0, b1, b2, b3, b4, b5, b6, b7, b8)
  X <- model.matrix(~age0 + sconnection0 + ses + sconnection0:ses, data = volunt)

  volunt$lp <- plogis(as.numeric(X %*% B))
  volunt$vol <- rbinom(nrow(volunt), 1, volunt$lp)

  return(volunt)
}


# nwords

sim_nwords <- function(n){
  # Generate the population

  S <- diag(c(2,2,2,8,8,8)) %*% (0.6 + diag(1 - 0.6, nrow = 6)) %*% diag(c(2,2,2,8,8,8))
  xs <- MASS::mvrnorm(1e5, mu = c(8, 5, 3, 20, 15, 8), Sigma = S, empirical = TRUE)
  colnames(xs) <- c("caregiving_low", "caregiving_middle", "caregiving_high",
                    "timebonding_low", "timebonding_middle", "timebonding_high")

  xs <- data.frame(xs)

  caregiving_d <- xs[, 1:3] |>
    pivot_longer(1:3) |>
    separate(name, c("x", "ses"), sep = "_")

  timebonding_d <- xs[, 4:6] |>
    pivot_longer(1:3) |>
    separate(name, c("x", "ses"), sep = "_")

  pop <- data.frame(
    id = 1:nrow(xs),
    timebonding = timebonding_d$value,
    caregiving = caregiving_d$value,
    ses = timebonding_d$ses
  )

  # to discrete

  pop$timebonding <- round(pop$timebonding)
  pop$timebonding <- ifelse(pop$timebonding < 0, 0, pop$timebonding)

  pop$caregiving <- round(pop$caregiving)
  pop$caregiving <- ifelse(pop$caregiving < 0, 0, pop$caregiving)

  pop$ses <- factor(pop$ses, levels = c("low", "middle", "high"))
  ses_prob <- c(0.3, 0.5, 0.2)
  nsample <- floor(n*ses_prob)
  datl <- split(pop, pop$ses)
  datl <- purrr::map2(datl, nsample, ~.x[sample(1:nrow(.x), .y), ])
  dat <- bind_rows(datl)

  dat$babysitter <- ifelse(dat$ses == "low",
                           sample(c("yes", "no"), sum(dat$ses == "low"), TRUE, prob = c(0.2, 0.8)),
                           ifelse(dat$ses == "middle",
                                  sample(c("yes", "no"), sum(dat$ses == "middle"), TRUE, prob = c(0.4, 0.6)),
                                  sample(c("yes", "no"), sum(dat$ses == "high"), TRUE, prob = c(0.8, 0.2))))


  dat$babysitter <- factor(dat$babysitter, levels = c("no", "yes"))
  dat$id <- 1:nrow(dat)
  dat <- set_contrasts(dat, contr.treatment)

  dat$timebonding0 <- dat$timebonding - mean(dat$timebonding)
  dat$caregiving0 <- dat$caregiving - mean(dat$caregiving)
  dat <- add_numeric_contrast(dat)

  theta <- 5 # expected vmr = var_from_theta(mean(dat$nwords), theta) / mean(dat$nwords)

  # true model nwords ~ caregiving + babysitter + timebonding*ses

  b0 <- log(20) # intercept
  b1 <- log(1.05) # caregiving
  b2 <- log(1) # babysitter
  b3 <- log(1.02) # timebonding (babysitter 0 and ses low)
  b4 <- log(0.9) # middle vs low
  b5 <- log(0.8) # high vs low
  b6 <- log(1.03) - b3 # timebonding effect middle vs low
  b7 <- log(1.04) - b3 # timebonding effect high vs low

  B <- c(b0, b1, b2, b3, b4, b5, b6, b7)

  X <- model.matrix(~caregiving0 + babysitter + timebonding0*ses, data = dat)
  dat$lp <- exp(X %*% B)
  #dat$nwords <- rpois(nrow(dat), dat$lp)
  dat$nwords <- MASS::rnegbin(nrow(dat), dat$lp, theta)

  return(dat)
}

# visibility ~ contrast

sim_psych <- function(ns, nt){
  ncatch <- floor(0.1 * nt)
  contrast <- seq(0, 1, length.out = nt - ncatch)
  contrast <- c(rep(0, ncatch), contrast)
  cond <- c("face", "gabor", "shape")

  dat <- expand.grid(
    id = 1:ns,
    cond = cond,
    contrast = contrast
  )

  dat$cond <- factor(dat$cond, levels = c("shape", "gabor", "face"))

  X <- model.matrix(~contrast * cond, data = dat)

  b0 <- qlogis(0.1)
  b1 <- 6
  b2 <- 0
  b3 <- log(1.5)
  b4 <- 1
  b5 <- 2

  sb0 <- 0.3
  sb1 <- 0.2

  b0i <- rnorm(ns, 0, sb0)
  b1i <- rnorm(ns, 0, sb1)

  dat$lp <- (b0 + b0i[dat$id]) + (b1 + b1i[dat$id]) * X[, "contrast"] + b2 * X[, "condgabor"] +
    b3 * X[, "condface"] + b4 * X[, "contrast:condgabor"] + b5 * X[, "contrast:condface"]

  dat$p <- plogis(dat$lp)
  dat$y <- rbinom(nrow(dat), 1, dat$p)

  return(dat)

  # dat |>
  #   filter(id < 20) |>
  #   ggplot(aes(x = contrast, y = p, color = cond, group = interaction(id, cond))) +
  #   geom_line()

}
