sim_tv_shows <- function(n = 100, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  n_watching <- n / 2 # just for simplicity

  dat <- data.frame(
    id = 1:n,
    tv_shows = rep(c(1, 0), each = n_watching)
  )

  dat$h_study <- ifelse(dat$tv_shows == 1, 20, 60)
  dat$h_study <- dat$h_study + round(runif(n, -15, 15))
  dat$h_study[dat$h_study <= 0] <- 0
  dat$coffee <- ifelse(dat$tv_shows == 1, 5, 2) +
    sample(-2:2, n, replace = TRUE)
  dat$presence <- rbinom(n, 1, 0.5)

  # Coefficients chosen to respect your qualitative story
  b0 <- -4 # baseline intercept
  b_h <- 0.08 # study hours: more hours -> higher log-odds
  b_tv <- -2 # watching TV reduces log-odds
  b_cof <- 0.10 # more coffee generally helps
  b_int <- 0.2 # coffee helps *even more* when watching TV
  b_presence <- 1.5 # study group helps
  b_presence_tv_shows <- -0.9 # but less helpful if watching TV

  dat$eta <- with(
    dat,
    b0 +
      b_h * h_study +
      b_tv * tv_shows +
      b_cof * coffee +
      b_int * tv_shows * coffee +
      b_presence * presence +
      b_presence_tv_shows * presence * tv_shows
  )

  dat$mu <- plogis(dat$eta) # probability of passing
  dat$passed <- rbinom(nrow(dat), 1, dat$mu)
  dat$presence <- factor(dat$presence)
  dat$tv_shows <- factor(dat$tv_shows)
  dat$age <- round(runif(n, 19, 25))
  dat$faculty <- sample(c("ing", "psy", "math", "med", "art"), n, TRUE)
  return(dat)
}


sim_dataset <- function(data, n = 100, seed = NULL) {
  fun <- switch(data, "tv_shows" = sim_tv_shows)
  fun(n, seed = seed)
}
