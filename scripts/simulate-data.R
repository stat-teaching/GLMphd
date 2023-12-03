source("scripts/data-funs.R")
devtools::load_all()

# Volunteering

set.seed(135)
volunt <- sim_volunteering(1000)

# fit <- glm(volunt ~ age0 + sconnection0 + ses + sconnection0:ses,
#            data = dat,
#            family = binomial(link = "logit"))

save(volunt, file = "data/volunt.rda")

# Nwords

set.seed(2024)
nwords <- sim_nwords(150)
nwords <- dplyr::select(nwords, id, timebonding, caregiving, ses, babysitter, nwords)

save(nwords, file = "data/nwords.rda")

# psych

psych <- sim_psych(20, 1000)
psych <- dplyr::select(psych, id, cond, contrast, y)

save(psych, file = "data/psych.rda")
