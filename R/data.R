#' Example dataset of students drop out rates.
#'
#' This dataset contains data about high-school drop out.
#'
#' @format A data frame with 500 rows and 4 variables:
#' \describe{
#'   \item{id}{Unique identifier for each student}
#'   \item{parenting}{The parenting style of the student: authoritarian, authoritative, neglectful or permissive}
#'   \item{academic}{Academic success: high or low}
#'   \item{drop}{Whether the student dropped out or not}
#' }
"drop"

#' Graduate Admissions Dataset
#'
#' This dataset contains information on applicants to a graduate program.
#'
#' @format A data frame with 400 rows and 4 variables:
#' \describe{
#'   \item{admit}{Binary outcome: 1 if admitted, 0 if not admitted}
#'   \item{gre}{GRE test score (numeric)}
#'   \item{gpa}{Grade Point Average (numeric, scale approx. 0–6)}
#'   \item{rank}{Prestige of undergraduate institution (1 = highest, 4 = lowest)}
#' }
#' @source Simulated or example data commonly used in statistics textbooks
"admission"

#' Example dataset on childcare and language exposure
#'
#' This dataset contains information about 150 children, including their
#' ID, time spent bonding, parental caregiving scale, socio-economic status,
#' whether they had a babysitter, and the number of words they produced.
#'
#' @format A data frame with 150 rows and 6 variables:
#' \describe{
#'   \item{id}{Child identifier (integer).}
#'   \item{timebonding}{Time spent bonding with caregivers, in minutes per day (integer).}
#'   \item{caregiving}{Parental caregiving scale}
#'   \item{ses}{Socio-economic status: "low" "middle" or "high" (character).}
#'   \item{babysitter}{Whether the child had a babysitter: "yes" or "no" (factor or character).}
#'   \item{nwords}{Number of words produced by the child (integer).}
#' }
#'
#' @source Simulated data for illustration purposes.
"nwords"

#' Example dataset on child temperament, attachment, and parental factors
#'
#' This dataset contains information on 122 children, including their temperament,
#' attachment style, parental self-efficacy, parental skills, and tantrum occurrences.
#'
#' @format A data frame with 122 rows and 6 variables:
#' \describe{
#'   \item{id}{Child identifier (integer).}
#'   \item{temperament}{Child temperament: "easy" or "difficult" (character).}
#'   \item{attachment}{Attachment style: "secure" or "insecure" (character).}
#'   \item{parent_se}{Parental self-efficacy score (numeric).}
#'   \item{parent_skills}{Parental skills score (numeric).}
#'   \item{tantrum}{Number of tantrums exhibited by the child (integer).}
#' }
#'
#' @source Simulated data for illustration purposes.
"tantrums"

#' Example dataset on adults' social connection and volunteering
#'
#' This dataset contains information on 6 adults, including their age,
#' social connection, socio-economic status, and volunteering activity.
#'
#' @format A data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{id}{Participant identifier (integer).}
#'   \item{age}{Age of the participant (integer).}
#'   \item{sconnection}{Social connection score (integer).}
#'   \item{ses}{Socio-economic status: coded numerically (integer).}
#'   \item{vol}{Volunteering indicator: 0 = no, 1 = yes (integer).}
#' }
#'
#' @source Simulated data for illustration purposes.
"volunt"

#' Simon Task
#'
#' @description This data set is from an online experiment
#' in which participants classified a shape presented on the
#' screen as a square or circle. The shape was displayed on
#' either the left or right side of the screen.
#' The dataset is a cleaned and simplified version of https://github.com/michael-franke/aida-package/blob/master/data/data_ST_raw.rda.
#'
#' @format A data frame with 25560 rows and 15 variables.
#' The most important variables in this data set are:
#' \describe{
#'   \item{id}{A unique identifier for each participant.}
#'   \item{rt}{The reaction time for each trial.}
#'   \item{congruence}{Whether the trial was a congruent or
#'   an incongruent trial.}
#'   \item{acc}{Whether the answer in the current
#'   trial was correct or incorrect.}
#'   \item{trial_type}{Whether the data is from a practice
#'   or a main test trial.}
#' }
#'
#' @source \url{https://raw.githubusercontent.com/michael-franke/intro-data-analysis/master/data_sets/simon-task.csv}
#'
#' \href{https://michael-franke.github.io/intro-data-analysis/app-93-data-sets-simon-task.html}{The web-book chapter that covers the Simon Task data set}.
"simon"
