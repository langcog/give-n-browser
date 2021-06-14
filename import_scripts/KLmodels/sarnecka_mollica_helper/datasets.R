#' GiveN trial data
#'
#' @description This dataset contains trial by trial GiveN data.
#'
#' @format A data frame with 27424 rows and 7 variables:
#' \describe{
#'   \item{Experiment}{The source experiment for the data first author's last name and year of publication}
#'   \item{Language}{The language in which the task was conducted.}
#'   \item{Participant}{A unique participant ID}
#'   \item{Sex}{The sex of the participant}
#'   \item{Age}{The age of the participant, rounded to nearest month}
#'   \item{Query}{The qurey for the trial}
#'   \item{Response}{The participant's response}
#' }
#'
#' @source Almoammer, A., Sullivan, J., Donlan, C., Marušič, F., ODonnell, T., & Barner, D. (2013).
#'   Grammatical morphology as a source of early number word meanings. \emph{Proceedings of the National
#'   Academy of Sciences}, 201313652.\cr\cr
#'   Boni, I., Jara-Ettinger, J., & Piantadosi, S.T. (unpublished). \cr\cr
#'   Jara-Ettinger, J. (unpublished). \cr\cr
#'   Krajcsi, A., Fintor, E., & Hodossy, L. (2018). A refined description of preschoolers initial symbolic number
#'   learning. Retrieved from \url{osf.io/2kh9s} doi: 10.31219/osf.io/2kh9s \cr\cr
#'   Piantadosi, S. T., Jara-Ettinger, J., & Gibson, E. (2014). Children’s learning of number words in an
#'   indigenous farming-foraging group. \emph{Developmental Science}, 17 (4), 553–563.\cr\cr
#'   Sarnecka, B. W., Kamenskaya, V. G., Yamana, Y., Ogura, T., & Yudovina, Y. B. (2007). From
#'   grammatical number to exact numbers: Early meanings of one,two, and three in English, Russian, and Japanese.
#'   \emph{Cognitive Psychology}, 55 (2), 136–168.\cr\cr
#'   Sarnecka, B. W., & Negen, J. (2019). Longitudinal Number-Knower Data. Retrieved from \url{https://osf.io/eznht/}
#'   doi: 10.17605/osf.io/eznht \cr\cr
#'   Wagner, K., Kimura, K., Cheung, P., & Barner, D. (2015). Why is number word learning hard? evidence from bilingual learners.
#'   \emph{Cognitive Psychology}, 83, 1–21.
"giveN"

#' Longitudinal GiveN data
#'
#' @description Tidy formatted version of the Sarnecka & Negen (2019) longitudinal GiveN dataset. Original description:
#' Longitudinal data from 97 preschool-age children, tested every 2 weeks for a total of up to 10 sessions, on two tasks
#' per session. Many data are missing, reflecting the fact that the child was either not in school or didn’t want to play
#' on the day of that session. In each session, the child completed two tasks: Give-N, where children construct a set of
#' 1-8 items, and What’s-on-this-card (WOTC), where children say the number of items that are in a picture. The tasks were
#' matched in that the pictures were actually photographs of the same small plastic counters used in the Give-N tasks, and
#' the set sizes requested in the Give-N task were the same as those pictured on the cards in the WOTC task. The word
#' “count” indicates that the child counted to produce the set (in Give-N) or counted the items in the picture (in WOTC).
#' Data were collected by undergraduate research assistants in the Sarnecka Lab in Cognitive Sciences at UC-Irvine, under
#' the direction of P.I. Barbara Sarnecka and Dr. James Negen, now at Durham University.
#'
#' @format A tibble with 23555 rows and 7 variables:
#' \describe{
#'   \item{Experiment}{The source experiment for the data first author's last name and year of publication}
#'   \item{Language}{The language in which the task was conducted.}
#'   \item{Participant}{A unique participant ID}
#'   \item{Gender}{The gender of the participant}
#'   \item{Age.M}{The age of the participant, rounded to nearest month}
#'   \item{Age.D}{The age of the participant, rounded to nearest day}
#'   \item{Session}{The testing session for the participant}
#'   \item{Task}{The number task, either GiveN or What's on this card? (WOTC)}
#'   \item{Query}{The qurey for the trial}
#'   \item{Response}{The participant's response}
#' }
#'
#' @source Sarnecka, B. W., & Negen, J. (2019). Longitudinal Number-Knower Data. Retrieved from \url{https://osf.io/eznht/}
#'   doi: 10.17605/osf.io/eznht
"lgGiveN"

#' Knower Level data
#'
#' @description This dataset contains knower level data for a large sample of children compiled from sources below. If the data were
#' provided as GiveN trial data, the knower levels were inferred via Lee & Sarnecka (2010)'s Bayesian data analysis as implemented by
#' the \code{\link{compute_kl}} function.
#'
#' @format A tibble with 2476 rows and 6 variables:
#' \describe{
#'   \item{Experiment}{The source experiment for the data first author's last name and year of publication}
#'   \item{Participant}{A unique participant ID}
#'   \item{Sex}{The sex of the participant}
#'   \item{Age}{The age of the participant, rounded to nearest month}
#'   \item{KL}{The knower level of the participant}
#'   \item{Language}{The language in which the task was conducted}
#' }
#'
#' @source Almoammer, A., Sullivan, J., Donlan, C., Marušič, F., ODonnell, T., & Barner, D. (2013).
#'   Grammatical morphology as a source of early number word meanings. \emph{Proceedings of the National
#'   Academy of Sciences}, 201313652.\cr\cr
#'   Barner, D., Chow, K., & Yang, S.-J. (2009). Finding ones meaning: A test of the relation between
#'   quantifiers and integers in language development. \emph{Cognitive psychology}, 58 (2), 195–219.\cr\cr
#'   Barner, D., Libenson, A., Cheung, P., & Takasaki, M. (2009). Cross-linguistic relations between quantifiers
#'   and numerals in language acquisition: Evidence from Japanese. \emph{Journal of experimental child psychology},
#'   103 (4), 421–440.\cr\cr
#'   Boni, I., Jara-Ettinger, J., & Piantadosi, S.T. (unpublished). \cr\cr
#'   Jara-Ettinger, J. (unpublished). \cr\cr
#'   Krajcsi, A., Fintor, E., & Hodossy, L. (2018). A refined description of preschoolers initial symbolic number
#'   learning. Retrieved from \url{osf.io/2kh9s} doi: 10.31219/osf.io/2kh9s \cr\cr
#'   Marušič, F., Plesničar, V., Razboršek, T., Sullivan, J., Barner, D., et al. (2016). Does grammatical
#'   structure accelerate number word learning? evidence from learners of dual and non-dual dialects of Slovenian.
#'   \emph{PloS one}, 11 (8), e0159208.\cr\cr
#'   Piantadosi, S. T., Jara-Ettinger, J., & Gibson, E. (2014). Children’s learning of number words in an
#'   indigenous farming-foraging group. \emph{Developmental Science}, 17 (4), 553–563.\cr\cr
#'   Sarnecka, B. W., Kamenskaya, V. G., Yamana, Y., Ogura, T., & Yudovina, Y. B. (2007). From
#'   grammatical number to exact numbers: Early meanings of one,two, and three in English, Russian, and Japanese.
#'   \emph{Cognitive Psychology}, 55 (2), 136–168.\cr\cr
#'   Sarnecka, B. W., & Negen, J. (2019). Longitudinal Number-Knower Data. Retrieved from \url{https://osf.io/eznht/}
#'   doi: 10.17605/osf.io/eznht \cr\cr
#'   Wagner, K., Kimura, K., Cheung, P., & Barner, D. (2015). Why is number word learning hard? evidence from bilingual learners.
#'   \emph{Cognitive Psychology}, 83, 1–21.
#'
"knowers"
