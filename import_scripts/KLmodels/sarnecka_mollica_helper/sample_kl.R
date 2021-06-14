#' Sample Knower Level Model
#'
#' Conduct a Bayesian data analysis to infer the knower level, prior response distribution and weight of evidence update.
#' This functions returns samples, which can be extracted with helper functions to evaluate knower levels, model
#' performance and parameters. This function is a wrapper for Lee & Sarnecka (2011) stan model and is adapted from
#' \url{https://github.com/stan-dev/example-models/blob/master/Bayesian_Cognitive_Modeling/CaseStudies/NumberConcepts/NumberConcept_1_Stan.R}
#'
#' @param giveNdata An objet of class \code{data.frame} containing data with Participant, Query and Response columns.
#' @param iter Number of total iterations per chain (including warmup; defaults to 2000).
#' @param chains Number of Markov chains (defaults to 4).
#' @param cores Number of cores to use when executing the chains in parallel, which defaults to 1 but we recommend setting the
#'   \code{mc.cores} option to be as many processors as the hardware and RAM allow (up to the number of chains). For non-Windows
#'   OS in non-interactive \R sessions, forking is used instead of PSOCK clusters.
#' @param ... Further arguments passed to Stan that is to \code{\link[rstan:sampling]{sampling}} or \code{\link[rstan:vb]{vb}}.
#'
#' @return samples An object of S4 class \code{\link{stanfit}}. However, if \code{cores > 1} and there is an error for any of the
#' chains, then the error(s) are printed. If all chains have errors and an error occurs before or during sampling, the returned
#' object does not contain samples. But the compiled binary object for the model is still included, so we can reuse the returned
#' object for another sampling.
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @references Lee, M. D., & Sarnecka, B. W. (2010). A model of knower-level behavior in number concept development.
#' \emph{Cognitive Science}, 34 (1), 51–67.\cr\cr Lee, M. D., & Sarnecka, B. W. (2011). Number-knower levels in young
#' children: Insights from Bayesian modeling. \cite{Cognition}, 120 (3), 391–402.
#'
#' @examples \dontrun{
#'
#' samples = sample_kl{giveN, iters=2000, chains=4, cores=4}
#' }
#'
#' @seealso \code{\link{stanfit}}
#'
#' @import Rcpp
#' @import dplyr
#' @import tidyr
#' @import rstan
#' @export

sample_kl <- function(giveNdata, iter=2000, chains=4, cores = getOption("mc.cores", 1L), ...) {
    # Wrapper for Lee & Sarnecka (2011) stan model
    # Adapted from https://github.com/stan-dev/example-models/blob/master/Bayesian_Cognitive_Modeling/CaseStudies/NumberConcepts/NumberConcept_1_Stan.R

    model <- "
    data {
      int<lower=1> ns;
      int<lower=1> nz;
      int<lower=1> gn;
      int<lower=1> mo;
      int gnq[ns];
      int gq[ns,mo];
      int ga[ns,mo];
    }

    parameters {
      vector<lower=0,upper=1>[gn] pitmp;
      real<lower=1,upper=1000> v;
    }

    transformed parameters {
      simplex[gn] pi;
      simplex[gn] npiprime[nz,gn];
      vector[nz] lp_parts[ns];
      // Base rate
      pi = pitmp / sum(pitmp);

      // Model
      for (i in 1:nz) {
        for (j in 1:gn) {
          vector[gn] piprime;
          for (k in 1:gn) {
            real ind1;
            real ind2;
            real ind3;
            real ind4;
            real ind5;

            // Will be 1 if Knower-Level (i.e, i-1) is Same or Greater than Answer
            ind1 = step((i - 1) - k);
            // Will be 1 for the Possible Answer that Matches the Question
            ind2 = k == j;
            // Will be 1 for 0-Knowers
            ind3 = i == 1;
            // Will be 1 for HN-Knowers
            ind4 = i == nz;
            ind5 = ind3 + ind4 * (2 + ind2)
              + (1 - ind4) * (1 - ind3) * (ind1 * ind2 + ind1 + 1);

            if (ind5 == 1)
              piprime[k] = pi[k];
            else if (ind5 == 2)
              piprime[k] = 1 / v * pi[k];
            else if (ind5 == 3)
              piprime[k] = v * pi[k];
          }
          for (k in 1:gn)
            npiprime[i,j,k] = piprime[k] / sum(piprime);
        }
      }

      for (i in 1:ns) {
        for (m in 1:nz) {
          real lp_parts_tmp;
          lp_parts_tmp = 0;

          // Probability a z[i]-Knower Will Answer ga[i,j] to Question gq[i,j]
          // is a Categorical Draw From Their Distribution over the 1:gn Toys
          for (j in 1:gnq[i]){
              lp_parts_tmp = lp_parts_tmp
              + categorical_lpmf(ga[i,j] | npiprime[m,gq[i,j]]);
          }
          lp_parts[i,m] = log(1.0 / nz) + lp_parts_tmp;
        }
      }
    }

    model {
      for (i in 1:ns)
        target += (log_sum_exp(lp_parts[i]));
    }

    generated quantities {
      vector[nz] prob;
      int z[ns];
      int predga[ns,gn];
      int predz[nz,gn];
      int predpi;

      for (i in 1:ns) {
        prob = softmax(lp_parts[i]);
        z[i] = categorical_rng(prob);
      }

      // Posterior Predictive
      for (i in 1:ns)
        for (j in 1:gn)
          predga[i,j] = categorical_rng(npiprime[z[i],j]);

      // Posterior Prediction For Knower Levels
      for (i in 1:nz)
        for (j in 1:gn)
          predz[i,j] = categorical_rng(npiprime[i,j]);

      predpi = categorical_rng(pi);
    }"

    # Number of participants
    ns <- length(unique(giveNdata$Participant))

    # Queries
    gq <- dplyr::select(giveNdata, Participant, Response, Query)
    gq <- dplyr::group_by(gq, Participant)
    gq <- dplyr::arrange(gq, Query)
    gq <- dplyr::mutate(gq, Time=1:dplyr::n())
    gq <- dplyr::ungroup(gq)
    gq <- dplyr::select(gq, -Response)
    gq <- tidyr::spread(gq, Time, Query, fill = 0)
    gq <- dplyr::select(gq, -Participant)
    gq <- unname(as.matrix(gq))

    # Number of queries per person
    gnq <- rowSums(((gq > 0)*1))

    # Responses to queries
    ga <- dplyr::select(giveNdata, Participant, Response, Query)
    ga <- dplyr::group_by(ga, Participant)
    ga <- dplyr::arrange(ga, Query)
    ga <- dplyr::mutate(ga, Time=1:dplyr::n())
    ga <- dplyr::ungroup(ga)
    ga <- dplyr::select(ga, -Query)
    ga <- tidyr::spread(ga, Time, Response, fill = 0)
    ga <- dplyr::select(ga, -Participant)
    ga <- unname(as.matrix(ga))

    # Maximum set in Give-N
    gn <- max(ga)

    # Max number of queries per person (a lot of zeros do to titration method)
    mo <- ncol(gq)

    # Knower Levels estimating
    nz <- 6

    data <- c("ns", "gnq", "gn", "ga", "gq", "nz", "mo")

    inx <- dplyr::select(giveNdata, Participant)
    inx <- dplyr::arrange(inx, Participant)
    inx <- dplyr::distinct(inx)
    inx <- dplyr::mutate(inx, index=1:dplyr::n())

    # parameters to be monitored:
    parameters <- c("pi", "predga", "predz", "predpi", "v", "z")

    s <- rstan::stan(model_code = model,
                data = data,
                pars = parameters,
                iter = iter,
                chains = chains,
                cores = cores,
                ...)

    s <- as(s, 'knowerSamples')
    s@inx <- inx

    s
}
