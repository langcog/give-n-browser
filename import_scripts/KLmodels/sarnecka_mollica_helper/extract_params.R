#' Extract inferred knower level from samples
#'
#' Extracts the inferred knower level from the samples.
#'
#' @param samples A stanfit object output from \code{sample_kl}
#' @param method A string specifying how to determine the knower level. Either posterior "mean" or posterior "mode"
#'
#' @return A data.frame with columns for Experiment, Paprticipant, Sex and Age (inherited from the input \code{giveNdata} and a
#' column specifying knower level (KL).
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' knowers <- extract_kl(samples, method='mean')
#'
#' }
#'
#' @seealso \code{\link{sample_kl}}
#'
#' @import dplyr
#' @import rstan
#' @import forcats
#' @export

extract_kl <- function(samples, method='mean'){
    if(!(method %in% c('mean','mode'))){
        stop("Method of estimation unknown. mean or mode")
    }
    z <- rstan::extract(samples)$z

    # d <- dplyr::select(data, Experiment, Participant, Sex, Age)
    # d <- dplyr::arrange(d, Participant)
    # d <- dplyr::distinct(d)
    d <- dplyr::arrange(samples@inx, index)
    if(method=='mean'){
        d <- dplyr::mutate(d,
                           KL=round(colSums(z)/nrow(z)),
                           KL = as.factor(KL),
                           KL = forcats::fct_recode(KL, 'Non'='1','One'='2','Two'='3','Three'='4','Four'='5','CP'='6'))
    } else {
        mv <- c()
        for (i in 1:nrow(d)) {  # computing mode of z for each child
            uz <- unique(z[, i])
            mv[i] <- uz[which.max(tabulate(match(z[, i], uz)))]
        }
        d <- dplyr::mutate(d,
                           KL= as.factor(mv),
                           KL = forcats::fct_recode(KL, 'Non'='1','One'='2','Two'='3','Three'='4','Four'='5','CP'='6'))
    }
    dplyr::select(d, -index)
}

#' Extract inferred intial response distribution from samples
#'
#' Extracts the inferred initial response distribution from the samples.
#'
#' @param samples A stanfit object output from \code{sample_kl}
#'
#' @return A data.frame with columns for Response and Probability of that response.
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' pi <- extract_pi(samples)
#'
#' }
#'
#' @seealso \code{\link{sample_kl}} \code{\link{plot_pi}}
#'
#' @import dplyr
#' @import tidybayes
#' @export

extract_pi <- function(samples){
    z <- tidybayes::spread_draws(samples, predpi)
    z <- dplyr::mutate(z, predpi=as.integer(predpi))
    z <- dplyr::group_by(z, predpi)
    z <- dplyr::summarise(z, N=dplyr::n())
    z <- dplyr::ungroup(z)
    Z <- sum(z$N)
    data.frame(Response=z$predpi,
               Probability=z$N/Z)
}

#' Plot the inferred intial response distribution
#'
#' Plots the inferred initial response distribution from the samples.
#'
#' @param samples A stanfit object output from \code{sample_kl}
#'
#' @return A ggplot2 plot object
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' pi <- plot_pi(samples)
#'
#' }
#'
#' @seealso \code{\link{sample_kl}} \code{\link{extract_pi}}
#'
#' @import dplyr
#' @import tidybayes
#' @import ggplot2
#' @export

plot_pi <- function(samples){
    z <- extract_pi(samples)
    g <- ggplot2::ggplot(z, aes(Response, Probability)) +
        geom_bar(stat='identity') +
        xlab('Response') +
        ylab('Base Rate') +
        scale_x_continuous(limits=c(0,15), breaks=seq(0,14,by=2)) + # added limits here, instead of coord_cartesian
        scale_y_continuous(breaks=seq(0,1,by=0.25), limits = c(0,1)) +
        theme_bw() +
        #coord_cartesian(xlim=c(1:15)) + # LAO: commented out because it was causing a zero_range error
        theme(legend.position='bottom')
    g
}

#' Extract inferred update strength from samples
#'
#' Extracts the inferred update strength $v$ from the samples.
#'
#' @param samples A stanfit object output from \code{sample_kl}
#'
#' @return A data.frame with columns for Posterior Mean and Lower and Upper posterior 95\% quantiles.
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' extract_v(samples)
#'
#' }
#'
#' @seealso \code{\link{sample_kl}}
#'
#' @import tidybayes
#' @export

extract_v <- function(samples){
    v <- tidybayes::spread_draws(samples, v)
    v <- v$v

    data.frame(Mean=mean(v),
               Lower=unname(quantile(v, probs=0.025)),
               Upper=unname(quantile(v, probs=0.975)))
}
