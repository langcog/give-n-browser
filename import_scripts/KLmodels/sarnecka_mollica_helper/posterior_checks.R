#' Extract posterior predictive distribution of GiveN task
#'
#' Extracts the posterior predictive distribution over responses for all queries. See vignette for more explanation.
#'
#' @param samples A stanfit object output from \code{sample_kl}
#'
#' @return A data.frame with columns for Knower Level (KL), Query, Response and the Count of samples in the posterior.
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' pred_pi <- pred_check(samples)
#'
#' }
#'
#' @seealso \code{\link{sample_kl}} \code{\link{plot_pred_check}}
#'
#' @import dplyr
#' @import tidybayes
#' @import forcats
#' @export

pred_check <- function(samples){

    pred <- tidybayes::spread_draws(samples, predz[KL, Query])
    pred <- dplyr::rename(pred, Response=predz)
    pred <- dplyr::ungroup(pred)
    pred <- dplyr::mutate(pred,
                          KL = as.factor(KL),
                          KL = forcats::fct_recode(KL, 'Non'='1','One'='2','Two'='3','Three'='4','Four'='5','CP'='6'))
    pred <- dplyr::group_by(pred, KL, Query, Response)
    pred <- dplyr::summarise(pred, count=dplyr::n())
    dplyr::ungroup(pred)
}


#' Plot GiveN data against posterior predictive distribution of GiveN task
#'
#' Plots GiveN data against posterior predictive distribution of GiveN task. See vignette for more explanation.
#'
#' @param data An object of class \code{data.frame} containing data in the format of \code{giveN}.
#' @param samples A stanfit object output from \code{sample_kl}
#' @param method A string specifying how to determine the knower level. Either posterior "mean" or posterior "mode"
#'
#' @return A ggplot2 plot
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' plot_pred_check(giveN, samples, method='mode')
#'
#' }
#'
#' @seealso \code{\link{sample_kl}} \code{\link{pred_check}}
#'
#' @import dplyr
#' @import tidybayes
#' @import forcats
#' @import ggplot2
#' @export

plot_pred_check <- function(data, samples, method='mode'){
    real <- extract_kl(samples, method=method)
    real <- dplyr::left_join(data, real)
    real <- dplyr::group_by(real, Language, KL, Query, Response)
    real <- dplyr::summarise(real, count=dplyr::n())
    real <- dplyr::ungroup(real)

    pred <- pred_check(samples)

    g <- ggplot2::ggplot(pred, aes(Query, Response)) +
        facet_grid(~KL, scales='free', switch = 'y') +
        geom_tile(aes(fill=count)) +
        geom_point(aes(size=count, color=Language),
                   data=real) +
        scale_fill_gradient(low='grey95',high='grey80') +
        scale_x_continuous(breaks=1:10) +
        scale_y_continuous(breaks=1:30) +
        guides(fill=F, color=F, size=guide_legend(title='N')) +
        theme_classic() +
        coord_cartesian(xlim=c(0,10)) +
        theme(legend.position = 'bottom', text=element_text(size=18))

    g
}

#' Plot a single participant GiveN data against posterior predictive distribution of GiveN task
#'
#' Plots a single participant's GiveN data against posterior predictive distribution of GiveN task. See vignette for more explanation.
#'
#' @param participant The participant ID as a string
#' @param data An object of class \code{data.frame} containing data in the format of \code{giveN}.
#' @param samples A stanfit object output from \code{sample_kl}
#' @param method A string specifying how to determine the knower level. Either posterior "mean" or posterior "mode"
#'
#' @return A ggplot2 plot
#'
#' @author Francis Mollica \email{mollicaf@@gmail.com}
#'
#' @examples \dontrun{
#'
#' samples <- sample_kl{giveN, iters=2000, chains=4, cores=4}
#' pred_check_p('S1', giveN, samples, method='mode')
#'
#' }
#'
#' @seealso \code{\link{sample_kl}} \code{\link{pred_check}} \code{\link{plot_pred_check}}
#'
#' @import dplyr
#' @import tidybayes
#' @import forcats
#' @import ggplot2
#' @export

pred_check_p <- function(participant, data, samples, method='mode'){
    real <- extract_kl(samples, method=method)
    real <- dplyr::filter(real, Participant==participant)
    real <- dplyr::inner_join(data, real)
    real <- dplyr::group_by(real, Language, KL, Query, Response)
    real <- dplyr::summarise(real, count=dplyr::n())
    real <- dplyr::ungroup(real)
    kl <- unique(real$KL)

    pred <- pred_check(samples)
    pred <- dplyr::filter(pred, KL==kl)

    g <- ggplot2::ggplot(pred, aes(Query, Response)) +
        facet_grid(~KL, scales='free', switch = 'y') +
        geom_tile(aes(fill=count)) +
        geom_point(aes(size=count, color=Language),
                   data=real) +
        scale_fill_gradient(low='grey95',high='grey80') +
        scale_x_continuous(breaks=1:10) +
        scale_y_continuous(breaks=1:30) +
        guides(fill=F, color=F, size=guide_legend(title='N')) +
        theme_classic() +
        ggtitle(paste('Participant:',participant)) +
        coord_cartesian(xlim=c(0,10)) +
        theme(legend.position = 'bottom', text=element_text(size=18))

    g
}
