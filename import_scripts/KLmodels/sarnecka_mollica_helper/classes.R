#' Class \code{knowersfit} of models fitted with the \pkg{giveN} package
#'
#' Models fitted with the \code{\link{giveN}} package are
#' represented as a \code{knowersfit} object, which contains the posterior
#' samples, and relevant data.
#'
#' @name knowersfit-class
#' @aliases knowersfit
#' @docType class
#'
#' @slot formula A \code{\link{brmsformula}} object
#' @slot model_name The model name as a string.
#' @slot model_pars A character vector of names of parameters (including transformed parameters and derived quantities).
#' @slot par_dims A named list giving the dimensions for all parameters. The dimension for a scalar parameter is given as
#' \code{numeric(0)}.
#' @slot mode An integer indicating the mode of the fitted model. \code{0} indicates sampling mode, \code{1} indicates test gradient mode
#' (no sampling is done), and \code{2} indicates error mode (an error occurred before sampling). Most methods for \code{stanfit} objects
#' are useful only if \code{mode=0}.
#' @slot sim A list containing simulation results including the posterior draws as well as various pieces of metadata used by many of the
#' methods for \code{stanfit} objects.
#' @slot inits The initial values (either user-specified or generated randomly) for all chains. This is a list with one component per
#' chain. Each component is a named list containing the initial values for each parameter for the corresponding chain.
#' @slot stan_args A list with one component per chain containing the arguments used for sampling (e.g. \code{iter}, \code{seed}, etc.).
#' @slot stanmodel The instance of S4 class \code{\linkS4class{stanmodel}}.
#' @slot date A string containing the date and time the object was created.
#' @slot .MISC Miscellaneous helper information used for the fitted model. This is an object of type \code{environment}. Users rarely
#' (if ever) need to access the contents of \code{.MISC}.
#'
#' @seealso
#'   \code{\link[rstan:stanfit-class]{stanfit-class}},
#'
#' @importClassesFrom rstan stanfit
#' @export

# brmsfit class
setClass('knowerSamples',
         slots=list(inx='data.frame'),
         contains = 'stanfit')
