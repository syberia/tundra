#' Tundra is a standardized classifier container format for R.
#'
#' Deploying models in production systems is generally a cumbersome process.
#' If analysis is performed in a language like R or SAS, the coefficients of the
#' model are usually extracted and translated to a "production-ready" language like
#' R or Java.
#'
#' However, this approach is flawed. The translation process is time consuming
#' and error-prone. R is demonstrably capable of serving models
#' in production environments as long as submillisecond latency is not a
#' requirement. This means it should be possible to push analysis performed in
#' R to directly score records in production systems without an intermediary.
#' This significantly decreases the cost of iterating on machine learning
#' models.
#'
#' A tundraContainer is a simple bundling of the two critical components of 
#' any machine learning model.
#'
#' \itemize{
#'   \item{The data preparation required to convert raw production data to
#'     a record that is acceptable to a trained classifier. For example,
#'     a regression-based model may need discretization of non-categorical
#'     variables or imputation of missing values.}
#'   \item{The trained classifier, usually a native R S3 object with
#'     a \code{train} method.}
#' }
#'
#' The former is provided by the \href{https://github.com/robertzk/mungebits22}{mungebits2}
#' package, while the latter is fully customizable to any R function. This
#' approach allows arbitrary data preparation and statistical methods, unlike
#' attempts such as PMML (Predictive Modeling Markup Language) which constrain
#' the space of possible data preparation methodologies and statistical
#' methodologies to a very limited subset.
#'
#' @name tundra
#' @docType package
NULL
