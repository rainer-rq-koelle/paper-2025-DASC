#' Group of functions page title
#' 
#' Group of functions Description section
#' 
#' Group of functions Details paragraph.
#'
#' @section After Arguments and Value sections:
#' Despite its location, this actually comes after the Arguments and Value sections.
#' Also, don't need to use null, could annotate first function, and then
#' using function name as the groupBy name is more intuitive.
#' 
#' @param x a param for toBar and notToBar
#' @param y a param just for notToBar
#' @return Hard to have one return section for all functions,
#' might want to have a manual list here.
#' @name anyNameButFunctionNameIsUnique
NULL

#' @rdname anyNameButFunctionNameIsUnique
alsoToBar <- function (x) { return("barred x") }

#' @section Another section:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @rdname anyNameButFunctionNameIsUnique
alsoNotToBar <- function (x,y) { return( "unbarred x with y") }

#' @rdname anyNameButFunctionNameIsUnique
#' @param z a param just for theQuestion.
#' 
#' Could put here, but easiest if all params are
#' described in the same place, with page documentation, so none get repeated.
alsoTheQuestion <- function (z) { return("question? z") }