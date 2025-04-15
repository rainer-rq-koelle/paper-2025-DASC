#' Utility functions to prepare study trajectory
#' 
#' some descripion
#' 
#' more details.
#'
#' @param .q_trj trajectory extracted by Q with OSN and ECTRL add other more often used params here
#' 
#' @return Hard to have one return section for >>>> ALL <<<<< functions,
#' @name anyNameButFunctionNameIsUnique
NULL

#' @rdname anyNameButFunctionNameIsUnique
prepare_trajectory <- function(.q_trj){
  this_trj <- .q_trj |> standardise_names()
  
  return(this_trj)
}

#' @section Another section:
#' @rdname anyNameButFunctionNameIsUnique
standardise_names <- function(.q_trj){
  names(.q_trj) <- toupper(names(.q_trj))
  return(.q_trj)
}

#' @rdname anyNameButFunctionNameIsUnique
#' @param z a param just for theQuestion.
#' 
#' Could put here, but easiest if all params are
#' described in the same place, with page documentation, so none get repeated.
alsoTheQuestion <- function (z) { return("question? z") }