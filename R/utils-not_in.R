#' '%not_in%' operator
#'
#' @name %not_in%
#' @rdname not_in
#' @keywords internal
#' @export
#' @usage lhs \%not_in\% rhs
#' @param lhs A vector of values
#' @param rhs A vector of values to compare to lhs
#' @return Returns `TRUE` if `lhs` is **not** present in `rhs`
`%not_in%` <- purrr::negate(`%in%`)
