#' '%not_in%' operator
#'
#' @name %not_in%
#' @rdname not_in
#' @keywords internal
#' @export
#' @usage x \%not_in\% table
#' @param x A vector of values
#' @param table A vector of values to compare to `table`
#' @return Returns `TRUE` if `x` is **not** present in `table`
`%not_in%` <- purrr::negate(`%in%`)
