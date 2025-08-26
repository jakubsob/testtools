#' Helper functions to quickly target the remote repository
#' @name git
NULL


#' @export
#' @rdname git
get_branch <- function() {
  res <- system("git status", intern = TRUE)
  stringr::str_remove(res[1], "On branch ")
}

#' @export
#' @rdname git
get_repo <- function() {
  res <- system("git config --get remote.origin.url", intern = TRUE)
  res[1] |>
    stringr::str_remove("git@github.com:") |>
    stringr::str_remove("[.]git") |>
    stringr::str_split("/") |>
    purrr::pluck(1, 2)
}

#' @export
#' @rdname git
get_owner <- function() {
  res <- system("git config --get remote.origin.url", intern = TRUE)
  res[1] |>
    stringr::str_remove("git@github.com:") |>
    stringr::str_remove("[.]git") |>
    stringr::str_split("/") |>
    purrr::pluck(1, 1)
}
