#' Download the latest snapshot artifacts from the CI and update local snapshots
#'
#' @details
#'
#' This function uses Github API to download the latest snapshot artifacts from the CI and update the local snapshots.
#'
#' @param name The name of the artifact
#' @param to Output directory for the artifact
#' @param branch The name of the branch
#' @param repo The name of the repository
#' @param owner The name of the owner of the repository
#' @return NULL, called for side effects
download_ci_artifact <- function(
  name,
  to = ".",
  branch = get_branch(),
  repo = get_repo(),
  owner = get_owner()
) {
  artifacts <- gh::gh(
    "GET /repos/{owner}/{repo}/actions/artifacts",
    owner = owner,
    repo = repo,
    .token = Sys.getenv("GITHUB_PAT")
  )
  artifacts <- artifacts |>
    purrr::pluck(2) |>
    purrr::map(\(x) {
      x$branch <- x$workflow_run$head_branch
      x
    }) |>
    dplyr::bind_rows()
  id <- artifacts |>
    dplyr::filter(!.data[["expired"]]) |>
    dplyr::filter(.data[["branch"]] == !!branch) |>
    dplyr::filter(.data[["name"]] == !!name) |>
    dplyr::filter(.data[["updated_at"]] == max(.data[["updated_at"]])) |>
    dplyr::slice_head(n = 1) |>
    dplyr::pull(.data[["id"]])
  gh::gh(
    "/repos/{owner}/{repo}/actions/artifacts/{id}/zip",
    owner = owner,
    repo = repo,
    id = id,
    .destfile = "artifact.zip",
    .token = Sys.getenv("GITHUB_PAT")
  )
  # It takes a moment for fille to be renamed to _snaps.zip, wait for it to happen
  while (TRUE) {
    if (fs::file_exists("artifact.zip")) {
      break
    }
  }
  utils::unzip("artifact.zip", exdir = to)
  fs::file_delete("artifact.zip")
  to
}

unpack_test_snapshots <- function(x) {
  fs::file_move(x, "_snaps")
  dir_old <- fs::path("tests", "testthat", "_snaps")
  if (fs::dir_exists(dir_old)) {
    fs::file_delete(dir_old)
  }
  fs::file_move("_snaps", dir_old)
  invisible(NULL)
}
