#' Get a list of stargazers of a repo.
#'
#' @param repo The repository name.
#' @param info_type The type of information we get from user profile, e.g. "login" or "email".
#'
#' @importFrom gh gh
#'
#' @examples
#' \donttest{
#'
#' repo <- "terrytangyuan/maintainer-tools"
#' get_stargazers(repo)
#' }
#'
#' @export
get_stargazers <- function(repo, info_type = "login") {
  get_user_info(repo, "stargazers", info_type)
}

#' Get a list of watchers of a repo.
#'
#' @param repo The repository name.
#' @param info_type The type of information we get from user profile, e.g. "login" or "email".
#'
#' @importFrom gh gh
#'
#' @examples
#' \donttest{
#'
#' repo <- "terrytangyuan/maintainer-tools"
#' get_watchers(repo)
#' }
#'
#' @export
get_watchers <- function(repo, info_type = "login") {
  get_user_info(repo, "subscribers", info_type)
}

get_user_info <- function(repo, user_type, info_type) {
  users <- gh::gh(sprintf("GET /repos/%s/%s", repo, user_type), .limit = Inf)
  emails <- lapply(users, function(user) {
    email <- gh::gh(sprintf("GET %s", user$url))[[info_type]]
    if (!is.null(email)) return(email)
  })
  Filter(Negate(is.null), emails) 
}
