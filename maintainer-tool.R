library(gh)

# TODO: Add statistics for repo contributions similar to https://github.com/kubeflow/common/pulse
# https://developer.github.com/v3/repos/statistics/
# https://github.com/skywinder/ActionSheetPicker-3.0/blob/develop/CHANGELOG.md

# TODO: Add acknowlegement section with contributors stats and unique users who are involved in issue/PR discussions.

labels_mapping <- list("size/S" = "Small Changes", "size/L" = "Large Changes", "size/M" = "Medium Changes")

get_changelog <- function(repo, from, until) {
  pull_requests <- gh(sprintf("GET /repos/%s/pulls?state=closed&direction=asc", repo), .limit = Inf)
  
  from_date <- as.Date(from)
  until_date <- as.Date(until)
  misc_section_title <- "Miscellaneous"
  sections <- list()
  pr_authors <- c()
  pr_participants <- c()
  for (pr in pull_requests) {
    merged_date <- pr$merged_at
    if (!is.null(merged_date) && from_date < merged_date && merged_date < until_date ) {
      change_item <- sprintf("* %s ([#%s](%s), [@%s](%s))\n", pr$title, pr$number, pr$html_url, pr$user$login, pr$user$html_url)
      # TODO: Get actual first and last name if any
      pr_authors <- unique(c(pr_authors, pr$user$login))
      pr_comments <- gh(sprintf("GET %s", pr$comments_url))
      pr_participants <- unique(c(pr_participants, unlist(lapply(pr_comments, function(comment) comment$user$login))))
      pr_detail <- gh(sprintf("GET /repos/%s/pulls/%s", repo, pr$number))
      labels <- pr_detail$labels
      if (length(labels) != 0) {
        for (label in labels) {
          if (label$name %in% names(labels_mapping))
            section_title <- labels_mapping[[label$name]]
          else
            section_title <- misc_section_title
          if (!section_title %in% names(sections)) sections[[section_title]] <- c()
          if (!change_item %in% sections[[section_title]])
            sections[[section_title]] <- c(sections[[section_title]], change_item)
        }
      } else {
        sections[[misc_section_title]] <-c(sections[[misc_section_title]], change_item)
      }
    }
  }
  return(list(
    sections = sections,
    participation = list(
      pr_authors = pr_authors,
      pr_participants = pr_participants)
    ))
}

repo <- "kubeflow/common"
from <- "2020-05-10"
until <- "2020-05-19"
logs <- print_change_log(repo, from, until)

