library(gh)

print_change_log <- function(repo, from, until) {
  pull_requests <- gh(sprintf("GET /repos/%s/pulls?state=closed&direction=asc", repo), .limit = Inf)
  
  from_date <- as.Date(from)
  until_date <- as.Date(until)
  for (pr in pull_requests) {
    merged_date <- pr$merged_at
    if (!is.null(merged_date) && from_date < merged_date && merged_date < until_date ) {
      change_item <- sprintf("* %s ([#%s](%s), [@%s](%s))\n", pr$title, pr$number, pr$html_url, pr$user$login, pr$user$html_url)
      cat(change_item)
    }
  }
}

repo <- "kubeflow/common"
from <- "2020-05-10"
until <- "2020-05-19"
print_change_log(repo, from, until)

