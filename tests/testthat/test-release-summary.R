context("Test summary functionalities")

repo <- "kubeflow/common"
from <- "2020-05-10"
until <- "2020-05-19"
labels_mapping <- list(
  "size/S" = "Small Changes",
  "size/L" = "Large Changes",
  "size/M" = "Medium Changes")

# Dummy test to get around CRAN check
test_that("This test block executes on CRAN", {
  expect_true(TRUE)
})

test_that("summaries can be generated correctly on pull requests", {
  skip_on_cran()
  pr_summary <- get_pull_requests_summary(
    repo, from, until, labels_mapping)
  expect_equal(names(pr_summary$sections), c("Miscellaneous", "Medium Changes", "Large Changes", "Small Changes"))
  expect_equal(pr_summary$participation,
               list(authors = c("terrytangyuan", "Jeffwan"),
                    participants = c("kubeflow-bot", "Jeffwan", "k8s-ci-robot", "merlintang", "terrytangyuan", "ChanYiLin")))
})

test_that("summaries can be generated correctly on issues", {
  skip_on_cran()
  issue_summary <- get_issues_summary(repo, from, until)
  expect_equal(issue_summary$participation,
               list(authors = c("Jeffwan", "terrytangyuan"),
                    participants = c("issue-label-bot[bot]", "Jeffwan", "gaocegege", "terrytangyuan", "kubeflow-bot", "k8s-ci-robot", "merlintang", "kf-label-bot-dev[bot]", "ChanYiLin")))

})

test_that("summaries can be generated correctly for releases", {
  skip_on_cran()
  release_summary <- get_release_summary(
    repo, from, until, labels_mapping)
  pr_summary <- release_summary$pr_summary
  issue_summary <- release_summary$issue_summary

  expect_equal(issue_summary$participation,
               list(authors = c("Jeffwan", "terrytangyuan"),
                    participants = c("issue-label-bot[bot]", "Jeffwan", "gaocegege", "terrytangyuan", "kubeflow-bot", "k8s-ci-robot", "merlintang", "kf-label-bot-dev[bot]", "ChanYiLin")))

  expect_equal(names(pr_summary$sections), c("Miscellaneous", "Medium Changes", "Large Changes", "Small Changes"))
  expect_equal(pr_summary$participation,
               list(authors = c("terrytangyuan", "Jeffwan"),
                    participants = c("kubeflow-bot", "Jeffwan", "k8s-ci-robot", "merlintang", "terrytangyuan", "ChanYiLin")))
})
