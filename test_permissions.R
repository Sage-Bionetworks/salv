# Description: Tests for permissions functions.
# Author: Haley Hunter-Zinck
# Date: 2021-10-07

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(testthat)
library(synapser)
synLogin()

source("permissions_fxns.R")

# test ----------------------------

test_that("is_team", {
  expect_false(is_team("hhz"))
  expect_true(is_team("GENIE BioPharma Collaborative"))
  expect_false(is_team("this_is_not_a_team_name_or_user_name_as_far_as_i_know"))
})

test_that("is_user", {
  expect_true(is_user("hhz"))
  expect_false(is_user("GENIE BioPharma Collaborative"))
  expect_false(is_user("this_is_not_a_team_name_or_user_name_as_far_as_i_know"))
})

test_that("is_synapse_entity", {
  expect_true(is_synapse_entity("syn7222066"))
  expect_false(is_synapse_entity("synA"))
  expect_false(is_synapse_entity("not_a_synapse_id"))
})

test_that("get_user_id", {
  expect_equal(get_user_id("hhz"), "3421669")
  expect_true(is.na(get_user_id("this_is_not_a_user_name_as_far_as_i_know")))
})

test_that("get_team_id", {
  expect_equal(get_team_id("project GENIE"), "3326313")
  expect_true(is.na(get_team_id("this_is_not_a_team_name_as_far_as_i_know")))
})

test_that("get_user_teams", {
  expect_true(length(get_user_teams(get_user_id("hhz"))) > 1)
  expect_true(length(get_user_teams(get_user_id("hhz"), return_ids = T)) > 1)
  expect_false(length(get_user_teams(get_user_id("this_is_not_a_user_name_as_far_as_i_know"))) > 0)
})

test_that("get_ranking_permission", {
  expect_equal(get_ranking_permission(sample(RANK_PERMISSIONS, length(RANK_PERMISSIONS))), RANK_PERMISSIONS[1])
  expect_false(get_ranking_permission(sample(RANK_PERMISSIONS, length(RANK_PERMISSIONS))) == RANK_PERMISSIONS[2])
  expect_true(is.na(get_ranking_permission(rep(NA, 5))))
  expect_true(is.na(get_ranking_permission(c("not_ranking", "hello", "world"))))
})

# close out ----------------------------

toc = as.double(Sys.time())
print(glue("Runtime: {round(toc - tic)} s"))
