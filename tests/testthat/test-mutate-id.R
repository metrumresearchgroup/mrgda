test_df <- dplyr::tibble(
     USUBJID = c("rand-stud-001", "rand-stud-002", "rand-stud-003"),
     WT = c(64, 23, 92),
     SEX = c(1, 2, 1))

test_that("mutate_id creates appropriate ID's for standard case [NMV-MID-001]", {
  test_id <- test_df %>% mutate_id(.unique_subject_identifier = USUBJID, .start_id = 1)
  expect_equal(test_id$ID[1], 1)
  expect_equal(test_id$ID[3], 3)
})

test_that("mutate_id creates appropriate ID's with unique starting ID [NMV-MID-002]", {
  test_id <- test_df %>% mutate_id(.unique_subject_identifier = USUBJID, .start_id = 105)
  expect_equal(test_id$ID[1], 105)
  expect_equal(test_id$ID[3], 107)
})
