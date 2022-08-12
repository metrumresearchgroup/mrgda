
test_that("No duplicates across id, tafd, and primary keys", {

    expect
    expect_message({
      test_df <- nm_join(MOD1)
    }, regexp = "join stats")
    expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
    expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS + PARTAB_NEW_COLS + 1)
})
