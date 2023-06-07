test_that("view_sdtm_domains returns a dataframe containing abbreviations and descriptions", {
  df_out <- view_sdtm_domains()
  expect_equal(df_out$DOMAIN_NAME[df_out$DOMAIN == "AE"], "Adverse Events")
  expect_equal(df_out$DOMAIN_NAME[df_out$DOMAIN == "LB"], "Laboratory Test Results")
  expect_equal(df_out$DOMAIN_NAME[df_out$DOMAIN == "DM"], "Demographics")
})
