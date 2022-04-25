test_that("original order doesn't matter",{
df1 <- mtcars %>% get_pca_scores()

df2 <- mtcars %>%
  dplyr::arrange(dplyr::across(dplyr::everything())) %>%
  get_pca_scores()
expect_equal(df1, df2)
})