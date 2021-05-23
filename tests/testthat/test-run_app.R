skip_on_cran()
skip_if_not(interactive())
test_that(
  "app launches",{
    golem::expect_running(sleep = 10)
  }
)