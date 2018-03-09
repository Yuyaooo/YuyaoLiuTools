context("Wrapper functions")

test_that("plotMyDataP plots the data using ggplot2", {
         df <- data.frame(
           Response = LETTER[1:5],
           Proportion = c(0.1,0.2,0.1,0.2,0.4)
         )
         aaa <- plotMyDataP(df)
         expect_identical(aaa$labels$y, "p")
         })

test_that("dplyrWrapper mutate the data", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  xa <- sum(d$x * d$p)
  expect_identical(dplyrWrapper(d), xa)
})



