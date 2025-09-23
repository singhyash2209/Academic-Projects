#project2_tests

library(pacman)
p_load(testthat)
test_that("Testing Problem 6(visible)", {
  expect_equal(happy_df$region[3], "Western Europe")
  expect_equal(happy_df$country[7], "Netherlands")
})

test_that("Testing Problem 7(visible)", {
  expect_equal(top_ten_df$region[5], "North America")
  expect_equal(top_ten_df$country[5], "Canada")
  expect_equal(nrow(top_ten_df), 10)
})

test_that("Testing Problem 8(visible)", {
  expect_equal(no_freedom_df$region[4], "Western Europe")
  expect_equal(no_freedom_df$country[6], "Sudan")
})
test_that("Testing Problem 9(visible)", {
  expect_equal(best_freedom_df$region[5], "Central and Eastern Europe")
  expect_equal(best_freedom_df$country[6], "Australia")
})

test_that("Testing Problem 10(visible)", {
  expect_equal(data_2015$gff_stat[6], 2.19346, tolerance = 1e-4)
  expect_equal(data_2015$country[6], "Finland")
})


test_that("Testing Problem 11(visible)", {
  expect_equal(happy_summary$mean_happiness[1], 5.375734	, tolerance = 1e-4)
  expect_equal(happy_summary$max_happiness[1], 7.587)
})

test_that("Testing Problem 12(visible)", {
  expect_equal(regional_stats_df$region[6], "North America")
  expect_equal(regional_stats_df$country_count[5], 20)
  expect_equal(regional_stats_df$mean_happiness[10], 6.69, tolerance = 1e-2)
  expect_equal(regional_stats_df$mean_freedom[5], .362, tolerance = 1e-3)

})

test_that("Testing Problem 13(visible)", {
  expect_equal(gdp_df[1,][[1]], 1.23, tolerance = 1e-2)
  expect_equal(gdp_df[1,][[2]], .523, tolerance = 1e-2)

})

test_that("Testing Problem 18(visible)", {
  expect_equal(age_stats_df$HR[6], 4.5	, tolerance = 1e-2)
  expect_equal(age_stats_df$R[10], 26.4	, tolerance = 1e-2)
  expect_equal(age_stats_df$H[10], 52.6	, tolerance = 1e-2)
})



test_that("Baseball Test(visible)", {
  expect_equal(nrow(baseball), 726)
})

test_that("Baseball Tibble Test 2(visible)", {
  expect_equal(length(baseball), 18)
})

test_that("Testing Problem 21(visible)", {
  expect_equal(baseball$BA[8], .219)
  expect_equal(max(baseball$BA), 1)
  expect_equal(min(baseball$BA), 0)
})

test_that("Testing Problem 23(visible)", {
  expect_equal(baseball$OBP[25], .363)
  expect_equal(max(baseball$OBP), 1)
  expect_equal(min(baseball$OBP), 0)
})

test_that("Testing Problem 24(visible)", {
  expect_equal(strikeout_artist$Last[1], "Incaviglia")
  expect_equal(strikeout_artist$Last[10], "Strawberry")
})

test_that("Testing Problem 30(visible)", {
  expect_equal(nrow(eligible_df), 251)
  expect_equal(length(eligible_df), 22)
})


test_that("Testing Problem 31(visible)", {
  expect_equal(nrow(mvp_candidates), 20)
  expect_equal(max(mvp_candidates$TotalRank), 120)
  expect_equal(min(mvp_candidates$TotalRank), 20)
  expect_equal(mvp_candidates$TotalRank[5], 75)
  expect_equal(mvp_candidates$Last[10], "Gibson")

})



