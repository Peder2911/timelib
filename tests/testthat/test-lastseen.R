test_that("lastseen works", {
   x <- c(0,0,0,1,0,0,0,0)
   y <- c(0,1,0,0,0,0,1,0)
   seen <- lastseen(x=x, y=y)
   expect_true(is.na(seen[1]))
   expect_true(all(seen[c(2,3,7,8)] == "y"))
   expect_true(all(seen[c(4,5,6)] == "x"))
})
