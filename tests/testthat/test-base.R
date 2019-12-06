test_that("flip works", {
   base <- c(0,0,1,1,0)
   flipped <- change(base,"onset")
   expect_equal(flipped[3],1)
})

test_that("offset works", {
   base <- sample(c(0,1),10,replace = TRUE)

   moved <- offset(base,1)

   res <- sapply(1:length(base),function(i){
      if(i > 1){
         expect_equal(moved[i],base[i-1])
      } else {
         expect_true(is.na(moved[i]))
      }
   })
})

test_that("nsince works", {
   x <- nsince(c(0,0,1,0,0,1,0,1,0,0,1,0,0))
   expect_equal(sum(is.na(x)),2)
   expect_equal(sum(x,na.rm = T),10)

   x <- nsince(c(1,0,2,1,0,2,0,2,0,1,2,0,1),case = 2)
   expect_equal(sum(is.na(x)),2)
   expect_equal(sum(x,na.rm = T),10)

   x <- nsince(c(1,NA,NA,1,NA,2,NA,2,NA,NA,2,NA,NA),case = 2)
   expect_equal(sum(is.na(x)),5)
   expect_equal(sum(x,na.rm = T),7)

   x <- nsince(c(NA,1,1,0,0,1,0),case = 1)
   expect_equal(sum(is.na(x)),1)
   expect_equal(sum(x,na.rm = T),4)

   x <- nsince(c(NA,NA,NA,NA,NA,1,0),case = 1)
   expect_equal(sum(is.na(x)),5)
   expect_equal(sum(x,na.rm = T),1)
})

test_that("sustain works", {
   isone <- function(x) x == 1
   x <- sustain(c(0,0,1,0,1),casefun=isone)
   expect_true(all(x[c(3,4,5)] == 1))
   expect_equal(sum(x), 3)

   x <- sustain(c(0,0,1,0,0,1),casefun=isone)
   expect_false(all(x[c(3,4,5,6)] == 1))
   expect_equal(sum(x), 3)

   x <- sustain(c(0,0,1,0,0,1), 2,casefun=isone)
   expect_true(all(x[c(3,4,5,6)] == 1))
   expect_equal(sum(x), 4)

   x <- sustain(c(0,1,0,1,0,0,1,0,1),casefun=isone)
   expect_true(all(x[c(2,3,4,5,7,8,9)] == 1))
   expect_true(x[6] == 0)
   expect_equal(sum(x), 7)

   naproof <- function(x){!is.na(x) & x == 1}
   x <- sustain(c(NA,1,0,1,NA,NA,1,NA,1),casefun=naproof)
   print(x)
   expect_true(all(x[c(2,3,4,5,7,8,9)] == 1))
   expect_equal(sum(is.na(x)),2)
   expect_equal(sum(x,na.rm = T), 7)
})

test_that("crop works", {
   x <- crop(c(0,1,1,1,0),2)
   expect_true(all(x[c(3,4,5)] == 0))

   x <- crop(c(0,1,1,1,0,0,1,1,1,0,0),2)
   expect_true(all(x[c(2,7)] == 1))
   expect_true(all(x[c(3,4,5,8,9,10)] == 0))

   x <- crop(c(0,1,1,1,1,0,1,1,1,1,0),3)
   expect_equal(sum(x), 2)

   x <- crop(c(0,1,1,1,1,0,1,1,1,1,0),2)
   expect_equal(sum(x), 4)

   x <- crop(c(0,1,1,1,1,1,1,1,1,1,0),2)
   expect_equal(sum(x), 7)

   x <- crop(c(0,NA,1,1,1,1,NA,1,1,1,0),2)
   expect_equal(sum(is.na(x)), 2)
})

test_that("until works", {
   x <- until(c(NA,NA,0,1,0,0,1,0,1,0,0,1,0,0))
   expect_false(any(is.na(x)))
   expect_equal(sum(x == 0), 10)
   expect_equal(sum(x == 1), 4)

   x <- until(c(NA,NA,0,1,0,0,1,0,1,0,0,1,0,0),replacement = 10)
   expect_true(all(x[c(1,2,3)] == 10))
   expect_true(x[4] == 1)

   x <- until(c(NA,NA,0,1,0,0,1,0,1,0,0,1,0,0),replacement = NA)
   expect_equal(sum(is.na(x)), 3)
})

test_that("isNth works", {
   x <- isNth(c(0,0,1,1,NA,0,1),1)
   expect_equal(sum(x),1)

   x <- isNth(c(0,NA,1,1,0,0,1),1,2)
   expect_equal(sum(x),1)
})
