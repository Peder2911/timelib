test_that("halflife halves",{
   range <- 1:10
   v <- range 
   functions <- lapply(range, function(t){
      halflife(t)
   })
   for(f in range){
      t <- functions[[f]](v)
      expect_equal(t[f],0.5)
      if(f > 1){
         expect_true(t[f-1]>0.5)
      }
      if(f < max(range)){
         expect_true(t[f+1]<0.5)
      }
   }
})

test_that("halflife precision",{
   x <- 1:20
   fn <- halflife(2)
   t <- fn(x)
   expect_equal(t[2],0.5)
   expect_equal(t[4],0.25)
   expect_equal(t[6],0.125)
})
