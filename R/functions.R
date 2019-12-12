#' change 
#' 
#' Rolls over a vector and "flips" the previous
#' value. Used to find onsets and terminations.
#' @export

change <- function(x,mode){
   flip <- function(x) as.numeric(!as.logical(x))
   mode_num <- switch(mode, onset = 1, term = 0)
   sapply(1:length(x), function(i){
      if(i > 1){
         as.numeric(x[i-1] == flip(mode_num) &
                    x[i] == mode_num)
      } else {
         NA 
   }})
}

#' offset 
#' 
#' "Moves" values along an axis. 
#' @export

offset <- function(x,n = 1){
   sapply(1:length(x), function(i){
      if(i > n){
         x[i-n]
      } else {
         NA
      }
   })
}

#' nsince 
#' 
#' rolls over vector, counting values since case 
#' @export

nsince <- function(x,case = 1){
   seen <- FALSE
   n <- 0
   sapply(x, function(i){
      iscase <- !is.na(i)
      if(iscase){
         iscase <- i == case
      }

      if(seen & iscase){
         n <<- 0
         0
      } else if(seen & ! iscase){
         n <<- n + 1
         n
      } else if(!seen & iscase){
         seen <<- TRUE
         0
      } else {
         NA
      }
   })
}

#' sustain 
#' 
#' rolls over vector, "sustaining" values that return TRUE through the case-function.
#' Values are sustained "tolerance" number of times.
#' @export
sustain <- function(x,casefun,tolerance = 1){
   tol <- 0 
   prevcase <- NA

   sapply(x, function(i){
      if (casefun(i)) {
         tol <<- tolerance
         prevcase <<- i
         i 

      } else {
         tol <<- tol - 1 
         if(tol < 0){
            i 
         } else {
            prevcase 
         }

      }
   })
}

#' crop
#'
#' rolls over vector, "cropping" series of 1 by \code{tail}, 
#' turning them into 0.
#' @export
crop <- function(x, tail){
   cropping <- 0 

   sapply(1:length(x), function(i){
      current <- x[i]
      lookahead <- x[i+tail]
      anymiss <- any(sapply(c(current,lookahead), is.na))
      if(i + tail > length(x) | anymiss){ 

      } else if (x[i] == 1 & x[i + tail] == 0){
         cropping <<- tail 

      }

      if(cropping > 0) {
         cropping <<- cropping - 1 
         0
         
      } else {
         x[i]

      }
   })
}

#' crop
#'
#' simple function that alters a series by inserting replacement value 
#' before encountering the case value. 
#' @export
until <- function(x,casefun,replacement=0,replaceMissing = TRUE){
   met <- FALSE
   sapply(x,function(i){
      if(!met){
         if(is.na(i)){
            rep <- ifelse(replaceMissing,replacement,NA)
         } else {
            rep <- replacement
         }

         met <<- casefun(i) 
         ifelse(met, i, rep)
      } else {
         i
      }
   })
}

#' isNth 
#'
#' returns TRUE for the n-th (ex. first) occurrence of case 
#' otherwise returns FALSE. 
#' @export
isNth <- function(x,case = 1, n = 1){
   found <- FALSE
   nfound <<- 0
   sapply(x, function(i){
      iscase <- i == case & ! is.na(i)
      if(iscase){
         nfound <<- nfound + 1
         ifelse(nfound == n,TRUE,FALSE)
      } else {
         FALSE
      }
   })
}

#' halflife 
#' 
#' Returns a function that will yield an exponential decay that halves every 
#' @export
halflife <- function(t, init = 1){
   function(x){
      init * (0.5 ^ (x * (1/t)))
   }
}
