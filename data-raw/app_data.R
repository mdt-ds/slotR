## code to prepare `app_data` 

## code to prepare `pay_tbl` dataset ----
library(dplyr)
library(stringr)

reel1 <- c("R", "P", "L", "S", "A", "N", "I")
reel2 <- c("R", "P", "L", "S", "A", "N", "I")
reel3 <- c("R", "P", "L", "S", "A", "N", "I")
pay_tbl <- tidyr::expand_grid(reel1, reel2, reel3)

pay_def <- function(reel1, reel2, reel3){
  reels <- paste0(reel1, reel2, reel3)
  Rs <- stringr::str_count(string = reels, pattern = "R")
  Ps <- stringr::str_count(string = reels, pattern = "P")
  Ns <- stringr::str_count(string = reels, pattern = "N")
  Ls <- stringr::str_count(string = reels, pattern = "L")
  Ss <- stringr::str_count(string = reels, pattern = "S")
  As <- stringr::str_count(string = reels, pattern = "A")
  Is <- stringr::str_count(string = reels, pattern = "I")
  pay <- c(0,2,4,8)[Rs+1]-c(0,2,4,8)[Ps+1]-c(0,1,2,4)[Ns+1] +
    c(0,0,2,4)[Ls+1]+c(0,0,2,4)[Ss+1]+c(0,0,2,4)[As+1]+c(0,0,2,4)[Is+1]
  return(pay)
}

pay_tbl <- dplyr::as_tibble(pay_tbl) %>% 
  dplyr::mutate(pay = purrr::pmap_dbl(.l = list(reel1, reel2, reel3), .f = pay_def)) %>% 
  dplyr::arrange(desc(pay))

## code to prepare `symbols` dataset goes here ----
symbols <- tibble::tibble(id = c("R", "P", "L", "S", "A", "N", "I"),
                          img = c('<img src = "www/R_logo.png" ></img>',
                                  '<img src = "www/Python_logo.png" ></img>',
                                  '<img src = "www/hex-Package.png" ></img>',
                                  '<img src = "www/hex-Shiny_App.png" ></img>',
                                  '<img src = "www/hex-Analysis.png" ></img>',
                                  '<img src = "www/hex-NA.png" ></img>',
                                  '<img src = "www/hex-Insight.png" ></img>'))

## table to display payout rules
payout <- tibble::tibble(symbol = c('<img src = "www/R_logo.png" width = "32px"></img>',
                                  '<img src = "www/Python_logo.png" width = "32px"></img>',
                                  '<img src = "www/hex-Analysis.png" width = "32px"></img>',
                                  '<img src = "www/hex-Insight.png" width = "32px"></img>',
                                  '<img src = "www/hex-Package.png" width = "32px"></img>',
                                  '<img src = "www/hex-Shiny_App.png" width = "32px"></img>',
                                  '<img src = "www/hex-NA.png" width = "32px"></img>'),
                         x1 =c(2,-2,0,0,0,0,-1),
                         x2 =c(4,-4,2,2,2,2,-2),
                         x3 =c(8,-8,4,4,4,4,-4))





## prepare sysdata.rda
usethis::use_data(symbols, pay_tbl, payout, internal = TRUE, overwrite = TRUE)
