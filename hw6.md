hw6
================
Keyi Wang
11/20/2019

``` r
birthweight = 
  read_csv("./data/birthweight.csv") %>% 
  janitor::clean_names()  
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.
