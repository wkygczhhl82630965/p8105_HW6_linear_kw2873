hw6
================
Keyi Wang
11/20/2019

``` r
birthweight = 
  read_csv("./data/birthweight.csv") %>% 
  janitor::clean_names()  %>%
   mutate(
    babysex = as.factor(case_when(
       babysex == 1 ~ "male",
       babysex == 2 ~ "female",
    )),
    frace = as.factor(case_when(
      frace == 1 ~ "White",
      frace == 2 ~ "Black",
      frace == 3 ~ "Asian",
      frace == 4 ~ "Puerto Rican",
      frace == 8 ~ "Other",
      frace == 9 ~ "Unknown",
    )),
    malform = as.factor(case_when(
      malform == 0 ~ "absent",
      malform == 1 ~ "present"
    )),
    mrace = as.factor(case_when(
      mrace == 1 ~ "White",
      mrace == 2 ~ "Black",
      mrace == 3 ~ "Asian",
      mrace == 4 ~ "Puerto Rican",
      mrace == 8 ~ "Other",
    ))
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
hypo_model = function(df) {
  lm(bwt ~ bhead + blength + delwt +menarche , data = df)
}

weight_model = hypo_model(birthweight)

  
birthweight %>% 
  add_predictions(weight_model) %>% 
  add_residuals (weight_model) %>% 
  ggplot(aes(x = pred, y = resid)) + 
    geom_point(alpha = 0.3,color = "orange") +
  labs (
        title = "Hypothesized Regression Model: Predicted Values vs Residuals",
        x = "Predicted Values",
        y = "Residuals"
      )
```

<img src="hw6_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" />
