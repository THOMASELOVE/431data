Update to Love-boost.R: `eda.ksam` instead of `eda.2sam`
================
Thomas E. Love, Ph.D.
2017-11-26

A Data Set
==========

Consider the following toy data set. It contains observations on 3 variables for 32 subjects. Specifically, we have a `subject` identification number (1-32), a `type` (low, middle, or high) and a quantitative `result` in each row.

``` r
test3sam <- read_csv("test3sam.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   subject = col_integer(),
    ##   type = col_character(),
    ##   result = col_integer()
    ## )

``` r
test3sam
```

    ## # A tibble: 32 x 3
    ##    subject   type result
    ##      <int>  <chr>  <int>
    ##  1       1    low     13
    ##  2       2    low     12
    ##  3       3    low     19
    ##  4       4    low     16
    ##  5       5    low     14
    ##  6       6    low     16
    ##  7       7    low     12
    ##  8       8    low     12
    ##  9       9 middle     23
    ## 10      10 middle     23
    ## # ... with 22 more rows

A numerical summary of the data
===============================

We have three independent samples here, and we can summarize them as follows:

``` r
test3sam %>% 
  group_by(type) %>% 
  summarize(n = n(), mean = mean(result), sd = sd(result), 
            median = median(result), min = min(result), max = max(result))
```

    ## # A tibble: 3 x 7
    ##     type     n   mean       sd median   min   max
    ##    <chr> <int>  <dbl>    <dbl>  <dbl> <dbl> <dbl>
    ## 1   high     8 26.000 3.585686   27.5    20    30
    ## 2    low     8 14.250 2.549510   13.5    12    19
    ## 3 middle    16 20.375 3.703602   22.0    15    25

The Problem
===========

The old `eda.2sam`, when applied to these data, incorrectly shows the data.

``` r
eda.2sam(outcome = test3sam$result,
         group = test3sam$type)
```

    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.

![](Problems_with_eda_2sam_files/figure-markdown_github/unnamed-chunk-3-1.png)

You'll note the appropriate warnings for the `notch` because of the small number of observations in each group, but look closely at the results. The data described as `high` in the Comparison Boxplot doesn't match the data described as `high` in the Comparison Histogram, and it looks like the boxplot may be correct, but the histogram certainly isn't, in terms of where the extreme values fall.

This is due to a bug in `eda.2sam`.

The Solution
============

I've written a new function, called `eda.ksam`, now available in the `Love-boost.R` script, which may fix this problem. At any rate, we now get histograms and boxplots that match the data, and each other.

``` r
eda.ksam(outcome = test3sam$result,
         group = test3sam$type)
```

    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.

![](Problems_with_eda_2sam_files/figure-markdown_github/unnamed-chunk-4-1.png)

Revising the Levels of the `type` variable into a factor
========================================================

I am not certain of this, but it appears that if we clean up the `type` information by creating a factor (rather than a character variable) and reordering the levels from alphabetical order, all using the `fct_relevel` function, we get better results.

``` r
test3sam_repaired <- test3sam %>%
  mutate(type = fct_relevel(type, "low", "middle", "high"))
```

``` r
eda.2sam(test3sam_repaired$result, test3sam_repaired$type)
```

    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.
    ## notch went outside hinges. Try setting notch=FALSE.

![](Problems_with_eda_2sam_files/figure-markdown_github/unnamed-chunk-6-1.png)
