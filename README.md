
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datalang

The goal of datalang is to …

## Installation

You can install the released version of datalang from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("datalang")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/datalang")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(datalang)

t <- translate_data(ggplot2::diamonds, system.file("specs/diamonds-es.yml", package = "datalang")) 

head(t)
#> # A tibble: 6 x 6
#>   precio quilate corte     claridad profundidad tabla
#>    <int>   <dbl> <ord>     <ord>          <dbl> <dbl>
#> 1    326   0.230 Ideal     SI2             61.5   55.
#> 2    326   0.210 Premium   SI1             59.8   61.
#> 3    327   0.230 Bueno     VS1             56.9   65.
#> 4    334   0.290 Premium   VS2             62.4   58.
#> 5    335   0.310 Bueno     SI2             63.3   58.
#> 6    336   0.240 Muy bueno VVS2            62.8   57.
```

``` r
create_rd(ggplot2::diamonds, system.file("specs/diamonds-es.yml", package = "datalang")) 
#>  [1] "\\docType{data}"                                                         
#>  [2] "\\name{diamantes}"                                                       
#>  [3] "\\alias{diamantes}"                                                      
#>  [4] "\\title{Precio de 50,000 diamantes}"                                     
#>  [5] "\\format{Un data.frame con 53,940 lineas y 10 variables"                 
#>  [6] "\\describe{"                                                             
#>  [7] "\\item{precio}{NULL}"                                                    
#>  [8] "\\item{quilate}{NULL}"                                                   
#>  [9] "\\item{corte}{Calidad del corte}"                                        
#> [10] "\\item{claridad}{Que tan claro es el diamante}"                          
#> [11] "\\item{profundidad}{NULL}"                                               
#> [12] "\\item{tabla}{NULL}"                                                     
#> [13] "}}"                                                                      
#> [14] "\\usage{diamantes}"                                                      
#> [15] "\\description{Un set que contiene los precios de casi 54,000 diamantes.}"
#> [16] "\\keyword{datasets}"
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
