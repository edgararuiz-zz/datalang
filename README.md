
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datalang

The goal of datalang is to translate data sets to other languages.

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
#> # A tibble: 6 x 9
#>   precio quilate corte     claridad profundidad tabla     x `TRUE`     z
#>    <int>   <dbl> <ord>     <ord>          <dbl> <dbl> <dbl>  <dbl> <dbl>
#> 1    326   0.230 Ideal     SI2             61.5   55.  3.95   3.98  2.43
#> 2    326   0.210 Premium   SI1             59.8   61.  3.89   3.84  2.31
#> 3    327   0.230 Bueno     VS1             56.9   65.  4.05   4.07  2.31
#> 4    334   0.290 Premium   VS2             62.4   58.  4.20   4.23  2.63
#> 5    335   0.310 Bueno     SI2             63.3   58.  4.34   4.35  2.75
#> 6    336   0.240 Muy bueno VVS2            62.8   57.  3.94   3.96  2.48
```

``` r
create_rd(ggplot2::diamonds, system.file("specs/diamonds-es.yml", package = "datalang")) 
#>  [1] "\\docType{data}"                                                         
#>  [2] "\\name{diamantes}"                                                       
#>  [3] "\\alias{diamantes}"                                                      
#>  [4] "\\title{Precio de 50,000 diamantes}"                                     
#>  [5] "\\format{Un data.frame con 53,940 lineas y 10 variables"                 
#>  [6] "\\describe{"                                                             
#>  [7] "\\item{precio}{Precio en dolares US}"                                    
#>  [8] "\\item{quilate}{Peso del diamante}"                                      
#>  [9] "\\item{corte}{Calided del corte}"                                        
#> [10] "\\item{claridad}{Medida de que tan claro es el diamante}"                
#> [11] "\\item{profundidad}{Porcentaje total de la profundidad}"                 
#> [12] "\\item{tabla}{Medida de la parte mas ancha del diamante}"                
#> [13] "\\item{x}{Largo in milimetros}"                                          
#> [14] "\\item{y}{Ancho in milimetros}"                                          
#> [15] "\\item{z}{Profundidad en milimetros}"                                    
#> [16] "}}"                                                                      
#> [17] "\\usage{diamantes}"                                                      
#> [18] "\\description{Un set que contiene los precios de casi 54,000 diamantes.}"
#> [19] "\\keyword{datasets}"
```
