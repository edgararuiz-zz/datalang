datalang
================

[![Travis build
status](https://travis-ci.org/edgararuiz/datalang.svg?branch=master)](https://travis-ci.org/edgararuiz/datalang)

[![Coverage
status](https://codecov.io/gh/edgararuiz/datalang/branch/master/graph/badge.svg)](https://codecov.io/github/edgararuiz/datalang?branch=master)

# datalang

The goal is to help people whose first, or preferred, language is not
English. This package aims to aid in translating data in packages, and
help files. This should lower the barrier of learning R, and data
analysis in general.

Even though this package can translate data on the fly, the primary way
it should be used is within another R package.

## How it works

The `datalang` package uses a translation spec file in order to perform
the translation. The expected spec file format is YAML. `datalang` uses
the YAML file to know which column names, and data values, to change.
The same YAML spec file can contain the information needed to produce a
help file.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/datalang")
```

## Example

``` r
my_spec <- system.file("specs/thisweek.yml", package = "datalang")


readLines(my_spec, encoding = "UTF-8")
#>  [1] "df:"                                                                     
#>  [2] "  source: thisweek"                                                      
#>  [3] "  name: esta_semana"                                                     
#>  [4] "variables:"                                                              
#>  [5] "  day:"                                                                  
#>  [6] "    trans: dia"                                                          
#>  [7] "    desc: Dia de la semana"                                              
#>  [8] "    values:"                                                             
#>  [9] "      friday: viernes"                                                   
#> [10] "      saturday: sábado"                                                  
#> [11] "      sunday: domingo"                                                   
#> [12] "  morning:"                                                              
#> [13] "    trans: manana"                                                       
#> [14] "    desc: Temperatura a las 10 AM"                                       
#> [15] "  afternoon:"                                                            
#> [16] "    trans: tarde"                                                        
#> [17] "    desc: Temperatura a las 3 PM"                                        
#> [18] "help:"                                                                   
#> [19] "  name: esta_semana"                                                     
#> [20] "  alias: esta_semana"                                                    
#> [21] "  title: Un data set de ejemplo"                                         
#> [22] "  description: Tres días de temperaturas tomadas en la mañana y la tarde"
```

``` r
library(datalang)


diamantes <- translate_data(my_spec) 

head(diamantes)
#>       dia manana tarde
#> 1 viernes     76    88
#> 2  sábado     71    85
#> 3 domingo     70    83
```

``` r
create_rd(my_spec) 
#>  [1] "\\docType{data}"                                                         
#>  [2] "\\name{esta_semana}"                                                     
#>  [3] "\\alias{esta_semana}"                                                    
#>  [4] "\\title{Un data set de ejemplo}"                                         
#>  [5] "\\describe{"                                                             
#>  [6] "\\item{dia}{Dia de la semana}"                                           
#>  [7] "\\item{manana}{Temperatura a las 10 AM}"                                 
#>  [8] "\\item{tarde}{Temperatura a las 3 PM}"                                   
#>  [9] "}}"                                                                      
#> [10] "\\description{Tres días de temperaturas tomadas en la mañana y la tarde}"
#> [11] "\\keyword{datasets}"
```
