datalang
================

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
my_spec <- system.file("specs/diamonds-es.yml", package = "datalang")


readLines(my_spec, encoding = "UTF-8")
#>  [1] "lang: es"                                                                                                
#>  [2] "df:"                                                                                                     
#>  [3] "  source: ggplot2::diamonds"                                                                             
#>  [4] "  name: diamantes"                                                                                       
#>  [5] "variables:"                                                                                              
#>  [6] "  price:"                                                                                                
#>  [7] "    trans: precio"                                                                                       
#>  [8] "    desc: Precio en dólares US (\\$326–\\$18,823)"                                                       
#>  [9] "  carat:"                                                                                                
#> [10] "    trans: quilate"                                                                                      
#> [11] "    desc: Peso del diamante (0.2–5.01)"                                                                  
#> [12] "  cut:"                                                                                                  
#> [13] "    trans: corte"                                                                                        
#> [14] "    desc: Calidad del corte (Regular, Bueno, Muy bueno, Premium, Ideal)"                                 
#> [15] "    values:"                                                                                             
#> [16] "      Good: Bueno"                                                                                       
#> [17] "      Very Good: Muy bueno"                                                                              
#> [18] "      Fair: Regular"                                                                                     
#> [19] "  color:"                                                                                                
#> [20] "    trans: color"                                                                                        
#> [21] "    desc: Color del diamante, de J (peor) a D (mejor)"                                                   
#> [22] "  clarity:"                                                                                              
#> [23] "    trans: claridad"                                                                                     
#> [24] "    desc: Medida de qué tan claro es el diamante (I1 (peor), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (mejor))"
#> [25] "  depth:"                                                                                                
#> [26] "    trans: profundidad"                                                                                  
#> [27] "    desc: Porcentaje de la profundidad total = z / mean(x, y) = 2 * z / (x + y) (43–79)"                 
#> [28] "  table:"                                                                                                
#> [29] "    trans: tabla"                                                                                        
#> [30] "    desc: Ancho de la parte superior del diamante con relación a su punto más ancho (43-95)"             
#> [31] "  x:"                                                                                                    
#> [32] "    trans: x"                                                                                            
#> [33] "    desc: Largo en milímetros"                                                                           
#> [34] "  y:"                                                                                                    
#> [35] "    trans: y"                                                                                            
#> [36] "    desc: Ancho en milímetros"                                                                           
#> [37] "  z:"                                                                                                    
#> [38] "    trans: z"                                                                                            
#> [39] "    desc: Profundidad en milímetros"                                                                     
#> [40] "help:"                                                                                                   
#> [41] "  name: diamantes"                                                                                       
#> [42] "  alias: diamantes"                                                                                      
#> [43] "  title: Precio de 50,000 diamantes"                                                                     
#> [44] "  description: Un set de datos que contiene los precios de casi 54,000 diamantes."                       
#> [45] "  usage: diamantes"                                                                                      
#> [46] "  format: Un data.frame con 53,940 líneas y 10 variables"
```

``` r
library(datalang)


diamantes <- translate_data(my_spec) 

head(diamantes)
#> # A tibble: 6 x 10
#>   price carat cut       color clarity depth table     x     y     z
#>   <int> <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1   326 0.23  Ideal     E     SI2      61.5    55  3.95  3.98  2.43
#> 2   326 0.21  Premium   E     SI1      59.8    61  3.89  3.84  2.31
#> 3   327 0.23  Bueno     E     VS1      56.9    65  4.05  4.07  2.31
#> 4   334 0.290 Premium   I     VS2      62.4    58  4.2   4.23  2.63
#> 5   335 0.31  Bueno     J     SI2      63.3    58  4.34  4.35  2.75
#> 6   336 0.24  Muy bueno J     VVS2     62.8    57  3.94  3.96  2.48
```

``` r
create_rd(my_spec) 
#>  [1] "\\docType{data}"                                                                                                 
#>  [2] "\\name{diamantes}"                                                                                               
#>  [3] "\\alias{diamantes}"                                                                                              
#>  [4] "\\title{Precio de 50,000 diamantes}"                                                                             
#>  [5] "\\format{Un data.frame con 53,940 líneas y 10 variables"                                                         
#>  [6] "\\describe{"                                                                                                     
#>  [7] "\\item{precio}{Precio en dólares US (\\$326–\\$18,823)}"                                                         
#>  [8] "\\item{quilate}{Peso del diamante (0.2–5.01)}"                                                                   
#>  [9] "\\item{corte}{Calidad del corte (Regular, Bueno, Muy bueno, Premium, Ideal)}"                                    
#> [10] "\\item{color}{Color del diamante, de J (peor) a D (mejor)}"                                                      
#> [11] "\\item{claridad}{Medida de qué tan claro es el diamante (I1 (peor), SI1, SI2, VS1, VS2, VVS1, VVS2, IF (mejor))}"
#> [12] "\\item{profundidad}{Porcentaje de la profundidad total = z / mean(x, y) = 2 * z / (x + y) (43–79)}"              
#> [13] "\\item{tabla}{Ancho de la parte superior del diamante con relación a su punto más ancho (43-95)}"                
#> [14] "\\item{x}{Largo en milímetros}"                                                                                  
#> [15] "\\item{y}{Ancho en milímetros}"                                                                                  
#> [16] "\\item{z}{Profundidad en milímetros}"                                                                            
#> [17] "}}"                                                                                                              
#> [18] "\\usage{diamantes}"                                                                                              
#> [19] "\\description{Un set de datos que contiene los precios de casi 54,000 diamantes.}"                               
#> [20] "\\keyword{datasets}"
```
