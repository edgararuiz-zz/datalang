datalang
================

[![Travis build
status](https://travis-ci.org/edgararuiz/datalang.svg?branch=master)](https://travis-ci.org/edgararuiz/datalang)
[![Coverage
status](https://codecov.io/gh/edgararuiz/datalang/branch/master/graph/badge.svg)](https://codecov.io/github/edgararuiz/datalang?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/datalang)](http://cran.r-project.org/package=datalang)

The goal is to help people whose first, or preferred, language is not
English. This package aids in translating the data and help files. The
hope is to lower the barrier of learning R, and also data analysis.

Even though this package can translate data on the fly, this package is
meant to be used inside another R package.

## Installation

The development version is availble on [GitHub](https://github.com/):

``` r
# install.packages("devtools")
devtools::install_github("edgararuiz/datalang")
```

## How it works

The `datalang` package uses a translation spec file for translation. The
expected file format is YAML. `datalang` uses the YAML file to know
which column names, and data values, to change. The same YAML spec file
can contain the information needed to produce a help file.

Here is a sample spec file:

``` yaml
df:
  source: thisweek
  name: esta_semana
variables:
  day:
    trans: dia
    desc: Dia de la semana
    values:
      friday: viernes
      saturday: sábado
      sunday: domingo
  morning:
    trans: manana
    desc: Temperatura a las 10 AM
  afternoon:
    trans: tarde
    desc: Temperatura a las 3 PM
help:
  name: esta_semana
  alias: esta_semana
  title: Un data set de ejemplo
  description: Tres días de temperaturas tomadas en la mañana y la tarde
```

So a the data set can go from this:

    #>        day morning afternoon
    #> 1   friday      76        88
    #> 2 saturday      71        85
    #> 3   sunday      70        83

To this:

    #>       dia manana tarde
    #> 1 viernes     76    88
    #> 2  sábado     71    85
    #> 3 domingo     70    83

## Basics

The `datalang` package includes a simple data set called: `thisweek`.
The package also has a YAML spec file with the translation.

``` r
my_spec <- system.file("specs/thisweek.yml", package = "datalang")
```

The `translate_data()` function can ready the YAML spec file found in
the `my_spec` file path. It uses that spec to locate the data set, in
this case `thisweek`, modifies a copy of the data set based on the spec,
and outputs a new, translated, data set.

``` r
translate_data(my_spec)
#>       dia manana tarde
#> 1 viernes     76    88
#> 2  sábado     71    85
#> 3 domingo     70    83
```

Not all of the spec’s entries were needed to translate the data. The
other entries are useful for creating the help documentation. In
`datalang` there are two functions that output the help:

1.  `create_rd()` - It outputs a character vector with contents that can
    be saved as an rd file.
2.  `create_html_help()` - It outputs a character vector with contents
    that can be saved as an HTML file.
