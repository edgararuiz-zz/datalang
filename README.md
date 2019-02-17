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

The development version is available on [GitHub](https://github.com/):

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

    #>       dia maniana tarde
    #> 1 viernes      76    88
    #> 2  sábado      71    85
    #> 3 domingo      70    83

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
#>       dia maniana tarde
#> 1 viernes      76    88
#> 2  sábado      71    85
#> 3 domingo      70    83
```

Not all of the spec’s entries were needed to translate the data. The
other entries are useful for creating the help documentation. In
`datalang` there are two functions that output the help:

1.  `create_rd()` - It outputs a character vector with contents that can
    be saved as an rd file.
2.  `create_html_help()` - It outputs a character vector with contents
    that can be saved as an HTML file.

## Using

There are two options to use `datalang` to provide accessibility to your
package and data:

1.  Use in a new R package dedicated to a translation to a single
    language
2.  Use to extend, and provide translations from inside an existing R
    package

### Option 1 - New package dedicated to a single translation

A package can be built to focus on translations to a single language.
Here are some of the features of this approach:

1.  The package contains copies of the data sets in the target language
2.  The created help files are R “friendly”

The `translate_folder()` function is a higher level wrapper function
that produces the translations and help files. The procedure would be to
run `translate_folder()` on the package’s project folder, and then
rebuilt the package. The default arguments have the current
expectations:

  - The translation spec YAML files will be located in the R project’s
    `inst/specs` folder.
  - The target folder to save the new data sets to will be `data`.
  - The location of the new help files will be `man`.

For a working example of this approach, check out the `datos` package.
This package is dedicated to translating data sets into Spanish:
<https://github.com/cienciadedatos/datos>.

### Option 2 - Provide translations as part of your existing package

The goal of this approach will be for the existing package to provide
the translated data, and the help files, without becoming bulky with
copies of the data. For this approach, `datalang` provides a function,
called `on_attach`, that can be run when the package is attached to the
R session.

When the package is loaded, via `library()`, then `datalang` can check
for a language setting, and then use that setting as both a trigger, and
the pointer to which language should `datalang` translate to. At that
point, the translated data set is created and loaded into the R session.

The `on_attach()` function can be added to the package’s `.onAttach()`
function, so it’s processed when the library is loaded. This is what is
currently on a fork of `gapminder`:

    .onAttach <- function(...) {
      datalang::on_attach("gapminder")
    }

Using this fork as an example. The language is set to Spanish by using
the LANGUAGE environment variable (`Sys.setenv(LANGUAGE = "es")`):

    Sys.setenv(LANGUAGE = "es")
    
    library(gapminder)
    
    #> El lenguaje asignado actualmente es español  
    #>   Cargando datos y/o ayuda de los siguientes objetos : 
    #>      gapminder  >->  paises 
    #>    Una nueva función de ayuda fue cargada: ayuda() 
    #>      Utilice asi: ayuda(paises)

#### Help function

Because the help documentation is created on-the-fly, it cannot be added
to the regular data file that functions such as `help()` use. So a new
help wrapper function is created that creates an HTML version of the
data set, or function, help. If the function, data set, or general topic
is not one created by `datalang` then the requested topic is passed
through to the `help()` function in the background.

The help’s function name will be set based on the language. So in
Spanish, the name will be: `ayuda`.
