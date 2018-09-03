# hksreviewr

The `hksreviewr` package provides users with a way to more meaningfully interact with the course evaluations
at the Harvard Kennedy School. Currently, each course evaluation exists as a PDF, making it difficult to
filter or sort based on quantitative metrics within the evaluations.

## Installation

You can install the most up-to-date version of `hksreviewr` from [GitHub](https://github.com) with:

``` r
if (!require("devtools")) { install.packages("devtools") }
devtools::install_github("rbrellen/hksreviewr")
```

## Example

To use the `hksreviewr` package properly, PDF copies of HKS course evaluations
need to have been downloaded and saved within one or more directories. There
should be no other files in these directories, as the `create_output` function
will loop over all files contained within the target directories.

Files can live within a single directory or multiple directories, allowing for
greater organizational flexibility within the user's file system. The
`create_output` function can accept either a single directory or a vector or
list of directories upon which to work.

``` r
## Files in a single directory
df <- hksreviewr::create_output('~/courses/')

## Files in multiple directories
df <- hksreviewr::create_output(c('~/courses/2017-2018/',
                                  '~/courses/2016-2017/',
                                  '~/courses/2015-2016/'))
```

Please note that the `hksreviewr` project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this project, you agree
to abide by its terms.
