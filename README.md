
<!-- README.md is generated from README.Rmd. Please edit that file -->

# authorlist

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/qiang-yang-ecology/authorlist.svg?branch=master)](https://travis-ci.com/qiang-yang-ecology/authorlist)
<!-- badges: end -->

The goal of authorlist is to creat the formatted author list for
manuscripts with many authors. Formatting the author list and the
affiliation list in Word for the manuscript with many coauthors is
painful and time-consuming. However, putting the author names and
affiliations together in a CSV file is easy. The package “authorlist”
automatically produces a Word document with the formatted author and
affiliation list ready for submission from a CSV file. The template of
the CSV file, which includes the coauthors’ names, affiliations, and
their order in the author list, can be downloaded from
<https://figshare.com/articles/dataset/CSV_template_for_R_package_authorlist_/16794901>.
The leading author of the manuscript could make it even easier by
sharing the CSV template through Dropbox or Google Drive and asking the
coauthors to fill in their information.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("qiang-yang-ecology/authorlist")
```

## Example

This is a basic example which shows you how to produce the Word document
with formatted author and affiliation list:

  - step 1: download the template from the following link to your
    computer:
    <https://figshare.com/articles/dataset/CSV_template_for_R_package_authorlist_/16794901>

  - step2: modify the csv file to include the coauthor information of
    your manuscript.

  - step3: produced the formatted author and affiliation list

<!-- end list -->

``` r
library(authorlist)
toAuthorlist(source.csv = "~/Downloads/authorlist.csv",
             # the csv including the author names and affiliations
             target = "~/Downloads/authorlist.docx",
             # where you save the produced word document
             type="numbered"
             # type of numbering the affiliations, could be numbered (numbers) or alphabeta (letters)
)
```
