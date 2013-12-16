Analysis and visualization code for Linzen, Kasyanenko, & Gouskova (2013):
Lexical and phonological variation in Russian prepositions, Phonology 30(3).

`knitr` is hard to install on R 3.0.1, better to use something like 2.15 
for the time being.

To use, set the environment variable RUSS_PREPS_ROOT to wherever the root
directory of the project is. Do this from the shell before you start R:

```bash
export RUSS_PREPS_ROOT=/Users/tal/Dropbox/russian_prepositions
R
```

Or from within an R session (before you `source` any of the files):

```r
Sys.setenv(RUSS_PREPS_ROOT='/Users/tal/Dropbox/russian_prepositions')
source('figures.r')
```
