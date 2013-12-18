Accompanying code for Linzen, Kasyanenko, & Gouskova (2013):
Lexical and phonological variation in Russian prepositions, Phonology 30(3). 
[[doi](http://dx.doi.org/10.1017/S0952675713000225)] 
[[pdf](http://tallinzen.net/media/papers/linzen_kasyanenko_gouskova_2013_phonology.pdf)].

The Python folder has code used for querying and scraping the online corpora
(Russian National Corpus and Yandex). The R directory has code related to
statistical analysis and visualization.

Please let us know (<linzen@nyu.edu>) if you found this code useful or have any questions.

To use the code, set the environment variable RUSS_PREPS_ROOT to wherever the root
directory of the project is. Run this command from the shell before you start R or
Python:

```bash
export RUSS_PREPS_ROOT=/Users/tal/Dropbox/russian_prepositions
```

Or from within an R session (before you `source` any of the files):

```R
Sys.setenv(RUSS_PREPS_ROOT='/Users/tal/Dropbox/russian_prepositions')
source('figures.r')
```

`knitr` is hard to install on R 3.0.1, better to use something like 2.15 
for the time being.
