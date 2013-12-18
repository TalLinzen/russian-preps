# -*- coding: utf-8 -*- 

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

import csv
import os

import constants
import funcs

class ParadigmFile(object):
    '''
    Read the paradigm file extracted from Zaliznjak's dictionary by Andrei
    Usachev.

    Zaliznjak, A.A. (1977). Grammatiyeskij slovar’ russkogo jazyka. [A 
    grammatical dictionary of the Russian language.] Moscow: Izdatelstvo 
    Russkij Jazyk.
    
    (Originally obtained from www.speakrus.ru/dict/all_forms.rar. This file is
    also bundled with the present code under the `resources` folder.)
    '''
    form_names = ['nom_sg', 'nom_pl', 'gen_sg', 'gen_pl', 'dat_sg', 'dat_pl', 
                  'acc_sg', 'acc_pl', 'inst_sg', 'inst_pl', 'prep_sg',
                  'prep_pl']

    def __init__(self, filename):
        self.lines = open(filename).readlines()
        self.read_file()

    def read_file(self):
        self.good_nouns = []
        for line in self.lines:
            line = line.strip()
            lemma, inflected = line.split('#')
            inflected = inflected.split(',')
            # Only include nouns with full paradigms
            if len(inflected) == len(self.form_names):
                d = {form_name: form.decode('utf8') for form_name, form in 
                     zip(self.form_names, inflected)}
                self.good_nouns.append((lemma.decode('utf8'), d))
