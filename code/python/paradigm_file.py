import csv, os
import constants, funcs

class ParadigmFile(object):

    form_names = ['nom_sg', 'nom_pl', 'gen_sg', 'gen_pl', 'dat_sg', 'dat_pl',
            'acc_sg', 'acc_pl', 'inst_sg', 'inst_pl', 'prep_sg', 'prep_pl']

    def __init__(self, filename):
        self.lines = open(filename).readlines()
        self.read_file()

    def read_file(self):
        self.good_nouns = []
        for line in self.lines:
            line = line.strip()
            lemma, inflected = line.split('#')
            inflected = inflected.split(',')
            if len(inflected) == 12:
                self.good_nouns.append((lemma.decode('utf8'), 
                    dict((form_name, form.decode('utf8')) for 
                        form_name, form in zip(self.form_names, inflected))))
