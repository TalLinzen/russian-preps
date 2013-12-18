# -*- coding: utf-8 -*-

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

import csv
import logging
import os
import pickle

import constants
from paradigm_file import ParadigmFile
from yandex_uncapped import YandexUncapped
from russian_national_corpus import RussianNationalCorpus

logger = logging.getLogger("Preposition corpus builder")
logging.basicConfig(loglevel=logging.INFO)

class PrepCountHarvester(object):
    '''
    >>> pch = PrepCountHarvester(constants.s)
    >>> first_word = pch.words[0]
    >>> pch.do_rnc_searches([first_word])
    >>> pch.rnc_shelf
    >>> for key, value in pch.rnc_shelf.items():
            print key, value
    "со бдений" 0
    "со бдениями" 0
    "с бдений" 0
    "с бдения" 0
    "со бдения" 0
    "с бдениями" 1
    "с бдением" 1
    "со бдением" 1
    бдение 672

    Or just search for all cluster initial words:
    >>> pch.do_rnc_searches(pch.words)  # Then wait a few days

    Then export to a CSV file:
    >>> pch.r_csv(pch.words, pch.rnc_shelf, 'r.csv')

    All CSV files are automatically written into $RUSS_PREPS_ROOTS/results,
    unless the last argument start with a slash (indicating an absolute path).
    '''

    def __init__(self, preposition):
        self.variants = preposition['variants']
        self.transcribed_variants = preposition['transcribed_variants']
        self.inflections = ['%s_%s' % (case, number) 
                            for case in preposition['cases'] 
                            for number in ['sg', 'pl']]
        self.paradigm_file = ParadigmFile(constants.paradigm_file)
        self.rnc_shelf = self._load_shelf(constants.rnc_shelf_file)
        self.yandex_shelf = self._load_shelf(constants.yandex_shelf_file)
        self.words = self.cluster_initial()

    def cluster_initial(self, frequency_threshold=5):
        '''
        Get all cluster-initial words from the paradigm dictionary.
        '''
        words = []
        for lemma, forms in self.paradigm_file.good_nouns:
            form = forms[self.inflections[0]]
            if form[0] not in constants.vowels:
                second = (form[1] if form[1] not in constants.znaks else 
                          form[2])
                if second not in constants.vowels:
                    words.append((lemma, forms))
        return words

    def _add_default_csv_directory(self, filename):
        if filename[0] != '/':
            filename = os.path.join(constants.default_csv_dir, filename)
        return filename

    def _load_shelf(self, filename):
        if os.path.exists(filename):
            f = open(filename)
            return pickle.load(f)
        else:
            return {}

    def _dump_shelf(self, shelf, filename):
        f = open(filename, 'w')
        pickle.dump(shelf, f)

    def print_list(self, what):
        for lemma, forms in what:
            print '%s:' % lemma,
            for key, value in forms.items():
                print '%s: %s ' % (key, value)

    def do_rnc_searches(self, words, only_lemmas=False):
        rnc = RussianNationalCorpus()
        unwritten_items = 0
        for lemma, forms in words:
            if lemma not in self.rnc_shelf:
                self.rnc_shelf[lemma] = rnc.lemma_query(lemma)
                unwritten_items += 1
            if not only_lemmas:
                for inflection in self.inflections:
                    form = forms[inflection]
                    terms = ['"%s %s"' % (variant, form.replace("'", '')) for 
                             variant in self.variants]
                    for term in terms:
                        if term not in self.rnc_shelf:
                            print term
                            logger.info('Adding %s' % term)
                            self.rnc_shelf[term] = rnc.exact_query(term)
                if unwritten_items >= 5:
                    self._dump_shelf(self.rnc_shelf, constants.rnc_shelf_file)
                    unwritten_items = 0

        self._dump_shelf(self.rnc_shelf, constants.rnc_shelf_file)


    def do_yandex_searches(self, words):
        yandex = YandexUncapped(delay=60)
        unwritten_items = 0
        for i, (lemma, forms) in enumerate(words):
            for inflection in self.inflections:
                form = forms[inflection]
                search_terms = ['"%s %s"' % (variant, form.replace("'", '')) 
                                for variant in self.variants]
                for term in search_terms:
                    if term not in self.yandex_shelf:
                        logger.info('Adding %s' % term)
                        encoded = term.encode('windows-1251')
                        self.yandex_shelf[term] = yandex.run(encoded)
                        unwritten_items += 1
                if unwritten_items >= 5:
                    logger.info('Dumping to file')
                    self._dump_shelf(self.yandex_shelf, 
                            constants.yandex_shelf_file)
                    unwritten_items = 0

        self._dump_shelf(self.yandex_shelf, constants.yandex_shelf_file)

    def create_csv(self, words, shelf, filename, pruning_function=None):
        filename = self._add_default_csv_directory(filename)
        writer = csv.writer(open(filename, 'w'))
        form_names = ['gen_sg', 'gen_pl', 'inst_sg', 'inst_pl']

        headers = ['lemma', 'tokens']
        for form_name in form_names:
            headers += [form_name, form_name + '_s', form_name + '_so']
        writer.writerow(headers)

        if pruning_function is None:
            pruning_function = lambda x, y: (x, y)

        for lemma, forms in words:
            row = [lemma.encode('utf8'), shelf.get(lemma, None)]
            for form_name in form_names:
                f = forms[form_name].replace("'", '')
                # Double quotes here are a hack, only Yandex queries have
                # quotes around them so this breaks RNC
                s = '"%s %s"' % (constants.s, f)
                so = '"%s %s"' % (constants.so, f)
                sval, soval = pruning_function(shelf.get(s), shelf.get(so))
                row += [x.encode('utf8') for x in 
                        [forms[form_name], str(sval), str(soval)]]
            writer.writerow(row)

    def find_vowels(self, word):
        return [index for index, letter in enumerate(word)
                if letter in constants.vowels]

    def get_initial_cluster(self, word):
        first_vowel = min(self.find_vowels(word))
        return word[:first_vowel]

    def get_stressed_syllable(self, word):
        if "'" in word:
            stress_loc = word.index("'")
        else:
            auto_stressed = [index for index, letter in enumerate(word) if 
                             letter in constants.automatically_stressed_vowels]
            if len(auto_stressed) >= 1:
                stress_loc = list(auto_stressed)[0]
            else:
                return None

        vowel_positions = self.find_vowels(word)
        return len([x for x in vowel_positions if x <= stress_loc])

    def r_csv(self, words, shelf, filename):
        filename = self._add_default_csv_directory(filename)
        writer = csv.writer(open(filename, 'w'))
        
        headers = ['prep', 'lemma', 'form', 'stressed_syll', 'case', 'number',
                   'short_cluster', 'full_cluster', 'noep', 'ep']
        writer.writerow(headers)

        rows = []
        for lemma, forms in words:
            for inflection in self.inflections:
                form = forms[inflection]
                search_terms = ['%s %s' % (variant, form.replace("'", '')) for
                                variant in self.variants]
                # Double quotes here are a hack, only Yandex queries have
                # quotes around them 
                search_terms = ['"%s"' % t for t in search_terms]
                variants = [shelf.get(t) for t in search_terms]

                cluster = self.get_initial_cluster(form)
                case, number = inflection.split('_')
                stressed_syll = self.get_stressed_syllable(form)
                prep = self.transcribed_variants[0]

                fields = [prep, lemma, form, str(stressed_syll), case, number,
                        cluster[:2], 
                        cluster] + map(str, variants)

                row = [x.encode('utf8') for x in fields]
                # Do not write duplicates
                if len(rows) == 0 or row != rows[-1]:
                    rows.append(row)

        writer.writerows(rows)
