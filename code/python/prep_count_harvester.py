# -*- coding: utf-8 -*-

import pickle, os, csv
import constants
from paradigm_file import ParadigmFile
from yandex_estimates import Yandex

tst = os.path.expanduser('~/Dropbox/russian_preps/morph_squib/searches/' 
        'estv_nominalizations/stv_nominalization_paradigms.txt')

class PrepCountHarvester(object):

    def __init__(self, preposition, filename):
        self.variants = preposition['variants']
        self.transcribed_variants = preposition['transcribed_variants']
        self.inflections = ['%s_%s' % (case, number) 
                for case in preposition['cases']
                for number in ['sg', 'pl']]
        self.paradigm_file = ParadigmFile(filename)
        self.yandex_shelf = self.load_shelf(constants.yandex_shelf_file)
        self.words = self.clusters()

    def clusters(self, frequency_threshold=5):
        words = []
        for lemma, forms in self.paradigm_file.good_nouns:
            form = forms[self.inflections[0]]
            if form[0] not in constants.vowels:
                second = (form[1] if form[1] not in constants.znaks 
                        else form[2])
                if second not in constants.vowels:
                    words.append((lemma, forms))
        return words

    def add_default_csv_directory(self, filename):
        if filename[0] != '/':
            filename = os.path.join(constants.default_csv_dir, filename)
        return filename

    def load_shelf(self, filename):
        if os.path.exists(filename):
            f = open(filename)
            return pickle.load(f)
        else:
            return {}

    def dump_shelf(self, shelf, filename):
        f = open(filename, 'w')
        pickle.dump(shelf, f)

    def print_list(self, what):
        for lemma, forms in what:
            print '%s:' % lemma,
            for key, value in forms.items():
                print '%s: %s ' % (key, value)

    def do_yandex_searches(self, words):
        yandex = Yandex(delay=60)
        unwritten_items = 0
        for i, (lemma, forms) in enumerate(words):
            for inflection in self.inflections:
                form = forms[inflection]
                search_terms = ['"%s %s"' % (variant, form.replace("'", '')) 
                        for variant in self.variants]
                for term in search_terms:
                    if term not in self.yandex_shelf:
                        print term
                        encoded = term.encode('windows-1251')
                        self.yandex_shelf[term] = yandex.run(encoded)
                        unwritten_items += 1
                if unwritten_items >= 5:
                    print 'dumping'
                    self.dump_shelf(self.yandex_shelf, 
                            constants.yandex_shelf_file)
                    unwritten_items = 0

        self.dump_shelf(self.yandex_shelf, constants.yandex_shelf_file)

    def create_csv(self, words, shelf, filename, pruning_function=None):
        filename = self.add_default_csv_directory(filename)
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
            auto_stressed = [index for index, letter in enumerate(word)
                    if letter in constants.automatically_stressed_vowels]
            if len(auto_stressed) >= 1:
                stress_loc = list(auto_stressed)[0]
            else:
                return None

        vowel_positions = self.find_vowels(word)
        return len([x for x in vowel_positions if x <= stress_loc])

    def r_csv(self, words, shelf, filename):
        '''
        pch.r_csv(pch.words2, pch.yandex_shelf, 'r.csv', prune_extreme_values)
        '''

        filename = self.add_default_csv_directory(filename)
        writer = csv.writer(open(filename, 'w'))
        
        headers = ['prep', 'lemma', 'form', 'stressed_syll',
                'case', 'number', 'short_cluster',
                'full_cluster', 'noep', 'ep']
        writer.writerow(headers)

        rows = []
        for lemma, forms in words:
            for inflection in self.inflections:
                form = forms[inflection]
                search_terms = ['%s %s' % (variant, form.replace("'", '')) 
                        for variant in self.variants]
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
