# -*- coding: utf-8 -*-
import os

project_dir = os.path.expanduser('~/Dropbox/russian_preps')
paradigms_file = os.path.join(project_dir, 'resources', 'paradigms.txt')
yandex_shelf_file = os.path.join(project_dir, 'results', 'morph_searches',
    'yandex.shelf')
default_csv_dir = os.path.join(project_dir, 'csv')

automatically_stressed_vowels = u'ё'
vowels = automatically_stressed_vowels + u'яюаеиоыуэ'
consontants = u'шртпщсдфгчклжхцвбнм'
znaks = [u'ь', u'ъ']
unvoiced_stops = u'птк'
voiced_stops = u'бдг'
unvoiced_fricatives = u'сфшщцчх'
voiced_fricatives = u'звж'
nasals = u'мн'
liquids = u'лp'
# Same sonority for palatalized consonants?
selkirk_sonority_scale = [unvoiced_stops, voiced_stops, unvoiced_fricatives,
        voiced_fricatives, nasals, liquids]

s = {
        'cases': ['inst', 'gen'], 
        'variants': [u'с', u'со'],
        'transcribed_variants': ['s', 'so']
    }

v = {
        'cases': ['acc', 'prep'],
        'variants': [u'в', u'во'],
        'transcribed_variants': ['v', 'vo']
    }

k = {
        'cases': ['dat'],
        'variants': [u'к', u'ко'],
        'transcribed_variants': ['k', 'ko']
    }

google_api_key = 'AIzaSyAF_CRT6GLhnt0Jae9NvlhhmW25Ff4IL1Y'

def build_sonority_dict(self):
    self.sonority = {}
    for group_index, group in enumerate(self.selkirk_sonority_scale):
        for consonant in group:
            self.sonority[consonant] = group_index
