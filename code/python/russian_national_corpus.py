# -*- coding: utf-8 -*- 

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

# For references, here are two examples of the queries that this class is
# based on:

# Exact form:
# http://search.ruscorpora.ru/search.xml?mycorp=&mysent=&mysize=&dpp=&spp=&spd=&t=100&text=lexform&mode=main&sort=gr_tagging&lang=en&req=%F1%EB%EE%E2%E0

# Lemma:
# http://search.ruscorpora.ru/search.xml?mycorp=&mysent=&mysize=&dpp=&spp=&spd=&text=lexgramm&mode=main&sort=gr_tagging&lang=en&parent1=0&level1=0&lex1=%EF%F0%E8%EC%E5%F0&gramm1=&sem1=&flags1=

import re
import urllib
import urllib2
from gzip import GzipFile
from cStringIO import StringIO

from httptools import FirefoxRequest

class RussianNationalCorpus(object):
    '''
    Send a query to the Russian National Corpus (ruscorpora.ru/en) and extract
    the number of matches. Works as of January 2014.

    >>> rnc = RussianNationalCorpus()

    To count exact matches in the corpus (following the RNC's definitions, 
    number of "contexts", i.e. number of times this form occurs in the corpus,
    as opposed to "documents"):
    >>> rnc.exact_query('ответов')
    2342

    Lemma search:
    >>> rnc.lemma_query('ответ')
    58550

    Note that ответов 'answer gen.pl' is not a lemma, and therefore has no
    matches:
    >>> rnc.lemma_query('ответов')
    0
    '''

    common_query_dict = {'mycorp': '',
                         'mysent': '',
                         'mysize': '',
                         'dpp': '',
                         'spp': '',
                         'spd': '',
                         'sort': 'gr_tagging',
                         'lang': 'en',
                         'mode': 'main'} 

    exact_query_dict = {'t': '100',
                        'text': 'lexform'}

    exact_query_dict.update(common_query_dict)

    lemma_query_dict = {'parent1': '0',
                        'level1': '0',
                        'text': 'lexgramm',
                        'nodia': '1',
                        'gramm1': '',
                        'sem1': '',
                        'flags1': ''} 

    lemma_query_dict.update(common_query_dict)

    regexp = r'([\d\s]+)</span>\xa0<span class="stat-caption">context'
    regexp = re.compile(regexp)

    def transcode(self, s):
        if type(s) == unicode:
            return s.encode('windows-1251')
        else:
            return s.decode('utf8').encode('windows-1251')

    def exact_query(self, query):
        d = self.exact_query_dict.copy()
        d['req'] = self.transcode(query)
        return self.lookup(d)

    def lemma_query(self, query):
        d = self.lemma_query_dict.copy()
        d['lex1'] = self.transcode(query)
        return self.lookup(d)

    def lookup(self, d):
        req_str = ('http://search.ruscorpora.ru/search.xml?' + 
                   urllib.urlencode(d))
        req = FirefoxRequest(req_str)
        urlinfo = urllib2.urlopen(req)
        string_io = StringIO(urlinfo.read())
        try:
            unzipped = GzipFile(fileobj=string_io).read()
        except IOError:
            unzipped = string_io.read()
        match = self.regexp.search(unzipped)
        if match is not None:
            group1 = match.group(1)
            return int(group1.replace(' ', ''))
        elif 'No results match the search query.' in unzipped:
            return 0
        else:
            v = ValueError('Unparsable response')
            v.response = unzipped
            raise v
