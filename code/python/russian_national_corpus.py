from httptools import FirefoxRequest
from gzip import GzipFile
import re, urllib, urllib2
from cStringIO import StringIO

class RussianNationalCorpus(object):

    '''
    Exact form:
    http://search.ruscorpora.ru/search.xml?mycorp=&mysent=&mysize=&dpp=&spp=&spd=&t=100&text=lexform&mode=main&sort=gr_tagging&lang=en&req=%F1%EB%EE%E2%E0

    Lemma:
    http://search.ruscorpora.ru/search.xml?mycorp=&mysent=&mysize=&dpp=&spp=&spd=&text=lexgramm&mode=main&sort=gr_tagging&lang=en&parent1=0&level1=0&lex1=%EF%F0%E8%EC%E5%F0&gramm1=&sem1=&flags1=
    '''

    common_query_dict = { 
        'mycorp': '',
        'mysent': '',
        'mysize': '',
        'dpp': '',
        'spp': '',
        'spd': '',
        'sort': 'gr_tagging',
        'lang': 'en',
        'mode': 'main'
    }

    exact_query_dict = {
        't': '100',
        'text': 'lexform',
    }

    exact_query_dict.update(common_query_dict)

    lemma_query_dict = {
        'parent1': '0',
        'level1': '0',
        'text': 'lexgramm',
        'nodia': '1',
        'gramm1': '',
        'sem1': '',
        'flags1': ''
    }

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
        unzipped = GzipFile(fileobj=string_io).read()
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

rnc = RussianNationalCorpus()
