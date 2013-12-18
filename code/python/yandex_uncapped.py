# -*- coding: utf-8 -*- 

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

import re
import random
import time
import sys
import urllib
import urllib2
from gzip import GzipFile
from cStringIO import StringIO

from httptools import FirefoxRequest

class YandexUncapped(object):
    '''
    Send a query to the Yandex search engine (yandex.ru), and extract the 
    estimated number of matches. This code works as of January 2014.
    Example:
        
    >>> y = YandexCapped()
    >>> y.run('ответов')
    490000000

    Note that Yandex will block you if you submit too many queries in a short
    period of time. To set a minimal delay between consecutive queries, use
    the `delay` argument (in seconds). Unfortunately, it appears that a very
    substantial delay is required to avoid get blocked.

    >>> y = YandexCapped(5)
    >>> y.run('ответов')
    490000000
    '''

    title_re = re.compile('var title = "(.*?)"')
    number_re = re.compile('(\d+)')

    def __init__(self, delay=0):
        self.delay = delay
        
    def run(self, query):
        response = self.send_request(query)
        data = self.unzip(response.read())
        title = self.title_re.search(data).group(1)
        match = self.number_re.search(title)
        if match is None:
            if 'ничего' in title:
                return 0
            else:
                raise ValueError('unknown title: %s' % title)
        else:
            number = int(match.group(1))
            if 'млн ответов' in title:
                return number * 1000000
            elif 'тыс. ответов' in title:
                return number * 1000
            else:
                return number

    def send_request(self, query):
        d = {'text': query,
             'lr': 21411,
             'noreask': 1,
             'nomisspell': 1} 
        req_str = 'http://yandex.ru/yandsearch?' + urllib.urlencode(d)
        req = FirefoxRequest(req_str)
        time.sleep(self.delay * (1 + random.random()))
        return urllib2.urlopen(req)

    def unzip(self, s):
        return GzipFile(fileobj=StringIO(s)).read()
