# -*- coding: utf-8 -*- 
from gzip import GzipFile
from cStringIO import StringIO
import re, time, sys, urllib, urllib2, random

from httptools import FirefoxRequest

class Yandex(object):

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

        d = {
                'text': query,
                'lr': 21411,
                'noreask': 1,
                'nomisspell': 1
            }

        req_str = 'http://yandex.ru/yandsearch?' + urllib.urlencode(d)
        req = FirefoxRequest(req_str)
        time.sleep(self.delay * (1 + random.random()))
        return urllib2.urlopen(req)


    def unzip(self, s):

        return GzipFile(fileobj=StringIO(s)).read()

# examples:
# y.run('ответов')  # millions
# y.run('ответовответов')   # 82
# y.run('ответовответовответовответов')  # none (ничего не найдено)
# y.run('гшис вход в систему яндекс нашлось тыс ответов') # thousands
