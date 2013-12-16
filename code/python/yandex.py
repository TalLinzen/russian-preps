from gzip import GzipFile
from cStringIO import StringIO
import re, time, sys, urllib, urllib2, random

from httptools import FirefoxRequest

class Yandex(object):

    results_per_page = 10
    max_results = 999

    def __init__(self, delay=0, just_count=True):
        self.item_number_re = re.compile(
                '<b class="b-serp-item__number">(\d+)</b>')
        self.no_results = 'Sorry, there are no results for this search query.'
        self.delay = delay
        self.just_count = just_count
        
    def run(self, query):
        page_number = self.max_results / self.results_per_page
        response = self.send_request(query, page_number)
        data = self.unzip(response.read())

        if self.no_results in data:
            return 0
        else:
            item_numbers = self.item_number_re.findall(data)
            assert item_numbers > 0
            return max(map(int, item_numbers))


    def send_request(self, query, page_number):
        d = {
                'text': query,
                'p': page_number,
                'lr': 202,
                'noreask': 1,
            }

        req_str = 'http://yandex.com/yandsearch?' + urllib.urlencode(d)
        req = FirefoxRequest(req_str)
        time.sleep(self.delay * (1 + random.random()))
        return urllib2.urlopen(req)


    def unzip(self, s):
        return GzipFile(fileobj=StringIO(s)).read()

    def extract_links(self, data):
        return self.links_re.findall(data)
