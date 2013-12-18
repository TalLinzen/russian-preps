# -*- coding: utf-8 -*- 

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

from cStringIO import StringIO
from gzip import GzipFile
from HTMLParser import HTMLParser, HTMLParseError
from urllib2 import Request, urlopen
import logging

logger = logging.getLogger("Preposition corpus builder")

class FirefoxRequest(Request):
    '''
    A request that masquerades as a Firefox request.
    '''

    def __init__(self, *args, **kwargs):
        Request.__init__(self, *args, **kwargs)

        logger.info("HTTP request: %s" % args[0])
        self.add_header('Cookie', 'guid=B11E0A0A522C8371X1378648945')
        self.add_header('User-Agent', 'Mozilla/5.0 (Macintosh; Intel Mac OS X' 
                        '10.8; rv:23.0) Gecko/20100101 Firefox/23.0')
        self.add_header('Accept', 'text/html,application/xhtml+xml,' 
                        'application/xml;q=0.9,*/*;q=0.8')
        self.add_header('Accept-Language', 'en-us,en;q=0.5')
        self.add_header('Accept-Encoding', 'gzip,deflate')
        self.add_header('Accept-Charset', 'ISO-8859-1,utf-8;q=0.7,*;q=0.7')

    def read(self):
        self.response = urlopen(self)
        self.html = self.response.read()
        self.url = self.response.geturl()
        if self.response.info().get('Content-Encoding', '').upper() == 'GZIP':
            self.html = GzipFile(fileobj=StringIO(self.html)).read()
        return self.html


class HTML2Text(HTMLParser):

    tags_to_ignore = ['script']

    def __init__(self):
        HTMLParser.__init__(self)
        self.discarded_tags = []
        self.data = ''

    def handle_starttag(self, tag, attrs):
        if tag in self.tags_to_ignore:
            self.discard_data = True
        elif tag in ('br', 'p'):
            self.data += '\n'

    def handle_data(self, data):
        if len(self.discarded_tags) == 0:
            self.data += data

    def handle_endttag(self, tag):
        self.discarded_tags.remove(tag)

    def feed_ignore_errors(self, data):
        for d in data:
            try:
                self.feed(d)
            except HTMLParseError, exc:
                pass
