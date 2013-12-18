# -*- coding: utf-8 -*- 

# Author: Tal Linzen <linzen@nyu.edu>
# License: BSD (3-clause)

# Linzen, Kasyanenko, & Gouskova (2013). (Lexical and phonological 
# variation in Russian prepositions, Phonology 30(3).)

import os

import constants

def find_vowels(word):
    return [index for index, letter in enumerate(word)
            if letter in constants.vowels]

def get_initial_cluster(word):
    first_vowel = min(find_vowels(word))
    return word[:first_vowel]

def get_stressed_syllable(word):
    if "'" in word:
        stress_loc = word.index("'")
    else:
        auto_stressed = [index for index, letter in enumerate(word) 
                         if letter in constants.automatically_stressed_vowels]
        if len(auto_stressed) >= 1:
            stress_loc = list(auto_stressed)[0]
        else:
            return None

    vowel_positions = find_vowels(word)
    return len([x for x in vowel_positions if x <= stress_loc])
