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

def add_default_csv_directory(filename):
    if filename[0] != '/':
        filename = os.path.join(constants.default_csv_dir, filename)
    return filename

