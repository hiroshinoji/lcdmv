#!/usr/bin/env python

import sys

CPOS = 3
DEP = 6
PUNCT = 'PUNCT'

def remove_punct(sentence):
    i = 0
    while i < len(sentence):
        l = sentence[i]
        upos = l[CPOS]
        if upos == PUNCT:
            parent = int(l[DEP])
            if (parent == i + 1): # NOTE that head index is 1-based, while loop is 0-based.
                print l # meaning head = dep; this should never be happen
            sentence = sentence[:i] + sentence[i+1:]
            for j, m in enumerate(sentence):
                d = int(m[DEP])
                if d == i + 1:
                    d = parent
                    m[DEP] = str(d)
                assert(d != i + 1)
                if d > i + 1:
                    m[DEP] = str(int(m[DEP]) - 1)
                if j >= i:
                    m[0] = str(int(m[0]) - 1)
            i -= 1
        i += 1
    return sentence

def read_conllx(input):
    sentences = []
    sentence = []

    for line in input:
        line = line.strip()
        if not line:
            if sentence:
                sentences.append(sentence)
                sentence = []
            continue
        sentence.append(line.split('\t'))
    if sentence:
        sentences.append(sentence)
    return sentences

def write_conllx(out, sentences):
    for sentence in sentences:
        for token in sentence:
            out.write('\t'.join(token) + '\n')
        out.write('\n')

if __name__ == '__main__':
    sentences = read_conllx(sys.stdin)
    sentences = [remove_punct(s) for s in sentences]
    write_conllx(sys.stdout, sentences)
