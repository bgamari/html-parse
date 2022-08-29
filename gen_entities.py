#!/usr/bin/env python3

import sys
import json

# expects entity summary from https://html.spec.whatwg.org/entities.json
entities = json.load(sys.stdin)

def escape(s):
    return s.replace('"', '\\"')

print('module Text.HTML.Parser.Entities (entities) where')
print('')
print('entities :: [(String, String)]')
print('entities = [')
print(',\n'.join(
    f'''  ("{name[1:-1]}", "{escape(details['characters'])}")'''
    for name, details in entities.items()
))
print('  ]')
