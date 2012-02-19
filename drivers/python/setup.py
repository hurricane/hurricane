#!/usr/bin/env python

from setuptools import setup

setup(
    name='hurricane',
    version='1.0.0',
    description='Hurricane driver',
    long_description=\
        'The Hurricane package includes libraries to communicate with ' \
        'the Hurricane messaging system, encode/decode all Erlang terms, ' \
        'and provides a WSGI server for use with Hurricane',
    platforms='Platform Independent',
    author='Ilia Cheishvili',
    author_email='ilia.cheishvili@gmail.com',
    url='http://www.github.com/icheishvili/hurricane',
    scripts=[
        'scripts/hurricane_wsgi_server',
    ],
    packages=[
        'hurricane',
    ],
)

