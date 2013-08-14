#!/usr/bin/env python

from setuptools import setup, find_packages

setup(
  name='Dust',
  version='0.1a17',
  description='Blocking-resistant network protocol',
  author='Brandon Wiley',
  author_email='dust@blanu.net',
  url='http://github.com/blanu/Dust',
#  packages=['Dust'],
  packages=find_packages(),
  package_data={
    '':['*.so', '*.dll'],
  },
  long_description="""\
    Dust is an Internet protocol designed to resist a number of attacks currently in active use to censor Internet communication.
  """,
  classifiers=[
    "Development Status :: 3 - Alpha",
    "Environment :: No Input/Output (Daemon)",
    "License :: OSI Approved :: BSD License",
    "Programming Language :: Python",
    "Intended Audience :: Developers",
    "Operating System :: OS Independent",
    "Topic :: Internet",
    "Topic :: Security :: Cryptography",
    "Topic :: Software Development :: Libraries :: Python Modules",
  ],
  keywords='cryptography privacy internet',
  license='BSD',
  install_requires=[
    'pyskein>=0.6',
    'bitstring',
    'PyYAML>=3.09',
  ],
#  console_scripts=[
#  ],
  zip_safe=True,
)