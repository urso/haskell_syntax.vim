
# Introduction

Improvement on the default vim haskell.vim syntax file:

- highlights all Types, not just built in ones
- highlights top level functions
- functions and types are highlighted properly in import/export lists
- repaired spell checking, thus only string and comments are spell checked
- FFI support
- QuasiQuotation support
- highlighting of top level Template Haskell slices

# Installation

## vundle

Add git repository to your vundle file:

    ...
    Bundle 'git://github.com/urso/haskell_syntax.vim.git'
    ...

## pathogen

Copy this diretory to your bundles directory:

    $ git clone https://github.com/urso/haskell_syntax.vim.git \
        $(HOME)/.vim/bundle/haskell_syntax.vim

