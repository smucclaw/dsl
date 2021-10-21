# mp

## L4 handles Legal Spreadsheets

This codebase reads CSV, interprets the L4 in it, and outputs various transformations intended for ingestion by related tools.

## Demo: parsing a Legal Spreadsheet as CSV

The "must sing" tab is available in CSV.

We save that file to disk:

    wget https://docs.google.com/spreadsheets/d/e/2PACX-1vSbknuGFkvvk_Pv9e97iDW9BInxd-Cj27-xC8d_SwveP1PxERV7ZmiAIU7PWXas2CEMH2bl8PyyD6X3/pub\?gid\=1043543357\&single\=true\&output\=csv -O sing.csv

Then we feed it to `stack run`.

    stack run < sing.csv

The output is a list of "rule" data structures parsed from the CSV.

### We transform it for downstream consumption

#### Output: JSON suitable for constructing a Web App

See vue-pure-pdpa

    stack run -- --as json

#### Output: JSON suitable for D3

    stack run -- --as d3

#### Output: An SVG

    stack run -- --as svg

### We evaluate it against some input

If a Legal Spreadsheet contains a test case, we can run those tests.

    stack run -- --tests

### We explain how we arrived at results

    stack run -- --tests --explain
