# Natural L4

#### Output: JSON suitable for D3

    stack run -- --as d3

#### Output: An SVG

    stack run -- --as svg

### We evaluate it against some input

If a Legal Spreadsheet contains a test case, we can run those tests.

    stack run -- --tests

### We explain how we arrived at results

    stack run -- --tests --explain
