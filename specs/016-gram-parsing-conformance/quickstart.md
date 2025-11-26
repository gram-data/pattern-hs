# Quickstart: Gram Parsing Conformance

**Feature**: Gram Parsing Conformance
**Branch**: `016-gram-parsing-conformance`

## Overview

This feature integrates the `tree-sitter-gram` test corpus to verify the correctness of the Haskell Gram parser.

## Running the Tests

1.  Ensure you have the `tree-sitter-gram` submodule initialized:
    ```bash
    git submodule update --init --recursive
    ```

2.  Run the test suite using `cabal`:
    ```bash
    cabal test gram-test
    ```

    This will execute:
    - Standard unit tests
    - **New**: Corpus conformance tests (scanning `libs/gram/test-data/tree-sitter-gram/test/corpus/`)

## Verifying a Specific Corpus File

To run only the corpus tests (filtering by "Corpus" in the test name):

```bash
cabal test gram-test --test-option=--match="Corpus"
```

## Troubleshooting Failures

If a corpus test fails:
1.  Identify the failing test case name from the output.
2.  Locate the corresponding file in `libs/gram/test-data/tree-sitter-gram/test/corpus/`.
3.  Extract the input string.
4.  Debug `Gram.Parse.fromGram` with that input.

