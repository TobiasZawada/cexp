# cexp (elisp-library)
Poorman's implementation of combined expressions.
Combined expressions are combinations of regular expressions and balanced expressions.
You can use `cexp-search-forward` for searching combined expressions.
Some clumsy way of storing the match-data and the balanced expressions is provided.

Example: You can search for definitions in TeX-files via the cexp

    \\def\\[[:alpha:]]\(#[0-9]\)*\!(^{.*}$\!)

The special construct `\!(...\!)` captures a balanced expression.

If applied to the TeX file

    \def\mdo#1{{\def\next{\relax}\def\tmp{#1}\ifx\next\tmp\else\def\next{#1\mdo}\expandafter}\next}
    
The search via `cexp-search-forward` with the above cexp returns the limits for the following groups:

1. The beginning and the end of the full match
2. The limits of the match for the regular expression before the balanced expression, i.e. `\def\mdo#1`
3. The limits of the captured group in the first regular expression, i.e., `#1`
4. The limits of the balanced expression, i.e., `{{\def\next{\relax}\def\tmp{#1}\ifx\next\tmp\else\def\next{#1\mdo}\expandafter}\next}`
