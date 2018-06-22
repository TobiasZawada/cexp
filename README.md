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


## An example from [emacs.stackexchange.com](https://emacs.stackexchange.com/questions/30432/how-can-i-select-only-the-content-of-a-media-in-css-with-regexp-in-emacs) matching the content of `@media` in css files

Assme you want to highlight the content in parenteses and curly brackets behind `@media` entries of css files.
Therefore you need to find `@media` followed by two balanced expressions and to identify the two balanced expressions.

    /*... css above*/
    /*tablet*/
    @media (max-width: 800px){
        ._desktop-only {display:none !important;}
        ._tablet-only {display:initial;}
        ._mobile-only {display:none;}
    }
    /*mobile*/
    @media (max-width: 480px){
         ._desktop-only {display:none;}
         ._tablet-only {display:none !important;}
         ._mobile-only {display:initial;} /* comment */
    }
    /*more css below...*/

You can use the following elisp expression:

`(cexp-search-forward "@media *\\!(.*\\!) *\\!(.*\\!)")`

which works just like `re-search-forward` with additional sexps.

If you run that elisp expression match data is set as follows:

- `(match-string 0)`: the overall match

        "@media (max-width: 480px){
          ._desktop-only {display:none;}
          ._tablet-only {display:none !important;}
          ._mobile-only {display:initial;} /* comment */
        }"


- `(match-string 1)`: the stuff before the first sexp `"@media "`

- `(match-string 2)`: the first sexp `"(max-width: 800px)"`

- `(match-string 3)`: the regular expression match within the first balanced expression, i.e. the match for `.*` within the match for the first `\\!(.*\\!)`: `"(max-width: 800px)"`

- `(match-string 4)`: the stuff between the first and the second sexp `""`

- `(match-string 5)`: the second balanced expression

        "{
         ._desktop-only {display:none !important;}
         ._tablet-only {display:initial;}
         ._mobile-only {display:none;}
        }"
- `(match-string 6)`: the match for `.*` within the second balanced expression, i.e., `"{"`

Match string 6 is right and perhaps most interesting. It shows the difference between the sub-match that matches a balanced expression (i.e. match string 5) and the match within the balanced expression (i.e. match string 6). The dot `.` only matches characters that are not new-lines and on the first line of the second balanced expression there is only the opening curly brace.
