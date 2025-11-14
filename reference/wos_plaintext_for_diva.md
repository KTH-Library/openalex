# Function which converts a wos_plaintext-string into a format which can be uploaded to DiVA, by adding ER tags (including a blank line) after each record

Function which converts a wos_plaintext-string into a format which can
be uploaded to DiVA, by adding ER tags (including a blank line) after
each record

## Usage

``` r
wos_plaintext_for_diva(x)
```

## Arguments

- x:

  character string with "wos-plaintext" format as returned from OpenAlex
  export API endpoint
