# Enter the OpenAlex API polite pool for faster requests by providing an email

Enter the OpenAlex API polite pool for faster requests by providing an
email

## Usage

``` r
openalex_polite(email)
```

## Arguments

- email:

  an email address, on the form "you@example.com" or "" to unset email

## Value

a logical depending on whether email was set or unset

## Examples

``` r
if (FALSE) { # \dontrun{
if(interactive()){
 # to set
 openalex_polite("you@example.com")
 # to unset
 openalex_polite("")
 }
} # }
```
