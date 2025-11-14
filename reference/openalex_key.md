# Use an API key for OpenAlex Premium Subscription

This provides access to the latest data, fresher than what snapshots
provide. It also enables faster requests and filtering on
from_created_date and from_updated_date fields.

## Usage

``` r
openalex_key(key)
```

## Arguments

- key:

  a premium subscription key

## Value

a logical depending on whether key was set or unset

## Details

Additional details...

<https://github.com/ourresearch/openalex-api-tutorials/blob/main/notebooks/getting-started/premium.ipynb>
<https://docs.openalex.org/api-entities/works/filter-works#from_created_date>
<https://docs.openalex.org/api-entities/works/filter-works#from_updated_date>

## Examples

``` r
if (FALSE) { # interactive()
 openalex_key("my_secret_api_key")
 openalex_key("")
}
```
