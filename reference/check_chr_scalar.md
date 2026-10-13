# Vérifer ce que l'utilisateurs donne

Vérifer ce que l'utilisateurs donne

## Usage

``` r
check_chr_scalar(
  x,
  complement = NULL,
  call = rlang::caller_env(),
  arg = rlang::caller_arg(x),
  ...
)
```

## Arguments

- x:

  l'objet à vérifer

- complement:

  Un complément d'information pour aguiller l'utilisaeur (optionnel)

- call:

  Pour que l emessage d'erreur mentionne la fonction que l'utilisateur a
  appelé

- arg:

  Pour choper le nom de l'argument dans lequel la fonction a été
  utilisé.

- classe:

  le qui identifie la classe

- message:

  Le message d'erreur à afficher

## Value

Rien du tout, mais un message d'erreur si ça foire

## Examples

``` r

#

```
