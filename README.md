# monotonePkg : Helpers broom & ggplot2 pour la régression isotone

Ce package R :

- ajoute les méthodes **broom** : `tidy()`, `augment()`, `glance()`  
  et la méthode **ggplot2** : `autoplot()`  
  pour les objets `gpava` et `activeset` du package **isotone** ;
- inclut une suite de tests **testthat** (8 tests) ;
- fournit un rapport **Quarto** comparant régressions linéaire et isotonique
  sur le jeu de données Abalone.

---

## Arborescence

```
Regression_Package_Dev/
│
├── DESCRIPTION
├── NAMESPACE          # généré par roxygen2
├── R/
│   └── methods.R
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-gpava.R
│       └── test-activeset.R
└── vignettes/
    └── rapport.qmd
```

---

## Installation en mode développement

```r
# depuis le dossier du dépôt
devtools::install(dependencies = TRUE)
```

---

## Workflow développeur

```r
# 1. Générer doc + NAMESPACE
devtools::document()

# 2. Charger le package
devtools::load_all()

# 3. Lancer les tests
devtools::test()     # → 8 tests ✓

# 4. Rendre le rapport
quarto::quarto_render("vignettes/rapport.qmd", output_format = "html")
```

---

## Extrait d’utilisation

```r
library(monotonePkg)
library(isotone)

set.seed(1)
y  <- rnorm(30)
iso <- gpava(y = y)

tidy(iso)
augment(iso)
glance(iso)
autoplot(iso)
```

---

## Contribution

1. Forkez le dépôt : `https://github.com/Poulete12/Regression_Package_Dev`
2. Créez une branche, ajoutez vos commits, ouvrez une Pull Request.
3. Tout ajout de code doit être couvert par des tests.

---

© 2025 – Antony Lehmann & Asso Ali Mullud
