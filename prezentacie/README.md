# Prezentácie / Slides

Quarto (reveal.js) prezentácie k siedmim tematickým blokom kurzu – `sk/` slovensky, `en/` anglicky.
Spoločná téma je v `theme.scss`, farby a štýl grafov v `setup.R`, nastavenia formátu v `sk/_metadata.yml` a `en/_metadata.yml`.

## Renderovanie

Potrebujete [Quarto](https://quarto.org/docs/get-started/) (≥ 1.5), R a balíky z [`cvicenie1.R`](../cvicenie1.R) (plus `knitr`, `rmarkdown`).

```bash
cd prezentacie/sk
quarto render 03_rozlisovacie_testy.qmd   # jedna prezentácia
quarto render .                           # všetky v priečinku
```

Alebo v RStudiu otvorte `.qmd` súbor a kliknite na **Render**. Výsledkom je jeden samostatný HTML súbor (asi 4 MB) – funguje aj offline; z internetu sa načítava len písmo pre kód. Niektoré snímky načítavajú dáta zo senzorika.com, preto renderovanie potrebuje internet.

Počas prezentácie: `F` – celá obrazovka, `S` – poznámky rečníka, `O` – prehľad snímok, `B` – stmavenie obrazovky (pauza).
