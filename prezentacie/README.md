# Prezentácie / Slides

Quarto (reveal.js) prezentácie k siedmim tematickým blokom kurzu – `sk/` slovensky, `en/` anglicky.
Spoločná téma je v `theme.scss`, farby a štýl grafov v `setup.R`, nastavenia formátu v `sk/_metadata.yml` a `en/_metadata.yml`, nastavenie projektu v `_quarto.yml`.

## Renderovanie

Potrebujete [Quarto](https://quarto.org/docs/get-started/) (≥ 1.5), R a balíky z [`cvicenie1.R`](../cvicenie1.R) (plus `knitr`, `rmarkdown`).

```bash
cd prezentacie
quarto render sk/03_rozlisovacie_testy.qmd   # jedna prezentácia
quarto render                                # všetkých 14
```

Alebo v RStudiu otvorte `.qmd` súbor a kliknite na **Render**. Výsledkom je malý HTML súbor (asi 45 kB), obrázky v priečinku `<názov>_files/` a spoločné knižnice v `libs/` – HTML preto nepresúvajte samostatne, vždy spolu s týmito priečinkami. Po úprave `.qmd` commitnite zdroj, vyrenderované `.html` aj `<názov>_files/`. Dáta sa načítavajú z priečinka `../datasety/`, internet treba len na písmo pre kód.

Počas prezentácie: `F` – celá obrazovka, `S` – poznámky rečníka, `O` – prehľad snímok, `B` – stmavenie obrazovky (pauza).
