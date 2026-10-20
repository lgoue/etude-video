# Analyse de puissance

`power.R` simule l'étude et estime la puissance (α = .05) de deux analyses :

- **LMM** : `lmer(note ~ niveau * modèle + (1 | participant) + (1 | vidéo))`, tests F de type III (Satterthwaite, lmerTest) ;
- **ANOVA à mesures répétées** (afex) sur les moyennes par participant et par cellule niveau × modèle.

## Hypothèses

- 12 vidéos par participant, équilibrées : 2 par cellule niveau (Low / Medium / High) × modèle (2).
- Chaque vidéo n'a qu'un niveau et un modèle ; vidéos par cellule : 4, 8, 16 ou 32 (soit 24 à 192 vidéos).
- VD : note moyenne des questions Likert après chaque vidéo, en écarts-types (variance totale ≈ 1) :
  participants 30 %, vidéos 10 %, résidu 60 %.
- Tailles d'effet (d sur l'écart-type total) : niveau = High − Low, modèle = A − B,
  interaction = différence de l'effet High − Low entre les deux modèles.
- 300 simulations par point (8 vidéos par cellule), 200 pour les autres (erreur type ≈ ±3 points).

Relancer avec d'autres valeurs : `VPC=16 N_GRID=80,160 SCEN=nul,moyen N_SIM=300 Rscript analysis/power.R`
(les variances et les autres paramètres sont en tête du script). Résultats : `power_results_v*.csv`.

## Résultats principaux (puissance du LMM)

| Vidéos / cellule | N | niveau d=.2 | niveau d=.35 | niveau d=.5 | modèle d=.2 | modèle d=.35 | interaction d=.35 |
|---|---|---|---|---|---|---|---|
| 8  | 80  | .26 | .67 | .91 | .43 | .93 | .25 |
| 8  | 160 | .27 | .72 | .96 | .51 | .95 | .21 |
| 8  | 260 | .27 | .74 | .95 | .55 | .95 | .19 |
| 16 | 80  | .42 | .86 | –   | .62 | .99 | .28 |
| 16 | 160 | .47 | .95 | –   | .83 | 1   | .38 |
| 32 | 80  | .50 | .96 | –   | .82 | 1   | .42 |
| 32 | 160 | .70 | 1   | –   | .93 | 1   | .55 |
| 32 | 260 | .80 | 1   | –   | .98 | 1   | .64 |

- **Le nombre de vidéos limite la puissance plus que le nombre de participants** : avec 8 vidéos par cellule,
  passer de 80 à 260 participants ne change presque rien, car la variabilité entre vidéos ne diminue pas.
- **L'ANOVA par participants n'est pas valide ici** : sans aucun effet réel, elle donne 13 à 91 % de faux positifs
  selon le plan, car elle ignore la variabilité entre vidéos. Utiliser le LMM (taux de faux positifs ≈ 5 %).
- **L'interaction niveau × modèle** est peu testable dans ces conditions (≤ 64 %), sauf effet plus grand ou beaucoup plus de vidéos.

## Plan réel (`power_plan.R`)

3 vidéos par combinaison genre agent (2) × genre transcript (3) × niveau (3) × modèle (3) × tâche (2) = 324 vidéos.
Attribution simulée avec le même algorithme que l'application (équilibre sur niveau, modèle, modèle × niveau, genres et tâche).
LMM : `note ~ niveau * modèle + agent + transcript + tâche + (1 | participant) + (1 | vidéo)`. 200 simulations par point.

Puissance du LMM pour un **petit effet** (d = .2) ; pour d = .35 tous les effets principaux sont ≥ 96 % dès N = 60.

| Vidéos / participant | N | niveau | modèle | genre transcript | genre agent | tâche | interaction (d = .35) |
|---|---|---|---|---|---|---|---|
| 12 | 60  | .56 | .55 | .57 | .84 | .82 | .21 |
| 12 | 120 | .76 | .81 | .80 | .95 | .98 | .32 |
| 12 | 180 | .85 | .81 | .86 | .98 | .98 | .45 |
| 12 | 240 | .90 | .88 | .92 | .99 | .99 | .47 |
| 18 | 60  | .71 | .69 | .73 | .91 | .91 | .32 |
| 18 | 120 | .84 | .87 | .84 | .99 | .98 | .48 |
| 18 | 180 | .92 | .94 | .93 | .99 | .99 | .54 |
| 18 | 240 | .94 | .91 | .94 | .99 | 1   | .60 |

Faux positifs sans effet réel : LMM ≈ 5 % ; ANOVA par participants 7 à 47 % (à éviter comme analyse principale).
Résultats : `power_plan_results_vpp12.csv`, `power_plan_results_vpp18.csv`.

## Interaction genre agent × genre transcript (`power_plan.R`, BAL_GT=1)

Effet simulé : la différence agent F − M change de d entre transcripts F et M. Puissance du LMM (`agent * transcript` dans le modèle) :

| Vidéos / participant | N | d = .2 | d = .35 | d = .5 |
|---|---|---|---|---|
| 12 | 100 | .23 | .54 | .90 |
| 12 | 150 | .26 | .71 | .94 |
| 12 | 200 | .32 | .76 | .96 |
| 18 | 100 | .30 | .73 | .97 |
| 18 | 150 | .31 | .76 | .98 |
| 18 | 200 | .39 | .87 | .99 |

Sans l'équilibrage de la combinaison (18 vidéos, N = 100) : .26 / .75 / .97, donc l'équilibrage change peu la puissance du LMM
(il garantit surtout que chaque participant voit les 6 combinaisons). L'ANOVA par participants affiche des valeurs plus hautes
mais n'est pas fiable (faux positifs gonflés, voir plus haut). Résultats : `power_gt_*.csv`.

# Analyse des résultats (`analyse_results.R`)

`Rscript analysis/analyse_results.R export.csv [dossier]` : lit l'export CSV de l'admin, applique les exclusions
(réglages en tête du script), calcule les scores (capacité, bienveillance, confiance, questionnaires) et ajuste les modèles
mixtes `vd ~ niveau * dimension + niveau * modèle + genre agent * genre transcript + tâche + (1 | participant) + (1 | vidéo)`.
Sorties dans `analysis/results/` (ignoré par git, car il contient des données de participants).
