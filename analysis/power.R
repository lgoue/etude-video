# Analyse de puissance par simulation : ANOVA à mesures répétées et modèle linéaire mixte (LMM)
# Plan supposé (à adapter ci-dessous) : chaque participant voit VIDEOS_PAR_PART vidéos, équilibrées
# entre niveau (3 : Low / Medium / High) et modèle (2), comme le fait l'algorithme d'attribution de l'application.
# Chaque vidéo n'a qu'un niveau et qu'un modèle (facteurs « intra-participant, inter-vidéos »).
# VD : note moyenne des questions Likert après la vidéo, exprimée en écarts-types (variance totale ≈ 1).
#
# Usage : Rscript analysis/power.R        (paquets : lme4, lmerTest, afex)

suppressMessages({ library(lme4); library(lmerTest); library(afex); library(parallel) })
set.seed(2026)

# ---- Paramètres du plan ----
VIDEOS_PAR_PART <- 12           # n_videos de l'expérience (hors pièges)
VIDEOS_PAR_CELLULE <- as.integer(Sys.getenv("VPC", "8"))  # vidéos disponibles par combinaison niveau × modèle (8 → 48 vidéos)
N_GRID <- as.integer(strsplit(Sys.getenv("N_GRID", "40,80,120,160,200,260"), ",")[[1]])
N_SIM <- as.integer(Sys.getenv("N_SIM", "300"))
ALPHA <- .05

# ---- Composantes de variance (standardisées, somme ≈ 1) ----
SD_PART <- sqrt(.30)            # différences entre participants (certains notent tout plus haut)
SD_VIDEO <- sqrt(.10)           # différences entre vidéos d'une même condition
SD_RES <- sqrt(.60)             # bruit résiduel

# ---- Tailles d'effet testées (d de Cohen sur l'écart-type total) ----
# niveau : High − Low ; modèle : A − B ; interaction : (High − Low chez A) − (High − Low chez B)
SCENARIOS <- c(nul = 0, petit = .2, moyen = .35, `moyen+` = .5)
if (nzchar(Sys.getenv("SCEN"))) SCENARIOS <- SCENARIOS[strsplit(Sys.getenv("SCEN"), ",")[[1]]]

sim_one <- function(n, d) {
  cells <- expand.grid(niveau = c(-1, 0, 1), modele = c(-.5, .5))
  vids <- cells[rep(seq_len(nrow(cells)), each = VIDEOS_PAR_CELLULE), ]
  vids$video <- seq_len(nrow(vids)); vids$w <- rnorm(nrow(vids), 0, SD_VIDEO)
  per_cell <- VIDEOS_PAR_PART / nrow(cells)
  dat <- do.call(rbind, lapply(seq_len(n), function(p) {
    v <- vids[unlist(lapply(split(seq_len(nrow(vids)), paste(vids$niveau, vids$modele)), function(ix) sample(ix, per_cell))), ]
    v$participant <- p; v$u <- rnorm(1, 0, SD_PART); v
  }))
  dat$y <- (d / 2) * dat$niveau + d * dat$modele + (d / 2) * dat$niveau * dat$modele + dat$u + dat$w + rnorm(nrow(dat), 0, SD_RES)
  dat$niveau_f <- factor(dat$niveau); dat$modele_f <- factor(dat$modele)
  dat$participant <- factor(dat$participant); dat$video <- factor(dat$video)
  # LMM : effets aléatoires croisés participant et vidéo, tests F de type III (Satterthwaite)
  m <- suppressMessages(lmer(y ~ niveau_f * modele_f + (1 | participant) + (1 | video), data = dat, contrasts = list(niveau_f = contr.sum, modele_f = contr.sum)))
  pl <- anova(m)[, "Pr(>F)"]
  # ANOVA à mesures répétées : moyenne par participant et par cellule (les vidéos sont moyennées)
  a <- suppressMessages(aov_ez("participant", "y", dat, within = c("niveau_f", "modele_f"), fun_aggregate = mean, anova_table = list(correction = "GG")))
  pa <- a$anova_table[c("niveau_f", "modele_f", "niveau_f:modele_f"), "Pr(>F)"]
  c(lmm = pl, anova = pa)
}

res <- do.call(rbind, lapply(names(SCENARIOS), function(s) do.call(rbind, lapply(N_GRID, function(n) {
  ps <- do.call(rbind, mclapply(seq_len(N_SIM), function(i) tryCatch(sim_one(n, SCENARIOS[[s]]), error = function(e) rep(NA, 6)), mc.cores = max(1, detectCores())))
  pw <- colMeans(ps < ALPHA, na.rm = TRUE)
  r <- data.frame(scenario = s, d = SCENARIOS[[s]], videos_par_cellule = VIDEOS_PAR_CELLULE, N = n,
    lmm_niveau = pw[1], lmm_modele = pw[2], lmm_inter = pw[3], anova_niveau = pw[4], anova_modele = pw[5], anova_inter = pw[6])
  print(r, row.names = FALSE, digits = 2); flush.console(); r
}))))
write.csv(res, file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))), paste0("power_results_v", VIDEOS_PAR_CELLULE, ".csv")), row.names = FALSE)
