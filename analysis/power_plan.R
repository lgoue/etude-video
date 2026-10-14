# Puissance pour le plan réel : 3 vidéos par combinaison genre agent (2) × genre transcript (3) × niveau (3)
# × modèle (3) × tâche (2) = 108 combinaisons, 324 vidéos. Chaque participant voit VIDEOS_PAR_PART vidéos,
# choisies par le même algorithme glouton que l'application (équilibre intra-participant sur tous les aspects
# cochés, dont la combinaison modèle × niveau, puis vidéos les moins vues).
# Analyses : LMM (participant et vidéo aléatoires) et ANOVA à mesures répétées niveau × modèle sur les moyennes par cellule.
#
# Usage : N_SIM=200 Rscript analysis/power_plan.R      (variables : N_GRID, SCEN, VPP)

suppressMessages({ library(lme4); library(lmerTest); library(afex); library(parallel) })
set.seed(2027)

VIDEOS_PAR_PART <- as.integer(Sys.getenv("VPP", "12"))
VIDEOS_PAR_COMBI <- 3
N_GRID <- as.integer(strsplit(Sys.getenv("N_GRID", "60,120,180,240,300"), ",")[[1]])
N_SIM <- as.integer(Sys.getenv("N_SIM", "200"))
ALPHA <- .05
SD_PART <- sqrt(.30); SD_VIDEO <- sqrt(.10); SD_RES <- sqrt(.60)
# d : écart entre les modalités extrêmes de chaque facteur (High − Low, GPT − Mistral, F − M, …) ;
# interaction : différence de l'effet High − Low entre GPT et Mistral
SCENARIOS <- c(nul = 0, petit = .2, moyen = .35, `moyen+` = .5)
if (nzchar(Sys.getenv("SCEN"))) SCENARIOS <- SCENARIOS[strsplit(Sys.getenv("SCEN"), ",")[[1]]]

vids0 <- expand.grid(agent = c("F", "M"), transcript = c("F", "M", "N"), niveau = c("Low", "Medium", "High"),
                     modele = c("gpt", "gemma", "mistral"), tache = c("nav", "qa"), rep = seq_len(VIDEOS_PAR_COMBI), stringsAsFactors = FALSE)
vids0$video <- seq_len(nrow(vids0)); vids0$modele_niveau <- paste(vids0$modele, vids0$niveau)
FEATS <- c("niveau", "modele", "modele_niveau", "agent", "transcript", "tache")

# Attribution identique à pick() de server.js
assign_videos <- function(n) {
  seen <- integer(nrow(vids0))
  lapply(seq_len(n), function(p) {
    cnt <- numeric(0); avail <- rep(TRUE, nrow(vids0)); out <- integer(0)
    for (k in seq_len(VIDEOS_PAR_PART)) {
      pen <- Reduce(`+`, lapply(FEATS, function(f) { v <- cnt[paste(f, vids0[[f]])]; v[is.na(v)] <- 0; v }))
      s <- 1000 * pen + seen + runif(nrow(vids0)); s[!avail] <- Inf
      b <- which.min(s); out <- c(out, b); avail[b] <- FALSE
      for (key in paste(FEATS, unlist(vids0[b, FEATS]))) cnt[key] <- if (is.na(cnt[key])) 1 else cnt[key] + 1
    }
    seen[out] <<- seen[out] + 1L; out
  })
}

sim_one <- function(n, d) {
  v <- vids0; v$w <- rnorm(nrow(v), 0, SD_VIDEO)
  niv <- c(Low = -1, Medium = 0, High = 1)[v$niveau]; mod <- c(mistral = -.5, gemma = 0, gpt = .5)[v$modele]
  v$mu <- (d / 2) * niv + d * mod + (d / 2) * niv * mod + d * c(F = .5, M = -.5)[v$agent] +
    d * c(F = .5, N = 0, M = -.5)[v$transcript] + d * c(qa = .5, nav = -.5)[v$tache]
  a <- assign_videos(n)
  dat <- data.frame(participant = rep(seq_len(n), each = VIDEOS_PAR_PART), video = unlist(a))
  dat <- cbind(dat, v[dat$video, c("niveau", "modele", "agent", "transcript", "tache", "w", "mu")])
  dat$y <- dat$mu + dat$w + rnorm(n, 0, SD_PART)[dat$participant] + rnorm(nrow(dat), 0, SD_RES)
  for (f in c("participant", "video", "niveau", "modele", "agent", "transcript", "tache")) dat[[f]] <- factor(dat[[f]])
  m <- suppressMessages(lmer(y ~ niveau * modele + agent + transcript + tache + (1 | participant) + (1 | video), data = dat,
    contrasts = list(niveau = contr.sum, modele = contr.sum, agent = contr.sum, transcript = contr.sum, tache = contr.sum)))
  at <- anova(m); pl <- at[c("niveau", "modele", "niveau:modele", "agent", "transcript", "tache"), "Pr(>F)"]
  ao <- suppressMessages(aov_ez("participant", "y", dat, within = c("niveau", "modele"), fun_aggregate = mean, anova_table = list(correction = "GG")))
  pa <- ao$anova_table[c("niveau", "modele", "niveau:modele"), "Pr(>F)"]
  c(pl, pa)
}

NAMES <- c("lmm_niveau", "lmm_modele", "lmm_inter", "lmm_agent", "lmm_transcript", "lmm_tache", "anova_niveau", "anova_modele", "anova_inter")
res <- do.call(rbind, lapply(names(SCENARIOS), function(s) do.call(rbind, lapply(N_GRID, function(n) {
  ps <- do.call(rbind, mclapply(seq_len(N_SIM), function(i) tryCatch(sim_one(n, SCENARIOS[[s]]), error = function(e) rep(NA, 9)), mc.cores = max(1, detectCores())))
  pw <- colMeans(ps < ALPHA, na.rm = TRUE); names(pw) <- NAMES
  r <- data.frame(scenario = s, d = SCENARIOS[[s]], videos_par_part = VIDEOS_PAR_PART, N = n, t(pw), n_ok = sum(complete.cases(ps)))
  print(r, row.names = FALSE, digits = 2); flush.console(); r
}))))
write.csv(res, file.path("analysis", paste0("power_plan_results_vpp", VIDEOS_PAR_PART, ".csv")), row.names = FALSE)
