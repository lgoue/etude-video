# Power for the final design, by simulation, with the same models as analyse_results.R.
#
# Usage: Rscript analysis/power_final.R video_list.csv [N per study, comma-separated] [n_sim]
#   e.g. Rscript analysis/power_final.R videos.csv 54,108 200
#
# Design simulated
#   - Video pool: the real list (attention-check videos removed). Each participant sees 18 QA + 18 Nav videos, chosen with the
#     same greedy balancing as the application (level, model, model x level, agent gender, transcript gender, dimension).
#   - Two studies of N participants each. A video is rated on ability in one study (S1: QA, S2: Nav) and on benevolence in the
#     other. Ability and benevolence have the same structure, so the power computed for the ability score applies to both.
#   - Ability score ~ normal, variance split participants 30 % / videos 10 % / residual 60 % (total 1), as in power_plan.R.
#     Model: score ~ level * dimension + level * model + agent_gender * transcript_gender + task + (1|participant) + (1|video).
#   - Same transcript, female vs male agent: paired t-test across transcripts (mean score of each version).
#   - Advice taken (0/1, all 36 videos): logistic mixed model, followed ~ level + model + agent_gender * transcript_gender + task.
# Effect sizes: d (Cohen, on the total SD of the score) for each effect at once:
#   level = High - Low; level x dimension = how much more the level moves the score when that dimension is manipulated;
#   model = GPT - Mistral; level x model = (High - Low) GPT minus (High - Low) Mistral; agent gender = F - M;
#   transcript gender = F - M; agent x transcript = (F - M agent) in F transcripts minus in M transcripts; task = QA - Nav.
#   For advice taken, the same effects in log-odds (0.2 ~ 5 points of percentage, 0.4 ~ 10, 0.6 ~ 15 around 50-60 %).

suppressMessages({ library(lme4); library(lmerTest); library(emmeans); library(parallel) })
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
options(contrasts = c("contr.sum", "contr.poly"))
args <- commandArgs(TRUE)
if (!length(args)) stop("Usage: Rscript analysis/power_final.R video_list.csv [N per study] [n_sim]")
N_GRID <- if (length(args) >= 2) as.integer(strsplit(args[2], ",")[[1]]) else c(54, 108)
N_SIM  <- if (length(args) >= 3) as.integer(args[3]) else 200
OUT    <- file.path("analysis", "power_final_results.csv")
set.seed(2027)

vl <- read.csv(text = sub("^﻿", "", readLines(args[1], encoding = "UTF-8", warn = FALSE)), check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
pool <- data.frame(level = vl$Niveau, agent_gender = vl[["Genre Agent"]], task = vl$Tache, transcript_gender = vl[["Genre Transcript"]],
                   dimension = vl[[intersect(c("Dimension manipulated", "Dimension"), names(vl))[1]]], transcript_no = vl$Transcript, model = vl$Modele, stringsAsFactors = FALSE)
pool$dimension[pool$dimension %in% c("Abilité", "Abilite", "ability")] <- "Ability"   # spelling variants in the sheet
pool <- pool[pool$level != "Attention", ]
pool$video <- seq_len(nrow(pool)); pool$model_level <- paste(pool$model, pool$level)
FEATS <- c("level", "model", "model_level", "agent_gender", "transcript_gender", "dimension")
cat("Video pool:", nrow(pool), "videos (QA", sum(pool$task == "QA"), "| Nav", sum(pool$task == "Nav"), ")\n")

# Greedy balanced assignment, as pick() in server.js: per participant, balance the features, then prefer the least-seen videos
assign <- function(n, idx, k = 18) {
  seen <- integer(length(idx)); sub <- pool[idx, ]
  lapply(seq_len(n), function(p) {
    cnt <- numeric(0); avail <- rep(TRUE, length(idx)); out <- integer(0)
    for (j in seq_len(k)) {
      pen <- Reduce(`+`, lapply(FEATS, function(f) { v <- cnt[paste(f, sub[[f]])]; v[is.na(v)] <- 0; v }))
      s <- 1000 * pen + seen + runif(length(idx)); s[!avail] <- Inf
      b <- which.min(s); out <- c(out, b); avail[b] <- FALSE
      for (key in paste(FEATS, unlist(sub[b, FEATS]))) cnt[key] <- if (is.na(cnt[key])) 1 else cnt[key] + 1
    }
    seen[out] <<- seen[out] + 1L; idx[out]
  })
}
QA <- which(pool$task == "QA"); NAV <- which(pool$task == "Nav")

code <- with(pool, list(lvl = c(Low = -1, Medium = 0, High = 1)[level], dimA = ifelse(dimension == "Ability", .5, -.5),
                        mod = c(Mistral = -.5, Gemma = 0, GPT = .5)[model], ag = ifelse(agent_gender == "Femme", .5, -.5),
                        tg = c(Homme = -.5, Neutre = 0, Femme = .5)[transcript_gender], tk = ifelse(task == "QA", .5, -.5)))
lin <- function(d) with(code, d / 2 * lvl + d / 2 * lvl * dimA + d * mod + d / 2 * lvl * mod + d * ag + d * tg + d * ag * tg + d * tk)
TERMS <- c("level", "level:dimension", "model", "level:model", "agent_gender", "transcript_gender", "agent_gender:transcript_gender", "task")

sim_one <- function(n, d, b) {
  # Each study: n participants, 18 QA + 18 Nav videos each
  rows <- do.call(rbind, lapply(c("S1", "S2"), function(st) {
    a <- mapply(c, assign(n, QA), assign(n, NAV), SIMPLIFY = FALSE)
    data.frame(study = st, participant = paste0(st, "_", rep(seq_len(n), each = 36)), video = unlist(a))
  }))
  v <- pool[rows$video, ]; rows <- cbind(rows, v[, c("level", "dimension", "model", "agent_gender", "transcript_gender", "task", "transcript_no")])
  # Ability ratings: QA videos in S1, Nav videos in S2
  ab <- rows[(rows$study == "S1" & rows$task == "QA") | (rows$study == "S2" & rows$task == "Nav"), ]
  w <- rnorm(nrow(pool), 0, sqrt(.10)); u <- setNames(rnorm(2 * n, 0, sqrt(.30)), unique(rows$participant))
  ab$y <- lin(d)[ab$video] + w[ab$video] + u[ab$participant] + rnorm(nrow(ab), 0, sqrt(.60))
  for (f in c("participant", "video", "level", "dimension", "model", "agent_gender", "transcript_gender", "task")) ab[[f]] <- factor(ab[[f]])
  m <- suppressMessages(lmer(y ~ level * dimension + level * model + agent_gender * transcript_gender + task + (1 | participant) + (1 | video), data = ab))
  p_lmm <- anova(m)[TERMS, "Pr(>F)"]
  # Same transcript, female vs male agent: paired t-test on the mean score of each version
  tm <- aggregate(y ~ transcript_no + agent_gender, ab, mean); wide <- reshape(tm, idvar = "transcript_no", timevar = "agent_gender", direction = "wide")
  wide <- wide[complete.cases(wide), ]
  p_pair <- if (nrow(wide) > 3) t.test(wide$y.Femme, wide$y.Homme, paired = TRUE)$p.value else NA
  # Advice taken on every video (log-odds effects b)
  fo <- rows; ufo <- setNames(rnorm(2 * n, 0, .5), unique(rows$participant)); wfo <- rnorm(nrow(pool), 0, .3)
  eta <- .3 + with(code, b * lvl + b * mod + b * ag + b * tg + b * ag * tg + b * tk)[fo$video] + wfo[fo$video] + ufo[fo$participant]
  fo$followed <- rbinom(nrow(fo), 1, plogis(eta))
  for (f in c("participant", "video", "level", "model", "agent_gender", "transcript_gender", "task")) fo[[f]] <- factor(fo[[f]])
  g <- suppressMessages(suppressWarnings(glmer(followed ~ level + model + agent_gender * transcript_gender + task + (1 | participant) + (1 | video), data = fo,
                                               family = binomial, control = glmerControl(optimizer = "bobyqa", calc.derivs = FALSE))))
  jt <- as.data.frame(joint_tests(g)); p_fol <- setNames(jt$p.value, jt$`model term`)[c("level", "model", "agent_gender", "transcript_gender", "agent_gender:transcript_gender", "task")]
  c(p_lmm, p_pair, p_fol, n_pairs = nrow(wide), n_per_video = nrow(ab) / length(unique(ab$video)))
}

NAMES <- c(paste0("score_", TERMS), "pairs_same_transcript", paste0("followed_", c("level", "model", "agent_gender", "transcript_gender", "agent_gender:transcript_gender", "task")))
SCEN <- data.frame(scenario = c("none", "small", "medium", "medium+"), d = c(0, .2, .35, .5), b = c(0, .2, .4, .6))
res <- do.call(rbind, lapply(N_GRID, function(n) do.call(rbind, lapply(seq_len(nrow(SCEN)), function(i) {
  ps <- do.call(rbind, mclapply(seq_len(N_SIM), function(k) tryCatch(sim_one(n, SCEN$d[i], SCEN$b[i]), error = function(e) rep(NA, length(NAMES) + 2)),
                                mc.cores = max(1, detectCores())))
  pw <- colMeans(ps[, seq_along(NAMES), drop = FALSE] < .05, na.rm = TRUE); names(pw) <- NAMES
  r <- data.frame(N_per_study = n, N_total = 2 * n, SCEN[i, ], t(round(pw, 3)), transcripts_paired = round(mean(ps[, length(NAMES) + 1], na.rm = TRUE)),
                  ratings_per_video = round(mean(ps[, length(NAMES) + 2], na.rm = TRUE), 1), n_ok = sum(complete.cases(ps)), check.names = FALSE)
  print(r, row.names = FALSE); flush.console(); r
}))))
write.csv(res, OUT, row.names = FALSE)
cat("Results written to", OUT, "\n")
