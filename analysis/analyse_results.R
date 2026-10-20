# Analysis of the study results, from the CSV files exported by the admin page ("Exporter CSV").
#
# Usage:   Rscript analysis/analyse_results.R exportS1.csv [exportS2.csv ...] [output_folder]
#          Several exports (one per study) are combined; each file's name (without "export" and ".csv") becomes its "study" label.
# Output:  output_folder (default: analysis/results/) gets
#            report.txt              all tests and tables, in reading order
#            data_trials.csv         clean data, one row per participant x video (scores computed)
#            data_participants.csv   one row per participant (questionnaire scores, exclusion flags)
#            data_videos.csv         one row per video: its ability rating and its benevolence rating side by side
#            data_transcripts_agent_gender.csv  same transcript, female vs male agent
#            *.png                   figures
# Packages: lme4, lmerTest, emmeans, afex, ggplot2, ordinal
#
# Design (from the exports): each row is one video seen by one participant.
#   Each video is rated on ABILITY items or on BENEVOLENCE items (which one depends on the study and the task: in S1 ability
#   is rated after QA videos and benevolence after Nav videos, in S2 the reverse). The script finds this from the data:
#   whichever items are filled in on a row are the ones used for that row, so question numbers do not matter.
#   Every video: advice trust ("I feel confident in the virtual character's advice"), self-confidence, a choice (2 options).
#   Video factors: level (Low/Medium/High), dimension manipulated (Ability/Benevolence), model (GPT/Gemma/Mistral),
#   agent gender (Femme/Homme), transcript gender (Femme/Homme/Neutre), task (QA/Nav), plus study (S1/S2...).
#   Main analysis: linear mixed models with crossed random intercepts for participants and videos.

suppressMessages({ library(lme4); library(lmerTest); library(emmeans); library(afex); library(ggplot2); library(ordinal) })
# The exports are UTF-8 (curly quotes, "…"): make sure R handles them even if the session locale is not UTF-8
if (!isTRUE(l10n_info()$`UTF-8`)) for (loc in c("C.UTF-8", "en_US.UTF-8", "fr_FR.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
options(width = 160, contrasts = c("contr.sum", "contr.poly"))
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 1e5, pbkrtest.limit = 1e5)

# =====================================================================================================
# SETTINGS: check these before running
# =====================================================================================================
# Working folder (exports are read from it, results written to analysis/results/ inside it). Skipped if it does not exist.
WORKDIR <- "C:/Users/lucie/Documents/TrustCalibration/Etude TAFFC analyse"
if (dir.exists(WORKDIR)) setwd(WORKDIR)
# Running from RStudio ("Source")? Put the files here: input exports first, output folder last (optional).
# Leave it empty to give the files on the command line instead (Rscript analysis/analyse_results.R exportS1.csv exportS2.csv).
RUN_ARGS <- c()   # e.g. c("exportS1.csv", "exportS2.csv")
args <- if (length(RUN_ARGS)) RUN_ARGS else commandArgs(trailingOnly = TRUE)
FILES <- args[grepl("\\.csv$", args, ignore.case = TRUE)]
if (!length(FILES)) FILES <- "export.csv"
OUT <- c(args[!grepl("\\.csv$", args, ignore.case = TRUE)], file.path("analysis", "results"))[1]
study_label <- function(f) {   # "exportS1.csv", "fake_exportS1.csv" -> "S1"; otherwise the file name
  s <- sub("\\.csv$", "", basename(f), ignore.case = TRUE); m <- regmatches(s, regexpr("S[0-9]+$", s))
  if (length(m)) m else sub("^export[_ -]*", "", s, ignore.case = TRUE) }

# Exclusions (participant level)
EXCLUDE_NOT_COMPLETED  <- TRUE   # keep only completed sessions
EXCLUDE_REJECTED       <- TRUE   # sessions marked "rejected" in the admin
EXCLUDE_FAILED_TRAPS   <- TRUE   # failed at least one attention-check video
EXCLUDE_FAILED_INSTRUCTED <- TRUE # wrong answer to an instructed item ("Select 5", "Select 3", "Select 7")
MAX_COMP_ATTEMPTS      <- Inf    # e.g. 3 to exclude people who needed more than 3 tries at the instructions check
SELF_ATTENTION_MIN     <- 2      # self-reported attention: 1 = inattentive, so answers below 2 are excluded (NA = no exclusion)
EXCLUDE_REPEAT_PID     <- TRUE   # same Prolific ID in several sessions/studies: keep only the first one (file order, then date)

# Stereotype questionnaire: the two blocks of 16 adjectives. Which group did each block ask about?
STEREO_TARGET_1 <- "men"     # first block
STEREO_TARGET_2 <- "women"   # second block

# Choice items ("I would answer" / "I would take the"): value 1 or 2 = first or second option shown.
# Behavioural trust = following the agent's recommendation:
#   QA : the agent recommends answer A when the video's question number is even, B when it is odd
#   Nav: the agent recommends the Left path when the question number is 0 or 2, Right otherwise
CHOICE_1 <- c(QA = "A", Nav = "Left")   # what option 1 means for each task (first option shown to participants)
recommended <- function(task, qn) ifelse(task == "QA", ifelse(qn %% 2 == 0, "A", "B"), ifelse(qn %in% c(0, 2), "Left", "Right"))
chosen <- function(task, choice) ifelse(choice == 1, CHOICE_1[task], ifelse(task == "QA", ifelse(CHOICE_1["QA"] == "A", "B", "A"), ifelse(CHOICE_1["Nav"] == "Left", "Right", "Left")))

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
REPORT <- file(file.path(OUT, "report.txt"), open = "wt")
say <- function(...) { txt <- paste0(...); cat(txt, "\n"); cat(txt, "\n", file = REPORT) }
show <- function(x) { out <- capture.output(print(x)); cat(out, sep = "\n"); cat(out, sep = "\n", file = REPORT); cat("\n", file = REPORT) }
title <- function(x) say("\n", strrep("=", 100), "\n", x, "\n", strrep("=", 100))
num <- function(x) suppressWarnings(as.numeric(x))
lgl <- function(x) tolower(as.character(x)) %in% c("true", "t", "1")
rmean <- function(lst) rowMeans(do.call(cbind, lst), na.rm = TRUE)   # row means of a list of equal-length vectors (safe with 1 row)

# =====================================================================================================
# 1. READ EACH EXPORT: participant table P and video table D
# =====================================================================================================
AGENTIC  <- c("Competent", "Effective", "Productive", "Task-Oriented", "Dominant", "Bold", "Assertive", "Competitive")
COMMUNAL <- c("Understanding", "Kind", "Compassionate", "Sympathetic", "Likeable", "Communicative", "Collaborative", "Relationship-oriented")
BFI <- list(extraversion = c("is talkative", "R:is reserved", "is outgoing"), agreeableness = c("R:is sometimes rude", "has a forgiving", "is considerate"),
            conscientiousness = c("does a thorough", "R:tends to be lazy", "does things efficiently"), neuroticism = c("worries a lot", "gets nervous", "R:is relaxed"),
            openness = c("is original", "values artistic", "active imagination"))

read_export <- function(file, study) {
  # Read as UTF-8 whatever the computer's locale, and drop the byte-order mark
  lines <- readLines(file, encoding = "UTF-8", warn = FALSE); lines[1] <- sub("^﻿", "", lines[1])
  raw <- read.csv(text = lines, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), encoding = "UTF-8", colClasses = "character")
  nm <- names(raw)
  col <- function(pattern, all = FALSE) { i <- grep(pattern, nm, perl = TRUE, ignore.case = TRUE); if (all) nm[i] else if (length(i)) nm[i[1]] else NA_character_ }
  # Value of the first non-empty column among those matching the pattern (each row fills only the items of its own task)
  pick <- function(pattern, rows = raw) { cs <- col(pattern, all = TRUE); if (!length(cs)) stop(file, ": no column matches ", pattern)
    v <- rep(NA_real_, nrow(rows)); for (c0 in cs) v <- ifelse(is.na(v), num(rows[[c0]]), v); v }

  # ---- Participants
  fr <- raw[!duplicated(raw$session_id), ]
  P <- data.frame(study = study, participant = paste0(study, "_", fr$participant), session_id = fr$session_id, prolific_pid = fr$prolific_pid,
                  completed = lgl(fr$completed), rejected = lgl(fr$rejected), abandoned = lgl(fr$abandoned),
                  comp_attempts = num(fr$comp_attempts), comp2_attempts = num(fr$comp2_attempts), part_order = fr$part_order, stringsAsFactors = FALSE)
  P$n_traps <- as.vector(tapply(lgl(raw$attention), raw$session_id, sum)[P$session_id])
  P$failed  <- as.vector(tapply(lgl(raw$attention) & !lgl(raw$attention_passed), raw$session_id, sum)[P$session_id])
  instr <- col("\\| Select [0-9]+$", all = TRUE)                                   # "Select 5" must be 5, etc.
  P$instructed_failed <- if (length(instr)) rowSums(do.call(cbind, lapply(instr, function(c) num(fr[[c]]) != as.numeric(sub(".*Select ", "", c)))), na.rm = TRUE) else 0
  stereo <- col("^Sexism \\| (?!Select)", all = TRUE)                              # 2 blocks x 16 adjectives (8 agentic, 8 communal)
  if (length(stereo) >= 32) {
    adj <- function(cols, words) cols[sub("^Sexism \\| (.*?)( \\(q[0-9]+\\))?$", "\\1", cols, perl = TRUE) %in% words]
    m <- function(cols) rmean(lapply(cols, function(c) num(fr[[c]])))
    b1 <- stereo[1:16]; b2 <- stereo[17:32]
    P[[paste0("agency_", STEREO_TARGET_1)]] <- m(adj(b1, AGENTIC)); P[[paste0("communion_", STEREO_TARGET_1)]] <- m(adj(b1, COMMUNAL))
    P[[paste0("agency_", STEREO_TARGET_2)]] <- m(adj(b2, AGENTIC)); P[[paste0("communion_", STEREO_TARGET_2)]] <- m(adj(b2, COMMUNAL))
    # Traditional gender stereotype index: men seen as more agentic and women as more communal (higher = more stereotypical)
    if (setequal(c(STEREO_TARGET_1, STEREO_TARGET_2), c("women", "men")))
      P$stereotype_index <- ((P$agency_men - P$agency_women) + (P$communion_women - P$communion_men)) / 2
  }
  atai <- col("^Attitude toward AI \\|", all = TRUE)                               # 6 items, all positively worded
  if (length(atai)) P$attitude_ai <- rmean(lapply(atai, function(c) num(fr[[c]])))
  for (tr in names(BFI)) P[[paste0("bfi_", tr)]] <- rmean(lapply(BFI[[tr]], function(it) {   # BFI-S, 1-7, R = reversed
    c0 <- col(paste0("^BFI-S \\|.*", sub("^R:", "", it))); if (is.na(c0)) return(rep(NA, nrow(P))); x <- num(fr[[c0]]); if (startsWith(it, "R:")) 8 - x else x }))
  for (c0 in col("^Demographique \\|", all = TRUE)) P[[paste0("demo_", substr(gsub("[^a-z0-9]+", "_", tolower(sub("^Demographique \\| ", "", c0))), 1, 40))]] <- fr[[c0]]
  c_att <- col("how carefully did you pay attention")
  P$self_attention <- if (!is.na(c_att)) num(fr[[c_att]]) else NA

  # ---- Videos (attention checks removed)
  r <- raw[!lgl(raw$attention), ]
  D <- data.frame(
    # Videos are identified by their link: several videos share the same name in the video list
    study = study, participant = paste0(study, "_", r$participant), session_id = r$session_id,
    video = ifelse(is.na(r$yt) | r$yt == "", r$video_name, sub("/download/?$", "", r$yt)), video_name = r$video_name, pos = num(r$pos), part = r$part,
    task = r$task, dimension = r$dimension, level = r$ability, model = r$model, agent_gender = r$gender, transcript_gender = r$transcript,
    question_no = r$question_no, transcript_no = r$transcript_no,
    abil_1 = pick("confident about the virtual character's knowledge", r), abil_2 = pick("very capable of indicating", r), ability_3pt = pick("assess the character.s ability", r),
    bene_1 = pick("go out of its way to help", r), bene_2 = pick("needs and desires are very important", r), benevolence_3pt = pick("evaluate the character.s benevolence", r),
    trust = pick("confident in the virtual character's advice", r),
    self_confidence = pick("^\\s*I (am confident in my answers|feel confident about my answers)", r),
    choice = ifelse(r$task == "QA", pick("^I would answer", r), pick("^I would take the", r)),
    stringsAsFactors = FALSE)
  list(P = P, D = D)
}

title("1. FILES")
parts <- lapply(FILES, function(f) { s <- study_label(f); say(sprintf("%-6s <- %s", s, f)); read_export(f, s) })
P <- do.call(rbind, lapply(parts, function(x) x$P[, Reduce(intersect, lapply(parts, function(y) names(y$P)))]))
D <- do.call(rbind, lapply(parts, `[[`, "D"))
STUDIES <- unique(P$study)

# =====================================================================================================
# 2. EXCLUSIONS
# =====================================================================================================
P$excl_not_completed  <- EXCLUDE_NOT_COMPLETED & !P$completed
P$excl_rejected       <- EXCLUDE_REJECTED & (P$rejected | P$abandoned)
P$excl_traps          <- EXCLUDE_FAILED_TRAPS & P$failed > 0
P$excl_instructed     <- EXCLUDE_FAILED_INSTRUCTED & P$instructed_failed > 0
P$excl_comp           <- !is.na(P$comp_attempts) & P$comp_attempts > MAX_COMP_ATTEMPTS
P$excl_self_attention <- !is.na(SELF_ATTENTION_MIN) & !is.na(P$self_attention) & P$self_attention < SELF_ATTENTION_MIN
pid <- ifelse(is.na(P$prolific_pid) | P$prolific_pid == "", NA, P$prolific_pid)
P$excl_repeat_pid     <- EXCLUDE_REPEAT_PID & !is.na(pid) & duplicated(pid)
P$excluded <- with(P, excl_not_completed | excl_rejected | excl_traps | excl_instructed | excl_comp | excl_self_attention | excl_repeat_pid)

title("2. PARTICIPANTS AND EXCLUSIONS")
ex <- sapply(split(P, P$study)[STUDIES], function(p) c(sessions = nrow(p), sapply(p[grep("^excl_", names(p))], sum), kept = sum(!p$excluded)))
show(cbind(ex, total = rowSums(ex)))
if (!any(!P$excluded)) { say("No participant left after exclusions: nothing to analyse. Check the exclusion settings at the top of the script."); close(REPORT); quit(save = "no") }

# =====================================================================================================
# 3. SCORES PER VIDEO
# =====================================================================================================
D <- D[D$session_id %in% P$session_id[!P$excluded], ]
fac <- function(x, lv = NULL) factor(x, levels = if (is.null(lv)) sort(unique(x)) else lv)
for (f0 in c("study", "participant", "video", "task", "dimension", "model", "agent_gender", "transcript_gender", "part")) D[[f0]] <- fac(D[[f0]])
D$level <- fac(D$level, c("Low", "Medium", "High")); D$study <- fac(as.character(D$study), STUDIES)
# Scores: mean of the two 7-point items (1-7); the 3-point item is analysed separately
D$ability_score     <- (D$abil_1 + D$abil_2) / 2
D$benevolence_score <- (D$bene_1 + D$bene_2) / 2
# Trustworthiness = whichever dimension was rated on this row (ability or benevolence)
D$measured <- factor(ifelse(!is.na(D$ability_score), "Ability", ifelse(!is.na(D$benevolence_score), "Benevolence", NA)))
D$trustworthiness     <- ifelse(D$measured == "Ability", D$ability_score, D$benevolence_score)     # 1-7
D$trustworthiness_3pt <- ifelse(D$measured == "Ability", D$ability_3pt, D$benevolence_3pt)         # 1-3

# Behavioural trust: did the participant follow the agent's recommendation? (1 = yes)
D$recommended <- recommended(as.character(D$task), num(D$question_no))
D$chosen      <- chosen(as.character(D$task), D$choice)
D$followed    <- as.integer(D$chosen == D$recommended)

# Behavioural trust index per participant: proportion of recommendations followed (overall and per task)
ti <- aggregate(followed ~ session_id, D, mean); P$trust_index <- ti$followed[match(P$session_id, ti$session_id)]
for (tk in levels(D$task)) { t0 <- aggregate(followed ~ session_id, D[D$task == tk, ], mean); P[[paste0("trust_index_", tk)]] <- t0$followed[match(P$session_id, t0$session_id)] }
write.csv(P, file.path(OUT, "data_participants.csv"), row.names = FALSE)

# Participant moderators merged into the video data (centred)
mods <- intersect(c("stereotype_index", "attitude_ai"), names(P))
for (m0 in mods) D[[m0]] <- as.numeric(scale(P[[m0]][match(D$session_id, P$session_id)], scale = FALSE))
write.csv(D, file.path(OUT, "data_trials.csv"), row.names = FALSE)

title("3. DATA")
say("Rows (participant x video, attention checks removed): ", nrow(D), " | participants: ", nlevels(droplevels(D$participant)), " | videos: ", nlevels(droplevels(D$video)))
say("Which dimension was rated after which task (found from the data):"); show(table(study = D$study, task = D$task, rated = D$measured, useNA = "ifany"))
show(table(study = D$study, level = D$level))
say("Videos per participant: ", paste(range(table(droplevels(D$participant))), collapse = " to "))

# Reliability of the two-item scores: inter-item correlation and Spearman-Brown coefficient
rel <- function(x, y) { ok <- complete.cases(x, y); if (sum(ok) < 3) return("not enough data"); r <- cor(x[ok], y[ok]); sprintf("r = %.2f, Spearman-Brown = %.2f", r, 2 * r / (1 + r)) }
say("Ability score (2 items): ", rel(D$abil_1, D$abil_2), " | benevolence score (2 items): ", rel(D$bene_1, D$bene_2))
cr <- function(a, b) { ok <- complete.cases(a, b); if (sum(ok) < 3) NA else round(cor(a[ok], b[ok]), 2) }
say("Correlation of the 3-point item with the 2-item score: ability ", cr(D$ability_3pt, D$ability_score), " | benevolence ", cr(D$benevolence_3pt, D$benevolence_score))
say("Recommendations followed (behavioural trust): ", round(mean(D$followed, na.rm = TRUE), 3), " overall | ",
    paste(sprintf("%s %.3f", levels(D$task), tapply(D$followed, D$task, mean, na.rm = TRUE)), collapse = " | "))
qs <- P[!P$excluded, ]
if (nrow(qs) > 1) {
  title("3b. PARTICIPANT QUESTIONNAIRES (kept participants)")
  show(round(t(sapply(qs[, grep("^(agency|communion|stereotype|attitude|bfi|self_attention|trust_index)", names(qs))], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))), 2))
  # Does the behavioural trust index agree with reported trust in the advice?
  rt <- aggregate(trust ~ session_id, D, mean); qs$advice_trust <- rt$trust[match(qs$session_id, rt$session_id)]
  if (sum(complete.cases(qs$trust_index, qs$advice_trust)) > 3) { say("Behavioural trust index vs mean reported advice trust:"); show(cor.test(qs$trust_index, qs$advice_trust)) }
}

# =====================================================================================================
# 4. DESCRIPTIVES AND FIGURES
# =====================================================================================================
title("4. DESCRIPTIVE STATISTICS (means of participant means)")
pm <- function(dv, by, data = D) {
  d <- data[!is.na(data[[dv]]), ]
  a <- aggregate(d[[dv]], c(d[by], list(participant = d$participant)), mean)       # one mean per participant and cell
  s <- aggregate(a$x, a[by], function(x) c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x)))
  cbind(s[by], round(as.data.frame(s$x), 3))
}
DVS <- c(ability_score = "Perceived ability, 2-item score (1-7)", benevolence_score = "Perceived benevolence, 2-item score (1-7)",
         ability_3pt = "Perceived ability, 3-point item (1-3)", benevolence_3pt = "Perceived benevolence, 3-point item (1-3)",
         followed = "Recommendation followed (proportion)",
         trust = "Trust in the agent's advice (1-7)", self_confidence = "Confidence in own answer (1-7)")
for (dv in names(DVS)) {
  say("\n--- ", DVS[[dv]])
  for (by in list("level", "model", c("study", "level"), c("task", "level"), c("dimension", "level"), c("agent_gender", "transcript_gender"))) show(pm(dv, by))
}

save_plot <- function(p, name, w = 8, h = 5) suppressMessages(ggsave(file.path(OUT, name), p, width = w, height = h, dpi = 150))
for (dv in names(DVS)) {
  p1 <- pm(dv, c("task", "dimension", "level", "model"))
  if (nrow(p1)) save_plot(ggplot(p1, aes(level, mean, colour = model, group = model)) + geom_point(position = position_dodge(.3)) +
      geom_line(position = position_dodge(.3)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .15, position = position_dodge(.3)) +
      facet_grid(task ~ dimension, labeller = labeller(dimension = function(x) paste(x, "manipulated"))) + labs(x = "Level", y = DVS[[dv]], colour = "Model") + theme_minimal(), paste0("fig_", dv, "_level_model.png"), 9, 6)
  p2 <- pm(dv, c("agent_gender", "transcript_gender"))
  if (nrow(p2)) save_plot(ggplot(p2, aes(transcript_gender, mean, colour = agent_gender, group = agent_gender)) + geom_point(position = position_dodge(.3)) +
      geom_line(position = position_dodge(.3)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .15, position = position_dodge(.3)) +
      labs(x = "Transcript gender", y = DVS[[dv]], colour = "Agent gender") + theme_minimal(), paste0("fig_", dv, "_gender.png"), 6, 4)
}

# =====================================================================================================
# 5. MIXED MODELS (main analysis)
# =====================================================================================================
# dv ~ level * dimension + level * model + agent_gender * transcript_gender + task + (1 | participant) + (1 | video)
# - type III F tests with Satterthwaite df (lmerTest), sum-to-zero contrasts
# - random intercepts for participants AND videos (each video is a sampled stimulus); add random slopes if the design allows
# - pairwise comparisons (Tukey) with emmeans for the main factors
# Drops terms that add no information given the previous ones (e.g. task and study when, for one dimension, each study
# used a single task): they cannot be estimated separately, so the report says which term was dropped.
drop_aliased <- function(terms, d) {
  keep <- character(0); rk <- 1
  for (t0 in terms) { r1 <- qr(model.matrix(as.formula(paste("~", paste(c(keep, t0), collapse = " + "))), d))$rank
    if (r1 > rk) { keep <- c(keep, t0); rk <- r1 } else say("Term dropped (confounded with the terms before it in this subset): ", t0) }
  keep
}
fit_lmm <- function(dv, data, fixed, label) {
  title(paste0("5. MIXED MODEL: ", label))
  d <- data[!is.na(data[[dv]]), ]
  d <- droplevels(d)
  varies <- function(x) if (is.numeric(d[[x]])) isTRUE(var(d[[x]], na.rm = TRUE) > 0) else nlevels(d[[x]]) > 1
  terms <- drop_aliased(fixed[vapply(fixed, function(v) all(vapply(strsplit(v, ":")[[1]], varies, TRUE)), TRUE)], d)
  if (!length(terms) || nlevels(d$participant) < 3) { say("Not enough data for this model (need >= 3 participants and variation in the factors)."); return(invisible(NULL)) }
  f <- as.formula(paste(dv, "~", paste(terms, collapse = " + "), "+ (1 | participant) + (1 | video)"))
  say("Formula: ", deparse(f, width.cutoff = 500))
  m <- tryCatch(lmer(f, data = d), error = function(e) { say("Model failed: ", conditionMessage(e)); NULL })
  if (is.null(m)) return(invisible(NULL))
  if (isSingular(m)) say("Note: singular fit (a random-effect variance is estimated at 0); results are still interpretable but check the random effects.")
  say("N observations = ", nobs(m), " | participants = ", nlevels(d$participant), " | videos = ", nlevels(d$video))
  show(anova(m, type = 3))
  say("Random effects:"); show(as.data.frame(VarCorr(m))[, c("grp", "vcov", "sdcor")])
  for (f0 in intersect(c("level", "model", "dimension", "agent_gender", "transcript_gender", "task", "study"), terms)) {
    e <- emmeans(m, as.formula(paste("~", f0)))
    say("Estimated marginal means: ", f0); show(summary(e))
    if (nlevels(d[[f0]]) > 2) show(pairs(e, adjust = "tukey"))
  }
  for (int in c("level:model", "agent_gender:transcript_gender", "level:dimension")) if (int %in% terms) {
    a <- strsplit(int, ":")[[1]]
    say("Simple effects of ", a[1], " within each ", a[2], ":"); show(pairs(emmeans(m, as.formula(paste("~", a[1], "|", a[2]))), adjust = "tukey"))
  }
  invisible(m)
}
FIXED_BOTH <- c("level", "dimension", "model", "agent_gender", "transcript_gender", "task", "study", "task:study", "level:dimension", "level:model", "agent_gender:transcript_gender")
FIXED_ONE  <- FIXED_BOTH   # terms that do not vary in a subset (e.g. task within one task) are dropped automatically
m_tw    <- fit_lmm("trustworthiness", D, FIXED_BOTH, "perceived trustworthiness, 2-item score (1-7), ability and benevolence ratings together")
m_abil  <- fit_lmm("ability_score", D, FIXED_ONE, "perceived ability, 2-item score (1-7), videos rated on ability")
m_bene  <- fit_lmm("benevolence_score", D, FIXED_ONE, "perceived benevolence, 2-item score (1-7), videos rated on benevolence")
m_trust <- fit_lmm("trust", D, FIXED_BOTH, "trust in the agent's advice (1-7)")
m_conf  <- fit_lmm("self_confidence", D, FIXED_BOTH, "confidence in own answer (1-7)")

# Exploratory moderation by participants' gender stereotypes and attitude toward AI
if (length(mods)) for (dv in c("ability_score", "benevolence_score", "trust")) {
  fit_lmm(dv, D, c(FIXED_BOTH, mods, if ("stereotype_index" %in% mods) c("agent_gender:stereotype_index", "agent_gender:transcript_gender:stereotype_index")),
          paste0(dv, " - exploratory moderation by ", paste(mods, collapse = " and ")))
}

# Models for the 3-point items and for following the agent: same fixed effects, tests with emmeans::joint_tests (Wald, type III)
fit_other <- function(dv, data, fixed, label, kind) {
  title(paste0("6. ", label))
  d <- droplevels(data[!is.na(data[[dv]]), ])
  varies <- function(x) if (is.numeric(d[[x]])) isTRUE(var(d[[x]], na.rm = TRUE) > 0) else nlevels(d[[x]]) > 1
  terms <- drop_aliased(fixed[vapply(fixed, function(v) all(vapply(strsplit(v, ":")[[1]], varies, TRUE)), TRUE)], d)
  if (!length(terms) || nlevels(d$participant) < 3) { say("Not enough data for this model (need >= 3 participants and variation in the factors)."); return(invisible(NULL)) }
  if (kind == "ordinal") d[[dv]] <- factor(d[[dv]], ordered = TRUE)
  f <- as.formula(paste(dv, "~", paste(terms, collapse = " + "), "+ (1 | participant) + (1 | video)"))
  say("Formula: ", deparse(f, width.cutoff = 500))
  m <- tryCatch(if (kind == "ordinal") clmm(f, data = d) else glmer(f, data = d, family = binomial, control = glmerControl(optimizer = "bobyqa")),
                error = function(e) { say("Model failed: ", conditionMessage(e)); NULL })
  if (is.null(m)) return(invisible(NULL))
  say("N observations = ", nrow(d), " | participants = ", nlevels(d$participant), " | videos = ", nlevels(d$video))
  show(joint_tests(m))
  for (f0 in intersect(c("level", "model", "dimension", "agent_gender", "transcript_gender", "task", "study"), terms)) {
    e <- emmeans(m, as.formula(paste("~", f0)), type = "response", mode = if (kind == "ordinal") "mean.class" else NULL)
    say(if (kind == "ordinal") "Estimated mean response (1-3): " else "Estimated probability of following: ", f0); show(summary(e))
    if (nlevels(d[[f0]]) > 2) show(pairs(e, adjust = "tukey"))
  }
  invisible(m)
}
fit_other("trustworthiness_3pt", D, FIXED_BOTH, "3-POINT ITEM (ability or benevolence, as rated): ordinal mixed model", "ordinal")
fit_other("ability_3pt", D, FIXED_ONE, "3-POINT ABILITY ITEM: ordinal mixed model", "ordinal")
fit_other("benevolence_3pt", D, FIXED_ONE, "3-POINT BENEVOLENCE ITEM: ordinal mixed model", "ordinal")
fit_other("followed", D, FIXED_BOTH, "FOLLOWING THE AGENT'S RECOMMENDATION (behavioural trust): logistic mixed model", "binomial")
for (tk in levels(D$task)) fit_other("followed", D[D$task == tk, ], FIXED_ONE, paste0("FOLLOWING THE AGENT'S RECOMMENDATION, ", tk, " videos: logistic mixed model"), "binomial")

# =====================================================================================================
# 7. COMPLEMENTARY REPEATED-MEASURES ANOVA (by-participant means)
# =====================================================================================================
# Ignores video-to-video variability, so its p-values are too liberal with this design: report it only as a complement.
title("7. COMPLEMENTARY RM-ANOVA (participant means; liberal, see comment in script)")
SPECS <- list(list(dv = "ability_score", w = c("level", "model")), list(dv = "ability_score", w = c("agent_gender", "transcript_gender")),
              list(dv = "benevolence_score", w = c("level", "model")), list(dv = "benevolence_score", w = c("agent_gender", "transcript_gender")))
for (spec in SPECS) {
  d <- droplevels(D[!is.na(D[[spec$dv]]), ])
  tab <- table(d$participant, interaction(d[spec$w], drop = TRUE))
  ok <- rownames(tab)[apply(tab > 0, 1, all)]                     # only participants who saw every cell
  if (length(ok) < 3) { say(spec$dv, " ~ ", paste(spec$w, collapse = " x "), ": skipped (fewer than 3 participants saw every cell)"); next }
  d <- droplevels(d[d$participant %in% ok, ])
  say(spec$dv, " ~ ", paste(spec$w, collapse = " x "), " (", length(ok), " of ", nrow(tab), " participants saw every cell)")
  a <- suppressMessages(aov_ez("participant", spec$dv, d, within = spec$w, fun_aggregate = mean, anova_table = list(correction = "GG", es = "pes")))
  show(a)
}


# =====================================================================================================
# 8. ABILITY AND BENEVOLENCE RATINGS OF EACH VIDEO
# =====================================================================================================
# Each video is rated on ability in one study and on benevolence in the other (S1: ability after QA, benevolence after Nav;
# S2: the reverse), always with the two 7-point items. Here the two ratings are put side by side, one row per video.
title("8. ABILITY AND BENEVOLENCE RATINGS OF EACH VIDEO (2-item 7-point scores)")
vkey <- c("video", "video_name", "task", "dimension", "level", "model", "agent_gender", "transcript_gender", "transcript_no")
# Grouping columns as text, with empty values kept as "" (aggregate() would otherwise drop every row where one of them is missing)
VK <- as.data.frame(lapply(D[vkey], function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }), stringsAsFactors = FALSE)
vmean <- function(dv) { ok <- !is.na(D[[dv]]); if (!any(ok)) return(NULL)
  a <- aggregate(D[[dv]][ok], VK[ok, , drop = FALSE], function(x) c(n = length(x), mean = mean(x), sd = if (length(x) > 1) sd(x) else NA))
  if (!nrow(a)) return(NULL)
  cbind(a[vkey], setNames(as.data.frame(matrix(a$x, nrow = nrow(a))), paste0(dv, c("_n", "_mean", "_sd")))) }
V <- Reduce(function(a, b) if (is.null(b)) a else merge(a, b, all = TRUE), lapply(c("ability_score", "benevolence_score", "ability_3pt", "benevolence_3pt", "trust", "followed"), vmean))
V$level <- factor(V$level, levels = intersect(c("Low", "Medium", "High", ""), unique(V$level)))
for (s0 in STUDIES) { st <- unique(D[D$study == s0, c("video", "measured")]); V[[paste0("rated_in_", s0)]] <- st$measured[match(V$video, st$video)] }
write.csv(V, file.path(OUT, "data_videos.csv"), row.names = FALSE)
both <- complete.cases(V$ability_score_mean, V$benevolence_score_mean)
say("Videos: ", nrow(V), " | rated on ability: ", sum(!is.na(V$ability_score_mean)), " | on benevolence: ", sum(!is.na(V$benevolence_score_mean)), " | on both: ", sum(both))
say("Ratings per video: ability ", paste(range(V$ability_score_n, na.rm = TRUE), collapse = "-"), " | benevolence ", paste(range(V$benevolence_score_n, na.rm = TRUE), collapse = "-"))
if (sum(both) > 3) {
  say("Correlation between a video's ability and benevolence ratings (videos rated on both):"); show(cor.test(V$ability_score_mean[both], V$benevolence_score_mean[both]))
  save_plot(ggplot(V[both, ], aes(ability_score_mean, benevolence_score_mean, colour = level, shape = dimension)) + geom_point(alpha = .8) +
    geom_abline(linetype = 2, colour = "grey60") + facet_wrap(~ task) + coord_equal(xlim = c(1, 7), ylim = c(1, 7)) +
    labs(x = "Mean ability rating (1-7)", y = "Mean benevolence rating (1-7)", colour = "Level", shape = "Dimension manipulated") + theme_minimal(),
    "fig_videos_ability_vs_benevolence.png", 9, 5)
}
say("\nMean ability and benevolence ratings by manipulated dimension and level (means of video means):")
show(aggregate(cbind(ability = ability_score_mean, benevolence = benevolence_score_mean) ~ dimension + level, V, function(x) round(mean(x, na.rm = TRUE), 2), na.action = na.pass))

# Manipulation check: the level of a video should move the rating of the dimension it manipulates.
# One model with both ratings: rating ~ rated dimension x manipulated dimension x level (+ other factors).
title("8b. MANIPULATION CHECK: rated dimension x manipulated dimension x level (mixed model)")
DL <- D[!is.na(D$trustworthiness), ]; DL$rated <- DL$measured
mc <- tryCatch(lmer(trustworthiness ~ rated * dimension * level + model + agent_gender * transcript_gender + task + (1 | participant) + (1 | video), data = DL),
               error = function(e) { say("Model failed: ", conditionMessage(e)); NULL })
# Figure: each rating (ability, benevolence) by level, for videos manipulating ability and for videos manipulating benevolence.
# The off-target panels show e.g. ability ratings when benevolence is manipulated.
DL$rated_dimension <- factor(paste(DL$rated, "rating"), levels = c("Ability rating", "Benevolence rating"))
DL$manipulated <- factor(paste(DL$dimension, "manipulated"))
cross <- pm("trustworthiness", c("manipulated", "level", "rated_dimension"), DL)
if (nrow(cross)) save_plot(ggplot(cross, aes(level, mean, colour = rated_dimension, group = rated_dimension)) + geom_point(position = position_dodge(.25), size = 2) +
    geom_line(position = position_dodge(.25)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .15, position = position_dodge(.25)) +
    facet_wrap(~ manipulated) + labs(x = "Level of the manipulated dimension", y = "Mean rating (2-item score, 1-7)", colour = NULL) +
    theme_minimal() + theme(legend.position = "bottom"), "fig_cross_ratings.png", 8, 4.5)
cross_t <- pm("trustworthiness", c("task", "manipulated", "level", "rated_dimension"), DL)
if (nrow(cross_t)) save_plot(ggplot(cross_t, aes(level, mean, colour = rated_dimension, group = rated_dimension)) + geom_point(position = position_dodge(.25), size = 2) +
    geom_line(position = position_dodge(.25)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .15, position = position_dodge(.25)) +
    facet_grid(task ~ manipulated) + labs(x = "Level of the manipulated dimension", y = "Mean rating (2-item score, 1-7)", colour = NULL) +
    theme_minimal() + theme(legend.position = "bottom"), "fig_cross_ratings_by_task.png", 8, 7)
say("Ratings by manipulated dimension, level and rated dimension (means of participant means; see fig_cross_ratings.png):"); show(cross)
if (!is.null(mc) && nlevels(droplevels(DL$participant)) >= 3) {
  say("N observations = ", nobs(mc)); show(anova(mc, type = 3))
  say("Effect of level within each rated x manipulated dimension:"); show(pairs(emmeans(mc, ~ level | rated * dimension), adjust = "tukey"))
} else say("Not enough data for this model.")

# =====================================================================================================
# 9. SAME TRANSCRIPT, DIFFERENT AGENT GENDER
# =====================================================================================================
# Videos with the same transcript_no say the same thing; only the agent (female / male) differs. For each rating, compare the
# two versions of each transcript: paired tests across transcripts, then a mixed model where transcript is a random effect
# (so the agent-gender effect is estimated within transcripts) with a random agent-gender slope (does the gender effect
# differ from one transcript to another?).
title("9. SAME TRANSCRIPT, FEMALE vs MALE AGENT")
GEN <- levels(D$agent_gender)
if (length(GEN) == 2 && any(!is.na(D$transcript_no) & D$transcript_no != "")) {
  pairs_out <- list()
  for (dv in c(ability = "ability_score", benevolence = "benevolence_score", advice_trust = "trust", followed = "followed")) {
    d <- D[!is.na(D[[dv]]) & !is.na(D$transcript_no) & D$transcript_no != "", ]
    if (!nrow(d)) next
    lab <- names(which(c(ability = "ability_score", benevolence = "benevolence_score", advice_trust = "trust", followed = "followed") == dv))
    say("\n--- ", lab, " (", dv, ")")
    m <- aggregate(d[[dv]], list(transcript_no = d$transcript_no, agent_gender = d$agent_gender), function(x) c(n = length(x), mean = mean(x)))
    m <- cbind(m[1:2], as.data.frame(m$x))
    w <- reshape(m, idvar = "transcript_no", timevar = "agent_gender", direction = "wide")
    g1 <- paste0("mean.", GEN[1]); g2 <- paste0("mean.", GEN[2])
    w <- w[complete.cases(w[[g1]], w[[g2]]), ]
    if (nrow(w) < 3) { say("Fewer than 3 transcripts seen with both agent genders: skipped."); next }
    info <- unique(d[, c("transcript_no", "task", "transcript_gender", "level", "dimension")]); info <- info[!duplicated(info$transcript_no), ]
    w <- merge(info, w); w$difference <- w[[g1]] - w[[g2]]; w$rating <- lab
    pairs_out[[lab]] <- w
    say(nrow(w), " transcripts seen with both a ", GEN[1], " and a ", GEN[2], " agent. Mean difference ", GEN[1], " - ", GEN[2], " = ", round(mean(w$difference), 3),
        " (SD across transcripts ", round(sd(w$difference), 3), ")")
    show(t.test(w[[g1]], w[[g2]], paired = TRUE)); show(suppressWarnings(wilcox.test(w[[g1]], w[[g2]], paired = TRUE)))
    say("Difference by transcript gender (mean, n transcripts):"); show(aggregate(difference ~ transcript_gender, w, function(x) round(c(mean = mean(x), n = length(x)), 3)))
    say("Transcripts with the largest differences:"); show(head(w[order(-abs(w$difference)), c("transcript_no", "task", "transcript_gender", "level", g1, g2, "difference")], 10))
    # Mixed model within transcripts (agent gender coded -0.5 / +0.5 for the random slope)
    d$ag <- ifelse(d$agent_gender == GEN[1], .5, -.5)
    fam <- if (dv == "followed") binomial else NULL
    fit <- function(f) tryCatch(suppressMessages(if (is.null(fam)) lmer(f, data = d, REML = FALSE) else glmer(f, data = d, family = fam, control = glmerControl(optimizer = "bobyqa"))),
                                error = function(e) NULL)
    f0 <- as.formula(paste(dv, "~ ag * transcript_gender + level + (1 | participant) + (1 | transcript_no) + (1 | video)"))
    f1 <- as.formula(paste(dv, "~ ag * transcript_gender + level + (1 | participant) + (1 + ag || transcript_no) + (1 | video)"))
    m0 <- fit(f0); m1 <- fit(f1)
    if (!is.null(m0) && nlevels(droplevels(d$participant)) >= 3) {
      say("Mixed model (agent gender within transcript; ag = ", GEN[1], " minus ", GEN[2], "): ", deparse(f0, width.cutoff = 500))
      show(round(coef(summary(m0)), 4))
      if (!is.null(m1)) { say("Does the agent-gender effect vary across transcripts? (random slope of ag by transcript, likelihood-ratio test)")
        show(anova(m0, m1)); show(as.data.frame(VarCorr(m1))[, c("grp", "var1", "sdcor")]) }
    }
  }
  if (length(pairs_out)) {
    PO <- do.call(rbind, lapply(pairs_out, function(w) { names(w) <- sub(paste0("^(n|mean)\\.(", paste(GEN, collapse = "|"), ")$"), "\\1_\\2", names(w)); w }))
    write.csv(PO, file.path(OUT, "data_transcripts_agent_gender.csv"), row.names = FALSE)
    pl <- do.call(rbind, lapply(pairs_out, function(w) rbind(data.frame(rating = w$rating, transcript_no = w$transcript_no, agent_gender = GEN[1], mean = w[[paste0("mean.", GEN[1])]], diff = w$difference),
                                                         data.frame(rating = w$rating, transcript_no = w$transcript_no, agent_gender = GEN[2], mean = w[[paste0("mean.", GEN[2])]], diff = w$difference))))
    pl$transcript_no <- reorder(factor(paste(pl$rating, pl$transcript_no)), pl$diff)
    pl <- pl[pl$rating %in% c("ability", "benevolence"), ]
    top <- unlist(lapply(split(pl, pl$rating), function(x) head(unique(as.character(x$transcript_no[order(-abs(x$diff))])), 40)))   # 40 largest gaps per rating
    pl <- droplevels(pl[as.character(pl$transcript_no) %in% top, ])
    if (nrow(pl)) save_plot(ggplot(pl, aes(mean, transcript_no, colour = agent_gender)) + geom_line(aes(group = transcript_no), colour = "grey70") + geom_point() +
      facet_wrap(~ rating, scales = "free_y") + scale_y_discrete(labels = function(x) sub("^\\S+ ", "", x)) +
      labs(x = "Mean rating (1-7)", y = "Transcript (40 largest differences, sorted)", colour = "Agent gender") + theme_minimal() + theme(axis.text.y = element_text(size = 5)),
      "fig_transcripts_agent_gender.png", 9, 8)
  }
} else say("No transcript_no in the data, or agent gender does not have exactly two levels: skipped.")

say("\nDone. Results in ", normalizePath(OUT))
close(REPORT)
