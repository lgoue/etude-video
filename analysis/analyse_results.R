# Analysis of the study results, from the CSV exported by the admin page ("Exporter CSV").
#
# Usage:   Rscript analysis/analyse_results.R path/to/export.csv [output_folder]
# Output:  output_folder (default: analysis/results/) gets
#            report.txt              all tests and tables, in reading order
#            data_trials.csv         clean data, one row per participant x video (scores computed)
#            data_participants.csv   one row per participant (questionnaire scores, exclusion flags)
#            *.png                   figures
# Packages: lme4, lmerTest, emmeans, afex, ggplot2
#
# Design (from the export): each row is one video seen by one participant.
#   Part 1 = QA videos, rated on ABILITY items; part 2 = Nav videos, rated on BENEVOLENCE items.
#   Both parts: advice trust ("I feel confident in the virtual character's advice"), self-confidence, a choice (2 options).
#   Video factors: level (Low/Medium/High), dimension manipulated (Ability/Benevolence), model (GPT/Gemma/Mistral),
#   agent gender (Femme/Homme), transcript gender (Femme/Homme/Neutre), task (QA/Nav).
#   Main analysis: linear mixed models with crossed random intercepts for participants and videos.

suppressMessages({ library(lme4); library(lmerTest); library(emmeans); library(afex); library(ggplot2) })
# The export is UTF-8 (curly quotes, "…"): make sure R handles it even if the session locale is not UTF-8
if (!isTRUE(l10n_info()$`UTF-8`)) for (loc in c("C.UTF-8", "en_US.UTF-8", "fr_FR.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
options(width = 160, contrasts = c("contr.sum", "contr.poly"))
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 1e5, pbkrtest.limit = 1e5)

# =====================================================================================================
# SETTINGS: check these before running
# =====================================================================================================
args <- commandArgs(trailingOnly = TRUE)
FILE <- if (length(args) >= 1) args[1] else "export.csv"
OUT  <- if (length(args) >= 2) args[2] else file.path("analysis", "results")

# Exclusions (participant level)
EXCLUDE_NOT_COMPLETED  <- TRUE   # keep only completed sessions
EXCLUDE_REJECTED       <- TRUE   # sessions marked "rejected" in the admin
EXCLUDE_FAILED_TRAPS   <- TRUE   # failed at least one attention-check video
EXCLUDE_FAILED_INSTRUCTED <- TRUE # wrong answer to an instructed item ("Select 5", "Select 3", "Select 7")
MAX_COMP_ATTEMPTS      <- Inf    # e.g. 3 to exclude people who needed more than 3 tries at the instructions check
SELF_ATTENTION_MIN     <- NA     # e.g. 3 to exclude self-reported attention below 3 (check the direction of that scale!)

# Item scales (number of points), used to put items on a common 0-1 scale before averaging
SCALE_DEFAULT <- 7
SCALE_3PT_PATTERN <- "How would you (assess|evaluate) the character"   # the two 3-point items

# Stereotype questionnaire: the two blocks of 16 adjectives. Which group did each block ask about?
STEREO_TARGET_1 <- "women"   # first block (q189-q204)  <- CHECK
STEREO_TARGET_2 <- "men"     # second block (q206-q221) <- CHECK

# Choice items ("I would answer" / "I would take the"): value 1 or 2 = first or second option shown.
# To analyse whether participants FOLLOWED the agent, say which answer the agent gave:
#   QA : "answer1" if the agent always recommends "answer 1" of the QA questions file, "answer2", or NA (unknown)
#   Nav: NA unless you add a column/rule giving the path recommended by the agent
QA_AGENT_RECOMMENDS <- NA

dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
REPORT <- file(file.path(OUT, "report.txt"), open = "wt")
say <- function(...) { txt <- paste0(...); cat(txt, "\n"); cat(txt, "\n", file = REPORT) }
show <- function(x) { out <- capture.output(print(x)); cat(out, sep = "\n"); cat(out, sep = "\n", file = REPORT); cat("\n", file = REPORT) }
title <- function(x) say("\n", strrep("=", 100), "\n", x, "\n", strrep("=", 100))

# =====================================================================================================
# 1. READ AND NAME COLUMNS
# =====================================================================================================
# Read as UTF-8 whatever the computer's locale (the export contains curly quotes and "…"), and drop the byte-order mark
lines <- readLines(FILE, encoding = "UTF-8", warn = FALSE); lines[1] <- sub("^\ufeff", "", lines[1])
raw <- read.csv(text = lines, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA"), encoding = "UTF-8")
nm <- names(raw)
col <- function(pattern, required = TRUE, all = FALSE) {
  i <- grep(pattern, nm, perl = TRUE, ignore.case = TRUE)
  if (!length(i)) { if (required) stop("Column not found: ", pattern); return(if (all) character(0) else NA_character_) }
  if (all) nm[i] else nm[i[1]]
}
num <- function(x) suppressWarnings(as.numeric(x))
lgl <- function(x) tolower(as.character(x)) %in% c("true", "t", "1")
rmean <- function(lst) rowMeans(do.call(cbind, lst), na.rm = TRUE)   # row means of a list of equal-length vectors (safe with 1 row)

# Per-video items (patterns match the question text; adapt them if you rename questions)
ITEMS <- list(
  qa_choice   = col("^I would answer"),
  nav_choice  = col("^I would take the"),
  abil_1      = col("confident about the virtual character's knowledge"),
  abil_2      = col("very capable of indicating"),
  abil_3      = col("assess the character.s ability"),
  qa_trust    = col("confident in the virtual character's advice.*q25"),
  qa_selfconf = col("^\\s*I am confident in my answers"),
  bene_1      = col("go out of its way to help"),
  bene_2      = col("needs and desires are very important"),
  bene_3      = col("evaluate the character.s benevolence"),
  nav_trust   = col("confident in the virtual character's advice.*q26"),
  nav_selfconf= col("^\\s*I feel confident about my answers")
)
scale_of <- function(colname) if (grepl(SCALE_3PT_PATTERN, colname, perl = TRUE)) 3 else SCALE_DEFAULT
pomp <- function(x, k) (num(x) - 1) / (k - 1)   # 1..k -> 0..1

# =====================================================================================================
# 2. PARTICIPANT-LEVEL DATA: questionnaires and exclusions
# =====================================================================================================
first_row <- raw[!duplicated(raw$session_id), ]
P <- data.frame(participant = first_row$participant, session_id = first_row$session_id, prolific_pid = first_row$prolific_pid,
                completed = lgl(first_row$completed), rejected = lgl(first_row$rejected), abandoned = lgl(first_row$abandoned),
                comp_attempts = num(first_row$comp_attempts), comp2_attempts = num(first_row$comp2_attempts),
                part_order = first_row$part_order, stringsAsFactors = FALSE)

# Attention-check videos
trap <- aggregate(cbind(n_traps = lgl(raw$attention), failed = lgl(raw$attention) & !lgl(raw$attention_passed)) ~ session_id, raw, sum)
P <- merge(P, trap, all.x = TRUE)

# Instructed items: "Select 5" must be 5, etc.
instr <- col("\\| Select [0-9]+$", required = FALSE, all = TRUE)
P$instructed_failed <- if (length(instr)) rowSums(do.call(cbind, lapply(instr, function(c) num(first_row[[c]][match(P$session_id, first_row$session_id)]) != as.numeric(sub(".*Select ", "", c)))), na.rm = TRUE) else 0

# Stereotypes: 2 blocks x 16 adjectives (8 agentic, 8 communal)
AGENTIC  <- c("Competent", "Effective", "Productive", "Task-Oriented", "Dominant", "Bold", "Assertive", "Competitive")
COMMUNAL <- c("Understanding", "Kind", "Compassionate", "Sympathetic", "Likeable", "Communicative", "Collaborative", "Relationship-oriented")
stereo_cols <- col("^Sexism \\| (?!Select)", required = FALSE, all = TRUE)
fr <- first_row[match(P$session_id, first_row$session_id), ]
if (length(stereo_cols) >= 32) {
  b1 <- stereo_cols[1:16]; b2 <- stereo_cols[17:32]
  adj <- function(cols, words) cols[sub("^Sexism \\| (.*?)( \\(q[0-9]+\\))?$", "\\1", cols, perl = TRUE) %in% words]
  m <- function(cols) rmean(lapply(cols, function(c) num(fr[[c]])))
  P[[paste0("agency_", STEREO_TARGET_1)]]   <- m(adj(b1, AGENTIC));  P[[paste0("communion_", STEREO_TARGET_1)]] <- m(adj(b1, COMMUNAL))
  P[[paste0("agency_", STEREO_TARGET_2)]]   <- m(adj(b2, AGENTIC));  P[[paste0("communion_", STEREO_TARGET_2)]] <- m(adj(b2, COMMUNAL))
  # Traditional gender stereotype index: men seen as more agentic and women as more communal (higher = more stereotypical)
  if (setequal(c(STEREO_TARGET_1, STEREO_TARGET_2), c("women", "men")))
    P$stereotype_index <- ((P$agency_men - P$agency_women) + (P$communion_women - P$communion_men)) / 2
}

# Attitude toward AI (mean of the 6 items, all positively worded)
atai <- col("^Attitude toward AI \\|", required = FALSE, all = TRUE)
if (length(atai)) P$attitude_ai <- rmean(lapply(atai, function(c) num(fr[[c]])))

# BFI-S (15 items, 1-7, reversed items marked R)
BFI <- list(extraversion = c("is talkative", "R:is reserved", "is outgoing"), agreeableness = c("R:is sometimes rude", "has a forgiving", "is considerate"),
            conscientiousness = c("does a thorough", "R:tends to be lazy", "does things efficiently"), neuroticism = c("worries a lot", "gets nervous", "R:is relaxed"),
            openness = c("is original", "values artistic", "active imagination"))
for (tr in names(BFI)) {
  v <- lapply(BFI[[tr]], function(it) { rev <- startsWith(it, "R:"); c0 <- col(paste0("^BFI-S \\|.*", sub("^R:", "", it)), required = FALSE); if (is.na(c0)) return(rep(NA, nrow(P))); x <- num(fr[[c0]]); if (rev) 8 - x else x })
  P[[paste0("bfi_", tr)]] <- rmean(v)
}

# Demographics and other single items
demo_cols <- col("^Demographique \\|", required = FALSE, all = TRUE)
short <- function(x) gsub("[^a-z0-9]+", "_", tolower(sub("^Demographique \\| ", "", x)))
for (c0 in demo_cols) P[[paste0("demo_", substr(short(c0), 1, 40))]] <- fr[[c0]]
c_att <- col("how carefully did you pay attention", required = FALSE)
P$self_attention <- if (!is.na(c_att)) num(fr[[c_att]]) else NA

# Exclusion flags
P$excl_not_completed <- EXCLUDE_NOT_COMPLETED & !P$completed
P$excl_rejected      <- EXCLUDE_REJECTED & (P$rejected | P$abandoned)
P$excl_traps         <- EXCLUDE_FAILED_TRAPS & P$failed > 0
P$excl_instructed    <- EXCLUDE_FAILED_INSTRUCTED & P$instructed_failed > 0
P$excl_comp          <- !is.na(P$comp_attempts) & P$comp_attempts > MAX_COMP_ATTEMPTS
P$excl_self_attention <- !is.na(SELF_ATTENTION_MIN) & !is.na(P$self_attention) & P$self_attention < SELF_ATTENTION_MIN
P$excluded <- with(P, excl_not_completed | excl_rejected | excl_traps | excl_instructed | excl_comp | excl_self_attention)

title("1. PARTICIPANTS AND EXCLUSIONS")
say("Sessions in the file: ", nrow(P))
for (f in grep("^excl_", names(P), value = TRUE)) say(sprintf("  %-22s %d", f, sum(P[[f]])))
say("Participants kept: ", sum(!P$excluded))
write.csv(P, file.path(OUT, "data_participants.csv"), row.names = FALSE)

# =====================================================================================================
# 3. TRIAL-LEVEL DATA
# =====================================================================================================
D <- raw[raw$session_id %in% P$session_id[!P$excluded] & !lgl(raw$attention), ]
LEVELS <- list(level = c("Low", "Medium", "High"))
D <- data.frame(
  participant = factor(D$participant), session_id = D$session_id, video = factor(D$video_name), pos = num(D$pos), part = factor(D$part),
  task = factor(D$task), dimension = factor(D$dimension), level = factor(D$ability, levels = LEVELS$level),
  model = factor(D$model), agent_gender = factor(D$gender), transcript_gender = factor(D$transcript),
  question_no = D$question_no, qa_answer_a = D$qa_answer_a, qa_answer_b = D$qa_answer_b,
  ability_items = I(sapply(ITEMS[c("abil_1", "abil_2", "abil_3")], function(c) pomp(D[[c]], scale_of(c)))),
  benevolence_items = I(sapply(ITEMS[c("bene_1", "bene_2", "bene_3")], function(c) pomp(D[[c]], scale_of(c)))),
  trust = ifelse(D$task == "QA", num(D[[ITEMS$qa_trust]]), num(D[[ITEMS$nav_trust]])),
  self_confidence = ifelse(D$task == "QA", num(D[[ITEMS$qa_selfconf]]), num(D[[ITEMS$nav_selfconf]])),
  choice = ifelse(D$task == "QA", num(D[[ITEMS$qa_choice]]), num(D[[ITEMS$nav_choice]])),
  stringsAsFactors = FALSE)
D$ability_index     <- rowMeans(D$ability_items, na.rm = FALSE)       # 0-1 (QA videos)
D$benevolence_index <- rowMeans(D$benevolence_items, na.rm = FALSE)   # 0-1 (Nav videos)
# Trustworthiness: the dimension measured for this task (ability on QA, benevolence on Nav), 0-1
D$trustworthiness <- ifelse(D$task == "QA", D$ability_index, D$benevolence_index)

# Did the participant follow the agent? (only if the recommended QA answer is known)
if (!is.na(QA_AGENT_RECOMMENDS)) {
  qn <- num(D$question_no); a1_is_A <- qn %% 2 == 0                        # answer A = answer 1 when the question number is even
  chosen_is_1 <- ifelse(D$choice == 1, a1_is_A, !a1_is_A)
  D$followed <- ifelse(D$task == "QA", as.integer(if (QA_AGENT_RECOMMENDS == "answer1") chosen_is_1 else !chosen_is_1), NA)
}

# Participant moderators merged into the trial data (centred)
mods <- intersect(c("stereotype_index", "attitude_ai"), names(P))
for (m0 in mods) D[[m0]] <- as.numeric(scale(P[[m0]][match(D$session_id, P$session_id)], scale = FALSE))

out_trials <- D; out_trials$ability_items <- NULL; out_trials$benevolence_items <- NULL
write.csv(out_trials, file.path(OUT, "data_trials.csv"), row.names = FALSE)

title("2. DATA")
say("Rows (participant x video, attention checks removed): ", nrow(D), " | participants: ", nlevels(droplevels(D$participant)), " | videos: ", nlevels(droplevels(D$video)))
show(table(task = D$task, level = D$level))
say("Videos per participant: ", paste(range(table(droplevels(D$participant))), collapse = " to "))

# Reliability of the 3-item indices (items on 0-1 scale)
alpha <- function(m) { m <- m[complete.cases(m), , drop = FALSE]; k <- ncol(m); if (nrow(m) < 3) return(NA); k / (k - 1) * (1 - sum(apply(m, 2, var)) / var(rowSums(m))) }
say(sprintf("Cronbach's alpha  ability index (QA): %.2f | benevolence index (Nav): %.2f",
            alpha(D$ability_items[D$task == "QA", ]), alpha(D$benevolence_items[D$task == "Nav", ])))
qs <- P[!P$excluded, ]
if (nrow(qs) > 1) {
  title("3. PARTICIPANT QUESTIONNAIRES (kept participants)")
  show(round(t(sapply(qs[, grep("^(agency|communion|stereotype|attitude|bfi|self_attention)", names(qs))], function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))), 2))
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
DVS <- c(trustworthiness = "Perceived trustworthiness (ability on QA, benevolence on Nav), 0-1",
         trust = "Trust in the agent's advice (1-7)", self_confidence = "Confidence in own answer (1-7)")
for (dv in names(DVS)) {
  say("\n--- ", DVS[[dv]])
  for (by in list("level", "model", c("task", "level"), c("dimension", "level"), c("agent_gender", "transcript_gender"))) show(pm(dv, by))
}

save_plot <- function(p, name, w = 8, h = 5) suppressMessages(ggsave(file.path(OUT, name), p, width = w, height = h, dpi = 150))
for (dv in names(DVS)) {
  p1 <- pm(dv, c("task", "dimension", "level", "model"))
  if (nrow(p1)) save_plot(ggplot(p1, aes(level, mean, colour = model, group = model)) + geom_point(position = position_dodge(.3)) +
      geom_line(position = position_dodge(.3)) + geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = .15, position = position_dodge(.3)) +
      facet_grid(task ~ dimension) + labs(x = "Level", y = DVS[[dv]], colour = "Model") + theme_minimal(), paste0("fig_", dv, "_level_model.png"))
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
fit_lmm <- function(dv, data, fixed, label) {
  title(paste0("5. MIXED MODEL: ", label))
  d <- data[!is.na(data[[dv]]), ]
  d <- droplevels(d)
  varies <- function(x) if (is.numeric(d[[x]])) isTRUE(var(d[[x]], na.rm = TRUE) > 0) else nlevels(d[[x]]) > 1
  keep <- function(f) f[vapply(f, function(v) all(vapply(strsplit(v, ":")[[1]], varies, TRUE)), TRUE)]
  terms <- keep(fixed)
  if (!length(terms) || nlevels(d$participant) < 3) { say("Not enough data for this model (need >= 3 participants and variation in the factors)."); return(invisible(NULL)) }
  f <- as.formula(paste(dv, "~", paste(terms, collapse = " + "), "+ (1 | participant) + (1 | video)"))
  say("Formula: ", deparse(f, width.cutoff = 500))
  m <- tryCatch(lmer(f, data = d), error = function(e) { say("Model failed: ", conditionMessage(e)); NULL })
  if (is.null(m)) return(invisible(NULL))
  if (isSingular(m)) say("Note: singular fit (a random-effect variance is estimated at 0); results are still interpretable but check the random effects.")
  say("N observations = ", nobs(m), " | participants = ", nlevels(d$participant), " | videos = ", nlevels(d$video))
  show(anova(m, type = 3))
  say("Random effects:"); show(as.data.frame(VarCorr(m))[, c("grp", "vcov", "sdcor")])
  for (f0 in intersect(c("level", "model", "dimension", "agent_gender", "transcript_gender", "task"), all.vars(f))) {
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
FIXED_BOTH <- c("level", "dimension", "model", "agent_gender", "transcript_gender", "task", "level:dimension", "level:model", "agent_gender:transcript_gender")
FIXED_ONE  <- setdiff(FIXED_BOTH, "task")
m_tw    <- fit_lmm("trustworthiness", D, FIXED_BOTH, "perceived trustworthiness (0-1), both tasks")
m_abil  <- fit_lmm("ability_index", D[D$task == "QA", ], FIXED_ONE, "perceived ability (0-1), QA videos")
m_bene  <- fit_lmm("benevolence_index", D[D$task == "Nav", ], FIXED_ONE, "perceived benevolence (0-1), Nav videos")
m_trust <- fit_lmm("trust", D, FIXED_BOTH, "trust in the agent's advice (1-7)")
m_conf  <- fit_lmm("self_confidence", D, FIXED_BOTH, "confidence in own answer (1-7)")

# Exploratory moderation by participants' gender stereotypes and attitude toward AI
if (length(mods)) for (dv in c("trustworthiness", "trust")) {
  fit_lmm(dv, D, c(FIXED_BOTH, mods, if ("stereotype_index" %in% mods) c("agent_gender:stereotype_index", "agent_gender:transcript_gender:stereotype_index")),
          paste0(dv, " - exploratory moderation by ", paste(mods, collapse = " and ")))
}

# Following the agent (QA), logistic mixed model
if ("followed" %in% names(D)) {
  title("6. FOLLOWING THE AGENT'S ANSWER (QA), logistic mixed model")
  d <- droplevels(D[D$task == "QA" & !is.na(D$followed), ])
  say("Proportion following the agent: ", round(mean(d$followed), 3))
  m <- tryCatch(glmer(followed ~ level * model + agent_gender * transcript_gender + dimension + (1 | participant) + (1 | video), data = d, family = binomial,
                      control = glmerControl(optimizer = "bobyqa")), error = function(e) { say("Model failed: ", conditionMessage(e)); NULL })
  if (!is.null(m)) { show(drop1(m, test = "Chisq")); show(summary(emmeans(m, ~ level, type = "response"))) }
} else {
  title("6. CHOICES")
  say("Set QA_AGENT_RECOMMENDS at the top of the script to analyse whether participants followed the agent. Raw choice proportions (option 1):")
  show(aggregate(cbind(option1 = choice == 1) ~ task + level, D, mean))
}

# =====================================================================================================
# 7. COMPLEMENTARY REPEATED-MEASURES ANOVA (by-participant means)
# =====================================================================================================
# Ignores video-to-video variability, so its p-values are too liberal with this design: report it only as a complement.
title("7. COMPLEMENTARY RM-ANOVA (participant means; liberal, see comment in script)")
for (spec in list(list(dv = "trustworthiness", w = c("level", "model")), list(dv = "trustworthiness", w = c("agent_gender", "transcript_gender")))) {
  d <- droplevels(D[!is.na(D[[spec$dv]]), ])
  tab <- table(d$participant, interaction(d[spec$w], drop = TRUE))
  ok <- rownames(tab)[apply(tab > 0, 1, all)]                     # only participants who saw every cell
  if (length(ok) < 3) { say(spec$dv, " ~ ", paste(spec$w, collapse = " x "), ": skipped (fewer than 3 participants saw every cell)"); next }
  d <- droplevels(d[d$participant %in% ok, ])
  say(spec$dv, " ~ ", paste(spec$w, collapse = " x "), " (", length(ok), " of ", nrow(tab), " participants saw every cell)")
  a <- suppressMessages(aov_ez("participant", spec$dv, d, within = spec$w, fun_aggregate = mean, anova_table = list(correction = "GG", es = "pes")))
  show(a)
}

say("\nDone. Results in ", normalizePath(OUT))
close(REPORT)
