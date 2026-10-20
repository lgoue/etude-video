# Builds a FAKE export (same columns as the admin CSV export) to test analyse_results.R.
#
# Usage: Rscript analysis/make_fake_export.R real_export.csv fake_export.csv [n_participants] [study_prefix] [seed] [other_prefix] [video_list.csv]
#   other_prefix: participant 10 reuses the Prolific ID of participant 10 of that other fake study (tests the repeat-participant exclusion)
#   video_list.csv: the real list of videos (columns Nom, URL, Niveau, Genre Agent, Tache, Genre Transcript, Dimension manipulated,
#                   Transcript, Modele, NQuestion, Reponse attendue si piege). Without it, a made-up pool of videos is used.
#                   URLs are not copied into the fake file: each video gets a neutral id ("video-<row>").
# The real export is only used as a column template: every answer in the fake file is randomly generated.
# Which task each rating block belongs to is read from the item wording: items about the "answer" are QA, about the "path" Nav
# (S1: ability after QA, benevolence after Nav; S2: the reverse). Advice/confidence items: first column QA, second Nav.
#
# Planted effects (what analyse_results.R should find):
#   - level: Low < Medium < High on ability/benevolence, advice trust and following the recommendation;
#     the level moves the rating of the MANIPULATED dimension more (+0.6 per step) than the other one (+0.2 per step)
#   - agent gender x transcript gender: female agent with a female transcript rated higher
#   - nothing else (model, dimension, task have no effect)
# Edge cases included: 1 participant who did not finish, 1 rejected, 1 who failed an attention-check video,
# 1 who failed an instructed item ("Select 5"), 1 who reported being inattentive (attention item = 1).

invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
args <- commandArgs(TRUE)
if (length(args) < 2) stop("Usage: Rscript analysis/make_fake_export.R real_export.csv fake_export.csv [n_participants]")
N <- if (length(args) >= 3) as.integer(args[3]) else 80
PREFIX <- if (length(args) >= 4) args[4] else "FAKE"
set.seed(if (length(args) >= 5) as.integer(args[5]) else 42)
OTHER <- if (length(args) >= 6 && nzchar(args[6]) && args[6] != "-") args[6] else NA
VIDEO_LIST <- if (length(args) >= 7) args[7] else NA

lines <- readLines(args[1], encoding = "UTF-8", warn = FALSE); lines[1] <- sub("^﻿", "", lines[1])
tpl <- read.csv(text = lines, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
cols <- names(tpl)
colx <- function(pattern) grep(pattern, cols, perl = TRUE, value = TRUE)[1]

if (!is.na(VIDEO_LIST)) {
  # Real list of videos
  vl <- read.csv(text = sub("^\ufeff", "", readLines(VIDEO_LIST, encoding = "UTF-8", warn = FALSE)), check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  pool <- data.frame(video_name = vl$Nom, ability = vl$Niveau, gender = vl[["Genre Agent"]], task = vl$Tache, transcript = vl[["Genre Transcript"]],
                     dimension = vl[["Dimension manipulated"]], transcript_no = vl$Transcript, model = vl$Modele, question_no = vl$NQuestion,
                     yt = paste0("video-", seq_len(nrow(vl))), stringsAsFactors = FALSE)
  traps <- pool[pool$ability == "Attention", ]; pool <- pool[pool$ability != "Attention", ]
} else {
  # Made-up pool: 3 videos per combination dimension x level x model x agent gender x transcript gender, for each task
  pool <- expand.grid(rep = 1:3, transcript = c("Femme", "Homme", "Neutre"), gender = c("Femme", "Homme"), model = c("GPT", "Gemma", "Mistral"),
                      ability = c("Low", "Medium", "High"), dimension = c("Ability", "Benevolence"), task = c("QA", "Nav"), stringsAsFactors = FALSE)
  pool$video_name <- with(pool, paste0(substr(dimension, 1, 1), "_", substr(ability, 1, 3), "_", task, "_Transcript", substr(transcript, 1, 1),
                                       "_Agent", substr(gender, 1, 1), "_", substr(model, 1, 2), "_", rep))
  pool$transcript_no <- as.integer(factor(with(pool, paste(rep, transcript, model, ability, dimension, task))))   # F and M versions share a transcript
  pool$question_no <- NA; pool$yt <- paste0("video-", seq_len(nrow(pool))); traps <- pool[0, ]
}
pool$tno <- as.integer(factor(pool$transcript_no))
# Same videos (and same video effects) in every fake study: the video effects use their own fixed seed
seed_participants <- .Random.seed; set.seed(7)
pool$w <- rnorm(nrow(pool), 0, .4)                                   # video-to-video variability
tr_gender <- rnorm(max(pool$tno), 0, .3)                            # gender effect that differs between transcripts
pool$w <- pool$w + ifelse(pool$gender == "Femme", .5, -.5) * tr_gender[pool$tno]
.Random.seed <- seed_participants
lvl <- c(Low = -1, Medium = 0, High = 1)

clip <- function(x, k) as.character(pmin(k, pmax(1, round(x))))
rows <- list()
for (p in seq_len(N)) {
  qa <- pool[pool$task == "QA", ][sample(sum(pool$task == "QA"), 18), ]
  nv <- pool[pool$task == "Nav", ][sample(sum(pool$task == "Nav"), 18), ]
  v <- rbind(qa, nv); n <- nrow(v); is_qa <- v$task == "QA"
  r <- as.data.frame(matrix("", n, length(cols), dimnames = list(NULL, cols)), check.names = FALSE, stringsAsFactors = FALSE)
  r$participant <- as.character(p); r$prolific_pid <- sprintf("%s%06d", PREFIX, p); r$session_id <- sprintf("fake-%s-%04d", tolower(PREFIX), p)
  r$pos <- as.character(seq_len(n) - 1); r$part <- ifelse(is_qa, "1", "2")
  for (k in c("video_name", "ability", "gender", "task", "transcript", "model", "dimension")) r[[k]] <- v[[k]]
  r$transcript_no <- as.character(v$transcript_no)
  r$yt <- v$yt
  r$question_no <- ifelse(is.na(v$question_no) | v$question_no == "", as.character(ifelse(is_qa, sample(0:121, n, TRUE), sample(0:3, n, TRUE))), v$question_no)
  r$attention <- "false"; r$ended <- "true"; r$audio_ok <- "true"; r$comp_attempts <- as.character(sample(1:2, 1, prob = c(.8, .2)))
  r$part_order <- "12"; r$comp2_attempts <- "1"; r$completed <- "true"; r$rejected <- "false"; r$abandoned <- "false"; r$consent <- "true"

  # Participant-level questionnaires (random, constant across the participant's rows)
  for (c0 in grep("^(Sexism|Attitude toward AI|BFI-S) \\|", cols, value = TRUE)) r[[c0]] <- as.character(sample(1:7, 1))
  for (c0 in grep("\\| Select [0-9]+$", cols, value = TRUE)) r[[c0]] <- sub(".*Select ", "", c0)          # instructed items answered correctly
  set_demo <- function(pat, val) { c0 <- colx(pat); if (!is.na(c0)) r[[c0]] <<- as.character(val) }
  set_demo("How do you identify", sample(1:3, 1)); set_demo("What is your age", sample(1:6, 1)); set_demo("highest level of education", sample(1:7, 1))
  set_demo("pay attention", sample(2:4, 1)); set_demo("female virtual character", sample(1:7, 1)); set_demo("male virtual character", sample(1:7, 1))

  # Per-video answers, generated from a latent trustworthiness with the planted effects
  u <- rnorm(1, 0, .7)
  base <- 4 + ifelse(v$gender == "Femme" & v$transcript == "Femme", .3, 0) + u + v$w
  lat_ab <- base + ifelse(v$dimension == "Ability", .6, .2) * lvl[v$ability]          # ability: moved most by ability-manipulated videos
  lat_be <- base + ifelse(v$dimension == "Benevolence", .6, .2) * lvl[v$ability]      # benevolence: moved most by benevolence-manipulated videos
  lat <- (lat_ab + lat_be) / 2
  lik <- function(extra = 0, l = lat) clip(l + extra + rnorm(n, 0, .8), 7)
  three <- function(l = lat) clip((l - 1) / 2 + rnorm(n, 0, .4), 3)
  qn <- as.numeric(r$question_no); rec_is_1 <- ifelse(is_qa, qn %% 2 == 0, qn %in% c(0, 2))     # option 1 = A (QA) / Left (Nav)
  follow <- runif(n) < plogis(.4 + .8 * lvl[v$ability])
  ch <- ifelse(follow == rec_is_1, "1", "2")
  set <- function(c0, val, rows_) r[[c0]] <<- ifelse(rows_, val, "")
  task_of <- function(c0) if (grepl("answer", c0)) is_qa else !is_qa        # the item wording says which task it follows
  set(colx("^I would answer"), ch, is_qa); set(colx("^I would take the"), ch, !is_qa)
  ab <- c(colx("knowledge of the right"), colx("very capable of indicating"), colx("assess the character.s ability")); qa_ab <- grepl("answer", ab[1])
  be <- c(colx("go out of its way"), colx("needs and desires"), colx("evaluate the character.s benevolence"))
  set(ab[1], lik(l = lat_ab), task_of(ab[1])); set(ab[2], lik(l = lat_ab), task_of(ab[1])); set(ab[3], three(lat_ab), task_of(ab[1]))
  be_rows <- if (qa_ab) !is_qa else is_qa
  set(be[1], lik(l = lat_be), be_rows); set(be[2], lik(l = lat_be), be_rows); set(be[3], three(lat_be), be_rows)
  adv <- grep("confident in the virtual character's advice", cols, value = TRUE)
  set(adv[1], lik(.2), is_qa); set(adv[2], lik(.2), !is_qa)
  set(colx("^\\s*I am confident in my answers"), lik(), is_qa); set(colx("^\\s*I feel confident about my answers"), lik(), !is_qa)
  r$question_order <- ifelse(is_qa, "252|254|255|256|257|258", "253|259|260|261|262|263")

  # Edge cases that analyse_results.R must exclude
  if (p == 3) { r <- r[1:10, ]; r$completed <- "false"; r$abandoned <- "true" }              # did not finish
  if (p == 4) r$rejected <- "true"                                                            # rejected in the admin
  if (p == 5) r[["Sexism | Select 5"]] <- "4"                                                 # failed an instructed item
  if (p == 6) { tr <- r[1, ]; tr$video_name <- if (nrow(traps)) traps$video_name[1] else "ATTENTION_CHECK"; tr$yt <- "video-trap"
    tr$attention <- "true"; tr$attention_passed <- "false"; r <- rbind(r, tr) }                                   # failed trap
  if (p == 7) set_demo("pay attention", 1)                                                    # self-reported inattentive
  if (p == 10 && !is.na(OTHER)) r$prolific_pid <- sprintf("%s%06d", OTHER, p)              # already took part in the other study
  rows[[p]] <- r
}
out <- do.call(rbind, rows)
write.csv(out, args[2], row.names = FALSE, fileEncoding = "UTF-8")
cat("Fake export written:", args[2], "|", N, "participants,", nrow(out), "rows\n")
