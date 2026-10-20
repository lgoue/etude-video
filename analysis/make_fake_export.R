# Builds a FAKE export (same columns as the admin CSV export) to test analyse_results.R.
#
# Usage: Rscript analysis/make_fake_export.R real_export.csv fake_export.csv [n_participants]
# The real export is only used as a column template: every answer in the fake file is randomly generated.
#
# Planted effects (what analyse_results.R should find):
#   - level: Low < Medium < High on ability/benevolence, advice trust and following the recommendation
#   - agent gender x transcript gender: female agent with a female transcript rated higher
#   - nothing else (model, dimension, task have no effect)
# Edge cases included: 1 participant who did not finish, 1 rejected, 1 who failed an attention-check video,
# 1 who failed an instructed item ("Select 5"), 1 who reported being inattentive (attention item = 1).

invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
args <- commandArgs(TRUE)
if (length(args) < 2) stop("Usage: Rscript analysis/make_fake_export.R real_export.csv fake_export.csv [n_participants]")
N <- if (length(args) >= 3) as.integer(args[3]) else 80
set.seed(42)

lines <- readLines(args[1], encoding = "UTF-8", warn = FALSE); lines[1] <- sub("^﻿", "", lines[1])
tpl <- read.csv(text = lines, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character")
cols <- names(tpl)
colx <- function(pattern) grep(pattern, cols, perl = TRUE, value = TRUE)[1]

# Video pool: 3 videos per combination dimension x level x model x agent gender x transcript gender, for each task
pool <- expand.grid(rep = 1:3, transcript = c("Femme", "Homme", "Neutre"), gender = c("Femme", "Homme"), model = c("GPT", "Gemma", "Mistral"),
                    ability = c("Low", "Medium", "High"), dimension = c("Ability", "Benevolence"), task = c("QA", "Nav"), stringsAsFactors = FALSE)
pool$video_name <- with(pool, paste0(substr(dimension, 1, 1), "_", substr(ability, 1, 3), "_", task, "_Transcript", substr(transcript, 1, 1),
                                     "_Agent", substr(gender, 1, 1), "_", substr(model, 1, 2), "_", rep))
pool$w <- rnorm(nrow(pool), 0, .4)                                   # video-to-video variability
lvl <- c(Low = -1, Medium = 0, High = 1)

clip <- function(x, k) as.character(pmin(k, pmax(1, round(x))))
rows <- list()
for (p in seq_len(N)) {
  qa <- pool[pool$task == "QA", ][sample(sum(pool$task == "QA"), 18), ]
  nv <- pool[pool$task == "Nav", ][sample(sum(pool$task == "Nav"), 18), ]
  v <- rbind(qa, nv); n <- nrow(v); is_qa <- v$task == "QA"
  r <- as.data.frame(matrix("", n, length(cols), dimnames = list(NULL, cols)), check.names = FALSE, stringsAsFactors = FALSE)
  r$participant <- as.character(p); r$prolific_pid <- sprintf("FAKE%06d", p); r$session_id <- sprintf("fake-session-%04d", p)
  r$pos <- as.character(seq_len(n) - 1); r$part <- ifelse(is_qa, "1", "2")
  for (k in c("video_name", "ability", "gender", "task", "transcript", "model", "dimension")) r[[k]] <- v[[k]]
  r$yt <- paste0("https://example.org/", v$video_name, ".mp4")
  r$question_no <- as.character(ifelse(is_qa, sample(0:121, n, TRUE), sample(0:3, n, TRUE)))
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
  lat <- 4 + .6 * lvl[v$ability] + ifelse(v$gender == "Femme" & v$transcript == "Femme", .3, 0) + u + v$w
  lik <- function(extra = 0) clip(lat + extra + rnorm(n, 0, .8), 7)
  three <- function() clip((lat - 1) / 2 + rnorm(n, 0, .4), 3)
  qn <- as.numeric(r$question_no); rec_is_1 <- ifelse(is_qa, qn %% 2 == 0, qn %in% c(0, 2))     # option 1 = A (QA) / Left (Nav)
  follow <- runif(n) < plogis(.4 + .8 * lvl[v$ability])
  ch <- ifelse(follow == rec_is_1, "1", "2")
  set <- function(pat, val, rows_) { c0 <- colx(pat); r[[c0]] <<- ifelse(rows_, val, "") }
  set("^I would answer", ch, is_qa); set("^I would take the", ch, !is_qa)
  set("knowledge of the right", lik(), is_qa); set("very capable of indicating", lik(), is_qa); set("assess the character.s ability", three(), is_qa)
  set("advice \\(q25", lik(.2), is_qa); set("^\\s*I am confident in my answers", lik(), is_qa)
  set("go out of its way", lik(), !is_qa); set("needs and desires", lik(), !is_qa); set("evaluate the character.s benevolence", three(), !is_qa)
  set("advice \\(q26", lik(.2), !is_qa); set("^\\s*I feel confident about my answers", lik(), !is_qa)
  r$question_order <- ifelse(is_qa, "252|254|255|256|257|258", "253|259|260|261|262|263")

  # Edge cases that analyse_results.R must exclude
  if (p == 3) { r <- r[1:10, ]; r$completed <- "false"; r$abandoned <- "true" }              # did not finish
  if (p == 4) r$rejected <- "true"                                                            # rejected in the admin
  if (p == 5) r[["Sexism | Select 5"]] <- "4"                                                 # failed an instructed item
  if (p == 6) { tr <- r[1, ]; tr$video_name <- "ATTENTION_CHECK"; tr$attention <- "true"; tr$attention_passed <- "false"; r <- rbind(r, tr) }  # failed trap
  if (p == 7) set_demo("pay attention", 1)                                                    # self-reported inattentive
  rows[[p]] <- r
}
out <- do.call(rbind, rows)
write.csv(out, args[2], row.names = FALSE, fileEncoding = "UTF-8")
cat("Fake export written:", args[2], "|", N, "participants,", nrow(out), "rows\n")
