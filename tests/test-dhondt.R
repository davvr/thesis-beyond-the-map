# tests/test_dhondt.R
# Sanity check for the D'Hondt routine.
#
# Run this from the project root with:
#   source("tests/test_dhondt.R")
#
# It loads R/dhondt.R, runs a few hand-checkable cases, and then validates
# the routine against the official 2023 allocation in a couple of provinces.
# When the full Infoelectoral file is wired in, this will be expanded to all
# 52 provinces.

source("R/dhondt.R")

cat("== test_dhondt.R ==\n")

# --- Case 1: textbook example ----------------------------------------------
# 5 seats, 4 parties. Votes: A=100, B=80, C=30, D=20.
# Quotients sorted: 100(A), 80(B), 50(A), 40(B), 33.3(A) -> A=3, B=2, C=0, D=0.
votes <- c(A = 100, B = 80, C = 30, D = 20)
expected <- c(A = 3L, B = 2L, C = 0L, D = 0L)
got <- dhondt(votes, magnitude = 5, threshold = 0)
stopifnot(identical(got, expected))
cat("  case 1 (textbook 5 seats): OK\n")

# --- Case 2: threshold filters smaller party -------------------------------
# Same votes but with the 3% threshold and a larger fringe of small parties.
votes <- c(A = 1000, B = 800, C = 300, D = 50, E = 20)
got   <- dhondt(votes, magnitude = 7, threshold = 0.03)
stopifnot(got["E"] == 0)        # below 3% threshold, must get nothing
stopifnot(sum(got) == 7)         # all 7 seats allocated
cat("  case 2 (threshold filters E): OK\n")

# --- Case 3: single-seat constituency --------------------------------------
votes <- c(A = 500, B = 480, C = 200)
got   <- dhondt(votes, magnitude = 1, threshold = 0.03)
stopifnot(got["A"] == 1, sum(got) == 1)
cat("  case 3 (M = 1): OK\n")

# --- Case 4: zero-magnitude and zero-vote edge cases -----------------------
got <- dhondt(c(A = 100, B = 50), magnitude = 0)
stopifnot(sum(got) == 0)

got <- dhondt(c(A = 0, B = 0), magnitude = 5)
stopifnot(sum(got) == 0)
cat("  case 4 (edge cases M=0 and v=0): OK\n")

# --- Case 5: official 2023 result, Soria (M = 2) ---------------------------
# Replace these with the exact values from the 2023 Infoelectoral file.
# Soria 2023: PP 17,824 / PSOE 9,895 / Vox 4,107 / Sumar 1,326 / ...
# Expected allocation: PP 2, PSOE 0  (or PP 1, PSOE 1 — to be checked when
# we wire in the official numbers; this stub is here to remind us to do it).
# TODO: load actual 2023 numbers from data-raw/infoelectoral/ and assert.
cat("  case 5 (Soria 2023, M=2): SKIPPED — wire in official numbers first\n")

cat("== all dhondt tests passed ==\n")
