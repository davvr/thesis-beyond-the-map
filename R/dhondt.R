# R/dhondt.R
# D'Hondt seat allocation routine.
# Tested against the official 2023 results in tests/test_dhondt.R.

#' Allocate seats with the D'Hondt method
#'
#' Implements D'Hondt with an optional vote-share threshold applied at the
#' constituency level. Returns the seat count for each party.
#'
#' Algorithm: for each seat 1..M, compute the quotient v_i / (s_i + 1) for
#' each party i where v_i is the party's vote total and s_i is the number of
#' seats already assigned to that party. The seat goes to the party with the
#' largest quotient. Ties are broken in favour of the party with more votes
#' overall (and, if still tied, the first one in input order).
#'
#' @param votes Named numeric vector. Vote totals for each party in this
#'   constituency. Names are party labels.
#' @param magnitude Integer. Number of seats to allocate.
#' @param threshold Numeric in [0, 1]. Parties whose vote share is strictly
#'   below this threshold are excluded from allocation. Defaults to 0.03 (3%),
#'   the legal threshold for the Spanish Congress.
#'
#' @return Named integer vector of length `length(votes)` with the seat count
#'   per party. Sums to `magnitude`.
#'
#' @examples
#' votes <- c(PP = 100, PSOE = 80, Vox = 30, Sumar = 20)
#' dhondt(votes, magnitude = 5, threshold = 0.03)
dhondt <- function(votes, magnitude, threshold = 0.03) {
  stopifnot(is.numeric(votes), !is.null(names(votes)),
            magnitude >= 0, magnitude == as.integer(magnitude),
            threshold >= 0, threshold <= 1)
  
  total <- sum(votes)
  if (total == 0 || magnitude == 0) {
    return(setNames(integer(length(votes)), names(votes)))
  }
  
  # Apply threshold
  eligible <- votes / total >= threshold
  v <- ifelse(eligible, votes, 0)
  
  seats <- setNames(integer(length(votes)), names(votes))
  for (k in seq_len(magnitude)) {
    quotients <- v / (seats + 1)
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1L
  }
  seats
}

#' Allocate seats across all provinces
#'
#' Convenience wrapper that applies `dhondt()` province by province.
#'
#' @param projections Tibble with columns `province`, `party`, `votes_proj`.
#' @param magnitudes Named integer vector. Number of seats per province
#'   (constituency code -> magnitude).
#' @param threshold Numeric. Vote-share threshold, default 0.03.
#'
#' @return Tibble with columns `province`, `party`, `seats`.
allocate_seats <- function(projections, magnitudes, threshold = 0.03) {
  # TODO: split by province, call dhondt() with the right magnitude, bind back.
  stop("allocate_seats(): not implemented yet")
}
