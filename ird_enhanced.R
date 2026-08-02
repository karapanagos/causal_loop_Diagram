# =============================================================================
# INTERRELATIONSHIP DIGRAPH (IRD) — Enhanced Script
# Based on: WHO CLD Participant Manual (de Pinho, 2015)
# Case study: Dynamics of Neonatal Mortality in Uganda
# =============================================================================

library(igraph)


# 1. SPECIFY VARIABLES ========================================================

variables <- c(
  "mothers attending ANC, hospital deliveries and PNC",
  "level of awareness of MHC & NHC",
  "health of mothers",
  "care of newborn",
  "death risk of neonate",
  "safe deliveries and PNC",
  "socio-economic status",
  "resource adequacy (staffing, drugs, log & supplies)",
  "neonatal survival",
  "mothers' birth preparedness",
  "health education by health workers",
  "perceptions and belief in myths"
)

codes <- c("ATTEND", "AWARE", "MHLTH", "NBCARE", "NRISK", "SAFEDEL",
           "SES", "RESRC", "NSURV", "BPREP", "HEDUC", "MYTHS")

var_lookup <- data.frame(code = codes, variable = variables,
                         stringsAsFactors = FALSE)


# 2. CREATE THE IRD ADJACENCY MATRIX ==========================================
# Rows = "from" (cause), Columns = "to" (effect). 1 = causal link, 0 = none.
# Rule: arrows are unidirectional — the stronger direction only.

n <- length(variables)
ird <- matrix(0, nrow = n, ncol = n, dimnames = list(codes, codes))

# Fill in the pairwise relationships (from -> to)
ird["ATTEND", "SAFEDEL"] <- 1
ird["ATTEND", "RESRC"]   <- 1
ird["ATTEND", "HEDUC"]   <- 1
ird["AWARE",  "NBCARE"]  <- 1
ird["AWARE",  "BPREP"]   <- 1
ird["AWARE",  "MYTHS"]   <- 1
ird["AWARE",  "ATTEND"]  <- 1
ird["MHLTH",  "NBCARE"]  <- 1
ird["MHLTH",  "NRISK"]   <- 1
ird["MHLTH",  "SAFEDEL"] <- 1
ird["NBCARE", "NRISK"]   <- 1
ird["NRISK",  "NSURV"]   <- 1
ird["SAFEDEL","NRISK"]   <- 1
ird["SES",    "BPREP"]   <- 1
ird["SES",    "ATTEND"]  <- 1
ird["SES",    "MHLTH"]   <- 1
ird["SES",    "NRISK"]   <- 1
ird["SES",    "SAFEDEL"] <- 1
ird["RESRC",  "SAFEDEL"] <- 1
ird["NSURV",  "MYTHS"]   <- 1
ird["BPREP",  "ATTEND"]  <- 1
ird["HEDUC",  "AWARE"]   <- 1
ird["HEDUC",  "MHLTH"]   <- 1
ird["MYTHS",  "NBCARE"]  <- 1


# 3. VALIDATION ================================================================

# Check no bidirectional arrows exist (should return 0 rows)
bidir <- which(ird == 1 & t(ird) == 1, arr.ind = TRUE)
if (nrow(bidir) > 0) {
  warning("Bidirectional relationships found — violates IRD rules:")
  print(bidir)
} else {
  message("OK: No bidirectional relationships. IRD matrix is valid.")
}

# Check diagonal is all zeros (no self-loops)
stopifnot(all(diag(ird) == 0))


# 4. DRIVER / OUTCOME ANALYSIS ================================================
# This is the core analytical step of the IRD methodology.
# out-degree (row sums) = number of variables this one drives
# in-degree  (col sums) = number of variables driving this one
# net = out - in: positive => driver, negative => outcome

ird_analysis <- data.frame(
  code      = codes,
  variable  = variables,
  out_arrows = rowSums(ird),
  in_arrows  = colSums(ird),
  stringsAsFactors = FALSE
)
ird_analysis$net <- ird_analysis$out_arrows - ird_analysis$in_arrows

  # Classify each variable
ird_analysis$role <- ifelse(
  ird_analysis$net > 0, "DRIVER",
  ifelse(ird_analysis$net < 0, "OUTCOME", "NEUTRAL")
)

# Sort by net influence (strongest drivers first)
ird_analysis <- ird_analysis[order(-ird_analysis$net), ]

cat("\n=== IRD DRIVER / OUTCOME ANALYSIS ===\n\n")
print(ird_analysis, row.names = FALSE)

cat("\n--- Key Drivers (root causes) ---\n")
print(ird_analysis[ird_analysis$role == "DRIVER",
                   c("code", "variable", "out_arrows", "in_arrows", "net")],
      row.names = FALSE)

cat("\n--- Key Outcomes (effects) ---\n")
print(ird_analysis[ird_analysis$role == "OUTCOME",
                   c("code", "variable", "out_arrows", "in_arrows", "net")],
      row.names = FALSE)


# 5. BUILD AND PLOT THE DIGRAPH ================================================

g <- graph_from_adjacency_matrix(ird, mode = "directed", diag = FALSE)

# Label wrapping function for readable multi-line labels
wrap_label <- function(x, width = 14) {
  sapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"))
}

# Assign full (wrapped) variable names as labels
V(g)$label <- wrap_label(var_lookup$variable[match(V(g)$name, var_lookup$code)])

# Colour-code by role: drivers = coral/red, outcomes = lightblue, neutral = grey
role_map <- setNames(ird_analysis$role, ird_analysis$code)
V(g)$color <- ifelse(
  role_map[V(g)$name] == "DRIVER",  "#E8927C",  # warm coral
  ifelse(role_map[V(g)$name] == "OUTCOME", "#7CB9E8", "#D3D3D3")  # blue / grey
)
V(g)$frame.color <- ifelse(
  role_map[V(g)$name] == "DRIVER",  "#C0392B",
  ifelse(role_map[V(g)$name] == "OUTCOME", "#2980B9", "#888888")
)

# Size nodes by total connections (in + out) to show overall involvement
total_degree <- degree(g, mode = "all")
V(g)$size <- 18 + total_degree * 3  # scale factor


# --- Plot A: Circular layout (traditional IRD) ---
par(mar = c(1, 1, 3, 1))  # reduce margins

plot(g,
     layout            = layout_in_circle(g),
     vertex.label      = V(g)$label,
     vertex.label.cex  = 0.55,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.arrow.size   = 0.4,
     edge.color        = "#555555",
     edge.curved       = 0.15,        # slight curve avoids overlapping arrows
     main = "Interrelationship Digraph — Neonatal Mortality in Uganda\n(circular layout)")

# Add a legend
legend("bottomleft",
       legend = c("Driver (net out > in)", "Outcome (net in > out)", "Neutral"),
       fill   = c("#E8927C", "#7CB9E8", "#D3D3D3"),
       border = c("#C0392B", "#2980B9", "#888888"),
       cex = 0.7, bty = "n")


# --- Plot B: Force-directed layout (may spread things more readably) ---
set.seed(42)  # for reproducibility of layout
par(mar = c(1, 1, 3, 1))

plot(g,
     layout            = layout_with_fr(g),
     vertex.label      = V(g)$label,
     vertex.label.cex  = 0.55,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.arrow.size   = 0.4,
     edge.color        = "#555555",
     edge.curved       = 0.15,
     main = "Interrelationship Digraph — Neonatal Mortality in Uganda\n(force-directed layout)")

legend("bottomleft",
       legend = c("Driver (net out > in)", "Outcome (net in > out)", "Neutral"),
       fill   = c("#E8927C", "#7CB9E8", "#D3D3D3"),
       border = c("#C0392B", "#2980B9", "#888888"),
       cex = 0.7, bty = "n")


# --- Plot C: Drivers at bottom, Outcomes at top (WHO manual style) ---
# Manual layout: y-position based on net score (drivers low, outcomes high)
# x-position spread evenly within each tier

# Sort by net for positioning
sorted <- ird_analysis[order(ird_analysis$net), ]
# Normalise net score to y-coordinates (drivers at bottom, outcomes at top)
y_pos <- scales::rescale(sorted$net, to = c(-1, 1))
# Spread x-positions to avoid overlap within similar y-levels
set.seed(123)
x_pos <- jitter(seq(-1, 1, length.out = nrow(sorted)), amount = 0.3)

manual_layout <- matrix(NA, nrow = n, ncol = 2)
for (i in seq_len(nrow(sorted))) {
  idx <- which(V(g)$name == sorted$code[i])
  manual_layout[idx, ] <- c(x_pos[i], y_pos[i])
}

par(mar = c(1, 1, 3, 1))
plot(g,
     layout            = manual_layout,
     vertex.label      = V(g)$label,
     vertex.label.cex  = 0.55,
     vertex.label.color = "black",
     vertex.label.font = 2,
     edge.arrow.size   = 0.4,
     edge.color        = "#555555",
     edge.curved       = 0.2,
     main = "IRD — Drivers (bottom) to Outcomes (top)")

legend("bottomleft",
       legend = c("Driver (net out > in)", "Outcome (net in > out)", "Neutral"),
       fill   = c("#E8927C", "#7CB9E8", "#D3D3D3"),
       border = c("#C0392B", "#2980B9", "#888888"),
       cex = 0.7, bty = "n")

# Note: Plot C uses the {scales} package for rescale(). Install if needed:
# install.packages("scales")


# 6. EXPORT UTILITIES ==========================================================

# Export adjacency matrix to CSV (for sharing with participants or archiving)
write.csv(ird, "ird_adjacency_matrix.csv")

# Export the analysis table
write.csv(ird_analysis, "ird_driver_outcome_analysis.csv", row.names = FALSE)

# Export edge list (useful for Kumu, Gephi, or other network tools)
edges <- which(ird == 1, arr.ind = TRUE)
edge_list <- data.frame(
  from      = rownames(ird)[edges[, 1]],
  to        = colnames(ird)[edges[, 2]],
  from_full = var_lookup$variable[match(rownames(ird)[edges[, 1]], var_lookup$code)],
  to_full   = var_lookup$variable[match(colnames(ird)[edges[, 2]], var_lookup$code)],
  stringsAsFactors = FALSE
)
write.csv(edge_list, "ird_edge_list.csv", row.names = FALSE)


# 7. AGGREGATION UTILITIES (for participatory sessions) ========================
# When multiple participants each fill in their own IRD matrix,
# you can aggregate them to find consensus.

aggregate_irds <- function(ird_list) {
  # ird_list: a list of adjacency matrices (same dimensions & names)
  # Returns: a matrix where each cell = count of participants who marked that link
  n_participants <- length(ird_list)
  agg <- Reduce("+", ird_list)

  cat("Number of participants:", n_participants, "\n")
  cat("Agreement matrix (count of participants marking each link):\n\n")

  return(list(
    agreement_matrix = agg,
    n_participants   = n_participants,
    # Proportion matrix: what fraction of participants agree on each link
    proportion_matrix = agg / n_participants
  ))
}

# Example usage (uncomment and adapt when you have multiple participants):
#
# participant_1 <- ird  # the matrix we already built
# participant_2 <- ird  # would be a different matrix
# participant_3 <- ird  # ...
#
# results <- aggregate_irds(list(participant_1, participant_2, participant_3))
#
# # Show only links where >= 2/3 of participants agree
# consensus_threshold <- 2/3
# consensus_ird <- ifelse(results$proportion_matrix >= consensus_threshold, 1, 0)
#
# # Check for bidirectional conflicts in the consensus
# bidir_conflicts <- which(consensus_ird == 1 & t(consensus_ird) == 1, arr.ind = TRUE)
# # These are pairs where the group is split on direction — flag for discussion
#
# # Build a consensus graph
# g_consensus <- graph_from_adjacency_matrix(consensus_ird, mode = "directed", diag = FALSE)


# 8. TEMPLATE GENERATOR (for distributing to participants) =====================
# Creates a blank Excel-ready CSV matrix that participants fill in

generate_blank_template <- function(codes, variables, filename = "ird_template.csv") {
  n <- length(codes)
  blank <- matrix("", nrow = n, ncol = n, dimnames = list(codes, codes))
  # Mark diagonal as N/A
  diag(blank) <- "X"

  # Add a header row with full variable names for reference
  template <- rbind(
    c("FROM \\ TO", codes),
    cbind(codes, blank)
  )

  write.csv(template, filename, row.names = FALSE)
  cat("Template saved to:", filename, "\n")
  cat("Instructions: Enter 1 if the ROW variable influences the COLUMN variable.\n")
  cat("Leave blank or 0 if no influence. Diagonal cells (X) should remain empty.\n")
  cat("Remember: only ONE direction per pair (the stronger influence).\n")

  # Also save a reference sheet
  ref <- data.frame(Code = codes, `Full Variable Name` = variables)
  write.csv(ref, sub("\\.csv", "_reference.csv", filename), row.names = FALSE)
  cat("Reference sheet saved to:", sub("\\.csv", "_reference.csv", filename), "\n")
}

# Uncomment to generate:
# generate_blank_template(codes, variables)
