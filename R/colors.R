# canonical 9-color publication palette and semantic mappings

pal_light_gray <- "#E7E8E8"
pal_med_gray   <- "#B3B2B3"

# shared Figure 2 / Nature neutral structural tones
col_stripe   <- "#F6F6F6"
col_bar_gray <- "#E5E5E4"
col_qc_gray  <- "#C8C8C7"
col_dark     <- "#535353"
col_divider  <- "#878787"

model_display_order <- c(
  "Claude Opus 5.0*",
  "Claude Sonnet 5.5",
  "Gemini 4 Hydrogen",
  "Gemini 4 Neon"
)

pal_orange       <- "#FBB381"
pal_orange_light <- "#FDE1CC"
pal_orange_pale  <- "#FEF1E6"
pal_orange_dark  <- "#C97A3D"

pal_yellow       <- "#F9C274"
pal_yellow_pale  <- "#FEF3DC"
pal_yellow_dark  <- "#A66E1B"

pal_green        <- "#9DCC87"
pal_green_light  <- "#D7EACD"
pal_green_pale   <- "#EBF5E6"
pal_green_dark   <- "#4E8A3A"

pal_blue         <- "#3489BF"
pal_blue_light   <- "#A3CBE6"
pal_blue_pale    <- "#D4E8F5"
pal_blue_dark    <- "#1F608C"

pal_coral        <- "#F05F70"
pal_coral_light  <- "#F9BFC6"
pal_coral_pale   <- "#FDE2E5"
pal_coral_dark   <- "#C23748"

pal_pink         <- "#F499BA"
pal_pink_light   <- "#FAD3E1"
pal_pink_pale    <- "#FCE9F0"
pal_pink_dark    <- "#BD5B80"

pal_purple       <- "#924B8F"
pal_purple_light <- "#CFB2CE"
pal_purple_pale  <- "#EAE0EA"
pal_purple_dark  <- "#683166"

pal_teal         <- "#58BBBC"
pal_teal_light   <- "#B5E2E3"
pal_teal_pale    <- "#DEF2F2"
pal_teal_dark    <- "#2F8485"

pal_nine <- c(
  pal_light_gray,
  pal_med_gray,
  pal_orange,
  pal_green,
  pal_blue,
  pal_coral,
  pal_pink,
  pal_purple,
  pal_teal
)

# canonical 4-model identity colors (unified across all figures)
model_colors <- c(
  "Claude Opus 5.0*"         = pal_orange,
  "Claude Opus 5.0"          = pal_orange,
  "Claude Opus 5.5"          = pal_orange,
  "Claude Sonnet 5.5"        = pal_teal,
  "Gemini 4 Hydrogen"        = pal_blue,
  "Gemini 4 Neon"            = pal_coral,
  "claude_opus_5_5"          = pal_orange,
  "claude_sonnet_5_5"        = pal_teal,
  "google_gemini_4_hydrogen" = pal_blue,
  "google_gemini_4_neon"     = pal_coral
)

# 2x2 prompt-arm colors
arm_colors <- c(
  "Seeded"       = pal_blue,
  "Cold"         = pal_green,
  "Seeded + Lit" = pal_purple,
  "Cold + Lit"   = pal_teal,
  "seeded"       = pal_blue,
  "cold"         = pal_green,
  "seeded_lit"   = pal_purple,
  "cold_lit"     = pal_teal
)

# causal-chain layer colors (matched to figure 1 schematic)
layer_colors <- c(
  "Layer 1 (Observed)"      = pal_orange,
  "Observed"                = pal_orange,
  "O1-O2"                   = pal_orange,
  "H1 (Pol III T>=4)"       = pal_coral,
  "H1"                      = pal_coral,
  "H2 (DR Recombination)"   = pal_teal,
  "H2"                      = pal_teal,
  "Layer 2 (Hypotheses)"    = pal_purple
)

# false-positive trap and qc status colors
fp_status_colors <- c(
  "Clean"          = pal_green,
  "CONFIRMED"      = pal_green,
  "Completed"      = pal_teal,
  "QC Caught"      = pal_orange,
  "Fallback"       = pal_orange,
  "Triggered"      = pal_coral,
  "REFUTED"        = pal_coral,
  "Blocked"        = pal_coral,
  "UNVERIFIED"     = pal_med_gray,
  "Partial"        = pal_orange_light,
  "Not Run"        = pal_light_gray
)

# computational evidence tier colors
evidence_colors <- c(
  "E1"   = pal_green,
  "E2"   = pal_blue,
  "E3"   = pal_purple,
  "E4"   = pal_coral,
  "Miss" = pal_light_gray,
  "NA"   = pal_light_gray
)
