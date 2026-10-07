## Lookup tables and helpers for IMOS deployment metadata (internal).
##  One source for species common names, release site spellings and the
##  state or country of each release site. Used by smru_clean_meta_imos(),
##  smru_build_meta_imos(), smru_write_meta() and imos_smru_dm_qc().

## species in the IMOS SATTAG metadata, with their common names. Common names
##  are lower case except for proper nouns.
.imos_species <- data.frame(
  species = c("Mirounga leonina",
              "Leptonychotes weddellii",
              "Arctocephalus forsteri",
              "Arctocephalus pusillus",
              "Neophoca cinerea",
              "Lepidochelys olivacea",
              "Natator depressus",
              "Chelonia mydas"),
  common_name = c("southern elephant seal",
                  "Weddell seal",
                  "New Zealand fur seal",
                  "Australian fur seal",
                  "Australian sea lion",
                  "olive ridley turtle",
                  "flatback turtle",
                  "green turtle"),
  group = c("seals", "seals", "seals", "seals", "seals",
            "turtles", "turtles", "turtles"),
  stringsAsFactors = FALSE
)

## release sites in use for IMOS seal and turtle deployments, in their
##  standard spelling, with their state or country
.imos_sites <- data.frame(
  release_site = c("Iles Kerguelen",
                   "Macquarie Island",
                   "Campbell Island",
                   "Scott Base",
                   "Cape Armitage",
                   "Dumont d'Urville",
                   "Casey",
                   "Davis",
                   "Montague Island",
                   "Tiwi Islands",
                   "Melville Island",
                   "North West Crocodile Island",
                   "Tiwi and North West Crocodile Islands",
                   "Milman Islet",
                   ## Australian sea lion and fur seal sites
                   "Blefuscu Island",
                   "Cape du Couedic",
                   "George Island",
                   "Greenly Island",
                   "Lewis Island",
                   "Liguanea Island",
                   "Little Wiers",
                   "Nicolas Baudin Island",
                   "Nuyts Reef",
                   "Olive Island",
                   "Pearson Island",
                   "Price Island",
                   "Red Islet",
                   "Rocky North Island",
                   "Rocky South Island",
                   "Seal Bay",
                   "Seal Slide",
                   "Six Mile Island",
                   "South Neptune Island",
                   "West Island",
                   "West Waldegrave Island",
                   ## other Antarctic sites
                   "Halley Research Station"),
  state_country = c("French Overseas Territory",
                    "Australia",
                    "New Zealand",
                    "New Zealand Antarctic Territory",
                    "New Zealand Antarctic Territory",
                    "French Antarctic Territory",
                    "Australian Antarctic Territory",
                    "Australian Antarctic Territory",
                    "Australia",
                    "Australia",
                    "Australia",
                    "Australia",
                    "Australia",
                    "Australia",
                    rep("Australia", 21),
                    "British Antarctic Territory"),
  stringsAsFactors = FALSE
)

## other spellings in use, mapped to the standard spelling (keys as imos_norm())
.imos_site_aliases <- c(
  "northwest crocodile island" = "North West Crocodile Island",
  "tiwi and northwest crocodile islands" = "Tiwi and North West Crocodile Islands",
  "iles keguelen" = "Iles Kerguelen",
  "crocodile island" = "North West Crocodile Island",
  "nicolas baudin" = "Nicolas Baudin Island",
  "nicolas baudin is" = "Nicolas Baudin Island"
)

## lower case, single spaces, no leading or trailing spaces
imos_norm <- function(x) tolower(trimws(gsub("[[:space:]]+", " ", x)))

## standard spelling of release sites; unknown sites are returned trimmed
imos_site <- function(x) {
  key <- imos_norm(x)
  out <- .imos_sites$release_site[match(key, imos_norm(.imos_sites$release_site))]
  miss <- is.na(out)
  out[miss] <- unname(.imos_site_aliases[key[miss]])
  miss <- is.na(out)
  out[miss] <- trimws(gsub("[[:space:]]+", " ", x[miss]))
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

## state or country of release sites; NA if the site is unknown
imos_state_country <- function(site) {
  .imos_sites$state_country[match(imos_norm(imos_site(site)), imos_norm(.imos_sites$release_site))]
}

## common names by species; the current value (trimmed) if the species is unknown
imos_common_name <- function(species, current = NA_character_) {
  cn <- .imos_species$common_name[match(imos_norm(species), imos_norm(.imos_species$species))]
  current <- rep_len(current, length(cn))
  ifelse(is.na(cn), trimws(current), cn)
}

## apply the standard release site, state or country and common name to a
##  metadata table. A state_country already present is kept for unknown sites.
imos_standardise_meta <- function(meta) {
  if ("release_site" %in% names(meta)) {
    meta$release_site <- imos_site(meta$release_site)
    sc <- imos_state_country(meta$release_site)
    if ("state_country" %in% names(meta)) sc <- ifelse(is.na(sc), meta$state_country, sc)
    meta$state_country <- sc
  }
  if (all(c("species", "common_name") %in% names(meta))) {
    meta$species <- trimws(meta$species)
    meta$common_name <- imos_common_name(meta$species, meta$common_name)
  }
  meta
}

## SMRU campaign id from a deployment reference, e.g. "ct128a" from
##  "ct128a-246BAT-12": letters, digits and an optional trailing letter.
##  Used for the campaign id in every output table and file name.
smru_cid <- function(ref) {
  stringr::str_extract(trimws(ref), stringr::regex("[a-z]+[0-9]+[a-z]?", ignore_case = TRUE))
}

## campaigns that are delayed-mode QC'd without WMO IDs: the 2022 IMOS turtle
##  campaigns had no WMO IDs. Every other campaign's deployments with no WMO ID
##  are dropped by imos_smru_dm_qc()
.imos_wmo_exempt_cids <- c("tu116", "tu117", "tu120")
