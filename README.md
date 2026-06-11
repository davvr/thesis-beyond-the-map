# Beyond the Map

Automated monthly pipeline that translates CIS opinion barometer data into
province-level seat projections under D'Hondt, with a public-facing website
showing the results.

TFM · MSc Computational Social Science · UC3M 2026 · Supervisor: Fran  
Live site: [davvr.github.io/thesis-beyond-the-map](https://davvr.github.io/thesis-beyond-the-map)

---

## What this is

A product-oriented thesis that builds a fully automated pipeline from raw CIS
microdata to public electoral visualizations, updated monthly without manual
intervention. The core deliverable is the pipeline itself and the live site,
the methodological choices (aggregation window, swing model) are treated as
parameters that can be refined iteratively. The current implementation covers
the full stack: automated data acquisition, D'Hondt seat projection, and a
public-facing site with three map views, top problems rankings, and ideological
evolution tracking. Future iterations could extend the methodology, incorporate
more barometers, or add further visualizations.

---

## Project structure

```
├── R/
│   ├── dhondt.R                  # D'Hondt seat allocation
│   ├── render_map.R              # Shared map rendering function (CSSLab style)
│   ├── read_catalogue.R          # Read barometers.json -> tibble (no scraping)
│   ├── scraping/
│   │   ├── list_barometers.R     # Scrape CIS catalogue -> 6 most recent barometers
│   │   ├── get_zip_url.R         # Extract ZIP URL from study page JSON-LD
│   │   └── download_zip.R        # Download and validate ZIP
│   └── actions/
│       └── update_barometers.R   # Monthly orchestrator (GitHub Actions entry point)
├── pipeline/
│   ├── map/
│   │   ├── map_agg.R             # Projection from 6-month aggregate
│   │   ├── map_last.R            # Projection from latest barometer only
│   │   └── map_last_swing.R      # Latest barometer + Uniform National Swing
│   ├── problems/
│   │   └── problems.R            # Top problems cited by Spaniards (3 views)
│   └── ideology/
│       └── ideology_evolution.R  # Ideological self-placement over time
├── data-raw/
│   ├── cis/                      # 6 most recent CIS microdata ZIPs (FIFO)
│   └── infoelectoral/            # 23J 2023 official provincial results
├── docs/                         # GitHub Pages site
│   ├── index.html
│   ├── barometers.json           # Auto-updated barometer metadata (consumed by site)
│   └── images/
│       ├── maps/
│       ├── problems/
│       └── ideology/
├── analysis/
│   └── first-steps/              # Exploratory scripts (not part of the pipeline)
└── .github/workflows/
    └── update_barometers.yml     # Monthly schedule (days 1–10, 10:00 UTC)
```

---

## Data

**CIS monthly barometers** — publicly available microdata from the
[Centro de Investigaciones Sociológicas](https://www.cis.es) under CC BY 4.0.
ZIP files are downloaded directly from the JSON-LD metadata embedded in each
study page (no form submission required). The pipeline maintains a rolling
window of the 6 most recent monthly barometers.

Key variables used: `INTENCIONGR` (vote intention), `PESO` (calibration weight),
`PESPANNA1/2/3` (top problems), `ESCIDEOL` (ideology scale 1–10), `PROV` (province).

**Infoelectoral 2023** — official 23J results by province, used as the baseline
for the UNS swing model.

---

## How it works

GitHub Actions runs on days 1–10 of each month:

1. Scrapes the CIS catalogue to detect a new barometer
2. Downloads the new ZIP and drops the oldest (FIFO → always 6)
3. Updates `docs/barometers.json`
4. Regenerates all maps, problems and ideology charts
5. Commits and pushes → GitHub Pages redeploys automatically

The site reads `barometers.json` via `fetch()` and renders barometer chips
dynamically — no manual HTML updates needed when a new barometer arrives.

To run locally:

```r
source("data-raw/cis/download_latest.R")       # refresh ZIPs
source("pipeline/map/map_agg.R")
source("pipeline/map/map_last.R")
source("pipeline/map/map_last_swing.R")
source("pipeline/problems/problems.R")
source("pipeline/ideology/ideology_evolution.R")
```

---

## Methodology

### D'Hondt projection

Weighted provincial vote shares from the CIS barometer(s) are fed into
D'Hondt allocation with a 3% threshold, using official province magnitudes
from the 23J 2023 results. Three map views are provided:

- **6-month aggregate** — pools 6 barometers (direct sum of PESO weights)
- **Latest month only** — maximum recency, minimum sample size
- **Latest + UNS** — latest barometer adjusted with Uniform National Swing

### Uniform National Swing

Following Butler & Stokes (1969), the national swing per party is the
difference between its current CIS share and its 23J share. This additive
swing is applied uniformly to each province's 2023 result (floored at 0).
Used as a canonical baseline, not as the optimal model.

### Known limitations

- The CIS systematically overestimates left-leaning parties (Tezanos effect).
  This is treated as a feature to document, not a bug to correct.
- Provincial sub-samples are small in depopulated provinces, making
  projections there structurally unreliable.
- UNS assumes uniform territorial change — a strong simplification in Spain's
  multi-party, territorially heterogeneous electoral system.

---

## References

- Butler, D. & Stokes, D. (1969). *Political Change in Britain*. Macmillan.
- Wilson, J. & Grofman, B. (2022). Evaluating electoral swing metrics. *Electoral Studies*.
- CIS microdata: [www.cis.es](https://www.cis.es) · CC BY 4.0