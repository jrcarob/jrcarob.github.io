# =============================================================================
# CURVE TRADES WITH MACROECONOMIC SIGNALS
# Free-Data Reproduction in R (FRED + OECD + IMF + World Bank)
# VERSION 2 - Fixed list() empty argument error
# FRED API KEY: 93d220ed3522c2a638c363198acf3eca
# NEW PACKAGE fred
# =============================================================================

# 1. SETUP & PACKAGES
# -----------------------------------------------------------------------------
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

pkgs <- c("tidyverse", "lubridate", "zoo", "xts", "RcppRoll", "ggplot2", 
          "patchwork", "scales", "httr", "jsonlite", "imfr", "fred", 
          "WDI", "corrplot", "broom", "gridExtra", "readr")
invisible(sapply(pkgs, install_if_missing))

theme_set(theme_minimal(base_size = 12) + 
            theme(legend.position = "bottom",
                  strip.text = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))

# -----------------------------------------------------------------------------
# 2. CONFIGURATION
# -----------------------------------------------------------------------------
FRED_API_KEY <- "93d220ed3522c2a638c363198acf3eca"   # https://fred.stlouisfed.org/docs/api/api_key.html
START_DATE <- as.Date("2000-01-01")
END_DATE   <- Sys.Date()

cids_dm <- sort(c("AUD", "CAD", "CHF", "EUR", "GBP", "JPY", "NOK", "NZD", "SEK", "USD"))
cids_g2 <- c("EUR", "USD")
cids_xg2 <- sort(setdiff(cids_dm, cids_g2))
cids_ez <- c("DEM", "ESP", "FRF", "ITL")
cids <- sort(union(cids_dm, cids_ez))

# OECD country codes
oecd_map <- c(AUD="AUS", CAD="CAN", CHF="CHE", EUR="EA19", GBP="GBR", 
              JPY="JPN", NOK="NOR", NZD="NZL", SEK="SWE", USD="USA")

# IMF country codes  
imf_map <- c(AUD="AU", CAD="CA", CHF="CH", EUR="U2", GBP="GB", 
             JPY="JP", NOK="NO", NZD="NZ", SEK="SE", USD="US")

# -----------------------------------------------------------------------------
# 3. FRED SERIES MAPPING (EXPLICIT - NO EMPTY ELEMENTS)
# -----------------------------------------------------------------------------

# Define each country's FRED mappings as separate, complete lists
# Only include countries where we have actual FRED series IDs
fred_USD <- list(DU02YYLD = "DGS2", DU10YYLD = "DGS10", 
                 UNEMPL = "UNRATE", CPI = "CPIAUCSL")
fred_EUR <- list(DU02YYLD = "IR3TIB01EZM156N", DU10YYLD = "IRLTLT01EZM156N")
fred_JPY <- list(DU02YYLD = "INTDSRJPM193N", DU10YYLD = "IRLTLT01JPM156N")
fred_GBR <- list(DU02YYLD = "IR3TIB01GBM156N", DU10YYLD = "IRLTLT01GBM156N")

# Combine into fred_map using list() - ensure NO empty or NULL elements
fred_map <- list(
  USD = fred_USD,
  EUR = fred_EUR,
  JPY = fred_JPY,
  GBP = fred_GBR
)

# Clean up temporary variables
rm(fred_USD, fred_EUR, fred_JPY, fred_GBR)

# -----------------------------------------------------------------------------
# 4. ROBUST DATA DOWNLOAD ENGINE
# -----------------------------------------------------------------------------

#' Safe FRED download using direct httr (bypasses fredr curl issues)
get_fred_direct <- function(series_id, name_override = NULL, api_key = FRED_API_KEY) {
  if (is.null(api_key) || api_key == "YOUR_FRED_API_KEY") {
    message("  SKIP FRED (no API key): ", series_id)
    return(NULL)
  }
  
  url <- "https://api.stlouisfed.org/fred/series/observations"
  query <- list(
    series_id = series_id,
    api_key = api_key,
    file_type = "json",
    observation_start = format(START_DATE, "%Y-%m-%d"),
    observation_end = format(END_DATE, "%Y-%m-%d")
  )
  
  tryCatch({
    resp <- GET(url, query = query, timeout(30))
    if (status_code(resp) != 200) {
      warning("FRED HTTP ", status_code(resp), ": ", series_id)
      return(NULL)
    }
    
    json <- content(resp, "text", encoding = "UTF-8") %>% fromJSON()
    if (is.null(json$observations) || length(json$observations) == 0) {
      warning("FRED empty: ", series_id)
      return(NULL)
    }
    
    df <- tibble(
      real_date = as.Date(json$observations$date),
      value = suppressWarnings(as.numeric(json$observations$value)),
      xcat = ifelse(is.null(name_override), series_id, name_override)
    ) %>% filter(!is.na(value))
    
    if (nrow(df) == 0) {
      warning("FRED no valid data: ", series_id)
      return(NULL)
    }
    
    message("  OK FRED: ", series_id, " (", nrow(df), " rows)")
    return(df)
  }, error = function(e) {
    warning("FRED error: ", series_id, " - ", conditionMessage(e))
    return(NULL)
  })
}

#' Safe OECD CSV download
get_oecd_csv <- function(dataset, filter_str, start_year = 2000) {
  url <- sprintf("https://stats.oecd.org/SDMX-JSON/data/%s/%s/all?startTime=%d-01&dimensionAtObservation=allDimensions&format=csv",
                 dataset, filter_str, start_year)
  
  tryCatch({
    df <- read.csv(url, stringsAsFactors = FALSE)
    if (nrow(df) == 0 || ncol(df) < 3) {
      warning("OECD empty: ", dataset, " | ", filter_str)
      return(NULL)
    }
    
    names(df) <- tolower(names(df))
    names(df) <- gsub("\\.", "_", names(df))
    
    message("  OK OECD: ", dataset, " | ", filter_str, " (", nrow(df), " rows)")
    return(as_tibble(df))
  }, error = function(e) {
    warning("OECD error: ", dataset, " | ", filter_str)
    return(NULL)
  })
}

#' Safe IMF download
get_imf <- function(database, indicator, country, start, end) {
  tryCatch({
    df <- imf_data(database_id = database, indicator = indicator, 
                   country = country, start = start, end = end, freq = "M")
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df <- df %>% 
      transmute(
        real_date = as.Date(paste0(year, "-", sprintf("%02d", as.numeric(month)), "-01")), 
        value = as.numeric(value),
        xcat = indicator,
        cid = country
      )
    
    message("  OK IMF: ", indicator, " | ", country)
    return(df)
  }, error = function(e) {
    warning("IMF error: ", indicator, " | ", country)
    return(NULL)
  })
}

#' Safe World Bank download
get_wb <- function(indicator, countries, start, end) {
  tryCatch({
    df <- WDI(indicator = indicator, country = countries, start = start, end = end)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    return(as_tibble(df))
  }, error = function(e) {
    warning("WB error: ", indicator)
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# 5. MASTER DOWNLOADER
# -----------------------------------------------------------------------------

download_macro_data <- function() {
  all_data <- list()
  
  # --- A. INTEREST RATES ---
  message("Downloading interest rates...")
  
  # FRED for countries in fred_map
  for (cid in names(fred_map)) {
    fm <- fred_map[[cid]]
    if (!is.null(fm$DU02YYLD)) {
      d <- get_fred_direct(fm$DU02YYLD, "DU02YYLD_NSA")
      if (!is.null(d)) {
        d$cid <- cid
        all_data[[length(all_data)+1]] <- d
      }
    }
    if (!is.null(fm$DU10YYLD)) {
      d <- get_fred_direct(fm$DU10YYLD, "DU10YYLD_NSA")
      if (!is.null(d)) {
        d$cid <- cid
        all_data[[length(all_data)+1]] <- d
      }
    }
  }
  
  # OECD for remaining countries
  oecd_countries <- setdiff(cids_dm, names(fred_map))
  for (cid in oecd_countries) {
    oecd_code <- oecd_map[cid]
    
    d10 <- get_oecd_csv("MEI_FIN", paste0(oecd_code, ".IRLTLT01.M"))
    if (!is.null(d10)) {
      d10 <- d10 %>% 
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")), 
                  value = obs_value, cid = cid, xcat = "DU10YYLD_NSA")
      all_data[[length(all_data)+1]] <- d10
    }
    
    d3m <- get_oecd_csv("MEI_FIN", paste0(oecd_code, ".IR3TIB01.M"))
    if (!is.null(d3m)) {
      d3m <- d3m %>% 
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")), 
                  value = obs_value, cid = cid, xcat = "DU02YYLD_NSA")
      all_data[[length(all_data)+1]] <- d3m
    }
  }
  
  # --- B. UNEMPLOYMENT ---
  message("Downloading unemployment...")
  for (cid in names(fred_map)) {
    fm <- fred_map[[cid]]
    if (!is.null(fm$UNEMPL)) {
      d <- get_fred_direct(fm$UNEMPL, "UNEMPLRATE_SA_3MMA")
      if (!is.null(d)) {
        d$cid <- cid
        all_data[[length(all_data)+1]] <- d
      }
    }
  }
  
  for (cid in setdiff(cids_dm, names(fred_map))) {
    oecd_code <- oecd_map[cid]
    unemp <- get_oecd_csv("MEI", paste0(oecd_code, ".LRHUTTTT.M"))
    if (!is.null(unemp)) {
      unemp <- unemp %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "UNEMPLRATE_SA_3MMA")
      all_data[[length(all_data)+1]] <- unemp
    }
  }
  
  # --- C. CPI INFLATION ---
  message("Downloading inflation...")
  for (cid in names(fred_map)) {
    fm <- fred_map[[cid]]
    if (!is.null(fm$CPI)) {
      d <- get_fred_direct(fm$CPI, "CPIH_SA_P1M1ML12")
      if (!is.null(d)) {
        d$cid <- cid
        all_data[[length(all_data)+1]] <- d
      }
    }
  }
  
  for (cid in setdiff(cids_dm, names(fred_map))) {
    oecd_code <- oecd_map[cid]
    cpi <- get_oecd_csv("MEI", paste0(oecd_code, ".CPALTT01.IXOB.M"))
    if (!is.null(cpi)) {
      cpi <- cpi %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "CPIH_SA_P1M1ML12")
      all_data[[length(all_data)+1]] <- cpi
    }
  }
  
  # --- D. GDP ---
  message("Downloading GDP...")
  for (cid in cids_dm) {
    oecd_code <- oecd_map[cid]
    gdp <- get_oecd_csv("QNA", paste0(oecd_code, ".B1_GE.VOBARSA.Q"))
    if (!is.null(gdp)) {
      gdp <- gdp %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "RGDP_SA_P1Q1QL4")
      all_data[[length(all_data)+1]] <- gdp
    }
  }
  
  # --- E. SURVEYS ---
  message("Downloading surveys...")
  for (cid in cids_dm) {
    oecd_code <- oecd_map[cid]
    
    cc <- get_oecd_csv("MEI", paste0(oecd_code, ".CSCICP03.IXNS.M"))
    if (!is.null(cc)) {
      cc <- cc %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "CCSCORE_SA")
      all_data[[length(all_data)+1]] <- cc
    }
    
    bc <- get_oecd_csv("MEI", paste0(oecd_code, ".BSCICP03.IXNS.M"))
    if (!is.null(bc)) {
      bc <- bc %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "SBCSCORE_SA")
      all_data[[length(all_data)+1]] <- bc
    }
  }
  
  # --- F. HOUSE PRICES ---
  message("Downloading house prices...")
  for (cid in cids_dm) {
    oecd_code <- oecd_map[cid]
    hpi <- get_oecd_csv("PRICES_CPI", paste0(oecd_code, ".HPI.NIXOB.Q"))
    if (!is.null(hpi)) {
      hpi <- hpi %>%
        filter(!is.na(obs_value)) %>%
        transmute(real_date = as.Date(paste0(time, "-01")),
                  value = obs_value, cid = cid, xcat = "HPI_SA_P1Q1QL4")
      all_data[[length(all_data)+1]] <- hpi
    }
  }
  
  # --- G. DEBT ---
  message("Downloading debt...")
  debt <- get_wb("GC.DOD.TOTL.GD.ZS", unname(oecd_map), 2000, year(Sys.Date()))
  if (!is.null(debt) && nrow(debt) > 0) {
    # Map ISO2 back to our CID codes
    iso2_to_cid <- setNames(names(oecd_map), 
                            countrycode::countrycode(oecd_map, "iso3c", "iso2c"))
    debt <- debt %>%
      filter(!is.na(GC.DOD.TOTL.GD.ZS)) %>%
      mutate(cid = iso2_to_cid[iso2c]) %>%
      filter(!is.na(cid)) %>%
      transmute(real_date = as.Date(paste0(year, "-12-01")),
                value = GC.DOD.TOTL.GD.ZS, cid = cid, xcat = "GGDGDPRATIOX10_NSA")
    all_data[[length(all_data)+1]] <- debt
  }
  
  # --- H. CREDIT ---
  message("Downloading credit...")
  for (cid in cids_dm) {
    imf_code <- imf_map[cid]
    credit <- get_imf("FSI", "FAS_FAFO_DC_CA_PT", imf_code, 2000, year(Sys.Date()))
    if (!is.null(credit) && nrow(credit) > 0) {
      credit$xcat <- "PCREDITBN_SJA_P1M1ML12"
      all_data[[length(all_data)+1]] <- credit
    }
  }
  
  # Combine
  if (length(all_data) == 0) {
    warning("NO DATA DOWNLOADED")
    return(NULL)
  }
  
  dfx <- bind_rows(all_data) %>%
    filter(real_date >= START_DATE, real_date <= END_DATE) %>%
    distinct(real_date, cid, xcat, .keep_all = TRUE)
  
  message("Download complete. Rows: ", nrow(dfx), 
          " | Series: ", length(unique(dfx$xcat)),
          " | CIDs: ", length(unique(dfx$cid)))
  
  return(dfx)
}

# -----------------------------------------------------------------------------
# 6. SYNTHETIC DATA FALLBACK
# -----------------------------------------------------------------------------

generate_synthetic_data <- function(seed = 42) {
  message("Generating synthetic demonstration data...")
  set.seed(seed)
  
  dates <- seq(START_DATE, END_DATE, by = "month")
  n <- length(dates)
  
  df_list <- list()
  
  for (cid in cids_dm) {
    base_2y <- switch(cid,
                      USD = 2.5, EUR = 1.5, JPY = 0.1, GBP = 2.0,
                      AUD = 3.0, CAD = 2.2, CHF = 0.5, NOK = 2.8,
                      NZD = 3.2, SEK = 1.8)
    base_10y <- base_2y + switch(cid,
                                 USD = 1.0, EUR = 1.2, JPY = 0.8, GBP = 1.1,
                                 AUD = 1.3, CAD = 1.0, CHF = 0.9, NOK = 1.1,
                                 NZD = 1.2, SEK = 1.0)
    
    eps_2y <- cumsum(rnorm(n, 0, 0.15))
    eps_10y <- cumsum(rnorm(n, 0, 0.12)) + 0.7 * eps_2y
    
    y2 <- base_2y + eps_2y + 2 * sin(2 * pi * (1:n) / 120)
    y10 <- base_10y + eps_10y + 1.5 * sin(2 * pi * (1:n) / 120)
    y2 <- pmax(y2, 0.01)
    y10 <- pmax(y10, 0.05)
    
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "DU02YYLD_NSA", value = y2
    )
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "DU10YYLD_NSA", value = y10
    )
    
    base_u <- switch(cid,
                     USD = 5.5, EUR = 7.5, JPY = 3.5, GBP = 5.0,
                     AUD = 5.8, CAD = 6.5, CHF = 3.2, NOK = 4.0,
                     NZD = 5.0, SEK = 7.0)
    u <- base_u + arima.sim(list(ar = 0.95), n) * 1.5
    u <- pmax(u, 2)
    
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "UNEMPLRATE_SA_3MMA", value = u
    )
    
    base_cpi <- 100
    cpi <- base_cpi * cumprod(1 + rnorm(n, 0.002, 0.003))
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "CPIH_SA_P1M1ML12", value = cpi
    )
    
    q_dates <- dates[month(dates) %in% c(3, 6, 9, 12)]
    base_gdp <- switch(cid,
                       USD = 10000, EUR = 8000, JPY = 5000, GBP = 2500,
                       AUD = 1200, CAD = 1500, CHF = 600, NOK = 400,
                       NZD = 200, SEK = 500)
    gdp <- base_gdp * cumprod(1 + rnorm(length(q_dates), 0.005, 0.01))
    
    df_list[[length(df_list)+1]] <- tibble(
      real_date = q_dates, cid = cid, xcat = "RGDP_SA_P1Q1QL4", value = gdp
    )
    
    cc <- 100 + arima.sim(list(ar = 0.85), n) * 10
    bc <- 100 + arima.sim(list(ar = 0.85), n) * 8 + 0.5 * (cc - 100)
    
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "CCSCORE_SA", value = cc
    )
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "SBCSCORE_SA", value = bc
    )
    
    hpi <- 100 * cumprod(1 + rnorm(n, 0.003, 0.008))
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "HPI_SA_P1Q1QL4", value = hpi
    )
    
    debt <- 60 + cumsum(rnorm(n, 0.02, 0.3))
    debt <- pmax(debt, 20)
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "GGDGDPRATIOX10_NSA", value = debt
    )
    
    credit <- 5 + arima.sim(list(ar = 0.9), n) * 3
    df_list[[length(df_list)+1]] <- tibble(
      real_date = dates, cid = cid, xcat = "PCREDITBN_SJA_P1M1ML12", value = credit
    )
  }
  
  bind_rows(df_list) %>%
    filter(real_date >= START_DATE, real_date <= END_DATE)
}

# -----------------------------------------------------------------------------
# 7. PANEL ENGINE
# -----------------------------------------------------------------------------

update_df <- function(df, dfa) {
  if (is.null(dfa) || nrow(dfa) == 0) return(df)
  df <- df %>%
    anti_join(dfa %>% select(real_date, cid, xcat), 
              by = c("real_date", "cid", "xcat"))
  bind_rows(df, dfa %>% select(real_date, cid, xcat, value))
}

pchg <- function(x, n = 12) {
  x <- zoo::na.locf(x, na.rm = FALSE)
  (x / dplyr::lag(x, n) - 1) * 100
}

diffn <- function(x, n = 1) {
  x <- zoo::na.locf(x, na.rm = FALSE)
  x - dplyr::lag(x, n)
}

ma <- function(x, n = 3) {
  RcppRoll::roll_mean(x, n, fill = NA, align = "right")
}

panel_calculator <- function(df, calcs, cids, external_func = list()) {
  result_list <- list()
  
  for (calc in calcs) {
    parts <- strsplit(calc, " = ")[[1]]
    if (length(parts) != 2) next
    new_xcat <- trimws(parts[1])
    expr_str <- trimws(parts[2])
    
    for (cid_val in cids) {
      cid_data <- df %>%
        filter(cid == cid_val) %>%
        select(real_date, xcat, value) %>%
        pivot_wider(names_from = xcat, values_from = value) %>%
        arrange(real_date)
      
      if (nrow(cid_data) == 0) next
      
      env <- as.list(cid_data)
      env <- c(env, external_func, list(
        pchg = pchg, diffn = diffn, ma = ma, 
        annualise_qoq = function(x) ((1 + x/100)^4 - 1) * 100,
        sqrt = sqrt, exp = exp, log = log, abs = abs,
        pmin = pmin, pmax = pmax, lag = dplyr::lag, lead = dplyr::lead
      ))
      
      result <- tryCatch({
        eval(parse(text = expr_str), envir = env)
      }, error = function(e) {
        rep(NA_real_, nrow(cid_data))
      })
      
      result_list[[length(result_list) + 1]] <- tibble(
        real_date = cid_data$real_date,
        cid = cid_val,
        xcat = new_xcat,
        value = as.numeric(result)
      )
    }
  }
  bind_rows(result_list)
}

make_zn_scores <- function(df, xcat, cids, sequential = TRUE, min_obs = 522,
                           est_freq = "m", neutral = "zero", pan_weight = 1,
                           thresh = 3, postfix = "_ZN") {
  result_list <- list()
  
  for (cid_val in cids) {
    cid_data <- df %>%
      filter(cid == cid_val, xcat == !!xcat) %>%
      arrange(real_date)
    
    if (nrow(cid_data) == 0) next
    
    vals <- cid_data$value
    n <- length(vals)
    z <- numeric(n)
    
    if (sequential) {
      for (i in seq_len(n)) {
        if (i < min_obs) { z[i] <- NA; next }
        w <- vals[1:i]; w <- w[!is.na(w)]
        if (length(w) < 10) { z[i] <- NA; next }
        mu <- mean(w); sig <- sd(w)
        z[i] <- if (sig == 0 || is.na(sig)) 0 else (vals[i] - mu) / sig
      }
    } else {
      mu <- mean(vals, na.rm = TRUE); sig <- sd(vals, na.rm = TRUE)
      z <- (vals - mu) / sig
    }
    
    z <- pmax(pmin(z, thresh), -thresh)
    
    result_list[[length(result_list) + 1]] <- tibble(
      real_date = cid_data$real_date, cid = cid_val,
      xcat = paste0(xcat, postfix), value = z
    )
  }
  bind_rows(result_list)
}

linear_composite <- function(df, xcats, cids, complete_xcats = FALSE,
                             new_cid = NULL, new_xcat = NULL) {
  result_list <- list()
  target_cids <- if (!is.null(new_cid)) new_cid else cids
  
  for (cid_val in target_cids) {
    if (!is.null(new_cid)) {
      sub <- df %>% filter(cid %in% cids, xcat %in% xcats)
      comp <- sub %>%
        group_by(real_date, xcat) %>%
        summarise(v = mean(value, na.rm = TRUE), .groups = "drop") %>%
        group_by(real_date) %>%
        summarise(value = mean(v, na.rm = TRUE), .groups = "drop")
    } else {
      sub <- df %>% filter(cid == cid_val, xcat %in% xcats)
      if (complete_xcats) {
        sub <- sub %>% group_by(real_date) %>% filter(n_distinct(xcat) == length(xcats))
      }
      comp <- sub %>%
        group_by(real_date) %>%
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
    }
    
    result_list[[length(result_list) + 1]] <- comp %>%
      mutate(cid = if (!is.null(new_cid)) new_cid else cid_val,
             xcat = if (!is.null(new_xcat)) new_xcat else "COMPOSITE")
  }
  bind_rows(result_list) %>% select(real_date, cid, xcat, value)
}

# -----------------------------------------------------------------------------
# 8. ANNUITY & FLATTENING
# -----------------------------------------------------------------------------

annuity_df <- function(x, T, eps = 1e-8) {
  x_safe <- ifelse(abs(x) >= eps, x, NA_real_)
  out <- (1 - exp(-x_safe * T)) / x_safe
  ifelse(abs(x) >= eps, out, T)
}

calc_flattening_returns <- function(df, cids) {
  calcs <- c(
    "DU02YYLD_DEC = DU02YYLD_NSA / 100",
    "DU10YYLD_DEC = DU10YYLD_NSA / 100",
    "A2 = annuity_df(DU02YYLD_DEC, 2)",
    "A10 = annuity_df(DU10YYLD_DEC, 10)",
    "w10v2 = A10 / A2",
    "DU02YXR_NSA = -1.9 * diffn(DU02YYLD_DEC)",
    "DU10YXR_NSA = -8.5 * diffn(DU10YYLD_DEC)",
    "DU10v02DVXR = DU10YXR_NSA - w10v2 * DU02YXR_NSA"
  )
  panel_calculator(df, calcs, cids, external_func = list(annuity_df = annuity_df))
}

# -----------------------------------------------------------------------------
# 9. FEATURE BUILDERS
# -----------------------------------------------------------------------------

build_labor <- function(df, cids) {
  # Check if required series exist
  required <- c("EMPL_NSA_P1M1ML12_3MMA", "WFORCE_NSA_P1Y1YL1_5YMM", "UNEMPLRATE_SA_3MMA")
  avail <- unique(df$xcat)
  
  if (!all(required %in% avail)) {
    message("  Labor: missing required series, skipping")
    return(df)
  }
  
  calcs <- c(
    "XEMPL_NSA_P1M1ML12_3MMA = pchg(EMPL_NSA_P1M1ML12_3MMA, 12) - pchg(WFORCE_NSA_P1Y1YL1_5YMM, 12)",
    "UNEMPLRATE_SA_3MMAv10YMM_NEG = -ma(UNEMPLRATE_SA_3MMA, 3)"
  )
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  labtights <- unique(dfa$xcat)
  
  for (xc in labtights) {
    df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  }
  labtightz <- paste0(labtights, "_ZN")
  
  dfa <- linear_composite(df, labtightz, cids_dm, complete_xcats = FALSE, new_xcat = "LABTIGHT")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "LABTIGHT", cids_dm))
  df
}

build_inflation <- function(df, cids) {
  avail <- unique(df$xcat)
  infs <- intersect(c("CPIH_SA_P1M1ML12", "CPIC_SA_P1M1ML12"), avail)
  
  if (length(infs) == 0) {
    message("  Inflation: no CPI data, skipping")
    return(df)
  }
  
  # Create YoY inflation
  if ("CPIH_SA_P1M1ML12" %in% avail) {
    df <- update_df(df, df %>% 
                      filter(xcat == "CPIH_SA_P1M1ML12") %>%
                      group_by(cid) %>%
                      mutate(value = pchg(value, 12), xcat = "CPIH_SJA_P6M6ML6AR") %>%
                      ungroup())
  }
  
  calcs <- paste0(infs, "vIET = ", infs, " - INFTEFF_NSA")
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  xinfs <- unique(dfa$xcat)
  
  for (xc in xinfs) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  xinfz <- paste0(xinfs, "_ZN")
  
  dfa <- linear_composite(df, xinfz, cids_dm, complete_xcats = FALSE, new_xcat = "XINF")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "XINF", cids_dm))
  df
}

build_financial <- function(df, cids) {
  avail <- unique(df$xcat)
  if (!all(c("PCREDITBN_SJA_P1M1ML12", "INFTEFF_NSA", "RGDP_SA_P1Q1QL4") %in% avail)) {
    message("  Financial: missing required series, skipping")
    return(df)
  }
  
  calcs <- c(
    "LTNOMGROWTH = INFTEFF_NSA + RGDP_SA_P1Q1QL4",
    "XPCREDIT_P1M1ML12 = PCREDITBN_SJA_P1M1ML12 - LTNOMGROWTH",
    "XHPI_P1M1ML12_3MMA = HPI_SA_P1Q1QL4 - LTNOMGROWTH"
  )
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  xfins <- grep("^X", unique(dfa$xcat), value = TRUE)
  
  for (xc in xfins) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  xfinz <- paste0(xfins, "_ZN")
  
  dfa <- linear_composite(df, xfinz, cids_dm, complete_xcats = FALSE, new_xcat = "XFIN")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "XFIN", cids_dm))
  df
}

build_overheating <- function(df, cids) {
  avail <- unique(df$xcat)
  req <- c("IMPORTS_SA_P1M1ML12_3MMA", "NRSALES_SA_P1M1ML12_3MMA", 
           "RGDP_SA_P1Q1QL4", "INFTEFF_NSA")
  if (length(intersect(req, avail)) < 2) {
    message("  Overheating: insufficient series, skipping")
    return(df)
  }
  
  calcs <- c(
    "XIMPORTS_SA_P1M1ML12_3MMA = IMPORTS_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4 - INFTEFF_NSA",
    "XNRSALES_SA_P1M1ML12_3MMA = NRSALES_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4 - INFTEFF_NSA",
    "XRPCONS_SA_P1M1ML12_3MMA = RPCONS_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4"
  )
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  overheats <- unique(dfa$xcat)
  
  for (xc in overheats) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  overheatz <- paste0(overheats, "_ZN")
  
  dfa <- linear_composite(df, overheatz, cids_dm, complete_xcats = FALSE, new_xcat = "OVERHEAT")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "OVERHEAT", cids_dm))
  df
}

build_debt <- function(df, cids) {
  if (!("GGDGDPRATIOX10_NSA" %in% unique(df$xcat))) {
    message("  Debt: no debt data, skipping")
    return(df)
  }
  
  xcat_fsc <- "GGDGDPRATIOX10_NSA"
  dict_trends <- list(DM=list(s=1, l=21, t=10), WQ=list(s=5, l=63, t=15), MY=list(s=21, l=252, t=20))
  
  fiscals <- c()
  for (nm in names(dict_trends)) {
    p <- dict_trends[[nm]]
    raw <- sprintf("%s_%dDv%dD", xcat_fsc, p$s, p$l)
    win <- sprintf("%sW%d_NEG", raw, p$t)
    
    df <- update_df(df, df %>% filter(xcat == xcat_fsc) %>% group_by(cid) %>%
                      mutate(value = ma(value, p$s) - lag(ma(value, p$l), p$s),
                             xcat = raw) %>% ungroup())
    df <- update_df(df, df %>% filter(xcat == raw) %>% 
                      mutate(value = -pmax(pmin(value, p$t), -p$t), xcat = win) %>% ungroup())
    fiscals <- c(fiscals, win)
  }
  
  for (xc in fiscals) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  fiscalz <- paste0(fiscals, "_ZN")
  
  dfa <- linear_composite(df, fiscalz, cids_dm, complete_xcats = FALSE, new_xcat = "DEBTIMPROVE")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "DEBTIMPROVE", cids_dm))
  df
}

build_confidence <- function(df, cids) {
  avail <- unique(df$xcat)
  confs <- intersect(c("CCSCORE_SA_3MMA", "SBCSCORE_SA_3MMA", "MBCSCORE_SA_3MMA"), avail)
  
  if (length(confs) == 0) {
    # Try to build 3MMA from raw
    raw_confs <- intersect(c("CCSCORE_SA", "SBCSCORE_SA", "MBCSCORE_SA"), avail)
    if (length(raw_confs) == 0) {
      message("  Confidence: no survey data, skipping")
      return(df)
    }
    for (xc in raw_confs) {
      df <- update_df(df, df %>% filter(xcat == xc) %>% group_by(cid) %>%
                        mutate(value = ma(value, 3), xcat = paste0(xc, "_3MMA")) %>% ungroup())
    }
    confs <- paste0(raw_confs, "_3MMA")
  }
  
  dfa <- linear_composite(df, confs, cids_dm, complete_xcats = FALSE, new_xcat = "CONFLEVEL")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "CONFLEVEL", cids_dm))
  
  # Changes
  for (xc in confs) {
    df <- update_df(df, df %>% filter(xcat == xc) %>% group_by(cid) %>%
                      mutate(value = diffn(value, 3), xcat = sub("3MMA", "D3M3ML3", xc)) %>% ungroup())
  }
  confchanges <- sub("3MMA", "D3M3ML3", confs)
  
  dfa2 <- linear_composite(df, confchanges, cids_dm, complete_xcats = FALSE, new_xcat = "CONFCHANGE")
  df <- update_df(df, dfa2)
  df <- update_df(df, make_zn_scores(df, "CONFCHANGE", cids_dm))
  df
}

build_qe <- function(df, cids) {
  avail <- unique(df$xcat)
  cbal <- "INTLIQGDP_NSA"
  
  if (!(cbal %in% avail)) {
    message("  QE: no liquidity data, skipping")
    return(df)
  }
  
  calcs <- c(
    paste0(cbal, "_D1M1ML6AR = ", cbal, "_D1M1ML6 * sqrt(2)"),
    paste0(cbal, "_D1M1ML3AR = ", cbal, "_D1M1ML3 * sqrt(4)"),
    paste0(cbal, "_D1M1ML1AR = ", cbal, "_D1M1ML1 * sqrt(12)")
  )
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  qes <- unique(dfa$xcat)
  
  for (xc in qes) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  qez <- paste0(qes, "_ZN")
  
  dfa <- linear_composite(df, qez, cids_dm, complete_xcats = FALSE, new_xcat = "QUANTEASE")
  df <- update_df(df, dfa)
  df <- update_df(df, make_zn_scores(df, "QUANTEASE", cids_dm))
  df
}

build_real_yields <- function(df, cids) {
  avail <- unique(df$xcat)
  req <- c("DU02YYLD_NSA", "DU10YYLD_NSA", "INFE2Y_JA", "INFE5Y_JA", "INFTEFF_NSA")
  
  # If inflation expectations missing, proxy from trailing CPI
  if (!("INFE2Y_JA" %in% avail) && "CPIH_SA_P1M1ML12" %in% avail) {
    df <- update_df(df, df %>% filter(xcat == "CPIH_SA_P1M1ML12") %>%
                      group_by(cid) %>%
                      mutate(value = ma(value, 24), xcat = "INFE2Y_JA") %>%
                      ungroup())
  }
  if (!("INFE5Y_JA" %in% avail) && "CPIH_SA_P1M1ML12" %in% avail) {
    df <- update_df(df, df %>% filter(xcat == "CPIH_SA_P1M1ML12") %>%
                      group_by(cid) %>%
                      mutate(value = ma(value, 60), xcat = "INFE5Y_JA") %>%
                      ungroup())
  }
  if (!("INFTEFF_NSA" %in% avail) && "CPIH_SA_P1M1ML12" %in% avail) {
    df <- update_df(df, df %>% filter(xcat == "CPIH_SA_P1M1ML12") %>%
                      group_by(cid) %>%
                      mutate(value = ma(value, 12), xcat = "INFTEFF_NSA") %>%
                      ungroup())
  }
  
  avail <- unique(df$xcat)  # refresh
  if (!all(c("DU02YYLD_NSA", "DU10YYLD_NSA") %in% avail)) {
    message("  Real yields: missing yield data, skipping")
    return(df)
  }
  
  calcs <- c(
    "DU02YRYLD = DU02YYLD_NSA - INFE2Y_JA",
    "DU10YRYLD = DU10YYLD_NSA - 0.5 * (INFE5Y_JA + INFTEFF_NSA)",
    "XDU10v2RYLD = DU10YRYLD - DU02YRYLD - 1"
  )
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  realdiffs <- grep("v2RYLD", unique(dfa$xcat), value = TRUE)
  
  for (xc in realdiffs) df <- update_df(df, make_zn_scores(df, xc, cids_dm))
  df
}

# -----------------------------------------------------------------------------
# 10. VISUALIZATION SUITE
# -----------------------------------------------------------------------------

view_timelines <- function(df, xcats, cids, ncol = 4, same_y = TRUE, 
                           cumsum = FALSE, title = NULL) {
  pd <- df %>% filter(xcat %in% xcats, cid %in% cids)
  if (cumsum) {
    pd <- pd %>% group_by(cid, xcat) %>% arrange(real_date) %>%
      mutate(value = cumsum(replace_na(value, 0))) %>% ungroup()
  }
  ggplot(pd, aes(x = real_date, y = value, color = xcat)) +
    geom_line(linewidth = 0.7) +
    facet_wrap(~cid, ncol = ncol, scales = if (same_y) "fixed" else "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    labs(title = title, x = NULL, y = NULL, color = NULL) +
    theme(legend.position = "bottom", panel.spacing = unit(0.5, "lines"))
}

correl_matrix <- function(df, xcats, cids, freq = "M", title = NULL) {
  pd <- df %>% filter(xcat %in% xcats, cid %in% cids)
  if (freq == "M") {
    pd <- pd %>% mutate(ym = as.yearmon(real_date)) %>%
      group_by(cid, ym, xcat) %>% summarise(value = last(value), .groups = "drop")
  }
  wide <- pd %>% pivot_wider(names_from = c(cid, xcat), values_from = value)
  mat <- cor(wide %>% select(-ym), use = "pairwise.complete.obs")
  mat[upper.tri(mat)] <- NA; diag(mat) <- NA
  
  cm <- as_tibble(mat, rownames = "var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
    filter(!is.na(corr))
  
  ggplot(cm, aes(x = var2, y = fct_rev(var1), fill = corr)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", corr)), size = 3) +
    scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                         limits = c(-1, 1), na.value = NA) +
    labs(title = title, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), panel.grid = element_blank())
}

# -----------------------------------------------------------------------------
# 11. SIGNAL CONSTRUCTION
# -----------------------------------------------------------------------------

construct_signals <- function(df, cids) {
  avail <- unique(df$xcat)
  factorz <- c("LABTIGHT_ZN", "XINF_ZN", "XFIN_ZN", "OVERHEAT_ZN",
               "DEBTIMPROVE_ZN", "CONFLEVEL_ZN", "CONFCHANGE_ZN",
               "QUANTEASE_ZN", "XDU10v2RYLD_ZN")
  xfactorz <- setdiff(factorz, "XDU10v2RYLD_ZN")
  
  avail_factors <- intersect(factorz, avail)
  avail_xfactors <- intersect(xfactorz, avail)
  
  if (length(avail_factors) > 0) {
    dfa <- linear_composite(df, avail_factors, cids_dm, 
                            complete_xcats = FALSE, new_xcat = "CP")
    df <- update_df(df, dfa)
  }
  
  if (length(avail_xfactors) > 0) {
    dfa <- linear_composite(df, avail_xfactors, cids_dm,
                            complete_xcats = FALSE, new_xcat = "XCP")
    df <- update_df(df, dfa)
    df <- update_df(df, make_zn_scores(df, "XCP", cids_dm))
    
    if ("XDU10v2RYLD_ZN" %in% avail) {
      dfa <- linear_composite(df, c("XCP_ZN", "XDU10v2RYLD_ZN"), cids_dm,
                              complete_xcats = FALSE, new_xcat = "BCP")
      df <- update_df(df, dfa)
    }
  }
  
  return(df)
}

# -----------------------------------------------------------------------------
# 12. MAIN WORKFLOW
# -----------------------------------------------------------------------------

run_analysis <- function() {
  message("=== STEP 1: Downloading Data ===")
  dfx <- tryCatch(download_macro_data(), error = function(e) NULL)
  
  if (is.null(dfx) || nrow(dfx) < 500) {
    message("\n=== FALLING BACK TO SYNTHETIC DATA ===")
    dfx <- generate_synthetic_data()
  }
  
  message("\nDataset: ", nrow(dfx), " rows, ", 
          length(unique(dfx$xcat)), " series, ",
          length(unique(dfx$cid)), " countries")
  
  message("\n=== STEP 2: Flattening Returns ===")
  dfa_flat <- calc_flattening_returns(dfx, cids_dm)
  dfx <- update_df(dfx, dfa_flat)
  
  message("\n=== STEP 3: Features ===")
  dfx <- build_labor(dfx, cids)
  dfx <- build_inflation(dfx, cids)
  dfx <- build_financial(dfx, cids)
  dfx <- build_overheating(dfx, cids)
  dfx <- build_debt(dfx, cids)
  dfx <- build_confidence(dfx, cids)
  dfx <- build_qe(dfx, cids)
  dfx <- build_real_yields(dfx, cids)
  
  message("\n=== STEP 4: Signals ===")
  dfx <- construct_signals(dfx, cids)
  
  message("\n=== STEP 5: Visualizations ===")
  p1 <- view_timelines(dfx, c("DU02YYLD_NSA", "DU10YYLD_NSA"), cids_dm, 
                       title = "Government Bond Yields")
  print(p1)
  
  if ("DU10v02DVXR" %in% unique(dfx$xcat)) {
    p2 <- view_timelines(dfx, "DU10v02DVXR", cids_dm, cumsum = TRUE,
                         title = "Cumulative 2s-10s Flattening Returns")
    print(p2)
  }
  
  avail_factors <- intersect(c("LABTIGHT_ZN", "XINF_ZN", "XFIN_ZN", 
                               "OVERHEAT_ZN", "DEBTIMPROVE_ZN"), unique(dfx$xcat))
  if (length(avail_factors) > 0) {
    for (f in avail_factors) {
      p <- view_timelines(dfx, f, cids_dm, title = paste("Factor:", f))
      print(p)
    }
  }
  
  message("\n=== Done ===")
  invisible(dfx)
}

# Execute
# dfx_final <- run_analysis()
