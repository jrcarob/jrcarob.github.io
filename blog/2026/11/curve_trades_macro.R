# =============================================================================
# 1. PACKAGES
# =============================================================================

# FRED API KEY: 93d220ed3522c2a638c363198acf3eca

# Install required packages if needed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

install_if_missing("tidyverse")
install_if_missing("lubridate")
install_if_missing("zoo")
install_if_missing("xts")
install_if_missing("quantmod")
install_if_missing("RcppRoll")
install_if_missing("ggplot2")
install_if_missing("patchwork")
install_if_missing("corrplot")
install_if_missing("PerformanceAnalytics")
install_if_missing("TTR")
install_if_missing("httr")
install_if_missing("jsonlite")
install_if_missing("DBI")
install_if_missing("RSQLite")
install_if_missing("tidymodels")
install_if_missing("broom")
install_if_missing("scales")

library(tidyverse)
library(lubridate)
library(zoo)
library(xts)
library(RcppRoll)
library(ggplot2)
library(patchwork)
library(corrplot)
library(PerformanceAnalytics)

# =============================================================================
# 2. PARAMETERS AND CONFIGURATION
# =============================================================================

START_DATE <- as.Date("2000-01-01")
END_DATE <- Sys.Date()

# Cross-sectional lists
cids_dm <- sort(c("AUD", "CAD", "CHF", "EUR", "GBP", "JPY", "NOK", "NZD", "SEK", "USD"))
cids_g2 <- c("EUR", "USD")
cids_xg2 <- sort(setdiff(cids_dm, cids_g2))
cids_ez <- c("DEM", "ESP", "FRF", "ITL")  # for EUR
cids <- sort(union(cids_dm, cids_ez))

# =============================================================================
# ALTERNATIVE DATA SOURCES
# =============================================================================

# 1. FRED (Federal Reserve Economic Data) - Free
#    https://fred.stlouisfed.org/
#    Package: fredr

install_if_missing("fredr")
library(fredr)

# Set your FRED API key (free at https://fred.stlouisfed.org/docs/api/api_key.html)
fredr_set_key("YOUR_FRED_API_KEY")

# 2. IMF Data - Free
#    https://data.imf.org/
#    Package: imfr

install_if_missing("imfr")
library(imfr)

# 3. OECD Data - Free
#    https://data.oecd.org/
#    Package: OECD

install_if_missing("OECD")
library(OECD)

# 4. World Bank - Free
#    Package: WDI

install_if_missing("WDI")
library(WDI)

# 5. ECB Statistical Data Warehouse - Free
#    https://sdw.ecb.europa.eu/

# 6. BIS (Bank for International Settlements) - Free
#    https://www.bis.org/statistics/index.htm

# 7. Quandl/NASDAQ Data Link - Some free, some paid
#    Package: Quandl

install_if_missing("Quandl")
library(Quandl)

# 8. Yahoo Finance (for market data) - Free
#    Package: quantmod

# 9. IMF International Financial Statistics
# 10. Bloomberg API (if you have terminal access)

# =============================================================================
# 4. CATEGORY DEFINITIONS
# =============================================================================

# Economic categories
labor <- c(
  "EMPL_NSA_P1M1ML12_3MMA",
  "EMPL_NSA_P1Q1QL4",
  "WFORCE_NSA_P1Y1YL1_5YMM",
  "WFORCE_NSA_P1Q1QL4_20QMM",
  "UNEMPLRATE_NSA_3MMA_D1M1ML12",
  "UNEMPLRATE_NSA_D1Q1QL4",
  "UNEMPLRATE_SA_D3M3ML3",
  "UNEMPLRATE_SA_3MMA",
  "UNEMPLRATE_SA_3MMAv5YMM",
  "UNEMPLRATE_SA_3MMAv10YMM"
)

growth <- c(
  "INTRGDP_NSA_P1M1ML12_3MMA",
  "IMPORTS_SA_P1M1ML12_3MMA",
  "NRSALES_SA_P1M1ML12_3MMA",
  "NRSALES_SA_P1Q1QL4",
  "RRSALES_SA_P1M1ML12_3MMA",
  "RRSALES_SA_P1Q1QL4",
  "RPCONS_SA_P1M1ML12_3MMA",
  "RPCONS_SA_P1Q1QL4",
  "RGDPTECH_SA_P1M1ML12_3MMA",
  "RGDP_SA_P1Q1QL4_20QMM"
)

inflation <- c(
  "CPIH_SA_P1M1ML12",
  "CPIH_SJA_P6M6ML6AR",
  "CPIC_SA_P1M1ML12",
  "CPIC_SJA_P6M6ML6AR",
  "INFTEFF_NSA",
  "INFE2Y_JA",
  "INFE5Y_JA",
  "HPI_SA_P1Q1QL4",
  "HPI_SA_P1M1ML12_3MMA"
)

monliq <- c(
  "PCREDITBN_SJA_P1M1ML12",
  "INTLIQGDP_NSA_D1M1ML1",
  "INTLIQGDP_NSA_D1M1ML3",
  "INTLIQGDP_NSA_D1M1ML6"
)

fiscal <- c(
  "GGDGDPRATIOX10_NSA",
  "USDGDPWGT_SA_3YMA"
)

surveys <- c(
  "CCSCORE_SA", "CCSCORE_SA_3MMA", "CCSCORE_SA_D3M3ML3", "CCSCORE_SA_D1Q1QL1",
  "SBCSCORE_SA", "SBCSCORE_SA_3MMA", "SBCSCORE_SA_D3M3ML3", "SBCSCORE_SA_D1Q1QL1",
  "MBCSCORE_SA", "MBCSCORE_SA_3MMA", "MBCSCORE_SA_D3M3ML3", "MBCSCORE_SA_D1Q1QL1"
)

ecos <- c(labor, inflation, growth, monliq, fiscal, surveys)

# Market categories
rets <- c("DU02YXR_NSA", "DU10YXR_NSA")
ylds <- c("DU02YYLD_NSA", "DU10YYLD_NSA")
markets <- c(rets, ylds)

# All categories
xcats <- c(ecos, markets)

# Tickers
extras <- c("USD_GB05YXR_NSA", "USD_EQXR_NSA")
tickers <- c(
  paste(rep(cids, each = length(xcats)), xcats, sep = "_"),
  extras
)

cat(sprintf("%d tickers to download\n", length(tickers)))

# =============================================================================
# 5. DATA STRUCTURE (long format dataframe)
# =============================================================================

# The JPMaQS data comes in long format:
# real_date | cid | xcat | value
# 
# We'll work with this structure throughout

# Create sample data structure (replace with actual download)
create_sample_data <- function() {
  dates <- seq(START_DATE, END_DATE, by = "day")
  
  expand.grid(
    real_date = dates,
    cid = cids_dm,
    xcat = xcats,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(value = NA_real_)
}

# =============================================================================
# 6. PRE-PROCESSING FUNCTIONS
# =============================================================================

# Equivalent to macrosynergy.management functions

#' Update dataframe with new data
update_df <- function(df, dfa) {
  # Remove overlapping rows
  df <- df %>%
    anti_join(
      dfa %>% select(real_date, cid, xcat),
      by = c("real_date", "cid", "xcat")
    )
  
  # Bind new data
  bind_rows(df, dfa)
}

#' Check availability
check_availability <- function(df, xcats, cids, missing_recent = FALSE) {
  df %>%
    filter(xcat %in% xcats, cid %in% cids) %>%
    group_by(cid, xcat) %>%
    summarise(
      start_date = min(real_date, na.rm = TRUE),
      end_date = max(real_date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = cid,
      values_from = c(start_date, end_date)
    )
}

# =============================================================================
# 7. PANEL CALCULATOR (Core Function)
# =============================================================================

#' Panel calculator - applies calculations panel-wise
panel_calculator <- function(df, calcs, cids, external_func = list()) {
  
  result_list <- list()
  
  for (calc in calcs) {
    # Parse calculation string
    # Format: "NEW_XCAT = expression"
    parts <- strsplit(calc, " = ")[[1]]
    new_xcat <- trimws(parts[1])
    expression_str <- trimws(parts[2])
    
    # Evaluate expression for each cid
    for (cid_val in cids) {
      cid_data <- df %>%
        filter(cid == cid_val) %>%
        pivot_wider(
          names_from = xcat,
          values_from = value
        )
      
      # Create environment with data and external functions
      env <- as.list(cid_data)
      env <- c(env, external_func)
      
      # Evaluate expression
      result <- tryCatch({
        eval(parse(text = expression_str), envir = env)
      }, error = function(e) {
        warning(sprintf("Error for %s, %s: %s", cid_val, new_xcat, e$message))
        rep(NA_real_, nrow(cid_data))
      })
      
      result_df <- tibble(
        real_date = cid_data$real_date,
        cid = cid_val,
        xcat = new_xcat,
        value = as.numeric(result)
      )
      
      result_list[[length(result_list) + 1]] <- result_df
    }
  }
  
  bind_rows(result_list)
}

# =============================================================================
# 8. Z-SCORE CALCULATION (make_zn_scores equivalent)
# =============================================================================

#' Calculate z-scores with sequential (expanding window) estimation
make_zn_scores <- function(df, xcat, cids, 
                           sequential = TRUE,
                           min_obs = 522,
                           est_freq = "m",
                           neutral = "zero",
                           pan_weight = 1,
                           thresh = 3,
                           postfix = "_ZN") {
  
  result_list <- list()
  
  for (cid_val in cids) {
    cid_data <- df %>%
      filter(cid == cid_val, xcat == !!xcat) %>%
      arrange(real_date)
    
    if (nrow(cid_data) == 0) next
    
    values <- cid_data$value
    
    if (sequential) {
      # Expanding window z-scores
      n <- length(values)
      z_scores <- numeric(n)
      
      for (i in seq_len(n)) {
        if (i < min_obs) {
          z_scores[i] <- NA_real_
          next
        }
        
        window_vals <- values[1:i]
        window_vals <- window_vals[!is.na(window_vals)]
        
        if (length(window_vals) < 10) {
          z_scores[i] <- NA_real_
          next
        }
        
        mu <- mean(window_vals, na.rm = TRUE)
        sigma <- sd(window_vals, na.rm = TRUE)
        
        if (sigma == 0 || is.na(sigma)) {
          z_scores[i] <- 0
        } else {
          z <- (values[i] - mu) / sigma
          # Winsorize at threshold
          z_scores[i] <- pmax(pmin(z, thresh), -thresh)
        }
      }
    } else {
      # Full-sample z-scores
      mu <- mean(values, na.rm = TRUE)
      sigma <- sd(values, na.rm = TRUE)
      z <- (values - mu) / sigma
      z_scores <- pmax(pmin(z, thresh), -thresh)
    }
    
    result_df <- tibble(
      real_date = cid_data$real_date,
      cid = cid_val,
      xcat = paste0(xcat, postfix),
      value = z_scores
    )
    
    result_list[[length(result_list) + 1]] <- result_df
  }
  
  bind_rows(result_list)
}

# =============================================================================
# 9. LINEAR COMPOSITE (linear_composite equivalent)
# =============================================================================

#' Linear composite of multiple categories
linear_composite <- function(df, xcats, cids, 
                             weights = NULL,
                             normalize_weights = TRUE,
                             complete_xcats = FALSE,
                             new_cid = NULL,
                             new_xcat = NULL) {
  
  result_list <- list()
  
  target_cids <- if (!is.null(new_cid)) new_cid else cids
  
  for (cid_val in target_cids) {
    
    if (!is.null(new_cid)) {
      # Weighted average across cids (for EUR aggregation)
      cid_data <- df %>%
        filter(cid %in% cids, xcat %in% xcats)
      
      if (!is.null(weights)) {
        # Apply weights
        weight_data <- df %>%
          filter(cid %in% cids, xcat == weights) %>%
          select(real_date, cid, weight = value)
        
        cid_data <- cid_data %>%
          left_join(weight_data, by = c("real_date", "cid"))
      }
      
      # Average across cids
      composite <- cid_data %>%
        group_by(real_date, xcat) %>%
        summarise(
          value = mean(value, na.rm = TRUE),
          .groups = "drop"
        )
      
    } else {
      # Average across xcats for each cid
      cid_data <- df %>%
        filter(cid == cid_val, xcat %in% xcats)
      
      if (!complete_xcats) {
        # Use available categories
        composite <- cid_data %>%
          group_by(real_date) %>%
          summarise(
            value = mean(value, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        # Require all categories
        composite <- cid_data %>%
          group_by(real_date) %>%
          filter(n() == length(xcats)) %>%
          summarise(
            value = mean(value, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }
    
    result_df <- composite %>%
      mutate(
        cid = if (!is.null(new_cid)) new_cid else cid_val,
        xcat = if (!is.null(new_xcat)) new_xcat else "COMPOSITE"
      ) %>%
      select(real_date, cid, xcat, value)
    
    result_list[[length(result_list) + 1]] <- result_df
  }
  
  bind_rows(result_list)
}

# =============================================================================
# 10. ANNUITY FUNCTION (for DV01 calculation)
# =============================================================================

annuity_df <- function(x, T, eps = 1e-8) {
  x_safe <- ifelse(abs(x) >= eps, x, NA_real_)
  out <- (1 - exp(-x_safe * T)) / x_safe
  out <- ifelse(abs(x) >= eps, out, T)
  return(out)
}

# =============================================================================
# 11. DV01-WEIGHTED FLATTENING RETURNS
# =============================================================================

calculate_flattening_returns <- function(df, cids) {
  
  calcs <- c(
    "DU02YYLD_DEC = DU02YYLD_NSA / 100",
    "DU10YYLD_DEC = DU10YYLD_NSA / 100",
    "A2 = annuity_df(DU02YYLD_DEC, 2)",
    "A10 = annuity_df(DU10YYLD_DEC, 10)",
    "w10v2 = A10 / A2",
    "DU10v02DVXR = DU10YXR_NSA - w10v2 * DU02YXR_NSA"
  )
  
  panel_calculator(df, calcs, cids, external_func = list(annuity_df = annuity_df))
}

# =============================================================================
# 12. FEATURE CONSTRUCTION
# =============================================================================

# Labor market tightness
construct_labor_tightness <- function(df, cids) {
  
  calcs <- c(
    "XEMPL_NSA_P1M1ML12_3MMA = EMPL_NSA_P1M1ML12_3MMA - WFORCE_NSA_P1Y1YL1_5YMM",
    "UNEMPLRATE_SA_3MMAv10YMM_NEG = -UNEMPLRATE_SA_3MMAv10YMM"
  )
  
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  labtights <- unique(dfa$xcat)
  
  # Z-scores
  for (xc in labtights) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  labtightz <- paste0(labtights, "_ZN")
  
  # Combined factor
  dfa_composite <- linear_composite(df, labtightz, cids_dm, 
                                    complete_xcats = FALSE,
                                    new_xcat = "LABTIGHT")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "LABTIGHT", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  return(df)
}

# Excess CPI inflation
construct_inflation <- function(df, cids) {
  
  infs <- c("CPIH_SA_P1M1ML12", "CPIH_SJA_P6M6ML6AR", 
            "CPIC_SA_P1M1ML12", "CPIC_SJA_P6M6ML6AR")
  
  calcs <- paste0(infs, "vIET = (", infs, " - INFTEFF_NSA)")
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  xinfs <- unique(dfa$xcat)
  
  # Z-scores
  for (xc in xinfs) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  xinfz <- paste0(xinfs, "_ZN")
  
  # Combined factor
  dfa_composite <- linear_composite(df, xinfz, cids_dm,
                                    complete_xcats = FALSE,
                                    new_xcat = "XINF")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "XINF", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  return(df)
}

# Excess financial expansion
construct_financial <- function(df, cids) {
  
  calcs <- c(
    "LTNOMGROWTH = INFTEFF_NSA + RGDP_SA_P1Q1QL4_20QMM",
    "XPCREDIT_P1M1ML12 = PCREDITBN_SJA_P1M1ML12 - LTNOMGROWTH",
    "XHPI_P1M1ML12_3MMA = HPI_SA_P1M1ML12_3MMA - LTNOMGROWTH"
  )
  
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  xfins <- grep("^X", unique(dfa$xcat), value = TRUE)
  
  # Z-scores
  for (xc in xfins) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  xfinz <- paste0(xfins, "_ZN")
  
  # Combined factor
  dfa_composite <- linear_composite(df, xfinz, cids_dm,
                                    complete_xcats = FALSE,
                                    new_xcat = "XFIN")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "XFIN", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  return(df)
}

# Activity overheating
construct_overheating <- function(df, cids) {
  
  calcs <- c(
    "XIMPORTS_SA_P1M1ML12_3MMA = IMPORTS_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM - INFTEFF_NSA",
    "XNRSALES_SA_P1M1ML12_3MMA = NRSALES_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM - INFTEFF_NSA",
    "XRRSALES_SA_P1M1ML12_3MMA = RRSALES_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM",
    "XRPCONS_SA_P1M1ML12_3MMA = RPCONS_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM",
    "XRGDPTECH_SA_P1M1ML12_3MMA = RGDPTECH_SA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM",
    "XINTRGDP_NSA_P1M1ML12_3MMA = INTRGDP_NSA_P1M1ML12_3MMA - RGDP_SA_P1Q1QL4_20QMM"
  )
  
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  overheats <- unique(dfa$xcat)
  
  # Z-scores
  for (xc in overheats) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  overheatz <- paste0(overheats, "_ZN")
  
  # Combined factor
  dfa_composite <- linear_composite(df, overheatz, cids_dm,
                                    complete_xcats = FALSE,
                                    new_xcat = "OVERHEAT")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "OVERHEAT", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  return(df)
}

# Public debt sustainability
construct_debt <- function(df, cids) {
  
  xcat_fsc <- "GGDGDPRATIOX10_NSA"
  
  dict_trends <- list(
    DM = list(short = 1, long = 21, thresh = 10),
    WQ = list(short = 5, long = 63, thresh = 15),
    MY = list(short = 21, long = 252, thresh = 20)
  )
  
  # This requires rolling calculations - simplified version
  # In practice, you'd implement proper rolling window logic
  
  calcs <- c()
  fiscals <- c()
  
  for (params in dict_trends) {
    short <- params$short
    long <- params$long
    thresh <- params$thresh
    
    raw <- sprintf("%s_%dDv%dD", xcat_fsc, short, long)
    win <- sprintf("%sW%d_NEG", raw, thresh)
    
    # Note: This is a simplified version
    # Full implementation needs proper rolling window functions
    calcs <- c(calcs, sprintf("%s = rollmean(%s, %d) - lag(rollmean(%s, %d), %d)",
                              raw, xcat_fsc, short, xcat_fsc, long, short))
    fiscals <- c(fiscals, win)
  }
  
  # Simplified - would need xts/zoo for proper rolling
  # dfa <- panel_calculator(df, calcs, cids)
  
  # For now, create placeholder
  dfa <- tibble(
    real_date = as.Date(character()),
    cid = character(),
    xcat = character(),
    value = numeric()
  )
  
  df <- update_df(df, dfa)
  
  # Z-scores and composite (simplified)
  
  return(df)
}

# Economic confidence
construct_confidence <- function(df, cids) {
  
  confs <- grep("3MMA$", surveys, value = TRUE)
  
  # Combined factor
  dfa_composite <- linear_composite(df, confs, cids_dm,
                                    complete_xcats = FALSE,
                                    new_xcat = "CONFLEVEL")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "CONFLEVEL", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  # Confidence changes
  confchanges <- grep("D3M3ML3$|D1Q1QL1$", surveys, value = TRUE)
  
  dfa_composite2 <- linear_composite(df, confchanges, cids_dm,
                                     complete_xcats = FALSE,
                                     new_xcat = "CONFCHANGE")
  df <- update_df(df, dfa_composite2)
  
  dfa_rescore2 <- make_zn_scores(df, "CONFCHANGE", cids_dm)
  df <- update_df(df, dfa_rescore2)
  
  return(df)
}

# Quantitative easing
construct_qe <- function(df, cids) {
  
  cbal <- "INTLIQGDP_NSA"
  
  calcs <- c(
    sprintf("%s_D1M1ML6AR = %s_D1M1ML6 * sqrt(2)", cbal, cbal),
    sprintf("%s_D1M1ML3AR = %s_D1M1ML3 * sqrt(4)", cbal, cbal),
    sprintf("%s_D1M1ML1AR = %s_D1M1ML1 * sqrt(12)", cbal, cbal)
  )
  
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  qes <- unique(dfa$xcat)
  
  # Z-scores
  for (xc in qes) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  qez <- paste0(qes, "_ZN")
  
  # Combined factor
  dfa_composite <- linear_composite(df, qez, cids_dm,
                                    complete_xcats = FALSE,
                                    new_xcat = "QUANTEASE")
  df <- update_df(df, dfa_composite)
  
  # Re-score
  dfa_rescore <- make_zn_scores(df, "QUANTEASE", cids_dm)
  df <- update_df(df, dfa_rescore)
  
  return(df)
}

# Real yield differentials
construct_real_yields <- function(df, cids) {
  
  calcs <- c(
    "DU02YRYLD = DU02YYLD_NSA - INFE2Y_JA",
    "DU10YRYLD = DU10YYLD_NSA - 0.5 * (INFE5Y_JA + INFTEFF_NSA)",
    "XDU10v2RYLD = DU10YRYLD - DU02YRYLD - 1"
  )
  
  dfa <- panel_calculator(df, calcs, cids)
  df <- update_df(df, dfa)
  
  realdiffs <- grep("v2RYLD", unique(dfa$xcat), value = TRUE)
  
  # Z-scores
  for (xc in realdiffs) {
    dfa_zn <- make_zn_scores(df, xc, cids_dm)
    df <- update_df(df, dfa_zn)
  }
  
  return(df)
}

# =============================================================================
# 13. VISUALIZATION FUNCTIONS
# =============================================================================

#' View timelines (equivalent to msp.view_timelines)
view_timelines <- function(df, xcats, cids, ncol = 4,
                           same_y = TRUE, cumsum = FALSE,
                           title = NULL, height = 1.8) {
  
  plot_data <- df %>%
    filter(xcat %in% xcats, cid %in% cids) %>%
    mutate(value = if (cumsum) cummean(value) else value)
  
  p <- ggplot(plot_data, aes(x = real_date, y = value, color = xcat)) +
    geom_line() +
    facet_wrap(~cid, ncol = ncol, scales = if (same_y) "fixed" else "free_y") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    labs(title = title, x = NULL, y = NULL) +
    theme(legend.position = "bottom")
  
  return(p)
}

#' Correlation matrix
correl_matrix <- function(df, xcats, cids, freq = "M",
                          cluster = FALSE, title = NULL) {
  
  # Aggregate to frequency
  if (freq == "M") {
    plot_data <- df %>%
      filter(xcat %in% xcats, cid %in% cids) %>%
      mutate(yearmon = as.yearmon(real_date)) %>%
      group_by(cid, yearmon, xcat) %>%
      summarise(value = last(value), .groups = "drop") %>%
      pivot_wider(names_from = c(cid, xcat), values_from = value)
  }
  
  # Calculate correlation
  corr_mat <- plot_data %>%
    select(-yearmon) %>%
    cor(use = "pairwise.complete.obs")
  
  # Plot
  corrplot(corr_mat, method = "color", type = "upper",
           title = title, mar = c(0, 0, 1, 0))
}

# =============================================================================
# 14. SIGNAL CONSTRUCTION
# =============================================================================

construct_signals <- function(df, cids) {
  
  factorz <- c("LABTIGHT_ZN", "XINF_ZN", "XFIN_ZN", "OVERHEAT_ZN",
               "DEBTIMPROVE_ZN", "CONFLEVEL_ZN", "CONFCHANGE_ZN",
               "QUANTEASE_ZN", "XDU10v2RYLD_ZN")
  
  xfactorz <- setdiff(factorz, "XDU10v2RYLD_ZN")
  
  # Simple conceptual parity
  dfa_cp <- linear_composite(df, factorz, cids_dm,
                             complete_xcats = FALSE,
                             new_xcat = "CP")
  df <- update_df(df, dfa_cp)
  
  # Balanced conceptual parity (excluding real yields)
  dfa_xcp <- linear_composite(df, xfactorz, cids_dm,
                              complete_xcats = FALSE,
                              new_xcat = "XCP")
  df <- update_df(df, dfa_xcp)
  
  # Re-score balanced
  dfa_xcp_zn <- make_zn_scores(df, "XCP", cids_dm)
  df <- update_df(df, dfa_xcp_zn)
  
  # Balanced conceptual parity with real yields
  dfa_bcp <- linear_composite(df, c("XCP_ZN", "XDU10v2RYLD_ZN"), cids_dm,
                              complete_xcats = FALSE,
                              new_xcat = "BCP")
  df <- update_df(df, dfa_bcp)
  
  # Hybrid factors (simplified - would need proper implementation)
  # CPH, BCPH, etc.
  
  return(df)
}

# =============================================================================
# 15. PnL CALCULATION
# ==============================================================================

#' Naive PnL calculation
calculate_pnl <- function(df, sig, ret, cids, 
                          sig_op = "zn_score_pan",
                          thresh = 3,
                          rebal_freq = "monthly",
                          vol_scale = 10,
                          rebal_slip = 1) {
  
  # Get signal and returns
  sig_data <- df %>%
    filter(xcat == sig, cid %in% cids) %>%
    select(real_date, cid, signal = value)
  
  ret_data <- df %>%
    filter(xcat == ret, cid %in% cids) %>%
    select(real_date, cid, return = value)
  
  # Merge
  pnl_data <- sig_data %>%
    inner_join(ret_data, by = c("real_date", "cid"))
  
  # Calculate positions (z-scored signal)
  if (sig_op == "zn_score_pan") {
    pnl_data <- pnl_data %>%
      group_by(cid) %>%
      mutate(
        position = signal / thresh,  # Simplified
        position = pmax(pmin(position, 1), -1)
      ) %>%
      ungroup()
  }
  
  # Calculate PnL
  pnl_data <- pnl_data %>%
    mutate(
      pnl = position * return,
      pnl_cumsum = cumsum(pnl)
    )
  
  return(pnl_data)
}

# =============================================================================
# 16. MAIN WORKFLOW
# =============================================================================

main <- function() {
  
  # Step 1: Download data (placeholder - replace with actual download)
  cat("Downloading data...\n")
  # dfx <- download_jpmaqs_data(tickers, START_DATE, END_DATE, client_id, client_secret)
  
  # For testing, create empty structure
  dfx <- create_sample_data()
  
  # Step 2: Pre-processing
  cat("Pre-processing...\n")
  
  # Remove survey levels that are not quarterly frequency
  # (equivalent to the Python filtering)
  
  # Rename quarterly tickers to monthly equivalents
  dict_repl <- c(
    "EMPL_NSA_P1Q1QL4" = "EMPL_NSA_P1M1ML12_3MMA",
    "WFORCE_NSA_P1Q1QL4_20QMM" = "WFORCE_NSA_P1Y1YL1_5YMM",
    "UNEMPLRATE_NSA_D1Q1QL4" = "UNEMPLRATE_NSA_3MMA_D1M1ML12",
    "NRSALES_SA_P1Q1QL4" = "NRSALES_SA_P1M1ML12_3MMA",
    "RRSALES_SA_P1Q1QL4" = "RRSALES_SA_P1M1ML12_3MMA",
    "RPCONS_SA_P1Q1QL4" = "RPCONS_SA_P1M1ML12_3MMA",
    "HPI_SA_P1Q1QL4" = "HPI_SA_P1M1ML12_3MMA",
    "CCSCORE_SA" = "CCSCORE_SA_3MMA",
    "SBCSCORE_SA" = "SBCSCORE_SA_3MMA",
    "MBCSCORE_SA" = "MBCSCORE_SA_3MMA",
    "CCSCORE_SA_D1Q1QL1" = "CCSCORE_SA_D3M3ML3",
    "MBCSCORE_SA_D1Q1QL1" = "MBCSCORE_SA_D3M3ML3",
    "SBCSCORE_SA_D1Q1QL1" = "SBCSCORE_SA_D3M3ML3"
  )
  
  dfx <- dfx %>%
    mutate(xcat = recode(xcat, !!!dict_repl))
  
  # Step 3: Calculate flattening returns
  cat("Calculating flattening returns...\n")
  dfa_flat <- calculate_flattening_returns(dfx, cids)
  dfx <- update_df(dfx, dfa_flat)
  
  # Step 4: Construct features
  cat("Constructing features...\n")
  dfx <- construct_labor_tightness(dfx, cids)
  dfx <- construct_inflation(dfx, cids)
  dfx <- construct_financial(dfx, cids)
  dfx <- construct_overheating(dfx, cids)
  # dfx <- construct_debt(dfx, cids)  # Simplified
  dfx <- construct_confidence(dfx, cids)
  dfx <- construct_qe(dfx, cids)
  dfx <- construct_real_yields(dfx, cids)
  
  # Step 5: Construct signals
  cat("Constructing signals...\n")
  dfx <- construct_signals(dfx, cids)
  
  # Step 6: Visualization
  cat("Creating visualizations...\n")
  
  # Plot cumulative returns
  p1 <- view_timelines(dfx, c("DU02YXR_NSA", "DU10YXR_NSA"), cids_dm, 
                       cumsum = TRUE, ncol = 4)
  print(p1)
  
  # Plot flattening returns
  p2 <- view_timelines(dfx, "DU10v02DVXR", cids_dm,
                       cumsum = TRUE, ncol = 4,
                       title = "Cumulative 2s-10s flattening returns")
  print(p2)
  
  cat("Done!\n")
  return(dfx)
}

# Run
# result <- main()