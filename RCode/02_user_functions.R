### Meta ###

# Author: Andreas Hoehn
# Version: 1.0
# Date:  2022-08-22
# About: this file contains all user-written functions 

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [1] Trim White Space After Splitting Strings

# input: a string with a white space at the end
# output: the cut string without a space at the end

.TrimWhiteSpace <- function(x) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [2] Repetitive Cleaning of Death Data from NRS for Scotland ###

# input: from the folder where the queried data is stored the data set will 
#        be loaded automatically, year, vector containing strings to name ages
# output: the clean data set 

.CleanSco <- function(input_year, input_sex, input_age_name){
  
  if(input_sex == "Male") {
    
  # read data for males
  data <- 
    data.table::data.table(read.csv(paste0("RData/ScotlandDownload/deaths-time-series-20-dt_",
                    input_year,".csv"), header = FALSE)[c(8:39), c(23,25:44)])
  } 
  if(input_sex == "Female") {
  data <- 
    data.table::data.table(read.csv(paste0("RData/ScotlandDownload/deaths-time-series-20-dt_",
                    input_year,".csv"), header = FALSE)[c(8:39), c(45,47:66)])
  }
  
  # assign correct column names based on E&W death data set 
  colnames(data) <- c("geography_name", unique(input_age_name)) 
  
  # melt into long format
  data_long <- melt(data, id.vars = c("geography_name"), value.name = "Dx")
  
  # ensure for comma as thousand separator to be removed in all deaths 
  data_long[, Dx := gsub(",","",as.character(Dx))]
  
  # rename age column
  setnames(data_long, old = "variable", new = "age_name")
  
  # setkey
  setkey(data_long, geography_name, age_name)
  
  # recode zero deaths and make numeric
  data_long[Dx == "-", Dx := 0]
  data_long[, Dx := as.numeric(Dx)]
  
  # remove 2 from geography names 
  data_long[, geography_name := gsub("2","",as.character(geography_name))]
  
  data_long[, geography_name := .TrimWhiteSpace(geography_name)]
  
  # ensure correct auxiliary variables + var types
  data_long[, date_name := input_year]
  data_long[, geography_name := as.character(geography_name)]
  data_long[, geography_code := as.character("na")]
  data_long[, sex_name       := as.character(input_sex)]
  data_long[, age_name       := as.character(age_name)]
  
  # introduce ctr_code
  data_long[, ctr_code := as.character("S")]
  
  setcolorder(data_long, c("date_name", "ctr_code", "geography_name",
                           "geography_code", "sex_name", "age_name", "Dx"))        
  
  return(data_long)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [3] Creating 3-Year Rolling Averages ###

# input: an input data set in long format with lower/upper year boundaries
# output: one dataset having Nx and Dx aggregated to get 3-year rolling averages 

# aggregate by what will later be ID and keep variables defining IDs
.Rolling3YearAverage <- function(input_data, year_lower, year_upper, definitions) {
  input_data <- copy(input_data[date_name  >= year_lower & date_name  <= year_upper])
  output_data <- input_data[, .(Nx = sum(Nx), Dx = sum(Dx),
                                ctr_code = unique(ctr_code)), 
                            by = c("geography_code","geography_name","sex_name","age_code")]
 
  # excluding geographies
  output_data[, excluded_geography := 0]
  output_data[geography_name %in% definitions$excluding_geographies_list,
    excluded_geography := 1]
  
  # correcting Nx Values 
  output_data[Nx < Dx, Nx := Dx ][   # no deaths larger than exposure pop
    Nx == 0, Nx := 1]                # min N = 1 per age group for TOPALS to run
  
  # estimate mx
  output_data[, mx := Dx / Nx]
  
  # assign year
  output_data[, year := year_upper - 1]
  
  # explicit return
  return(output_data)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [4] Estimate Life Tables Based on Chiang 1984 ###

# input: a long-format data set containing Nx and Dx, an ID for each group
# output: a life table for this data set snippet

.LTBChiang5y <- function(input_data, input_ID) {
  
  # define input data
  input_data <- copy(input_data[ID == input_ID, ])
  
  # assignment #
  Dx <- input_data$Dx
  Nx <- input_data$Nx
  mx <- input_data$mx
  
  age_code  <- levels(as.factor(input_data$age_code))
  n                <- c(1, 4, rep(5, length(age_code)-3),0)
  ax               <- c(0.08, 2, rep(2.5,  length(age_code)-3),0)
  ax[length(ax)]   <- 1 / mx[length(mx)]
  qx               <- (n*mx) / (1+(n-ax)*mx)
  qx[length(qx)]   <- 1.0
  px               <- 1.0 - qx
  lx               <- rep(x = 0, times = length(mx))
  lx[1]            <- 100000
  for (i in 2:(length(lx))){
    lx[i]          <- lx[i-1] * px[i-1]
  }
  dx              <- lx * qx
  dx[length(dx)]  <- lx[[length(dx)]]
  Lx              <- n * c(lx[-1],0) + ax * dx
  Lx[length(Lx)]  <- lx[length(Lx)] / mx[length(Lx)]
  Tx              <- rev(cumsum(rev(Lx)))
  ex              <- Tx / lx
  ltb_return <- data.table::data.table(
    ID = unique(input_data$ID),
    ctr_code = unique(input_data$ctr_code),
    geography_code = unique(input_data$geography_code),
    geography_name = unique(input_data$geography_name),
    year = unique(input_data$year),
    sex_name = unique(input_data$sex_name),
    age_code, n, Nx, Dx, mx, qx, ax, lx, dx, Lx, Tx, ex)
  # explicit return
  return(ltb_return)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [5] Query Std from HMD ###

# input:
# output:
# about:

.ReadStdPop <- function(country, item, year) {
  data <-  data.table::data.table(readHMDweb(country, item,
        definitions$HMD_access[1], definitions$HMD_access[2], fixup = TRUE))
  data <- data[Year == year, .(mx)][1:111] # single ages
  output_vector <- log(data$mx)
  return(output_vector)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6 - A] Run TOPALS Model ###

# About: A modified Version of Carl Schmertmanns R Code for fitting TOPALS Model
# forked from GitHub and modified
# URL: https://github.com/schmert/TOPALS/blob/master/TOPALS_fit.R
# further details: https://github.com/schmert/TOPALS/blob/master/TOPALS_fitting_with_grouped_data.pdf
# Accessed: 2022-08-18

.TOPALS_fit = function(N, D, std,
                       age_group_bounds,
                       knot_positions, 
                       penalty_precision,
                       max_iter,
                       alpha_tol,
                       details) {
  
  ## single years of age from 0 to (A-1)
  A   <- length(std)
  age <- 0:(A-1)
  
  ## B is an AxK matrix. Each column is a linear B-spline basis function
  B <- splines::bs(age, knots = knot_positions, degree = 1 )
  K <- ncol(B) 
  
  D1 <- diff(diag(K), diff = 1)
  P  <- penalty_precision * crossprod(D1)
  
  ## number and width of age groups
  G     <- length(age_group_bounds) -1   
  nages <- diff(age_group_bounds)
  
  ## weighting matrix for mortality rates (assumes uniform
  ## distribution of single-year ages within groups)
  W = matrix(0, nrow = G, ncol = A, 
             dimnames = list(head(age_group_bounds, -1), age))
  
  offset <- 0
  for (g in 1:G) {
    W[g, offset + 1:nages[g]] <- 1/nages[g]
    offset <- offset + nages[g]
  }
  
  ## penalized log lik function
  Q <- function(alpha) {
    M <- W %*% exp(std + B %*% alpha)
    likelihood <- sum(D * log(M) - N * M)
    penalty    <- 1/2 * t(alpha) %*% P %*% alpha
    return(likelihood - penalty)
  }
  
  #------------------------------------------------
  # iteration function: 
  # next alpha vector as a function of current alpha
  #------------------------------------------------
  
  next_alpha <- function(alpha) {
    mu <- as.vector(exp( std + B %*% alpha))
    M  <- as.vector(W %*% mu)
    
    Dhat <- N * M
    
    X <- W %*% diag(mu) %*% B
    A <- diag(N/M)
    
    y <- (D-Dhat)/N + X %*% alpha
    
    updated_alpha <- solve(t(X) %*% A %*% X + P, t(X) %*% A %*% y)
    return(as.vector(updated_alpha))
  }
  
  ## main iteration:     
  a <- rep(0, K)
  
  niter = 0
  repeat {
    niter      <- niter + 1
    last_param <- a
    a          <- next_alpha(a)  # update
    change     <- a - last_param
    
    converge <- all(abs(change) < alpha_tol)
    overrun  <- (niter == max_iter)
    
    if (converge | overrun) { break }
    
  } # repeat
  
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    
    mu    <- as.vector(exp(std + B %*% a))
    M     <- as.vector(W %*% mu)
    dhat  <- N * M
    
    X     <- W %*% diag(mu) %*% B
    A     <- diag(N/M)
    
    covar <- solve(t(X) %*% A %*% X + P)
    
    return( list( alpha             = a, 
                  D                 = D,
                  N                 = N,
                  age_group_bounds  = age_group_bounds,
                  knots             = knot_positions,
                  std               = std,
                  B                 = B,
                  logm              = std + B %*% a,
                  covar             = covar,
                  Qvalue            = Q(a),
                  converge          = converge, 
                  maxiter           = overrun))
  } else return(a) 
  
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6 - B] Kannisto Model ###

# about: what has been smoothed with TOPALS goes then through Kannisto Model
# input: age boundaries and the logmx rates that came out of TOPALS
# output: a vector containing the TOPALS smoothed death rates, but Kannisto Model
# run over the ages 90+

.KannistoModel <- function(mx) {
  
  logmx <- log(mx)
  
  fit_ages <- 70:89
  extrapolated_ages <- 90:110
  
  # estimate logits
  fit_mx     <- exp(logmx[71:90])
  fit_logits <- log(fit_mx / (1 - fit_mx))
  logit_coef <- coef(lm(fit_logits ~ fit_ages))
  log_a      <- logit_coef[1]
  b          <- logit_coef[2]
  
  # multiply with age etc.
  extrapolated_logits <- log_a + (b * extrapolated_ages)
  extrapolated_logmx  <- log(1 / (1 + exp(-extrapolated_logits)))
  
  # combine vectors 
  output   <- exp(c(logmx[1:90], extrapolated_logmx))
  
  # explicit return
  return(output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [6 - C] Run TOPALS + Kannisto Model ###

# about: this function combines TOPALS and Kannisto to be run together
# input: a 5-year age group life table and a standard population
# output: a data.table containing a LTB with 1-year age groups which has been 
# subject to TOPALS smoothing and Kannisto Model

.RunTOPALSandKannisto <- function(data_input, std_pops) {
  
  # define age boundaries
  age_boundaries <- c(0, 1, seq(5,85,5), 95) 

  # Scotland Males # 
  if(unique(data_input$ctr) == "S" & unique(data_input$sex_name) == "Male") {
    std_input <- std_pops[[1]]
  }
  # Scotland Females # 
  if(unique(data_input$ctr) == "S" & unique(data_input$sex_name) == "Female") {
    std_input <- std_pops[[2]]
  }
  # Other Males # 
  if(unique(data_input$ctr) != "S" & unique(data_input$sex_name) == "Male") {
    std_input <- std_pops[[3]]
  }
  # Other Females # 
  if(unique(data_input$ctr) != "S" & unique(data_input$sex_name) == "Female") {
    std_input <- std_pops[[4]]
  }
  
  # estimate via TOPALS for grouped data
  fit <- .TOPALS_fit(N = data_input$Nx,
                     D = data_input$Dx,
                     std = std_input,
                     age_group_bounds = age_boundaries,
                     knot_positions     = c(0, 1, 10, 20, 40, 60, 80),
                     penalty_precision = 2,
                     max_iter = 100,
                     alpha_tol = 0.00005,
                     details = TRUE)
  
  # create 1-year age group LTB based on fit
  mx_topals <- as.vector(exp(fit$logm))
  mx_std    <- as.vector(exp(fit$std))
  
  # apply kannisto thatcher smoothing 
  mx_topals_kannisto <- .KannistoModel(mx = mx_topals)
  
  # run 1-year age group life table 
  age_code         <- seq(0,110,1)
  n                <- rep(1, length(mx_topals_kannisto))
  ax               <- c(0.08, rep(0.5,  length(mx_topals_kannisto)-2),0)
  ax[length(ax)]   <- 1 / mx_topals_kannisto[length(mx_topals_kannisto)]
  qx               <- (n*mx_topals_kannisto) / (1+(n-ax)*mx_topals_kannisto)
  qx[length(qx)]   <- 1.0
  px               <- 1.0 - qx
  lx               <- rep(x = 0, times = length(mx_topals_kannisto))
  lx[1]            <- 100000
  for (i in 2:(length(lx))){
    lx[i]          <- lx[i-1] * px[i-1]
  }
  dx              <- lx * qx
  dx[length(dx)]  <- lx[[length(dx)]]
  Lx              <- n * c(lx[-1],0) + ax * dx
  Lx[length(Lx)]  <- lx[length(Lx)] / mx_topals_kannisto[length(Lx)]
  Tx              <- rev(cumsum(rev(Lx)))
  ex              <- Tx / lx
  ltb_return <- data.table::data.table(
    ID = unique(data_input$ID),
    ctr_code = unique(data_input$ctr_code),
    geography_code = unique(data_input$geography_code),
    geography_name = unique(data_input$geography_name),
    year  = unique(data_input$year),
    sex_name = unique(data_input$sex_name), 
    age_code, n, mx_topals, mx_topals_kannisto, mx_std,
    qx, ax, lx, dx, Lx, Tx, ex)
  
  # explicit return
  return(ltb_return)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

### [7] Mapping Age Structure of Survey and Life Tables ###

# about: this function ensures that life tables can be merged with survey utils 
# input: the life table data set 
# output: the same life table data set but with a age_code_survey variable 
# matching the format of what we obtained from understanding society 

.MappingAgesSurveyNOMIS <- function(data_input) {
  data_output <- copy(data_input)
  data_output[, age_code_survey := 999]
  data_output[age_code < 20, age_code_survey := 20]
  data_output[age_code >= 20 & age_code <= 24, age_code_survey := 24]
  data_output[age_code >= 25 & age_code <= 29, age_code_survey := 29]
  data_output[age_code >= 30 & age_code <= 34, age_code_survey := 34]
  data_output[age_code >= 35 & age_code <= 39, age_code_survey := 39]
  data_output[age_code >= 40 & age_code <= 44, age_code_survey := 44]
  data_output[age_code >= 45 & age_code <= 49, age_code_survey := 49]
  data_output[age_code >= 50 & age_code <= 54, age_code_survey := 54]
  data_output[age_code >= 55 & age_code <= 59, age_code_survey := 59]
  data_output[age_code >= 60 & age_code <= 64, age_code_survey := 64]
  data_output[age_code >= 65 & age_code <= 69, age_code_survey := 69]
  data_output[age_code >= 70 & age_code <= 74, age_code_survey := 74]
  data_output[age_code >= 75 & age_code <= 79, age_code_survey := 79]
  data_output[age_code >= 80 & age_code <= 84, age_code_survey := 84]
  data_output[age_code >= 85, age_code_survey := 85]
  return(data_output)
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

