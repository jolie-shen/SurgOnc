library(stringdist)
library(tidyverse)
library(httr)
library(jsonlite)

# library(mice) {bmd.pattern}

##### Convenience functions #####
bhead <- function(data, nrow = 6, ncol = nrow) {
    # A modification of head for me which simply limits both the columns and the rows instead of just the rows
    if (nrow(data) < ncol) nrow <- nrow(data)
    if (ncol(data) < ncol) ncol <- ncol(data) 
    if ('data.table' %in% class(data)) return(head(data, nrow)[,colnames(data)[1:ncol], with = F])
    # return(data[seq_len(nrow), seq_len(ncol), drop = F]) # if there are duplicate column names it will make them unique
    
    cn <- colnames(data)
    dout <- data[seq_len(nrow), seq_len(ncol), drop = F]
    colnames(dout) <- cn[seq_len(ncol)] # this is to prevent the automatic check.names = TRUE behavior of `[` 
    return(dout)
    # return(head(data, nrow)[,colnames(data)[1:ncol], drop = F]) 
}

btail <- function(data, nrow = 6, ncol = nrow) {
    # A modification of tail for me which simply limits both the columns and the rows instead of just the rows
    if (nrow(data) < ncol) nrow <- nrow(data)
    if (ncol(data) < ncol) ncol <- ncol(data) 
    if ('data.table' %in% class(data)) return(tail(data, nrow)[,colnames(data)[(ncol(data)-ncol + 1):ncol(data)], with = F])
    return(tail(data, nrow)[,colnames(data)[(ncol(data)-ncol + 1):ncol(data)], drop = F]) 
}

bmerge <- function(x, y, 
                   by = 0, 
                   by.x = by, 
                   by.y = by, 
                   all = FALSE, 
                   all.x = all,
                   all.y = all,
                   sort = TRUE, 
                   suffixes = c('.x','.y'),
                   incomparables = NULL, 
                   ...) {
    # A function that, by default, merges on rownames, preserving the rownames and not creating any additional columns
    # if by != 0, behaves normally as merge() does
    # generally I prefer not to use row.names and would prefer to use by.x and by.y
    
    if (by == 0 & by.x == by & by.y == by) {
        dout <- merge(x, y, 
                      by = by, by.x = by.x, by.y = by.y, 
                      all = all, all.x = all.x, all.y = all.y, 
                      sort = sort, suffixes = suffixes, incomparables = incomparables)
        row.names(dout) <- dout[, 'Row.names'] 
        dout[, 'Row.names'] <- NULL
        dout
        
    } else merge(x, y, 
                 by = by, by.x = by.x, by.y = by.y, 
                 all = all, all.x = all.x, all.y = all.y, 
                 sort = sort, suffixes = suffixes, incomparables = incomparables)
}

`%nin%` <- function(a, b) {
    ! (a %in% b)
}


bwide <- function(df, n = 6, ncol = nrow, nrow = n) {
    # taken from stackoverflow: https://stackoverflow.com/questions/44831594/print-tibble-with-column-breaks-as-in-v1-3-0
    # for preventing cutting off of tbl_df objects
    
    if(ncol > ncol(df)) ncol <- ncol(df)
    
    df %>%
        select(1:ncol) %>%
        head(n = nrow) %>%
        as.data.frame() %>%
        tibble:::shrink_mat(width = Inf, rows = NA, n = ncol, star = FALSE) %>%
        `[[`("table") 
}

ball.na <- function(...) {
  # Returns a logical, TRUE/FALSE, are all of the inputs NAs? 
  all(sapply(list(...), is.na))
}

twide <- function(tbldf, n = 6, ncols = Inf, nrows = n) {
    # I think this is the appropriate method to work on new tbl_df objects! 
    
    if(ncols > ncol(tbldf)) ncols <- ncol(tbldf)
    if(nrows > nrow(tbldf)) nrows <- nrow(tbldf)
    
    tbldf %>%
        dplyr::select(1:ncols) %>%
        head(n = nrows) %>%
        print(width = Inf, n = nrows)
}

bcor <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), spline = TRUE, ...) 
{
    # code here is adapted from this function: PerformanceAnalytics::chart.Correlation() and also 
    # here: https://stackoverflow.com/questions/15271103/how-to-modify-this-correlation-matrix-plot
    # I essentially create my own version of panel.smooth (bpanel.smooth) that simply removes the spline
    
    x = checkData(R, method = "matrix")
    if (missing(method)) 
        method = method[1]
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                          method, cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor)) 
            cex <- 0.8/strwidth(txt)
        test <- cor.test(x, y, method = method)
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", 
                                                                                  "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
        text(0.8, 0.8, Signif, cex = cex, col = 2)
    }
    f <- function(t) {
        dnorm(t, mean = mean(x), sd = sd.xts(x))
    }
    hist.panel = function(x, ...) {
        par(new = TRUE)
        hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
             main = "", breaks = "FD")
        lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
        rug(x)
    }
    
    bpanel.smooth <-function (x, y, col = "blue", bg = NA, pch = 18, 
                              cex = 0.8, col.smooth = "red", span = 2/3, iter = 3, spline = TRUE, ...) 
    {
        points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    }
    
    if(spline) {
        if (histogram) 
            pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
                  diag.panel = hist.panel, method = method, ...)
        else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
                   method = method, ...)
    } else {
        if (histogram) 
            pairs(x, gap = 0, lower.panel = bpanel.smooth, upper.panel = panel.cor, 
                  diag.panel = hist.panel, method = method, ...)
        else pairs(x, gap = 0, lower.panel = bpanel.smooth, upper.panel = panel.cor, 
                   method = method, ...)
    }
}


boverlap <- function(df, id = NULL) {
  # if there is an id column, indicate which colnum it corresponds to
  # assumes that data is all logical TRUE or FALSE
  # this has a very narrow use case, example usage is below
  if(!is.null(id)) {
    idcol <- df[, id]
    df <- cbind.data.frame(idcol, df[, -id])
    colnames(df)[1] <- 'idcol'
  } else {
    idcol <- seq(dim(df)[1])
    df <- cbind.data.frame(idcol, df)
    colnames(df)[1] <- 'idcol'
  }
  
  df_long <-
    tidyr::gather(df, key = 'key', value = 'classification', -idcol) %>% 
    filter(classification) %>% 
    select(-classification)
  
  return(
    do.call(table, df_long) %>%
    {t(.) %*% .} # %*% is matrix multiplication
  )
  
  
  # ----- Example ----
  # exdf <- data.frame(id = LETTERS[1:6],
  #                  blue1 = c(T,T,T,T,T,T),
  #                  blue2 = c(T,T,F,F,T,T),
  #                  red1 = c(T,F,T,F,T,F),
  #                  red2 = c(F,F,T,F,F,F))
  # 
  # boverlap(exdf, 1)
  # *Note that the diagonal tells you how many "positive" ids there are, this data really makes the most 
  # sense when it is in long format, because people can only match on TRUE cases
}

brename <- function(.data, oldnames, newnames) {
  # function for simple renaming of a data frame
  # oldnames is an input of the "old names" which you wish to change
  # newnames is an input of the "new names" that will be used--must be the same length/order
  
  cns <- colnames(.data)
  change_index <- match(oldnames, cns)
  # change_index <- change_index[!is.na(change_index)]
  cns[change_index] <- newnames
  colnames(.data) <- cns
  return(.data)
}

bcount <- function(df, ..., d = 3, cumn = FALSE) {

  if('n' %in% names(df)) print("Your data already has a column named 'n', this may cause problems...")
  
  newdf <- 
    df %>% 
    count(...) %>%
    mutate(nn = round(n / sum(n), d)) %>%
    arrange(desc(n))
    
  if(cumn) {
    newdf <- 
      newdf %>%
      mutate(cumn = round(cumsum(n / sum(n)), d))
    }
    
  return(newdf)
}

bany_na <- function(.data, removeNARows = T) {
  
  .out <- 
    .data %>%
    mutate(anyNAs = pmap_lgl(., function(...) any(sapply(list(...), anyNA))))
  
  if(removeNARows) {
    .out <- .out %>% filter(!anyNAs)
  }
  
  return(.out)
}

# table2_disease_combo %>% 
#   select(rowname, totaln_industry, totaln_other, totaln_nih) %>% 
#   tidyr::gather(key = 'key', value = 'value', -rowname) %>%
#   {xtabs(value ~ key + rowname, data = .)}

brename <- function(.data, old, new) {
  # .data <- table1_disease_total
  # old <- names(table1_disease_total)[-1]
  # new <- paste0(names(table1_disease_total)[-1], '_total')
  
  colnames(.data)[match(old, colnames(.data)) %>% {.[!is.na(.)]}] <- new
  
  return(.data)
}


bchisqr <- function(.data, categories, ..., type = c('long','wide')) {
  # chisquare function using syntax that seems pretty common sense when working with data frame data...
  
  # example syntax: table2_disease_combo %>% bchisqr(categories = rowname, totaln_industry, totaln_nih, totaln_other, type = 'wide')
  # example syntax: table2_disease_combo %>% bchisqr(categories = rowname, industry_any3, type = 'long')
  # see here for more:
  #     https://www.statmethods.net/stats/frequencies.html
  #     https://stats.stackexchange.com/questions/92627/how-to-use-the-chi-squared-test-to-determine-if-data-follow-the-poisson-distribu
  #     https://mgimond.github.io/Stats-in-R/ChiSquare_test.html
       
  categ <- enquo(categories)
  vvars <- enquos(...)
  
  if(identical(type, c('long', 'wide'))) stop("You must enter whether this is long or wide data!")
  
  if(type == 'wide') {
    longdata <- 
      .data %>% 
      select(!!categ, !!! vvars) %>%
      tidyr::gather(key = 'key', value = 'value', - !! categ)
  } else if(type == 'long') {
    longdata <- .data
  }
  
  freqtable <- 
    xtabs(as.formula(paste0('value ~ key + ', quo_name(categ))), data = longdata)
  
  # check for any that have 0 observations
  mincheck <- 
      freqtable %>% 
      as.data.frame() %>% 
      dplyr::rename(myrowname = 1) %>%
      group_by(myrowname) %>% 
      summarise(totaln = sum(Freq)) %>% 
      pull(totaln) %>% 
      min()
  if(mincheck == 0) {
      print("You have level(s) with 0 observations! Your Chi-Squared result will be NaN")
      print(freqtable %>% 
                as.data.frame() %>% 
                rename(myrowname = 1) %>%
                group_by(myrowname) %>% 
                summarise(totaln = sum(Freq)) %>%
                filter(totaln == 0))
  }
  
  xsqr <- chisq.test(freqtable)
  
  return(xsqr)
  
}

bmatch_fuzzy_strings <- function(target_string, candidate_names) {
  # function for performing fuzzy string matching and returning the lowest (best) score and any values that share that score
  
  distances <- stringdist::stringdist(target_string, candidate_names)
  
  min_score <- min(distances, na.rm = TRUE) 
  min_locations <- which(distances == min_score) 
  
  min_values <- candidate_names[min_locations]
  names(min_values) <- rep(x = min_score, length(min_locations))
  
  return(min_values)
}

blogit <- function(p) {
  # perform logit calculation on p-values...essentially calculates the log-odds
  log((p / (1 - p)))
}

bexpit <- function(logodds) {
  # takes log-odds and gives you a probability. This is the inverse logit
  exp(logodds) / (1 + exp(logodds))
}


bpivotwider_single_factor_to_logical <- function(inputdata, column, makenames = FALSE, add_prefix = NULL) {
  
  # example call: bpivotwider_single_factor_to_logical(full_psych_df, 'industry_any3', FALSE, 'industry_any3_individual')
  
  # Take a data frame and a column that has factors, and create new column, with each factor level being a column...
    
  .data <- 
    inputdata %>%
    mutate(always_true_col = TRUE) %>%
    tibble::rowid_to_column(var = 'rowid_for_bpivotwider')
  
  stable_cols <- setdiff(colnames(.data), column)
  newdata <- 
    .data %>% 
    filter(!is.na(!! rlang::sym(column)))
  
  if(!is.null(add_prefix) & !identical(add_prefix, FALSE)) {
    if(identical(TRUE, add_prefix)) add_prefix <- quo_name(column)
    
    newdata <-
      newdata %>%
      mutate(!! quo_name(column) := paste0(add_prefix, '_', !! rlang::sym(column)))
      # mutate(!! quo_name(column) := case_when(
      #   is.na(!! rlang::sym(column)) ~ NA,
      #   TRUE ~ paste0(column, '_', !! rlang::sym(column)))
      # )
  }
  
    newdata <- 
    newdata %>%
    pivot_wider(names_from = column,
                values_from = always_true_col)
  
  if(makenames) {
    fixcols <- 
      make.names(colnames(newdata))
    
    newcol_positions <- which(colnames(newdata) %nin% stable_cols)
    
    colnames(newdata)[newcol_positions] <- fixcols[newcol_positions]
  }
  
  newcols <- setdiff(colnames(newdata), stable_cols)
  
  newdata2 <- 
    newdata %>%
    select(rowid_for_bpivotwider, one_of(newcols)) %>%
    mutate_at(vars(-one_of('rowid_for_bpivotwider')), .funs = list(~ if_else(is.na(.), FALSE, .))) # convert NA into FALSE
  
  left_join(.data, 
            newdata2,
            by = 'rowid_for_bpivotwider') %>%
    select(-rowid_for_bpivotwider, 
           -always_true_col)
}

testfunction <- function(.data, column) {
  
  # column <- enquo(column)
  
  .data %>%
    # mutate(quo_name(quo(column)) := sqrt(!! rlang::sym(column))
    mutate(!! quo_name(column) :=  paste0(column, '_', !! rlang::sym(column)))
}

bpivotwider_multiple_factors_to_logical <- function(.data, columns, makenames = FALSE, add_prefix = FALSE) {
  
  input_list <- c(list(.data),
                  as.list(columns))
  
  Reduce(f = function(x, y) bpivotwider_single_factor_to_logical(x, column = y, makenames = makenames, add_prefix = add_prefix),
         x = input_list)
  
}

br_mice_methods_generation <- function(dataset, binary = 'logreg', multi = 'polyreg', continuous = 'norm', exclude_variables = NULL) {
  
  # use this function to generate an input methods vector for use with mice::mice() for multiple imputation
  # use methods(mice) to see list of available methods
  # example call: 
  # methods_input_imp_china <- br_mice_methods_generation(full_china_imp_vars_df, 
  #                                                     binary = 'logreg', multi = 'polyreg', continuous = 'norm')
  #
  
  input_classes <- sapply(dataset, class) 
  unique_classes <- unique(input_classes)
  if(any(unique_classes %nin% c('factor', 'numeric'))) stop("The mice algorithm requires only factor and numeric data, no logicals, etc")
  
  lookup_method_vector = c(continuous, binary, multi)
  names(lookup_method_vector) <- c('numeric', 'br_binary', 'br_multi')
  
  input_types <- 
    sapply(dataset, function(icol) {
      if(class(icol) == 'numeric') return('numeric')
      if(length(levels(icol)) > 2) return('br_multi')
      if(length(levels(icol)) == 2) return('br_binary') else(stop("Cannot have factor variables with only 1 level..."))
    })
  
  input_names <- names(input_types)
  output_methods <- lookup_method_vector[input_types]
  names(output_methods) <- input_names
  
  if(!is.null(exclude_variables)) output_methods[names(output_methods) %in% exclude_variables] <- ''
  
  return(output_methods)
}


##### Functions for formatting for figures, etc ######

bcolorviz <- function(hex_color_vector, include_names = TRUE) {
    xx <- barplot(rep(3, length(hex_color_vector)),
            col = hex_color_vector,
            axes = FALSE,
            names.arg = hex_color_vector,
            ylim = c(0, 4))
    if(include_names) text(x = xx, y = 3.5, labels = names(hex_color_vector))
}

ct_create_colorramp_named <- function(colorpoints, namevector, exclude_ends = TRUE) {

    len_vec <- length(namevector)
    if(exclude_ends) gradient_size <- len_vec + 2 else gradient_size <- len_vec

    colorvector <- grDevices::colorRampPalette(colors = colorpoints)(gradient_size)

    if(exclude_ends) colorvector <- colorvector[c(-1,-gradient_size)] # remove first and last

    names(colorvector) <- namevector

    return(colorvector)

}
bpadding <- function(num, width = 4, makepercent = FALSE, num_decimals = 2, sig_figs = NULL, silent = FALSE) {
  
  sapply(num, function(inum) bpadding_single(inum, width = width, makepercent = makepercent, num_decimals = num_decimals, sig_figs = sig_figs, silent = silent))

}

bpadding_single <- function(num, width = 4, makepercent = FALSE, num_decimals = 2, sig_figs = NULL, silent = FALSE) {
  # exclude numeric values that can't be processed
  if(is.na(num) | is.nan(num) | !is.finite(num)) return(NA_character_)

  if (is.null(sig_figs)) {
    if (makepercent) {
      sprintf('%%%s.%sf', width - 1, num_decimals) %>% sprintf(num * 100) %>% paste0('%')
    } else {
      sprintf('%%%s.%sf', width, num_decimals) %>% sprintf(num)
    }
  } else { # means we're using significant figures
    if (!silent)
      cat("Running bpadding()...Significant figures will override any decimal specifications\n")
  
  if (makepercent) {
    ending <- '%'
    newnum <- (num * 100)
  } else {
    ending <- ''
    newnum <- num
  }
  # from StackOverflow: https://stackoverflow.com/questions/3245862/format-numbers-to-significant-figures-nicely-in-r
  newnum2 <- formatC(
    signif(newnum, digits = sig_figs),
    digits = sig_figs,
    format = "fg",
    flag = "#"
  ) %>% paste0(ending)
  
  final_num <- sprintf("%%%ss", width) %>% sprintf(newnum2)
  
  return(final_num
  )
  }
}

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
    # from here: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
    
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}



# Moved these to Brandonfunctions

# ------------------------- The Following Functions Are Rather Specific To CT Analysis -----------------------------

bbinarize <- function(.data, var, othername = 'other') {
  
  # take a boolean and convert into a binary character with sensible names as defaults
  
  vvar <- enquo(var)
  # print(vvar)
  # print(quo_name(vvar))
  # print(bhead(.data %>% select(!!vvar)))
  
  if(class(.data %>% pull(!! vvar)) != 'logical') {
    return(.data)
  }
  
  .data <-
    .data %>%
    # side note: we use := here because can't unquote a name when assigning something to it
    # also, just so happens that when using !! on LHS, it has to evaluate to a string, thus the quo_name()
    mutate(!!quo_name(vvar) := case_when(
      is.na(!!vvar) ~ NA_character_, 
      !!vvar ~ quo_name(vvar),
      TRUE ~ othername
    ))
  
  return(.data)
  
}

bbexplore_factor1 <- function(.data, dependentvar, explanatoryvar, 
                            keepn = c('finaln','n','newn','n2','newn2'), 
                            addxsqr = c('none', 'pval', 'full'),
                            pct = c('explanatory', 'dependent', 'both'),
                            na.rm = FALSE, dep_na.rm = FALSE, expl_na.rm = FALSE) {
  
  if(identical(keepn, c('finaln','n','newn','n2','newn2'))) keepn <- 'finaln'
  if(identical(addxsqr, c('none','pval','full'))) addxsqr <- 'none'
  if(identical(pct, c('explanatory', 'dependent', 'both'))) pct <- 'explanatory'
  
  dep <- enquo(dependentvar)
  expl <- enquo(explanatoryvar)
  
  if(pct == 'explanatory') pct <- expl else pct <- dep # what type of grouping to determine the percentages
  
  if(na.rm) .data <- .data %>% filter(!is.na(!!dep), !is.na(!!expl))
  if(dep_na.rm) .data <- .data %>% filter(!is.na(!!dep))
  if(expl_na.rm) .data <- .data %>% filter(!is.na(!!expl))
  .data <- .data %>% mutate_if(is.logical, factor)
  
.finaldata <-
    .data %>% 
    count(!!dep, !!expl) %>%
    mutate(n2 = sprintf(paste0(' %',max(nchar(n)),'s'),n)) %>%
    group_by(!!pct) %>%
    mutate(newn = round(100*n / sum(n), 1)) %>% 
    ungroup() %>%
    mutate(newn2 = sprintf(paste0('(%',max(nchar(newn)),'.1f)'),newn)) %>%
    mutate(finaln = paste0('|', n2,'  ', newn2,'|'))

.finaldata <-
    .data %>% 
    count(!!dep, !!expl) %>%
    mutate(n2 = sprintf(paste0(' %',max(nchar(n)),'s'),n)) %>%
    group_by(!!pct) %>%
    mutate(newn = round(100*n / sum(n), 1)) %>% 
    ungroup() %>%
    mutate(newn2 = sprintf(paste0('(%',max(nchar(newn)),'.1f)'),newn)) %>%
    mutate(finaln = paste0('|', n2,'  ', newn2,'|'))    










  .finaldata <-
    .data %>% 
    count(!!dep, !!expl) %>%
    mutate(n2 = sprintf(paste0(' %',max(nchar(n)),'s'),n)) %>%
    group_by(!!pct) %>%
    mutate(newn = round(100*n / sum(n), 1)) %>% 
    ungroup() %>%
    mutate(newn2 = sprintf(paste0('(%',max(nchar(newn)),'.1f)'),newn)) %>%
    mutate(finaln = paste0('|', n2,'  ', newn2,'|'))
  
  .conttable <-
    .finaldata %>%
    select(!!dep, !!expl, n) %>%
    spread(!!dep, n, fill= 0) %>%
    select(- !!expl)
  

  if(nrow(.conttable) > 1) {
      xsqr <- chisq.test(.conttable)
      xsqrpval <- xsqr$p.value # p-value
      xsqrdf <- xsqr$parameter # df
      xsqrstat <- round(xsqr$statistic, 1) # X-squared 
  } else {
      print(paste0("The variable {",quo_name(expl),"} does not have more than one unique level"))
      xsqrpval <- 0.9999
      xsqrdf <- 0
      xsqrstat <- 0
  }
  
  xsqrout <- paste0('df=',xsqrdf,', X^2=',xsqrstat,', pval<',formatC(xsqrpval, digits = 3, format = 'e'))
  if(addxsqr == 'pval') xsqrout <- formatC(xsqrpval, digits = 3, format = 'e')
  
  .outtable <-
    .finaldata %>%
    select(!!dep, !!expl, !!rlang::sym(keepn)) %>% 
    spread(!!dep, !!rlang::sym(keepn), fill= 0)
  
  if(addxsqr != 'none') {
    .outtable <- 
      .outtable %>%
      mutate(xsqr = xsqrout)
  }
  
  return(.outtable)
}


bbexplore_factors <- function(.data, dependent, ...,
                             keepn = c('finaln','n','newn','n2','newn2'), 
                             addxsqr = c('none', 'pval', 'full'),
                             pct = c('explanatory', 'dependent'),
                             na.rm = FALSE, dep_na.rm = FALSE, expl_na.rm = FALSE,
                             str_vs_num = c('str','num'), binarize = TRUE) {
  
  if(identical(pct, c('explanatory', 'dependent'))) pct <- 'explanatory'
  if(identical(str_vs_num, c('str','num'))) str_vs_num <- 'str'
  
  dep <- enquo(dependent)
  expls <- enquos(...)
  if(binarize) .data <- .data %>% bbinarize(!! dep)
  .data <- .data %>% mutate_if(is.logical, factor)
  
  # print('going now')
  # 
  # 
  # print(head(.data, 20))

  lapply(expls, function(explain) {
    
    finaltable <- 
      cbind.data.frame(explvar= quo_name(explain), 
                       bexplore_factor1(.data, !!dep, !!explain, 
                                        keepn = keepn, addxsqr = addxsqr, pct = pct,
                                        na.rm = na.rm, dep_na.rm = dep_na.rm, expl_na.rm = expl_na.rm),
                       stringsAsFactors = F) %>%
      dplyr::rename(varlevels = !!explain) 
    
    if(str_vs_num == 'str') {
      finaltable <-
        rbind(finaltable, finaltable[nrow(finaltable), ] %>% map_chr(function(...) return(' ')))
    }
    
    return(finaltable)
      
    }
  ) %>% 
  bind_rows()
}

bexplore_factor1 <- function(.data, dependentvar, explanatoryvar, 
                            keepn = c('finaln','n','newn','n2','newn2'), 
                            addxsqr = c('none', 'pval', 'full'),
                            pct = c('explanatory', 'dependent'),
                            na.rm = FALSE, dep_na.rm = FALSE, expl_na.rm = FALSE) {
  
  if(identical(keepn, c('finaln','n','newn','n2','newn2'))) keepn <- 'finaln'
  if(identical(addxsqr, c('none','pval','full'))) addxsqr <- 'none'
  if(identical(pct, c('explanatory', 'dependent'))) pct <- 'explanatory'
  
  dep <- enquo(dependentvar)
  expl <- enquo(explanatoryvar)
  
  if(pct == 'explanatory') pct <- expl else pct <- dep # what type of grouping to determine the percentages
  
  if(na.rm) .data <- .data %>% filter(!is.na(!!dep), !is.na(!!expl))
  if(dep_na.rm) .data <- .data %>% filter(!is.na(!!dep))
  if(expl_na.rm) .data <- .data %>% filter(!is.na(!!expl))
  .data <- .data %>% mutate_if(is.logical, factor)
  
  .finaldata <-
    .data %>% 
    count(!!dep, !!expl) %>%
    mutate(n2 = sprintf(paste0(' %',max(nchar(n)),'s'),n)) %>%
    group_by(!!pct) %>%
    mutate(newn = round(100*n / sum(n), 1)) %>% 
    ungroup() %>%
    mutate(newn2 = sprintf(paste0('(%',max(nchar(newn)),'.1f)'),newn)) %>%
    mutate(finaln = paste0('|', n2,'  ', newn2,'|'))
  
  .conttable <-
    .finaldata %>%
    select(!!dep, !!expl, n) %>%
    spread(!!dep, n, fill= 0) %>%
    select(- !!expl)
  

  if(nrow(.conttable) > 1) {
      xsqr <- chisq.test(.conttable)
      xsqrpval <- xsqr$p.value # p-value
      xsqrdf <- xsqr$parameter # df
      xsqrstat <- round(xsqr$statistic, 1) # X-squared 
  } else {
      print(paste0("The variable {",quo_name(expl),"} does not have more than one unique level"))
      xsqrpval <- 0.9999
      xsqrdf <- 0
      xsqrstat <- 0
  }
  
  xsqrout <- paste0('df=',xsqrdf,', X^2=',xsqrstat,', pval<',formatC(xsqrpval, digits = 3, format = 'e'))
  if(addxsqr == 'pval') xsqrout <- formatC(xsqrpval, digits = 3, format = 'e')
  
  .outtable <-
    .finaldata %>%
    select(!!dep, !!expl, !!rlang::sym(keepn)) %>% 
    spread(!!dep, !!rlang::sym(keepn), fill= 0)
  
  if(addxsqr != 'none') {
    .outtable <- 
      .outtable %>%
      mutate(xsqr = xsqrout)
  }
  
  return(.outtable)
}


bexplore_factors <- function(.data, dependent, ...,
                             keepn = c('finaln','n','newn','n2','newn2'), 
                             addxsqr = c('none', 'pval', 'full'),
                             pct = c('explanatory', 'dependent'),
                             na.rm = FALSE, dep_na.rm = FALSE, expl_na.rm = FALSE,
                             str_vs_num = c('str','num'), binarize = TRUE) {
  
  if(identical(pct, c('explanatory', 'dependent'))) pct <- 'explanatory'
  if(identical(str_vs_num, c('str','num'))) str_vs_num <- 'str'
  
  dep <- enquo(dependent)
  expls <- enquos(...)
  if(binarize) .data <- .data %>% bbinarize(!! dep)
  .data <- .data %>% mutate_if(is.logical, factor)
  
  # print('going now')
  # 
  # 
  # print(head(.data, 20))

  lapply(expls, function(explain) {
    
    finaltable <- 
      cbind.data.frame(explvar= quo_name(explain), 
                       bexplore_factor1(.data, !!dep, !!explain, 
                                        keepn = keepn, addxsqr = addxsqr, pct = pct,
                                        na.rm = na.rm, dep_na.rm = dep_na.rm, expl_na.rm = expl_na.rm),
                       stringsAsFactors = F) %>%
      dplyr::rename(varlevels = !!explain) 
    
    if(str_vs_num == 'str') {
      finaltable <-
        rbind(finaltable, finaltable[nrow(finaltable), ] %>% map_chr(function(...) return(' ')))
    }
    
    return(finaltable)
      
    }
  ) %>% 
  bind_rows()
}

bglm_table <- function(.data, dependentvar, explanatoryvar, conflevel = 0.95, 
                       deplevels = NULL, expllevels = NULL,
                       keepcols = c('stats','full'), 
                       verbose = FALSE) {
  
  # works on only a single variable! 
  # expllevels allows you to set the explanatory levels that should be included in the model (rest is NA)
  # deplevels allows you to set the dependent levels that should be included in the model (rest is NA)
  dep <- enquo(dependentvar)
  expl <- enquo(explanatoryvar)
  .data <- .data %>% mutate_if(is.logical, factor) # may need to use %>% droplevels()
  
  # side note: we use := here because can't unquote a name when assigning something to it
  # also, just so happens that when using !! on LHS, it has to evaluate to a string, thus the quo_name()
  if(!is.null(deplevels)) .data <- .data %>% mutate(!!quo_name(dep) := factor(!!dep, levels = deplevels))
  if(!is.null(expllevels)) .data <- .data %>% mutate(!!quo_name(expl) := factor(!!expl, levels = expllevels))
  if(identical(keepcols, c('stats','full'))) keepcols <- 'stats'
  
  # remove NAs
  .data <- 
      .data %>% 
      select(!!dep, !! expl) 
  
  if(verbose) {
      # show how many NAs / levels we have
      print("Total NA per col...")
      .totalna <- 
          .data %>% 
          summarise_all(.funs = list( ~sum(is.na(.)))) %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column() %>%
          arrange(desc(V1)) %>%
          brename('V1', 'TotalNAs')
      
      .totallevels <- 
          .data %>% 
          summarise_all(.funs = list( ~length(unique(.)))) %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column() %>%
          brename('V1', 'TotalLevels')
      
      print(left_join(.totalna, .totallevels, by = 'rowname'))
      print(paste0("Before removing NAs, your data is {",nrow(.data),"} rows by {", ncol(.data),"} columns"))
  }
  
  # finish processing data
  .data <- 
      .data %>%
      mutate_if(is.logical, factor) %>%
      mutate_if(is.character, factor) %>%
      bany_na(removeNARows = T) %>% # remove NAs for regression
      select(-anyNAs) %>%
      droplevels() # drop any levels that aren't being used

  if(verbose) {
      print(paste0("After removing NAs, your data is {",nrow(.data),"} rows by {", ncol(.data),"} columns"))
      
  }
  
  fmla1 <- as.formula(paste(quo_name(dep), '~', quo_name(expl)))

  glmfit1 <- glm(formula = fmla1,
                 data = .data,
                 family = 'binomial')
  
  # do something different for factor vs continuous
  if(length(glmfit1$xlevels) > 0) {
    # is factor
    glmlevels <- glmfit1$xlevels[[1]] 
  } else {
    # is continuous
    glmlevels <- quo_name(expl)
  }

  glmcoeff <- coef(glmfit1) # remember that the coefficients are not odds ratios until you exponentiate them (exp())
  glmOR <- c(NA, exp(glmcoeff[2:length(glmcoeff)]))
  glmconf <- exp(confint.default(glmfit1, level = conflevel))
  # glmconf <- exp(confint(glmfit1, level = conflevel)) # for some reason confint.default is faster, the CI is *slightly* diff
  glmconf_lower <- c(NA, glmconf[2:nrow(glmconf),1])
  glmconf_upper <- c(NA, glmconf[2:nrow(glmconf),2])
  glmpvals <- c(NA, coef(summary(glmfit1))[,'Pr(>|z|)'][2:nrow(glmconf)])
  
  glmtable_raw <- data.frame(
    varlevels = glmlevels,
    glmOR = glmOR,
    glm_OR_conf_low = glmconf_lower,
    glm_OR_conf_high = glmconf_upper,
    glmpvals = glmpvals,
    stringsAsFactors = F
  )
  
  # add # of levels per varlevel
  # do something different for factor vs continuous
  if(length(glmfit1$xlevels) > 0) {
      # is factor
      levelnum <- 
          .data %>% 
          group_by(!! expl) %>% 
          summarise(total_per_varlevel = n()) %>% 
          dplyr::rename(varlevels = !!expl) %>%
          mutate(varlevels = as.character(varlevels))
  } else {
      # is continuous
      levelnum <- 
          data.frame(varlevels = quo_name(expl),
                     total_per_varlevel = nrow(.data))
  }
  
  glmtable <- 
      left_join(glmtable_raw, 
                levelnum,
                by = 'varlevels')
  
  if(length(glmfit1$xlevels) == 0) {
    # if continuous, we don't have an "intercept" as the top row, so we can just shave that off
    glmtable <- glmtable[2, ] # it should only be one row
  }

  formatted_glmtable <- 
    glmtable %>% 
    mutate(FMT_OR = bvec_format_num(glmOR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
           FMT_PVAL = formatC(glmpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
           FMT_up = bvec_format_num(glm_OR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
           FMT_low = bvec_format_num(glm_OR_conf_low, cap=100),
           FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
           OR_full_p = paste0(FMT_OR, ' ', FMT_conf, '; p<',FMT_PVAL),
           OR_full = paste0(FMT_OR, ' ', FMT_conf))
  
  if(keepcols == 'stats') {
    formatted_glmtable <- formatted_glmtable %>% select(varlevels, OR_full, glmpvals, OR_full_p, total_per_varlevel)
  }
  
  return(formatted_glmtable)
  
}


buni_vs_full_glmtable <- function(.data, dependentvar, ...,
                                  conflevel = 0.95, 
                                  uni_to_multi_pval_cutoff = 5E-2,
                                  uni_to_multi_stringency = c('any_under', 'none_over', 'all'),
                                  binarize = FALSE,
                                  proportion_outcome = FALSE,
                                  include_intercept_data = FALSE,
                                  remove_expl_with_1_level = FALSE,
                                  force_inclusion = NA # a string of any variables you want to be included in multi eg c('primary_purpose','br_phase2')
                                  ) {
    
  if(identical(uni_to_multi_stringency, c('any_under', 'none_over', 'all'))) {
    uni_to_multi_stringency <- 'any_under'
    
    # for any_under, if a factor has any level that on univariate has a significance < the cutoff, the variable is also included in the multi
    #     none_over, all of the factor levels have to exceed the pval cutoff in order to be included (this is probably unnecessarily stringent?)
    #     all, just include all the variables in both
  }
  
  dep <- enquo(dependentvar)
  expls <- enquos(...)
  
  # dep <- quo(were_results_reported) #*#
  # expls <- quos(industry_any2, industry_any1, #*#
  #               primary_purpose, lead_agency_class, br_phase2) #*#
  # # expls <- quos(br_phase2, lead_agency_class) #*#
  # .data <- vindf #*#
  # conflevel <- 0.95
  
  # select only the data we need
  .data <- 
    .data %>% 
    select(!!dep, !!! expls) 
  
  # calculate dependent variable NAs
  .totaldepna <- 
      .data %>%
      summarise(totaldepna = sum(is.na(!!dep))) %>%
      pull(totaldepna)
  
  # show how many NAs / levels we have
  print("Total NA per col...")
  .totalna <- 
    .data %>% 
    summarise_all(.funs = list( ~sum(is.na(.)))) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    arrange(desc(V1)) %>%
    brename('V1', 'TotalNAs')
      
  .totallevels <- 
    .data %>% 
    summarise_all(.funs = list( ~length(unique(.)))) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    brename('V1', 'TotalLevels')
  
  .NAtable <- left_join(.totalna, .totallevels, by = 'rowname')
  print(.NAtable)
  .oldrownum <- nrow(.data)
  .oldcolnum <- ncol(.data)
  print(paste0("Before removing NAs, your data is {", .oldrownum,"} rows by {", .oldcolnum,"} columns"))
  
  # more processing data
  .data <- 
    .data %>%
    mutate_if(is.logical, factor) %>%
    mutate_if(is.character, factor) %>%
    bany_na(removeNARows = T) %>% # remove NAs for regression
    select(-anyNAs) %>%
    droplevels() # drop any levels that aren't being used
  
  .newrownum <- nrow(.data)
  .newcolnum <- ncol(.data)
  print(paste0("After removing NAs, your data is {", .newrownum,"} rows by {", .newcolnum,"} columns"))
  
  # option to remove explanatory vars that don't have more than 1 level...
  if(remove_expl_with_1_level) {
    
    vars_with_1_level <- .data %>% select(!!! expls) %>% summarise_all(n_distinct) %>% {which(. == 1)}
    
    # only select ones with more than 1 level
    if(length(vars_with_1_level) > 0) {
      expls <- expls[-vars_with_1_level]
    }
    
    vars_removed <- 
      sapply(expls[vars_with_1_level], quo_name)
    
    expl_1level_warning <- paste0("Removed the following variables due to having only 1 level: ", paste0(vars_removed, collapse = '; '))
    
    print(expl_1level_warning)
    
  } else expl_1level_warning <- ''
  
  depvarlevels <- 
    .data %>% 
    pull(!!dep) %>% 
    factor() %>% 
    levels()
  
  if(proportion_outcome | length(depvarlevels) > 10) {
    proportion_outcome <- TRUE
    depvarlevels <- 'proportional_outcome'
  } else {
    depvarlevels <- depvarlevels %>% paste0(collapse = '; ')
  }
  
  if(!proportion_outcome) print(paste0("Dependent Variable Levels Are (in order): ", depvarlevels))

  depvarlevels <- paste0(depvarlevels, '; varname = ', quo_name(dep))

  # get how many counts of the dependent variable there are, and what the levels are
  depvarcounts <- .data %>% count(!! dep) %>% mutate(combo = paste(!!dep, n, sep = '_')) %>% pull(combo) %>% paste0(collapse = '; ')
  if(!proportion_outcome) depvarlevels <- paste0(depvarlevels, '; counts = ', depvarcounts)
  # --------------------------------------------------------------------#
  # -------------     do the univariate analysis    --------------------
  # --------------------------------------------------------------------#
  
    
  print("Performing Univariate")
  
  # first create the univariate OR tables for all of the different explanatory variables
  univariate_OR_df <-
    lapply(expls, function(explain) {
      finaltable <-
        cbind.data.frame(explvar = quo_name(explain),
                         bglm_table(.data, !!dep, !!explain, conflevel = conflevel, keepcols = 'stats'),
                         stringsAsFactors = F)
      if(nrow(finaltable) > 1) {
        # ie this is for a factor, not continuous
        row1 <- finaltable %>% slice(1:1) %>% mutate(OR_full = NA, glmpvals = NA)
        finaltable <- rbind.data.frame(row1,
                                       finaltable[2:nrow(finaltable), ],
                                       stringsAsFactors = F)
      }

      return(finaltable)
      }) %>%
    bind_rows() %>%
    select(explvar, varlevels, OR_full, glmpvals, total_per_varlevel) %>%
    rename(univariate_OR = OR_full,
           univariate_pval = glmpvals) %>%
    left_join(.NAtable, 
              by = c('explvar' = 'rowname')) %>% 
    group_by(explvar) %>%
    mutate(PctNAs = mean(TotalNAs) / .oldrownum) %>%
    ungroup() %>%
    mutate(
      ChangeDimensions = paste0(
        "Dependent variable has ",
        .totaldepna,
        " (",
        round(.totaldepna / .oldrownum, 4) * 100,
        "%) NAs; After removing NAs, your data goes from (rows x columns) ({",
        .oldrownum,
        "} x {",
        .oldcolnum,
        "})  to  ({",
        .newrownum,
        "} x {",
        .newcolnum,
        "}), a drop of ",
        round((.oldrownum - .newrownum) / .oldrownum, 4) * 100,
        "%. ",
        expl_1level_warning,
        collapse = ''
      )
    )
  
  print("Performing Multivariate")
  
  # --------------------------------------------------------------------------------------------#
  # 
  # ------------------------------- get the multivariate OR table ------------------------------
  #
  # --------------------------------------------------------------------------------------------#
  
  expls_quo_names <- sapply(expls, function(ei) quo_name(ei))
  
  # fmla2 <- as.formula(paste0(quo_name(dep), 
  #                            ' ~ ',
  #                            paste0(expls_quo_names, collapse = ' + ')))
  
  # keep only those if one of their levels is < uni_to_multi_pval_cutoff
  fmla3_any_under_pval <- 
    univariate_OR_df %>%
    filter(univariate_pval < uni_to_multi_pval_cutoff) %>%
    pull(explvar) %>%
    unique()
  fmla3_any_over_pval <- 
    univariate_OR_df %>%
    filter(univariate_pval > uni_to_multi_pval_cutoff) %>%
    pull(explvar) %>%
    unique()
  fmla3_none_under_pval <- 
    setdiff(expls_quo_names, 
            fmla3_any_under_pval)
  fmla3_none_over_pval <- 
    setdiff(expls_quo_names, 
            fmla3_any_over_pval)
  
  if(uni_to_multi_stringency == 'any_under') {
    fmla3_multi_var_list <- fmla3_any_under_pval
  } else if(uni_to_multi_stringency == 'none_over') {
    fmla3_multi_var_list <- fmla3_none_over_pval
  } else if(uni_to_multi_stringency == 'all') {
    fmla3_multi_var_list <- expls_quo_names
  }
  
  if(!is.na(force_inclusion)) {
    fmla3_multi_var_list <- c(fmla3_multi_var_list, force_inclusion) %>% unique()
  }
  
  if(length(fmla3_multi_var_list) == 0) stop("\nYou do not have any variables that pass your multivariate inclusion criteria!\n")
  
  fmla3 <- paste0(quo_name(dep),
                   ' ~ ',
                   paste0(fmla3_multi_var_list, collapse = ' + '))
  
  
  glmfit2 <- glm(fmla3,
                 data = .data,
                 family = 'binomial')
  
  # grab conf_intervals
  conf_table <-
    confint.default(glmfit2, level = conflevel) %>%
    exp() %>%  # you exponentiate the coefficients to get odds ratios
    as.data.frame(stringsAsFactors = F)
  colnames(conf_table) <- c('glm_OR_conf_low','glm_OR_conf_high')
  
  if(conf_table %>% bany_na(removeNARows = F) %>% pull(anyNAs) %>% any()) {
    message("\nWarning: There may be variables which are linear combinations of other variables, and thus cannot
          be included in a logistic regression!")
  }
  
  conf_table <- 
    conf_table %>%
    # as.data.frame(stringsAsFactors = F) %>%
    tibble::rownames_to_column('glmlevels') 
  
  coef_table <- 
    coef(summary(glmfit2)) %>%
    as.data.frame(stringsAsFactors = F) %>%
    tibble::rownames_to_column('glmlevels')
  
  stats_table <- 
    left_join(conf_table, # this should go first or else you lose the rows that are NAs
              coef_table, 
              by = 'glmlevels') %>%
    mutate(glmOR = exp(Estimate),
           glmpvals = `Pr(>|z|)`)
  
  # this was the old way that I coded this...
  # conf_table <-
  #   confint.default(glmfit2, level = conflevel) %>%
  #   exp() # you exponentiate the coefficients to get odds ratios
  # colnames(conf_table) <- c('glm_OR_conf_low','glm_OR_conf_high')
  # stats_table <- 
  #   coef(summary(glmfit2)) %>% 
  #   cbind(conf_table) %>%
  #   as.data.frame(stringsAsFactors = F) %>%
  #   tibble::rownames_to_column('glmlevels') %>%
  #   mutate(glmOR = exp(Estimate),
  #          glmpvals = `Pr(>|z|)`)
  
  # format everything so the strings look nice
  coef_full_table <- 
    stats_table %>% 
    mutate(FMT_OR = bvec_format_num(glmOR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
           FMT_PVAL = formatC(glmpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
           FMT_up = bvec_format_num(glm_OR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
           FMT_low = bvec_format_num(glm_OR_conf_low, cap=100),
           FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
           OR_full_p = paste0(FMT_OR, ' ', FMT_conf, '; p<',FMT_PVAL),
           OR_full = paste0(FMT_OR, ' ', FMT_conf))
  
  icoefrown <- 
    coef_full_table %>% 
    select(glmlevels, OR_full, glmpvals)
  
  
  if(include_intercept_data) {
    # save the data for intercept, will be output later
    coef_intercept_row <-
      stats_table %>%
      filter(glmlevels == '(Intercept)') %>%
      mutate(
        intercept_row_data = paste0(
          "For Imputed Multivariate Intercept, OR = {",
          glmOR,
          "}; CI is {",
          glm_OR_conf_low,
          ", ",
          glm_OR_conf_high,
          "}; P-value is {",
          glmpvals,
          "}. "
        )
      )
    
    if(nrow(coef_intercept_row) != 1) {
      warning("Trying to extract data for intercept of model...unusual intercept output, suspect some error with model")
      intercept_row_data <- "Trying to extract data for intercept of model...unusual intercept output, suspect some error with model"
    } else {
      intercept_row_data <- coef_intercept_row %>% pull(intercept_row_data)
    }
    
    intercept_glm_OR <- coef_intercept_row %>% pull(glmOR)
    intercept_glm_OR_conf_low <- coef_intercept_row %>% pull(glm_OR_conf_low)
    intercept_glm_OR_conf_high <- coef_intercept_row %>% pull(glm_OR_conf_high)
    intercept_glm_OR_pvalue <- coef_intercept_row %>% pull(glmpvals)
    
  }
  
  
  # just need to turn the first row into NA
  icoefrow1 <- 
    icoefrown %>%
    slice(1:1) %>%
    mutate(glmlevels = NA,
           OR_full = NA,
           glmpvals = NA)
  # will iterate through using this table
  icoef_table <-
    rbind.data.frame(icoefrow1,
                     icoefrown[2:nrow(icoefrown), ],
                     stringsAsFactors = F)
  
  # create list of coefficient tables
  multi_coef_row1 <- icoef_table[1, ]
  iicoef_table <- icoef_table[-1, ]
  
  # Goal: Match Coefficient Table Rows with the Corresponding Covariates / Levels of those Covariates...
  
  # I think there's a more elegant way than doing it this way...I used to use the glmfit2$xlevels,
  # but that was only appropriate if all the explains are factors, see example commented out below...
  #   
  # coef_breakpoints <- c(0, glmfit2$xlevels %>% lengths %>% {. - 1} %>% cumsum)
  # 
  # coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
  # for(i in seq(length(coef_breakpoints)-1)) {
  #   coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
  # }
  # coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
  # for(i in seq(length(coef_break_list))) {
  #   coef_table_list[[i]] <- 
  #     rbind.data.frame(multi_coef_row1,
  #                      iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
  #                      stringsAsFactors = F)
  # }
  
  # how many levels should we expect? 
  multilevels <- 
    .data[, fmla3_multi_var_list] %>%
    lapply(levels)
  
  numlevels <- # will be "0" for continuous, >1 for all factors, assumes that all remaining columns are either factor
    multilevels %>%
    lengths()
  
  # based on number of levels, we know how many rows we should expect. We subtract 1 because the first level is used
  # as the reference when we deal with factors, so there is no distinct coefficient for that
  # abs() to account for numeric columns having "0" levels, but we basically want that to be treated as 1, so abs(0-1) = 1
  coef_breakpoints <- c(0, numlevels %>% {. -1} %>% {cumsum(abs(.))}) 
  
  
  coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
  for(i in seq(length(coef_breakpoints)-1)) {
    coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
  }
  coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
  for(i in seq(length(coef_break_list))) {
    if(diff(coef_break_list[[i]]) > 0) {
      # we are dealing with a factor b/c it must correspond to more than 1 row
      coef_table_list[[i]] <- 
        rbind.data.frame(multi_coef_row1,
                         iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
                         stringsAsFactors = F)
    } else {
      # we are dealing with a continuous variable because it corresponds to only 1 row
      coef_table_list[[i]] <-
        as.data.frame(iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ],
                      stringsAsFactors = F)
    }
  }
  
  
  # extract the stats for each explanatory variable
  multivariate_OR_df <-
    lapply(seq(fmla3_multi_var_list), function(i) {
      
      explain <- fmla3_multi_var_list[i]
      
      iglmlevels <- multilevels[[i]]
      if(is.null(iglmlevels)) iglmlevels <- explain # TRUE for continuous variables 
      iglmcoeff <- coef_table_list[[i]]
      iexpl <- data.frame('explvar' = explain, stringsAsFactors = F)
      
      out_table <-
        cbind.data.frame(iexpl,
                         iglmlevels,
                         iglmcoeff,
                         stringsAsFactors = F)
      
      # we keep the first "row" because this will always correspond to the reference level, thus it's always NA
      icoef_table <- icoef_table[-(2:length(iglmlevels)), ]
      
      return(out_table)
    }) %>%
    bind_rows() %>%
    rename(varlevels = iglmlevels,
           multivariate_OR = OR_full,
           multivariate_pval = glmpvals) %>%
    select(-glmlevels)
  
  
  if(include_intercept_data) {
    multivariate_OR_df <- 
      multivariate_OR_df %>%
      mutate(intercept_glm_OR = intercept_glm_OR,
             intercept_glm_OR_conf_low = intercept_glm_OR_conf_low,
             intercept_glm_OR_conf_high = intercept_glm_OR_conf_high,
             intercept_glm_OR_pvalue = intercept_glm_OR_pvalue,
             intercept_row_data = intercept_row_data)
  }
  
  # return(multivariate_OR_df)
  
  uni_n_multi_table_df <- 
    left_join(univariate_OR_df,
              multivariate_OR_df,
              by = c('explvar', 'varlevels')) %>%
    mutate(multivar_sig_p = case_when(
           multivariate_pval < 0.0001 ~ '**** <0.0001',
           multivariate_pval < 0.001 ~ '*** <0.001',
           multivariate_pval < 0.01 ~ '** <0.01',
           multivariate_pval < 0.05 ~ '* <0.05',
           TRUE ~ ' '
      )) %>% 
    mutate(depvarlevels = depvarlevels) %>% 
    select(-total_per_varlevel, -TotalNAs, -TotalLevels, -PctNAs, -ChangeDimensions, -depvarlevels, -starts_with('intercept_'), everything(), 
           total_per_varlevel, TotalNAs, PctNAs, starts_with('intercept_'), ChangeDimensions, depvarlevels) 
  
  
  return(uni_n_multi_table_df)
}

b_cox_imputation <- function(.data, timevariable, censorvariable, ...,
                            input_predictionmatrix = NULL, input_methods = NULL,
                            n_maxit = 5, n_m = 5,
                            input_imp_object = NULL,
                            mice_seed = NULL,
                            conflevel = 0.95, 
                            binarize = FALSE) {
    # Before running this function, you need to figure out what predictionmatrix you want and which methods you want. You can get these by running the following 
    # code before calling this function and providing the corresponding outputs as inputs
    # 
    # 
    # ini_imp1 <- 
    #   mice(.data, maxit = 0)
    # 
    # pred_imp1 <- 
    #   ini_imp1$predictorMatrix
    # 
    # meth_imp1 <- 
    #   ini_imp1$method
    #
    # pred_imp1
    # pred_imp1[, c(timevariable, censorvariable)] <- 0 # whatever the name of the dependent variable is, don't use the outcome to predict the covariates
    
    # pred_imp12 <- quickpred(.data, exclude = depvar_rrace, include = explvars_rrace) # another way to make a matrix, allows you to specify
    #                                                                                                # some settings, if you're interested you can look these up
    #
    # meth_imp1 # You need to determine if you want to use the default methods or if you want to make some changes. 
    # methods(mice) # to see available methods... 
    # I tend to like 'norm' for numeric, 'logreg' (logistic) for binary, 'polyreg' (multinomial logistic) for categorical
    # 
    # make sure everything that has missing values has a method that is appropriate
    # cbind(meth_imp1 %>% as.matrix(),
    #       .data %>% summarise_all(list(~ sum(is.na(.)) / n())) %>% as.matrix() %>% t() %>% round(3),
    #       .data %>% summarise_all(list(~ class(.))) %>% as.matrix() %>% t()
    # )
    # save your pred_imp2 (or pred_imp1) as your input predictionmatrix and save meth_imp1 as your input methods vector

    # ------------------------------------------------------------------------------------#
    # 
    # ------------------------------- Processing and Set Up ------------------------------
    #
    # ------------------------------------------------------------------------------------#
    
    if(is.null(mice_seed)) mice_seed <- as.integer(runif(1) * 1000)
    
    # get the time/censor variables and the independent variables
    tvar <- enquo(timevariable)
    cvar <- enquo(censorvariable)
    expls <- enquos(...)

    var_ids <- paste0('time-variable = ', quo_name(tvar), '; censor-variable = ', quo_name(cvar))
    print(var_ids)

    # select only the data we need
    .data <- 
        .data %>% 
        select(!!tvar, !!cvar, !!! expls) 

    # calculate dependent variable NAs
    .totaltimena <- 
      .data %>%
      summarise(totaltimena = sum(is.na(!!tvar))) %>%
      pull(totaltimena)

    .totaleventna <- 
      .data %>%
      summarise(totaleventna = sum(is.na(!!cvar))) %>%
      pull(totaleventna)

    # show how many NAs / levels we have
    print("Total NA per col...")
    .totalna <- 
        .data %>% 
        summarise_all(.funs = list(~ sum(is.na(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        arrange(desc(V1)) %>%
        brename('V1', 'TotalNAs')
    
    .totallevels <- 
        .data %>% 
        summarise_all(.funs = list(~ length(unique(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        brename('V1', 'TotalLevels')
    
    nrow_data <- nrow(.data)

    .NAtable <- 
        left_join(.totalna, .totallevels, by = 'rowname') %>% 
        mutate(pct_NA = round(100 * (TotalNAs / nrow_data), 1) %>% paste0('%'))
    print(.NAtable)

    # do some initial processing...
    .data <- 
        .data %>%
        mutate_if(is.logical, factor) %>% # false becomes 0, TRUE becomes 1
        mutate_if(is.character, factor) %>% # mice algorithm doesn't work when these are characters
        droplevels() # drop any levels that aren't being used, we won't be imputing stuff from which we have no example data! 
                          
  # Make the formula:
  expls_quo_names <- sapply(expls, function(ei) quo_name(ei))
  fmla3_multi_var_list <- expls_quo_names # in the uni_vs_multi model, we would do some steps between here to filter down based on pval  


  fmla3 <- paste0('Surv(',quo_name(tvar),", ",quo_name(cvar), ") ~ ", 
                    paste0(fmla3_multi_var_list, collapse = ' + '))

    print("Imputation Step")
    
    # --------------------------------------------------------------------------------#
    # 
    # ------------------------------- Perform Imputation ------------------------------
    #
    # --------------------------------------------------------------------------------#
    
    if(is.null(input_imp_object)) {
        # take a look at what kind of missingness we're dealing with here...
        print("Plotting pattern of missing data")
        mice::md.pattern(.data, plot = TRUE, rotate.names = TRUE) # (how many NAs is this by percentage?)
        
        # show how many missing we have as a percentage! 
        print("Show how many missing we have as a percentage")
        mice::md.pattern(.data, plot = FALSE, rotate.names = TRUE) %>% 
        {.[nrow(.), ] / nrow(.data)} %>%
        {. * 100} %>% 
            round(1) %>% 
            {data.frame(pct_missing = .)} %>%
            print()
        
        # Show the prediction matrix they gave us
        print("Show the input prediction matrix")
        print(input_predictionmatrix)
        print("Show the input methods vector")
        print(input_methods)
        
        # Impute the data
        print(paste0("Perform imputation with maxit = ", n_maxit, " and m = ", n_m))
        
        imp1 <- 
            mice::mice(.data, 
                       maxit = n_maxit,
                       m = n_m,
                       predictorMatrix = input_predictionmatrix,
                       method = input_methods,
                       seed = mice_seed)
        
        print("Check any logged events")
        print(imp1$loggedEvents)
        
        # Ensure no further missing data
        print("Check if there is still missing data after our imputation")
        mice::complete(imp1) %>% md.pattern(rotate.names = TRUE, plot = TRUE)
        print("Details of the imputation model used")
        print(imp1)
        print("Check out the plots")
        plot(imp1)
    } else {
        print("Using pre-built imputation object...")
        imp1 <- input_imp_object
        print("Will not produce diagnostic imputation plots/tables since using pre-created imputation input")
        
    }
    
    n_imputed_rows <- mice::complete(imp1) %>% nrow()
    n_imputed_cols <- mice::complete(imp1) %>% ncol()
    
    # Perform our regression
    print("Perform our Cox regression")
    fit_imp1 <- 
        with(imp1, coxph(formula = as.formula(fmla3)))
    
    print("Pooling results...")
    pool_fit_imp1 <-  pool(fit_imp1)
    
    print("Creating summary outputs")
    

    summary_table <- 
            summary(pool_fit_imp1, conf.int = TRUE, exponentiate = TRUE, conf.level = conflevel)
    
    full_summary_table <- 
        mice:::summary.mipo(pool_fit_imp1, type = 'all', conf.int = TRUE, conf.level = conflevel, exponentiate = FALSE) # same thing, lets you control the conf level
    
    # ----------------------------------------------------------------------------------------------------#
    # 
    # ------------------------------- Format Imputation Regression Results  ------------------------------#
    #
    # ----------------------------------------------------------------------------------------------------#
    
    print("Format all the outputs into a nice table")
    
    # Much of this code is copied/adapted from my uni_vs_multi_cox function
    coef_table <- 
        summary_table %>%
        tibble::rownames_to_column('coxlevels') %>%
        select(coxlevels, Estimate = estimate, `Std. Error` = std.error, `z value` = statistic, `Pr(>|z|)` = p.value)
    
    # select the two columns that correspond to the upper and lower confidence estimates
    conf_table <- 
        summary_table[, c((ncol(summary_table) - 1), ncol(summary_table))]
    
    colnames(conf_table) <- c('cox_HR_conf_low','cox_HR_conf_high')
    
    conf_table <- 
        conf_table %>%
        # as.data.frame(stringsAsFactors = F) %>%
        tibble::rownames_to_column('coxlevels') 
    
    stats_table <- 
        left_join(conf_table, # this should go first or else you lose the rows that are NAs
                  coef_table, 
                  by = 'coxlevels') %>%
        mutate(coxHR = Estimate, # I don't need to exponentiate in this version because I've already exponentiated in the summary_table
               coxpvals = `Pr(>|z|)`)
    
    # format everything so the strings look nice
    coef_full_table <- 
        stats_table %>% 
        mutate(FMT_HR = bvec_format_num(coxHR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_PVAL = formatC(coxpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_up = bvec_format_num(cox_HR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
               FMT_low = bvec_format_num(cox_HR_conf_low, cap=100),
               FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
               HR_full_p = paste0(FMT_HR, ' ', FMT_conf, '; p<',FMT_PVAL),
               HR_full = paste0(FMT_HR, ' ', FMT_conf))
       
    icoefrown <- 
        coef_full_table %>% 
        select(coxlevels, HR_full, coxpvals)
    # just need to turn the first row into NA
    icoefrow1 <- 
        icoefrown %>%
        slice(1:1) %>%
        mutate(coxlevels = NA,
               HR_full = NA,
               coxpvals = NA)
    
    # will iterate through using this table
    icoef_table <-
        rbind.data.frame(icoefrow1,
                         icoefrown[1:nrow(icoefrown), ], # glm has an intercept but cox doesn't (why we start at 1 here instead of 2)
                         stringsAsFactors = F)
    
    # create list of coefficient tables
    multi_coef_row1 <- icoef_table[1, ]
    iicoef_table <- icoef_table[-1, ]
    
    # how many levels should we expect? 
    multilevels <- 
        .data[, fmla3_multi_var_list] %>%
        lapply(levels)
    
    numlevels <- # will be "0" for continuous, >1 for all factors, assumes that all remaining columns are either factor
        multilevels %>%
        lengths()
    
    # based on number of levels, we know how many rows we should expect. We subtract 1 because the first level is used
    # as the reference when we deal with factors, so there is no distinct coefficient for that
    # abs() to account for numeric columns having "0" levels, but we basically want that to be treated as 1, so abs(0-1) = 1
    coef_breakpoints <- c(0, numlevels %>% {. -1} %>% {cumsum(abs(.))}) 
    
    
    coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_breakpoints)-1)) {
        coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
    }
    coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_break_list))) {
        if(diff(coef_break_list[[i]]) > 0) {
            # we are dealing with a factor b/c it must correspond to more than 1 row
            coef_table_list[[i]] <- 
                rbind.data.frame(multi_coef_row1,
                                 iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
                                 stringsAsFactors = F)
        } else {
            # we are dealing with a continuous variable because it corresponds to only 1 row
            coef_table_list[[i]] <-
                as.data.frame(iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ],
                              stringsAsFactors = F)
        }
    }
    
    
    # extract the stats for each explanatory variable
    pooled_imputed_HR_df <-
        lapply(seq(fmla3_multi_var_list), function(i) {
            
            explain <- fmla3_multi_var_list[i]
            
            icoxlevels <- multilevels[[i]]
            if(is.null(icoxlevels)) icoxlevels <- explain # TRUE for continuous variables 
            icoxcoeff <- coef_table_list[[i]]
            iexpl <- data.frame('explvar' = explain, stringsAsFactors = F)
            
            out_table <-
                cbind.data.frame(iexpl,
                                 icoxlevels,
                                 icoxcoeff,
                                 stringsAsFactors = F, 
                                 row.names = NULL)
            
            # we keep the first "row" because this will always correspond to the reference level, thus it's always NA
            icoef_table <- icoef_table[-(2:length(icoxlevels)), ]
            
            return(out_table)
        }) %>%
        bind_rows() %>%
        rename(varlevels = icoxlevels,
               multivariate_HR = HR_full,
               multivariate_pval = coxpvals) %>%
        select(-coxlevels) %>%
        mutate(impute_multivar_sig_p = case_when(
            multivariate_pval < 0.0001 ~ '**** <0.0001',
            multivariate_pval < 0.001 ~ '*** <0.001',
            multivariate_pval < 0.01 ~ '** <0.01',
            multivariate_pval < 0.05 ~ '* <0.05',
            TRUE ~ ' '
        )) %>%
        brename(c('multivariate_HR', 'multivariate_pval'),
                c(paste0('impute_multivariate_HR_(', 100*conflevel, '%)'), 'impute_multivariate_pval')) %>%
        mutate(impute_settings = paste0('methods = ', paste0(input_methods, collapse = ','), '; maxit = ', n_maxit, '; m = ', n_m)) %>%
        mutate(impute_dimensions = paste0('Rows = ', n_imputed_rows, '; Cols = ', n_imputed_cols))
    
    return(list('pooled_formatted_table' = pooled_imputed_HR_df,
                'raw_output_table' = full_summary_table %>% tibble::rownames_to_column('variables_levels'),
                'impute_model_full' = imp1))
                            }

b_glm_imputation <- function(.data, dependentvar, ...,
                             input_predictionmatrix = NULL,
                             input_methods = NULL,
                             n_maxit = 5,
                             n_m = 5,
                             mice_seed = NULL,
                             input_imp_object = NULL,
                             conflevel = 0.95, 
                             binarize = FALSE,
                             proportion_outcome = FALSE, 
                             include_intercept_data = FALSE,
                             include_percents_output = FALSE,
                             remove_expl_with_1_level = FALSE,
                             force_inclusion = NA # a string of any variables you want to be included in multi eg c('primary_purpose','br_phase2')
) {
    # Before running this function, you need to figure out what predictionmatrix you want and which methods you want. You can get these by running the following 
    # code before calling this function and providing the corresponding outputs as inputs
    # 
    # 
    # ini_imp1 <- 
    #   mice(.data, maxit = 0)
    # 
    # pred_imp1 <- 
    #   ini_imp1$predictorMatrix
    # 
    # meth_imp1 <- 
    #   ini_imp1$method
    #
    # pred_imp1
    # pred_imp1[, 'race_reports_anyrace'] <- 0 # whatever the name of the dependent variable is, don't use the outcome to predict the covariates
    
    # pred_imp12 <- quickpred(.data, exclude = depvar_rrace, include = explvars_rrace) # another way to make a matrix, allows you to specify
    #                                                                                                # some settings, if you're interested you can look these up
    #
    # meth_imp1 # You need to determine if you want to use the default methods or if you want to make some changes. 
    # methods(mice) # to see available methods... 
    # I tend to like 'norm' for numeric, 'logreg' (logistic) for binary, 'polyreg' (multinomial logistic) for categorical
    # 
    # make sure everything that has missing values has a method that is appropriate
    # cbind(meth_imp1 %>% as.matrix(),
    #       .data %>% summarise_all(list(~ sum(is.na(.)) / n())) %>% as.matrix() %>% t() %>% round(3),
    #       .data %>% summarise_all(list(~ class(.))) %>% as.matrix() %>% t()
    # )
    # save your pred_imp2 (or pred_imp1) as your input predictionmatrix and save meth_imp1 as your input methods vector
    
    
    # ------------------------------------------------------------------------------------#
    # 
    # ------------------------------- Processing and Set Up ------------------------------
    #
    # ------------------------------------------------------------------------------------#
    
    if(is.null(mice_seed)) mice_seed <- as.integer(runif(1) * 1000)
    
    # get the dependent and independent variables
    dep <- enquo(dependentvar)
    expls <- enquos(...)
    
    # select only the data we need
    .data <- 
        .data %>% 
        select(!!dep, !!! expls) 
    
    # calculate dependent variable NAs
    .totaldepna <- 
        .data %>%
        summarise(totaldepna = sum(is.na(!!dep))) %>%
        pull(totaldepna)
    
    # show how many NAs / levels we have
    print("Total NA per col...")
    .totalna <- 
        .data %>% 
        summarise_all(.funs = list(~ sum(is.na(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        arrange(desc(V1)) %>%
        brename('V1', 'TotalNAs')
    
    .totallevels <- 
        .data %>% 
        summarise_all(.funs = list(~ length(unique(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        brename('V1', 'TotalLevels')
    
    nrow_data <- nrow(.data)

    .NAtable <- 
        left_join(.totalna, .totallevels, by = 'rowname') %>% 
        mutate(pct_NA = round(100 * (TotalNAs / nrow_data), 1) %>% paste0('%'))
    print(.NAtable)
    
    # do some initial processing...
    .data <- 
        .data %>%
        mutate_if(is.logical, factor) %>% # false becomes 0, TRUE becomes 1
        mutate_if(is.character, factor) %>% # mice algorithm doesn't work when these are characters
        droplevels() # drop any levels that aren't being used, we won't be imputing stuff from which we have no example data! 

    # option to remove explanatory vars that don't have more than 1 level...
    if(remove_expl_with_1_level) {
      
      vars_with_1_level <- .data %>% select(!!! expls) %>% summarise_all(n_distinct) %>% {which(. == 1)}
      
      # only select ones with more than 1 level
      if(length(vars_with_1_level) > 0) {
        expls <- expls[-vars_with_1_level]
      }
      
      vars_removed <- 
        sapply(expls[vars_with_1_level], quo_name)
      
      expl_1level_warning <- paste0("Removed the following variables due to having only 1 level: ", paste0(vars_removed, collapse = '; '))
      
      print(expl_1level_warning)
      
    } else expl_1level_warning <- ''
    
    # grab our dependent variables so we can output these later
    depvarlevels <- 
        .data %>% 
        pull(!!dep) %>% 
        factor() %>% 
        levels()
    
    if(proportion_outcome | length(depvarlevels) > 4) {
        proportion_outcome <- TRUE
        depvarlevels <- 'proportional_outcome'
    } else {
        depvarlevels <- depvarlevels %>% paste0(collapse = '; ')
    }
    
    if(!proportion_outcome) print(paste0("Dependent Variable Levels Are (in order): ", depvarlevels))
    
    depvarlevels <- paste0(depvarlevels, '; varname = ', quo_name(dep), ';')
    
    expls_quo_names <- sapply(expls, function(ei) quo_name(ei))
    
    fmla3_multi_var_list <- expls_quo_names # this will be strings
    
    fmla3 <- paste0(quo_name(dep),
                    ' ~ ',
                    paste0(fmla3_multi_var_list, collapse = ' + '))
    
    print("Imputation Step")
    
    # --------------------------------------------------------------------------------#
    # 
    # ------------------------------- Perform Imputation ------------------------------
    #
    # --------------------------------------------------------------------------------#
    
    if(is.null(input_imp_object)) {
        # take a look at what kind of missingness we're dealing with here...
        
        print("Plotting pattern of missing data")
        mice::md.pattern(.data, plot = TRUE, rotate.names = TRUE) # (how many NAs is this by percentage?)
        
        # show how many missing we have as a percentage! 
        print("Show how many missing we have as a percentage")
        mice::md.pattern(.data, plot = FALSE, rotate.names = TRUE) %>% 
        {.[nrow(.), ] / nrow(.data)} %>%
        {. * 100} %>% 
            round(1) %>% 
            {data.frame(pct_missing = .)} %>%
            print()
        
        # Show the prediction matrix they gave us
        print("Show the input prediction matrix")
        print(input_predictionmatrix)
        print("Show the input methods vector")
        print(input_methods)
        
        # Impute the data
        print(paste0("Perform imputation with maxit = ", n_maxit, " and m = ", n_m))
        
        imp1 <- 
            mice::mice(.data, 
                       maxit = n_maxit,
                       m = n_m,
                       predictorMatrix = input_predictionmatrix,
                       method = input_methods,
                       seed = mice_seed)
        
        print("Check any logged events")
        print(imp1$loggedEvents)
        
        # Ensure no further missing data
        print("Check if there is still missing data after our imputation")
        mice::complete(imp1) %>% select(!!dep, !!! expls) %>% md.pattern(rotate.names = TRUE, plot = TRUE)
        print("Details on the imputation model used")
        print(imp1)
        print("Check out the plots")
        plot(imp1)
        
    } else {
        print("Using pre-built imputation object...")
        imp1 <- input_imp_object
        
        print("Will not produce diagnostic imputation plots/tables since using pre-created imputation input")
        
    }
    
    n_imputed_rows <- mice::complete(imp1) %>% nrow()
    n_imputed_cols <- mice::complete(imp1) %>% ncol()
    
    # Perform our regression
    print("Perform our GLM regression")
    fit_imp1 <- 
        with(imp1, glm(as.formula(fmla3), family = 'binomial'))
    
    print("Pooling results...")
    pool_fit_imp1 <-  pool(fit_imp1)
    
    print("Creating summary outputs")
    

    summary_table <- 
            summary(pool_fit_imp1, conf.int = TRUE, exponentiate = TRUE, conf.level = conflevel)
    
    
    full_summary_table <- 
        mice:::summary.mipo(pool_fit_imp1, type = 'all', conf.int = TRUE, conf.level = conflevel, exponentiate = FALSE) # same thing, lets you control the conf level
    
    # ----------------------------------------------------------------------------------------------------#
    # 
    # ------------------------------- Format Imputation Regression Results  ------------------------------#
    #
    # ----------------------------------------------------------------------------------------------------#
    
    print("Format all the outputs into a nice table")
    
    # Much of this code is copied/adapted from my uni_vs_multi_glm function
    coef_table <- 
        summary_table %>%
        tibble::rownames_to_column('glmlevels') %>%
        select(glmlevels, Estimate = estimate, `Std. Error` = std.error, `z value` = statistic, `Pr(>|z|)` = p.value)
    
    # select the two columns that correspond to the upper and lower confidence bounds
    conf_table <- 
        summary_table[, c((ncol(summary_table) - 1), ncol(summary_table))]
    
    colnames(conf_table) <- c('glm_OR_conf_low','glm_OR_conf_high')
    
    conf_table <- 
        conf_table %>%
        # as.data.frame(stringsAsFactors = F) %>%
        tibble::rownames_to_column('glmlevels') 
    
    stats_table <- 
        left_join(conf_table, # this should go first or else you lose the rows that are NAs
                  coef_table, 
                  by = 'glmlevels') %>%
        mutate(glmOR = Estimate, # I don't need to exponentiate in this version because I've already exponentiated in the summary_table
               glmpvals = `Pr(>|z|)`)
    
    # format everything so the strings look nice
    coef_full_table <- 
        stats_table %>% 
        mutate(FMT_OR = bvec_format_num(glmOR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_PVAL = formatC(glmpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_up = bvec_format_num(glm_OR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
               FMT_low = bvec_format_num(glm_OR_conf_low, cap=100),
               FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
               OR_full_p = paste0(FMT_OR, ' ', FMT_conf, '; p<',FMT_PVAL),
               OR_full = paste0(FMT_OR, ' ', FMT_conf))

   
    icoefrown <- 
        coef_full_table %>% 
        select(glmlevels, OR_full, glmpvals)
    
    if(include_intercept_data) {
      # save the data for intercept, will be output later
      coef_intercept_row <-
        stats_table %>%
        filter(glmlevels == '(Intercept)') %>%
        mutate(
          intercept_row_data = paste0(
            "For Imputed Multivariate Intercept, OR = {",
            glmOR,
            "}; CI is {",
            glm_OR_conf_low,
            ", ",
            glm_OR_conf_high,
            "}; P-value is {",
            glmpvals,
            "}. "
          )
        )
      
      if(nrow(coef_intercept_row) != 1) {
        warning("Trying to extract data for intercept of model...unusual intercept output, suspect some error with model")
        intercept_row_data <- "Trying to extract data for intercept of model...unusual intercept output, suspect some error with model"
      } else {
        intercept_row_data <- coef_intercept_row %>% pull(intercept_row_data)
      }
      
      intercept_glm_OR <- coef_intercept_row %>% pull(glmOR)
      intercept_glm_OR_conf_low <- coef_intercept_row %>% pull(glm_OR_conf_low)
      intercept_glm_OR_conf_high <- coef_intercept_row %>% pull(glm_OR_conf_high)
      intercept_glm_OR_pvalue <- coef_intercept_row %>% pull(glmpvals)
      
    }

    # just need to turn the first row into NA
    icoefrow1 <- 
        icoefrown %>%
        slice(1:1) %>%
        mutate(glmlevels = NA,
               OR_full = NA,
               glmpvals = NA)
    
    # will iterate through using this table
    icoef_table <-
        rbind.data.frame(icoefrow1,
                         icoefrown[2:nrow(icoefrown), ],
                         stringsAsFactors = F)
    
    # create list of coefficient tables
    multi_coef_row1 <- icoef_table[1, ]
    iicoef_table <- icoef_table[-1, ]
    
    # how many levels should we expect? 
    multilevels <- 
        .data[, fmla3_multi_var_list] %>%
        lapply(levels)
    
    numlevels <- # will be "0" for continuous, >1 for all factors, assumes that all remaining columns are either factor
        multilevels %>%
        lengths()
    
    # based on number of levels, we know how many rows we should expect. We subtract 1 because the first level is used
    # as the reference when we deal with factors, so there is no distinct coefficient for that
    # abs() to account for numeric columns having "0" levels, but we basically want that to be treated as 1, so abs(0-1) = 1
    coef_breakpoints <- c(0, numlevels %>% {. -1} %>% {cumsum(abs(.))}) 
    
    
    coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_breakpoints)-1)) {
        coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
    }
    coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_break_list))) {
        if(diff(coef_break_list[[i]]) > 0) {
            # we are dealing with a factor b/c it must correspond to more than 1 row
            coef_table_list[[i]] <- 
                rbind.data.frame(multi_coef_row1,
                                 iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
                                 stringsAsFactors = F)
        } else {
            # we are dealing with a continuous variable because it corresponds to only 1 row
            coef_table_list[[i]] <-
                as.data.frame(iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ],
                              stringsAsFactors = F)
        }
    }
    
    
    # extract the stats for each explanatory variable
    pooled_imputed_OR_df <-
        lapply(seq(fmla3_multi_var_list), function(i) {
            
            explain <- fmla3_multi_var_list[i]
            
            iglmlevels <- multilevels[[i]]
            if(is.null(iglmlevels)) iglmlevels <- explain # TRUE for continuous variables 
            iglmcoeff <- coef_table_list[[i]]
            iexpl <- data.frame('explvar' = explain, stringsAsFactors = F)
            
            out_table <-
                cbind.data.frame(iexpl,
                                 iglmlevels,
                                 iglmcoeff,
                                 stringsAsFactors = F,
                                 row.names = NULL)
            
            # we keep the first "row" because this will always correspond to the reference level, thus it's always NA
            icoef_table <- icoef_table[-(2:length(iglmlevels)), ]
            
            return(out_table)
        }) %>%
        bind_rows() %>%
        rename(varlevels = iglmlevels,
               multivariate_OR = OR_full,
               multivariate_pval = glmpvals) %>%
        select(-glmlevels) %>%
        mutate(impute_multivar_sig_p = case_when(
            multivariate_pval < 0.0001 ~ '**** <0.0001',
            multivariate_pval < 0.001 ~ '*** <0.001',
            multivariate_pval < 0.01 ~ '** <0.01',
            multivariate_pval < 0.05 ~ '* <0.05',
            TRUE ~ ' '
        )) %>%
        brename(c('multivariate_OR', 'multivariate_pval'),
                c(paste0('impute_multivariate_OR_(', 100*conflevel, '%)'), 'impute_multivariate_pval')) %>%
        mutate(depvarlevels = depvarlevels) %>% 
        mutate(impute_settings = paste0('methods = ', paste0(input_methods, collapse = ','), '; maxit = ', n_maxit, '; m = ', n_m)) %>%
        mutate(impute_dimensions = paste0('Rows = ', n_imputed_rows, '; Cols = ', n_imputed_cols)) 
    
    if(include_intercept_data) {
      pooled_imputed_OR_df <- 
        pooled_imputed_OR_df %>%
        mutate(intercept_glm_OR = intercept_glm_OR,
               intercept_glm_OR_conf_low = intercept_glm_OR_conf_low,
               intercept_glm_OR_conf_high = intercept_glm_OR_conf_high,
               intercept_glm_OR_pvalue = intercept_glm_OR_pvalue,
               intercept_row_data = intercept_row_data)
    }
    
    # if you want a separate table that also includes the estimates of incremental change in probability relative to baseline/reference probability
    if(include_percents_output) {
      
      # Make a table that gives the pcts instead of the odds ratios...
      pct_summary_table <- 
        summary(pool_fit_imp1, conf.int = TRUE, exponentiate = FALSE, conf.level = conflevel)
      
      pct_conf_table <- 
        pct_summary_table[, c((ncol(summary_table) - 1), ncol(summary_table))]
      
      pct_pvalue_table <-
        pct_summary_table[, 'p.value', drop = FALSE]
      
      colnames(pct_conf_table) <- c('glm_beta_conf_low','glm_beta_conf_high')
      
      pct_output_table_original <- 
        pct_output_table <- 
        cbind(pct_summary_table[, 'estimate', drop = FALSE],
              pct_conf_table) %>% 
        as.matrix()
      
      beta_intercept_row <- pct_output_table['(Intercept)', ]
      
      # identify the numeric cols
      numeric_cols <- names(numlevels[which(numlevels == 0)])
      
      # identify median value for those cols
      if(length(numeric_cols) > 0) {
        print("TEST")
        numeric_cols_median <- 
          .data %>%
          select(one_of(numeric_cols)) %>%
          summarise_all(.funs = list(~ median(., na.rm = TRUE))) %>% 
          as.matrix()
        
        numeric_cols_median_matrix <- 
          Reduce(f = function(a,b) cbind(a, b),
                 x = list(numeric_cols_median, numeric_cols_median, numeric_cols_median))
        
        # multiply their coefficients by their median value
        pct_output_table[numeric_cols, ] <- pct_output_table[numeric_cols, , drop = FALSE] * numeric_cols_median_matrix
        print("OK")
      }
      
      # reference probability log odds will be intercept plus any numeric values whose median value is not 0
      reference_probability_log_odds <- pct_output_table[c('(Intercept)', numeric_cols), 'estimate']
      sum_reference_probability_log_odds <- reference_probability_log_odds %>% sum() # we only want the "estimate"
      baseline_reference_probability <- exp(sum_reference_probability_log_odds) / (1 + exp(sum_reference_probability_log_odds))
      
      
      # Now need to calculate the incremental increase in probability above the reference. This is done by adding the log odds of the
      # reference case to the log odds of the incremental increase and then doing the inverse logit. But we don't need intercept because 
      # it does not incrementally change at all! 
      
      incremental_log_odds_table <- pct_output_table_original[-1, ] + sum_reference_probability_log_odds
      incremental_probability_table <- incremental_log_odds_table %>% bexpit() %>% {. - baseline_reference_probability}
      
      probcol_rownames <- row.names(incremental_probability_table)
      incremental_probability_table_formatting <- 
        apply(X = incremental_probability_table, 2, FUN = function(x) bpadding(x, makepercent = TRUE, num_decimals = 1))
      incremental_probability_table_formatting <- 
        incremental_probability_table_formatting %>%
        brename(old = c('estimate', 'glm_beta_conf_low', 'glm_beta_conf_high'), 
                new = c('glm_estimate_incremental_pct_change', 'glm_pcts_conf_low', 'glm_pcts_conf_high'))
      row.names(incremental_probability_table_formatting) <- probcol_rownames
      
      incremental_probability_table_formatting_final <- # add the Intercept row so we know the baseline probability...
        rbind(matrix(c(bpadding(baseline_reference_probability, 
                                makepercent = TRUE, 
                                num_decimals = 1) %>%
                         paste0(' <= Baseline Probability'), 
                       'NA', 'NA'), 
                     nrow = 1, 
                     dimnames = list('(Intercept)')), 
              incremental_probability_table_formatting)
      
      
      # The above tells us that for every unit increase in the independent variable, the probability for the outcome increases by ___%,
      # Using the data in the table. Note these are relative to the BASELINE probability using the reference cases (intercept + numerics), 
      # i.e. anything where the reference case is not "0" so we have to actually acount for their beta coefficients. 
      
      proportion_regression_final_output <- 
        Reduce(f = function(a, b) cbind(a, b),
               x = list(pct_output_table_original,
                        pct_pvalue_table,
                        incremental_probability_table_formatting_final)
        ) %>%
        tibble::rownames_to_column('glmlevels') %>%
        left_join(coef_full_table %>% select(glmlevels, OR_full_p),
                  by = 'glmlevels')
      
    }
    
    output_list <- 
      list('pooled_formatted_table' = pooled_imputed_OR_df,
           'raw_output_table' = full_summary_table  %>% tibble::rownames_to_column('variables_levels'),
           'impute_model_full' = imp1)
    
    if(include_percents_output) {
      output_list <- c(output_list,
                       'percentage_table' = list(proportion_regression_final_output))
    }
    
    return(output_list)
    
}


bformat_num <- function(num, dec = 2, cap = Inf, na_response = NA) {
  # try bformat_num(64.42424)
  # returns a string

  # note that this does not do much for really small, positive numbers! 
  # if dec = 2, anything below 0.01 becomes '0.00'; if dec = 4 anything below 0.0001 becomes '0.0000', etc
  if(is.na(num)) return(na_response)
  
  if(abs(num) > cap) {
    if(num < 0) {
      finalnum <- paste0('<-',cap) 
    } else {
      finalnum <- paste0('>',cap)
    }
    return(finalnum)
  }
  formatting <- paste0('%.',dec,'f')
  sprintf(formatting, num)
}

bvec_format_num <- function(numvec, dec = 2, cap = Inf, na_response = NA, alignWidth = F) {
  # try bvec_format_num(c(1,3.252, 0.00015342, 25245, 24513.124, 4.42424, -15125.1251, -10.1515))
  
  vec <- sapply(numvec, function(inum) bformat_num(inum, dec = dec, cap = cap, na_response = na_response))
  
  if(alignWidth) {
    maxn <- max(nchar(vec))
    vec <- sprintf(paste0('% ',maxn, 's'), vec)
  }
  
  return(vec)
}

bglmfull <- function(.data, dependentvar, ..., conflevel = 0.95, na.rm=F) {
  dep <- enquo(dependentvar)
  # expl <- enquos(...) # don't actualy need this...

  .data <- .data %>% mutate_if(is.logical, factor)
  
  left_join(bexplore_factors(.data = .data, dependent = !!dep, ..., na.rm = na.rm),
            buni_vs_full_glmtable(.data = .data, dependentvar = !!dep, ..., conflevel = conflevel),
            by = c('explvar', 'varlevels'))
}

bmd.pattern <- function(.matrix, rounddigits = 0) {
  # if rounddigits != 0, it will give you a character dataframe (so that we can control # of digits without
  # making all of the values have to have that # of digits)
  # if rounddigits == 0, it returns a numeric dataframe
  
  if('data.frame' %in% class(.matrix)) {
    .matrix <- 
      .matrix %>%
      mutate_if(is.character, ~as.integer(factor(.))) %>% # this method requires numeric, not character data
      mutate_if(is.factor, as.integer) # factors must be represented as numbers for the missing data
  }
  
  initmx <- mice::md.pattern(.matrix)
  
  noneMissing <- 
    row.names(initmx) %>% 
    gsub(pattern = ' ', replacement = '', x = .) %>% 
    as.numeric() %>%
    {.[1]}
  
  totalrows <- nrow(.matrix)
  totalMissingCases <- initmx[nrow(initmx), ncol(initmx)]
  anyMissingCases <- totalrows - noneMissing
  
  finalrow <- initmx[nrow(initmx), ]
  finalrow[length(finalrow)] <- anyMissingCases # the last value double counts overlap, we want total only
  
  totalPropMissing <- 100*(finalrow/totalrows)
  
  rownames(initmx)[nrow(initmx)] <- 'AnyNA'
  finalmx <- rbind(initmx, totalPropMissing)
  row.names(finalmx)[nrow(finalmx)] <- 'AnyNA_pct'
  colnames(finalmx)[ncol(finalmx)] <- 'TotalNA'
  mxrownames <- row.names(finalmx)
  # finalmx <- round(finalmx, 0) # all appear as integers
  finaldf <- as.data.frame(finalmx)
  row.names(finaldf) <-
    make.names(mxrownames, unique = TRUE) %>%
    {gsub(pattern='(.*\\d)(\\.)(\\d*)', replacement='\\1\\.\\.\\3', x = .)} %>% # change from 7.2 to 7_2
    {gsub(pattern='X\\.*', replacement = ' ', x = .)} %>% # remove the X.... that make.names() adds
    {sprintf(fmt = paste0('%',max(nchar(.)),'s'), .)} # align them 
  
  if(rounddigits != 0) {
    dfrownames <- rownames(finaldf)
    dffinalrow <- finaldf[nrow(finaldf), ] %>% round(rounddigits) %>% as.character() %>% paste0('%')
    finaldf <- finaldf %>% mutate_all(as.character)
    finaldf[nrow(finaldf), ] <- dffinalrow
    row.names(finaldf) <- dfrownames
    finaldf <- finaldf %>% select(TotalNA, everything())
  } else {
    dfrownames <- rownames(finaldf)
    finaldf <- finaldf %>% mutate_all(list( ~round(., rounddigits)))
    row.names(finaldf) <- dfrownames
    finaldf <- finaldf %>% select(TotalNA, everything())
  }
  
  return(finaldf)
}

bcox_table <- function(.data, timevar, censorvar, explanatoryvar, conflevel = 0.95,
                       deplevels = NULL, expllevels = NULL,
                       keepcols = c('stats', 'full'),
                       verbose = FALSE) {
    
    
    
    # works on only a single variable! 
    # expllevels allows you to set the explanatory levels that should be included in the model (rest is NA)
    # deplevels allows you to set the dependent levels that should be included in the model (rest is NA)
    tvar <- enquo(timevar)
    cvar <- enquo(censorvar)
    expl <- enquo(explanatoryvar)
    .data <- .data %>% mutate_if(is.logical, factor) # may need to use %>% droplevels()
    
    # side note: we use := here because can't unquote a name when assigning something to it
    # also, just so happens that when using !! on LHS, it has to evaluate to a string, thus the quo_name()
    if(!is.null(expllevels)) .data <- .data %>% mutate(!!quo_name(expl) := factor(!!expl, levels = expllevels))
    if(identical(keepcols, c('stats','full'))) keepcols <- 'stats'
    
    # remove NAs
    .data <- 
        .data %>% 
        select(!!tvar, !!cvar, !! expl) 
    
    if(verbose) {
        # show how many NAs / levels we have
        print("Total NA per col...")
        .totalna <- 
            .data %>% 
            summarise_all(.funs = list( ~sum(is.na(.)))) %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            arrange(desc(V1)) %>%
            brename('V1', 'TotalNAs')
        
        .totallevels <- 
            .data %>% 
            summarise_all(.funs = list( ~length(unique(.)))) %>%
            t() %>%
            as.data.frame() %>%
            tibble::rownames_to_column() %>%
            brename('V1', 'TotalLevels')
        
        print(left_join(.totalna, .totallevels, by = 'rowname'))
        print(paste0("Before removing NAs, your data is {",nrow(.data),"} rows by {", ncol(.data),"} columns"))
    }
    
    # finish processing data
    .data <- 
        .data %>%
        mutate_if(is.logical, factor) %>%
        mutate_if(is.character, factor) %>%
        bany_na(removeNARows = T) %>% # remove NAs for regression
        select(-anyNAs) %>%
        droplevels() # drop any levels that aren't being used
    
    if(verbose) {
        print(paste0("After removing NAs, your data is {",nrow(.data),"} rows by {", ncol(.data),"} columns"))
        
    }
    
    fmla1 <- as.formula(paste0('Surv(',quo_name(tvar),", ",quo_name(cvar), ") ~ ", quo_name(expl)))
    
    coxfit1 <- coxph(formula = fmla1, 
                     data = .data)
    
    # do something different for factor vs continuous
    if(length(coxfit1$xlevels) > 0) {
        # is factor
        coxlevels <- coxfit1$xlevels[[1]] 
    } else {
        # is continuous
        coxlevels <- quo_name(expl)
    }
    
    coxcoeff <- coef(coxfit1) # remember that the coefficients are not odds ratios until you exponentiate them (exp())
    coxHR <- c(NA, exp(coxcoeff)) 
    coxconf <- exp(confint.default(coxfit1, level = conflevel))
    # glmconf <- exp(confint(glmfit1, level = conflevel)) # for some reason confint.default is faster, the CI is *slightly* diff
    coxconf_lower <- c(NA, coxconf[1:nrow(coxconf),1])
    coxconf_upper <- c(NA, coxconf[1:nrow(coxconf),2])
    coxpvals <- c(NA, coef(summary(coxfit1))[,'Pr(>|z|)'][1:nrow(coxconf)])
    
    coxtable_raw <- data.frame(
        varlevels = coxlevels,
        coxHR = coxHR,
        cox_HR_conf_low = coxconf_lower,
        cox_HR_conf_high = coxconf_upper,
        coxpvals = coxpvals,
        stringsAsFactors = F
    )
    
    # add # of levels per varlevel
    # do something different for factor vs continuous
    if(length(coxfit1$xlevels) > 0) {
        # is factor
        levelnum <- 
            .data %>% 
            group_by(!! expl) %>% 
            summarise(total_per_varlevel = n()) %>% 
            dplyr::rename(varlevels = !!expl) %>%
            mutate(varlevels = as.character(varlevels))
    } else {
        # is continuous
        levelnum <- 
            data.frame(varlevels = quo_name(expl),
                       total_per_varlevel = nrow(.data))
    }
    
    coxtable <- 
        left_join(coxtable_raw,
                  levelnum, 
                  by = 'varlevels')
    
    if(length(coxfit1$xlevels) == 0) {
        # if continuous, we don't have an "intercept" as the top row, so we can just shave that off
        coxtable <- coxtable[2, ] # it should only be one row
    }
    
    formatted_coxtable <- 
        coxtable %>% 
        mutate(FMT_HR = bvec_format_num(coxHR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_PVAL = formatC(coxpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_up = bvec_format_num(cox_HR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
               FMT_low = bvec_format_num(cox_HR_conf_low, cap=100),
               FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
               HR_full_p = paste0(FMT_HR, ' ', FMT_conf, '; p<',FMT_PVAL),
               HR_full = paste0(FMT_HR, ' ', FMT_conf))
    
    return(formatted_coxtable)
    
}

  
buni_vs_full_coxtable <- function(.data, timevariable, censorvariable, ...,
                                  conflevel = 0.95, 
                                  uni_to_multi_pval_cutoff = 5E-2,
                                  uni_to_multi_stringency = c('any_under', 'none_over', 'all'),
                                  force_inclusion = NA # a string of any variables you want to be included in multi eg c('primary_purpose', 'br_phase2')
) {
    
    # get the time/censor variables and the independent variables
    tvar <- enquo(timevariable)
    cvar <- enquo(censorvariable)
    expls <- enquos(...)
    .data <- .data %>% mutate_if(is.logical, factor) # may need to use %>% droplevels()
    
    if(identical(uni_to_multi_stringency, c('any_under', 'none_over', 'all'))) {
        uni_to_multi_stringency <- 'any_under'
        
        # for any_under, if a factor has any level that on univariate has a significance < the cutoff, the variable is also included in the multi
        #     none_over, all of the factor levels have to exceed the pval cutoff in order to be included (this is probably unnecessarily stringent?)
        #     all, just include all the variables in both
    }
    
    var_ids <- paste0('time-variable = ', quo_name(tvar), '; censor-variable = ', quo_name(cvar))
    print(var_ids)
    # -------------------------------------------------------------------------#
    #                     Process Data And Ensure Proper Formatting            #
    # -------------------------------------------------------------------------#
    
    # select only the data we need
    .data <- 
        .data %>% 
        select(!!tvar, !!cvar, !!! expls) 

    # calculate dependent variable NAs
    .totaltimena <- 
      .data %>%
      summarise(totaltimena = sum(is.na(!!tvar))) %>%
      pull(totaltimena)

    .totaleventna <- 
      .data %>%
      summarise(totaleventna = sum(is.na(!!cvar))) %>%
      pull(totaleventna)
    
    # show how many NAs / levels we have
    print("Total NA per col...")
    .totalna <- 
        .data %>% 
        summarise_all(.funs = list( ~sum(is.na(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        arrange(desc(V1)) %>%
        brename('V1', 'TotalNAs')
    
    .totallevels <- 
        .data %>% 
        summarise_all(.funs = list( ~length(unique(.)))) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column() %>%
        brename('V1', 'TotalLevels')
    
    .NAtable <- left_join(.totalna, .totallevels, by = 'rowname')
    print(.NAtable)
    .oldrownum <- nrow(.data)
    .oldcolnum <- ncol(.data)
    print(paste0("Before removing NAs, your data is {", .oldrownum,"} rows by {", .oldcolnum,"} columns"))
    
    # finish processing data
    .data <- 
        .data %>%
        mutate_if(is.logical, factor) %>%
        mutate_if(is.character, factor) %>%
        bany_na(removeNARows = T) %>% # remove NAs for regression
        select(-anyNAs) %>%
        droplevels() # drop any levels that aren't being used
    
    
    .newrownum <- nrow(.data)
    .newcolnum <- ncol(.data)
    print(paste0("After removing NAs, your data is {", .newrownum,"} rows by {", .newcolnum,"} columns"))
    
    # ------------------------------------------------#
    #             Do the Univariate Analysis          #
    # ------------------------------------------------#
    
    print("Performing Univariate")
    
    univariate_HR_df <-
        lapply(expls, function(explain) {
            finaltable <-
                cbind.data.frame(explvar = quo_name(explain),
                                 bcox_table(.data, !!tvar, !!cvar, !!explain, conflevel = conflevel, keepcols = 'stats'),
                                 stringsAsFactors = F)
            if(nrow(finaltable) > 1) {
                # ie this is for a factor, not continuous
                row1 <- finaltable %>% slice(1:1) %>% mutate(HR_full = NA, coxpvals = NA)
                finaltable <- rbind.data.frame(row1,
                                               finaltable[2:nrow(finaltable), ],
                                               stringsAsFactors = F)
            }
            
            return(finaltable)
        }) %>%
        bind_rows() %>%
        select(explvar, varlevels, HR_full, coxpvals, total_per_varlevel) %>%
        rename(univariate_HR = HR_full,
               univariate_pval = coxpvals) %>%
        left_join(.NAtable, 
              by = c('explvar' = 'rowname')) %>% 
        group_by(explvar) %>%
        mutate(PctNAs = mean(TotalNAs) / .oldrownum) %>%
        ungroup() %>%
        mutate(
            ChangeDimensions = paste0(
                "Time-variable has ",
                .totaltimena,
                " (",
                round(.totaltimena / .oldrownum, 4) * 100,
                "%) NAs; Event-variable has ",
                .totaleventna,
                " (",
                round(.totaleventna / .oldrownum, 4) * 100,
                "%) NAs; After removing NAs, your data goes from (rows x columns) ({",
                .oldrownum,
                "} x {",
                .oldcolnum,
                "})  to  ({",
                .newrownum,
                "} x {",
                .newcolnum,
                "}), a drop of ",
                round((.oldrownum - .newrownum) / .oldrownum, 4) * 100,
                "%",
                collapse = ''
            )
        )
      
    
    print("Performing Multivariate")
    
    # --------------------------------------------------------------------------------------------#
    # 
    # ------------------------------- get the multivariate HR table ------------------------------
    #
    # --------------------------------------------------------------------------------------------#
    
    # Step 1: Make the formula that you need depending on how we want to select variables for 
    #         inclusion into our multivariate analysis (e.g. any_under vs all vs etc)
    
    
    expls_quo_names <- sapply(expls, function(ei) quo_name(ei))
    
    # fmla2 <- as.formula(paste0(quo_name(dep), 
    #                            ' ~ ',
    #                            paste0(expls_quo_names, collapse = ' + ')))
    
    # keep only those if one of their levels is < uni_to_multi_pval_cutoff
    fmla3_any_under_pval <- 
        univariate_HR_df %>%
        filter(univariate_pval < uni_to_multi_pval_cutoff) %>%
        pull(explvar) %>%
        unique()
    fmla3_any_over_pval <- 
        univariate_HR_df %>%
        filter(univariate_pval > uni_to_multi_pval_cutoff) %>%
        pull(explvar) %>%
        unique()
    fmla3_none_under_pval <- 
        setdiff(expls_quo_names, 
                fmla3_any_under_pval)
    fmla3_none_over_pval <- 
        setdiff(expls_quo_names, 
                fmla3_any_over_pval)
    
    if(uni_to_multi_stringency == 'any_under') {
        fmla3_multi_var_list <- fmla3_any_under_pval
    } else if(uni_to_multi_stringency == 'none_over') {
        fmla3_multi_var_list <- fmla3_none_over_pval
    } else if(uni_to_multi_stringency == 'all') {
        fmla3_multi_var_list <- expls_quo_names
    }
    
    if(!is.na(force_inclusion)) {
        fmla3_multi_var_list <- c(fmla3_multi_var_list, force_inclusion) %>% unique()
    }
    
    if(length(fmla3_multi_var_list) == 0) stop("\nYou do not have any variables that pass your multivariate inclusion criteria!\n")
    
    
    fmla3 <- paste0('Surv(',quo_name(tvar),", ",quo_name(cvar), ") ~ ", 
                    paste0(fmla3_multi_var_list, collapse = ' + '))
    
    # Step 2: Fit the actual cox regression ------------------------------------------------------------#
    
    coxfit2 <- coxph(formula = as.formula(fmla3),
                     data = .data)
    
    # Step 3: Grab the confidence intervals and other stats --------------------------------------------#
    
    
    # grab conf_intervals
    conf_table <-
        confint.default(coxfit2, level = conflevel) %>%
        exp() %>%  # you exponentiate the coefficients to get hazard ratios
        as.data.frame(stringsAsFactors = F)
    colnames(conf_table) <- c('cox_HR_conf_low','cox_HR_conf_high')
    
    if(conf_table %>% bany_na(removeNARows = F) %>% pull(anyNAs) %>% any()) {
        message("\nWarning: There may be variables which are linear combinations of other variables, and thus cannot
                be included in a cox regression!")
    }
    
    conf_table <- 
        conf_table %>%
        # as.data.frame(stringsAsFactors = F) %>%
        tibble::rownames_to_column('coxlevels') 
    
    coef_table <- 
        coef(summary(coxfit2)) %>%
        as.data.frame(stringsAsFactors = F) %>%
        tibble::rownames_to_column('coxlevels')
    
    stats_table <- 
        left_join(conf_table, # this should go first or else you lose the rows that are NAs
                  coef_table, 
                  by = 'coxlevels') %>%
        mutate(coxHR = `exp(coef)`,
               coxpvals = `Pr(>|z|)`)
    
    
    # Step 4: Format all the stats values so they look nice --------------------------------------------#
    
    
    coef_full_table <- 
        stats_table %>% 
        mutate(FMT_HR = bvec_format_num(coxHR, cap=100) %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_PVAL = formatC(coxpvals, digits = 2, format = 'e') %>% {sprintf(paste0('%', max(nchar(.), na.rm=T), 's'), .)},
               FMT_up = bvec_format_num(cox_HR_conf_high, cap=100) %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'), .)},
               FMT_low = bvec_format_num(cox_HR_conf_low, cap=100),
               FMT_conf = paste0('(',FMT_low,' - ',FMT_up,')') %>% {sprintf(paste0('%',max(nchar(.), na.rm=T),'s'),.)},
               HR_full_p = paste0(FMT_HR, ' ', FMT_conf, '; p<',FMT_PVAL),
               HR_full = paste0(FMT_HR, ' ', FMT_conf))  
    
    # Step 5: Add the reference level to all the variables --------------------------------------------#
    
    
    icoefrown <- 
        coef_full_table %>% 
        select(coxlevels, HR_full, coxpvals)
    # just need to turn the first row into NA
    icoefrow1 <- 
        icoefrown %>%
        slice(1:1) %>%
        mutate(coxlevels = NA,
               HR_full = NA,
               coxpvals = NA)
    # will iterate through using this table
    icoef_table <-
        rbind.data.frame(icoefrow1,
                         icoefrown[1:nrow(icoefrown), ], # glm has an intercept but cox doesn't
                         stringsAsFactors = F)
    
    # create list of coefficient tables
    multi_coef_row1 <- icoef_table[1, ]
    iicoef_table <- icoef_table[-1, ]
    
    # Goal: Match Coefficient Table Rows with the Corresponding Covariates / Levels of those Covariates...
    
    # I think there's a more elegant way than doing it this way...I used to use the glmfit2$xlevels,
    # but that was only appropriate if all the explains are factors, see example commented out below...
    #   
    # coef_breakpoints <- c(0, glmfit2$xlevels %>% lengths %>% {. - 1} %>% cumsum)
    # 
    # coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    # for(i in seq(length(coef_breakpoints)-1)) {
    #   coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
    # }
    # coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    # for(i in seq(length(coef_break_list))) {
    #   coef_table_list[[i]] <- 
    #     rbind.data.frame(multi_coef_row1,
    #                      iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
    #                      stringsAsFactors = F)
    # }
    
    # how many levels should we expect? 
    multilevels <- 
        .data[, fmla3_multi_var_list] %>%
        lapply(levels)
    
    numlevels <- # will be "0" for continuous, >1 for all factors, assumes that all remaining columns are either factor
        multilevels %>%
        lengths()
    
    # based on number of levels, we know how many rows we should expect. We subtract 1 because the first level is used
    # as the reference when we deal with factors, so there is no distinct coefficient for that
    # abs() to account for numeric columns having "0" levels, but we basically want that to be treated as 1, so abs(0-1) = 1
    coef_breakpoints <- c(0, numlevels %>% {. -1} %>% {cumsum(abs(.))}) 
    
    
    coef_break_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_breakpoints)-1)) {
        coef_break_list[[i]] <- c(coef_breakpoints[i]+1,coef_breakpoints[i+1])
    }
    coef_table_list <- vector(mode = 'list', length = length(coef_breakpoints)-1)
    for(i in seq(length(coef_break_list))) {
        if(diff(coef_break_list[[i]]) > 0) {
            # we are dealing with a factor b/c it must correspond to more than 1 row
            coef_table_list[[i]] <- 
                rbind.data.frame(multi_coef_row1,
                                 iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ], 
                                 stringsAsFactors = F)
        } else {
            # we are dealing with a continuous variable because it corresponds to only 1 row
            coef_table_list[[i]] <-
                as.data.frame(iicoef_table[coef_break_list[[i]][1]:coef_break_list[[i]][2], ],
                              stringsAsFactors = F)
        }
    }
    
    
    # extract the stats for each explanatory variable
    multivariate_HR_df <-
        lapply(seq(fmla3_multi_var_list), function(i) {
            
            explain <- fmla3_multi_var_list[i]
            
            icoxlevels <- multilevels[[i]]
            if(is.null(icoxlevels)) icoxlevels <- explain # TRUE for continuous variables 
            icoxcoeff <- coef_table_list[[i]]
            iexpl <- data.frame('explvar' = explain, stringsAsFactors = F)
            
            out_table <-
                cbind.data.frame(iexpl,
                                 icoxlevels,
                                 icoxcoeff,
                                 stringsAsFactors = F,
                                 row.names = NULL)
            
            # we keep the first "row" because this will always correspond to the reference level, thus it's always NA
            icoef_table <- icoef_table[-(2:length(icoxlevels)), ]
            
            return(out_table)
        }) %>%
        bind_rows() %>%
        rename(varlevels = icoxlevels,
               multivariate_HR = HR_full,
               multivariate_pval = coxpvals) %>%
        select(-coxlevels)
    
    # return(multivariate_HR_df)
    
    uni_n_multi_table_df <- 
        left_join(univariate_HR_df,
                  multivariate_HR_df,
                  by = c('explvar', 'varlevels')) %>%
        mutate(multivar_sig_p = case_when(
            multivariate_pval < 0.0001 ~ '**** <0.0001',
            multivariate_pval < 0.001 ~ '*** <0.001',
            multivariate_pval < 0.01 ~ '** <0.01',
            multivariate_pval < 0.05 ~ '* <0.05',
            TRUE ~ ' '
        )) %>% 
        mutate(variable_ids = var_ids) %>%
        select(-total_per_varlevel, -TotalNAs, -TotalLevels, -PctNAs, -ChangeDimensions, -variable_ids, everything(), 
               total_per_varlevel, TotalNAs, PctNAs, ChangeDimensions, variable_ids)
    
    return(uni_n_multi_table_df)
    
    }


generateSummaryGrowthDataStatistics <- function(inputdata, additional_columns = '', date_limits = c(2008, 2017),
                                                includeSponsor = TRUE, includeRegion = TRUE, 
                                                disease_columns = '') {
    
    # I use "spec" here equivalent to how 'neuro' or 'opto' is used in the main script...
    # I don't yet have any procedure for handling includeSponsor or includeRegion, but ideally it would skip 
    # calculation on these groups if we don't desire it (e.g. if we already filtered inputdata to be one region)
    
    # can later consider adding in some code to also calculate these stats for specialty areas (e.g. anxiety, depression, etc)
    
    # additional_columns should be of the form: list('repeatsponsor' = c('industry', 'NIH', 'academic'), 'some_disease1' = c('anxiety','ptsd','suicide'))
        # Side Note: For each element in list (e.g. 'repeatsponsor' or 'some_disease1'), there will be a total generated and reported in the output table
        # that calculates all the non-NA rows for that column (to be considered non-NA, *any* of the columns for that group must be non-NA)
        # This can be and is used when doing comparisons and percentages, etc. 
        # Caveat: Assumes that each of the new columns are logical columns, no code yet for handling factors...could probably expand those into a series
        #         of logical vectors as well
    # use of disease_columns is deprecated; its use can be sufficiently replaced by additional_columns now
    
    # date_limits is INCLUSIVE (i.e. [2008, 2017])

    include_disease <- (! identical('', disease_columns))
    include_additional <- c(! identical('', additional_columns))
    
    # generate names if there are additional column groups without names
    if(include_additional) {
        if(! identical(class(additional_columns), 'list')) stop("The 'additional_columns' variable must be input as a list of different column name vectors")
        current_names <- names(additional_columns)
        default_names <- paste0('additional_group_', seq(additional_columns))
        additional_names <- default_names
        additional_names <- additional_names[current_names != ''] <- current_names[current_names != '']
    }
    
    default_cols <- 
        c('nct_id', 'study_first_submitted_date', 'industry_any3', 
          'NorthAmerica', 'Europe', 'EastAsia', 'all_regions',
          'NorthAmerica_any_facilities', 'NorthAmerica_only_facilities', 'USA_any_facilities', 'USA_only_facilities', 
          'br_good3c_single_random', 'br_good4c_double_random', 'br_good5c_single_random_dmc', 'br_good6c_double_random_dmc')
    
    if(include_disease) {
        default_cols <- c(default_cols, disease_columns)
    }
    
    if(include_additional) {
        default_cols <- c(default_cols, unlist(additional_columns))
    }
        
    
    spec_trial_p1 <- 
        inputdata %>%
        select(one_of(default_cols)) %>%
        mutate(total_trials = TRUE,
               year_trial = year(study_first_submitted_date),
               neither3regions = pmap_lgl(list(!!! rlang::syms(c("NorthAmerica","Europe","EastAsia"))),
                                            function(...) ! any(sapply(list(...), function(i) i)))) %>% 
        select(-study_first_submitted_date)
    
    spec_trial_yeartotal <- 
        spec_trial_p1 %>%
        count(year_trial, 
              name = 'total_peryear')
    
    if(include_disease) {
        spec_trial_growth_disease_freq <- 
            spec_trial_p1 %>%
            select(nct_id, year_trial, one_of(disease_columns)) %>%
            mutate(all_disease_na = pmap_lgl(list(!!! rlang::syms(disease_columns)),
                                             function(...) all(sapply(list(...), function(i) is.na(i))))) %>%
            mutate(total_trials_disease = TRUE) %>%
            filter(!all_disease_na) %>%
            select(-nct_id, -all_disease_na) %>%
            group_by(year_trial) %>%
            summarise_all(.funs = list( ~sum(.)))
        
        spec_trial_growth_disease_pct <- 
            spec_trial_growth_disease_freq %>%
            mutate_at(vars(-one_of('year_trial', 'total_trials_disease')),
                      .funs = list( ~. / !! rlang::sym('total_trials_disease')))
    }
    
    if(include_additional) {
        print("Calculating data for custom columns...")
        
        # I use some purrr stuff here...
        additional_data <- 
            lapply(seq(additional_columns), function(i) {
            i_name <- additional_names[i]
            i_cols <- additional_columns[[i]]
            
            i_df_freq <- 
                spec_trial_p1 %>%
                select(nct_id, year_trial, one_of(i_cols)) %>%
                mutate(all_i_nas = pmap_lgl(list(!!! rlang::syms(i_cols)),
                                            function(...) all(sapply(list(...), function(i) is.na(i))))) %>%
                mutate(!! rlang::sym(paste0('total_trials_',i_name)) := TRUE) %>%
                filter(!all_i_nas) %>%
                select(-nct_id, -all_i_nas) %>%
                group_by(year_trial) %>%
                summarise_all(.funs = list( ~sum(., na.rm = TRUE)))
            
            i_df_final_freq <- 
                i_df_freq %>%
                rename_at(vars(
                    -one_of('year_trial', paste0('total_trials_',i_name))
                ),
                function(i)
                    paste0(i, '_freq'))
            
            i_df_pct <-
                i_df_freq %>%
                mutate_at(vars(-one_of('year_trial', paste0('total_trials_',i_name))),
                          .funs = list( ~. / !! rlang::sym(paste0('total_trials_',i_name))))
            
            i_df_final_pct <- 
                i_df_pct %>% 
                rename_at(vars(
                    -one_of('year_trial', paste0('total_trials_',i_name))
                ),
                function(i)
                    paste0(i, '_pct')) %>%
                select(- !! rlang::sym(paste0('total_trials_', i_name)))
            
            return(list(i_df_freq, i_df_pct, final_freq = i_df_final_freq, final_pct = i_df_final_pct))
        })
        
        additional_data_raw <- 
            additional_data %>%
            purrr::map(magrittr::extract, c(1,2)) # I don't think we actually use any of these again...
        
        additional_data_final <- 
            additional_data %>%
            purrr::map(magrittr::extract, c('final_freq','final_pct')) # I don't think we actually use any of this...
        
        
    }
    
    
    
    spec_trial_growth_sponsor_freq <- 
        spec_trial_p1 %>%
        select(nct_id, industry_any3, total_trials, year_trial) %>%
        filter(!is.na(industry_any3)) %>% 
        tidyr::pivot_wider(names_from = 'industry_any3', values_from = 'total_trials', values_fill = list(total_trials = FALSE)) %>%
        # mutate(total_trials_sponsor = TRUE) %>% # this would ignore all the NA in industry_any3
        select(-nct_id) %>%
        group_by(year_trial) %>%
        summarise_all(.funs = list( ~sum(.))) %>%
        ungroup() %>% 
        left_join(spec_trial_yeartotal,
                  by = 'year_trial') %>%
        rename(total_trials_sponsor = total_peryear) # This is because ALL trials have sponsorship data, we only have NAs because I removed the
    # U.S. Fed cases, and for the denominator for these I want to include the USFed cases...
    
    spec_trial_growth_sponsor_pct <- 
        spec_trial_growth_sponsor_freq %>%
        mutate_at(vars(-one_of('year_trial', 'total_trials_sponsor')),
                  .funs = list( ~. / !! rlang::sym('total_trials_sponsor')))
    
    spec_trial_growth_region_freq <-
        spec_trial_p1 %>%
        select(nct_id, year_trial, NorthAmerica, Europe, EastAsia, neither3regions,
               NorthAmerica_any_facilities, NorthAmerica_only_facilities, USA_any_facilities, USA_only_facilities, all_regions) %>% 
        filter(!is.na(all_regions)) %>% 
        mutate(total_trials_region = TRUE) %>%
        select(-nct_id, -all_regions) %>%
        group_by(year_trial) %>%
        summarise_all(.funs = list( ~sum(.)))
    
    spec_trial_growth_region_pct <- 
        spec_trial_growth_region_freq %>%
        mutate_at(vars(-one_of('year_trial', 'total_trials_region')),
                  .funs = list( ~. / !! rlang::sym('total_trials_region')))
    
    spec_trial_growth_goodc_freq <- 
        spec_trial_p1 %>%
        select(nct_id, total_trials, year_trial,
               br_good3c_single_random, br_good4c_double_random, br_good5c_single_random_dmc, br_good6c_double_random_dmc) %>%
        mutate(good3c_single_random = case_when(
            br_good3c_single_random ~ 'rigor3c_single_random_yes',
            !br_good3c_single_random ~ 'rigor3c_single_random_no'
        )) %>%
        mutate(good4c_double_random = case_when(
            br_good4c_double_random ~ 'rigor4c_double_random_yes',
            !br_good4c_double_random ~ 'rigor4c_double_random_no'
        )) %>%
        mutate(good5c_single_random_dmc = case_when(
            br_good5c_single_random_dmc ~ 'rigor5c_single_random_dmc_yes',
            !br_good5c_single_random_dmc ~ 'rigor5c_single_random_dmc_no'
        )) %>%
        mutate(good6c_double_random_dmc = case_when(
            br_good6c_double_random_dmc ~ 'rigor6c_double_random_dmc_yes',
            !br_good6c_double_random_dmc ~ 'rigor6c_double_random_dmc_no'
        )) %>%
        filter(!is.na(br_good3c_single_random)) %>% 
        select(-br_good3c_single_random,
               -br_good4c_double_random, 
               -br_good5c_single_random_dmc, 
               -br_good6c_double_random_dmc) %>%
        tidyr::pivot_longer(cols = starts_with('good'), 
                            names_to = 'rigor', 
                            values_to = 'rigor_value') %>%
        arrange(rigor, rigor_value) %>% # this is mostly just to make the order easier to read
        tidyr::pivot_wider(id_cols = c(nct_id, year_trial), 
                           names_from = rigor_value, 
                           values_from = total_trials, 
                           values_fill = list(total_trials = FALSE)) %>%
        mutate(total_trials_good3c_single_random = TRUE, 
               total_trials_good4c_double_random = TRUE, 
               total_trials_good5c_single_random_dmc = TRUE, 
               total_trials_good6c_double_random_dmc = TRUE) %>%
        select(-nct_id) %>%
        group_by(year_trial) %>%
        summarise_all(.funs = list( ~sum(.)))
    
    spec_trial_growth_goodc_pct <- 
        spec_trial_growth_goodc_freq %>%
        mutate_at(vars(starts_with('rigor3c')),
                  .funs = list( ~. / !! rlang::sym('total_trials_good3c_single_random'))) %>%
        mutate_at(vars(starts_with('rigor4c')),
                  .funs = list( ~. / !! rlang::sym('total_trials_good4c_double_random'))) %>%
        mutate_at(vars(starts_with('rigor5c')),
                  .funs = list( ~. / !! rlang::sym('total_trials_good5c_single_random_dmc'))) %>%
        mutate_at(vars(starts_with('rigor6c')),
                  .funs = list( ~. / !! rlang::sym('total_trials_good6c_double_random_dmc')))
    
    spec_trial_growth_total_all <- 
        inputdata %>% 
        mutate(year_trial = year(study_first_submitted_date)) %>%
        group_by(year_trial) %>%
        summarise(total_trials_all = n())
    
    list_growth_data <- list(
        spec_trial_growth_total_all, # has the "true" totals 
        spec_trial_growth_sponsor_freq %>%
            rename_at(vars(-one_of('year_trial','total_trials_sponsor')),
                      function(i) paste0(i, '_freq')),
        spec_trial_growth_region_freq %>%
            rename_at(vars(-one_of('year_trial','total_trials_region')),
                      function(i) paste0(i, '_freq')),
        spec_trial_growth_goodc_freq %>%
            rename_at(vars(starts_with('rigor')),
                      function(i) paste0(i, '_freq')),

        
        spec_trial_growth_sponsor_pct %>%
            rename_at(vars(-one_of('year_trial','total_trials_sponsor')),
                      function(i) paste0(i, '_pct')) %>%
            select(-total_trials_sponsor),
        spec_trial_growth_region_pct %>%
            rename_at(vars(-one_of('year_trial','total_trials_region')),
                      function(i) paste0(i, '_pct')) %>%
            select(-total_trials_region),
        spec_trial_growth_goodc_pct %>%
            rename_at(vars(starts_with('rigor')),
                      function(i) paste0(i, '_pct')) %>%
            select(-starts_with('total_')) # don't need the "totals" from the percentage tables, only the frequency tables
    )
    
    if (include_disease) {
        list_growth_data <- c( # append the disease data to the list as well...
            list_growth_data,
            list(
                spec_trial_growth_disease_freq %>%
                    rename_at(vars(
                        -one_of('year_trial', 'total_trials_disease')
                    ),
                    function(i)
                        paste0(i, '_freq')),
                spec_trial_growth_disease_pct %>%
                    rename_at(vars(
                        -one_of('year_trial', 'total_trials_disease')
                    ),
                    function(i)
                        paste0(i, '_pct')) %>%
                    select(-total_trials_disease)
            )
        )
    }
    
    if(include_additional) {
        list_growth_data <- 
            c(list_growth_data, 
              Reduce(f = function(a,b) c(a,b),
                     x = additional_data_final))
    }
    
    spec_trial_growth_data <- 
        Reduce(f = function(a, b) left_join(a, b, by = 'year_trial'),
               x = list_growth_data
                   ) %>%
        filter(year_trial >= date_limits[1],
               year_trial <= date_limits[2])
    
    aag_spec_trials <- 
        spec_trial_growth_data %>%
        mutate_at(vars(-year_trial), 
                  function(x) (x - lag(x))/lag(x)) %>%
        summarise_at(vars(-year_trial), 
                     function(x) mean(x, na.rm = TRUE)) %>%
        t()
    
    cagr_spec_trials <- 
        spec_trial_growth_data %>%
        summarise_at(vars(-year_trial),
                     function(x) ((last(x)/first(x))^ (1/(length(x) - 1))) - 1) %>%
        t()
    
    kendall_spec_trials <- 
        spec_trial_growth_data %>%
        summarise_at(vars(-year_trial), 
                     function(x) Kendall::MannKendall(x)$sl) %>%
        t()
    
    bonferroni_kendall_spec_trials_pct <- # add a correction by multiplying pval by # of vars we're performing this on (this is probably too stringent)
        spec_trial_growth_data %>%
        summarise_at(vars(-year_trial), 
                     function(x) ((ncol(spec_trial_growth_data) - 1)/2) * Kendall::MannKendall(x)$sl) %>% 
        Reduce(f = cbind) %>%
        t()
    
    ols_spec_trials <-
        spec_trial_growth_data %>%
        summarise_at(vars(-year_trial), 
                     function(x) summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)']) %>%
        t()
    
    bonferroni_ols_spec_trials <-
        spec_trial_growth_data %>%
        summarise_at(vars(-year_trial), 
                     function(x) ((ncol(spec_trial_growth_data) - 1)/2) * summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)']) %>%
        t()
    
    armitage_freq_groups <- 
        list(group1 = c('NorthAmerica','Europe','EastAsia', 'neither3regions') %>% paste0('_freq'),
             group2 = c('Industry', 'NIH', 'Other') %>% paste0('_freq'))
    
    if(include_disease) {
        armitage_freq_groups <- c(armitage_freq_groups, list(
            group3 = disease_columns %>% paste0('_freq')
        ))
    }
    
    
    if(include_additional) {
        armitage_freq_groups <- c(armitage_freq_groups, 
                                  lapply(additional_columns, function(ivec) paste0(ivec, '_freq')))
    }
    
    armitage_freq_groups <- armitage_freq_groups[armitage_freq_groups %>% lengths() %>% {. > 1}] # only keep if there's more than one group! 

    armitage_spec_freq <- 
        lapply(armitage_freq_groups, 
               FUN = function(igroup) {
                   idf <-
                       spec_trial_growth_data %>%
                       select(year_trial, one_of(igroup))
                   idf %>%
                       tidyr::gather(-year_trial, key = 'bvar', value = 'freq') %>%
                       xtabs(formula = freq ~ bvar + year_trial) %>%
                       coin::chisq_test(scores = list('year_trial' = seq(nrow(idf)))) %>%
                       {
                           iresult <- .
                           iresults <-
                               c(
                                   'armitage_chisq' = coin::statistic(iresult),
                                   'armitage_pval' = coin::pvalue(iresult)
                               )
                           trialvar <- colnames(idf)[-1]
                           ifinal <-
                               as.data.frame(trialvar, stringsAsFactors = F) %>%
                               mutate(armitage_chisq = iresults[1],
                                      armitage_pval = iresults[2])
                           return(ifinal)
                       }
               }
        ) %>%
        bind_rows()
    
    
    spec_growth_statistics <- 
        cbind.data.frame(aagr = aag_spec_trials,
                         cagr = cagr_spec_trials,
                         kendall_pval = kendall_spec_trials,
                         ols_pval = ols_spec_trials) %>%
        tibble::rownames_to_column('trialvar') %>%
        tbl_df() %>%
        left_join(armitage_spec_freq, # are the industry vs NIH vs other stat sig different from each other?
                  by = 'trialvar') %>%
        mutate(notes = "Mann-Kendall is 2-sided pval test for monotonic trend; ols is 2-sided pval for linear trend; armitage is test for sig difference between curves within each group (funding, region)")
        
    return(list("Data" = spec_trial_growth_data, "Statistics" = spec_growth_statistics))
}

# This is a modified version of the above that is "agnostic", does not have any assumed columns except for the presence of year_trial

bgenerateSummaryGrowthDataStatistics <- function(inputdata, additional_columns = '', date_limits = c(2008, 2017),
                                                 includeSponsor = TRUE, includeRegion = TRUE, 
                                                 disease_columns = '') {
  
  # I use "spec" here equivalent to how 'neuro' or 'opto' is used in the main script...
  # I don't yet have any procedure for handling includeSponsor or includeRegion, but ideally it would skip 
  # calculation on these groups if we don't desire it (e.g. if we already filtered inputdata to be one region)
  
  # can later consider adding in some code to also calculate these stats for specialty areas (e.g. anxiety, depression, etc)
  
  # additional_columns should be of the form: list('repeatsponsor' = c('industry', 'NIH', 'academic'), 'some_disease1' = c('anxiety','ptsd','suicide'))
  # Side Note: For each element in list (e.g. 'repeatsponsor' or 'some_disease1'), there will be a total generated and reported in the output table
  # that calculates all the non-NA rows for that column (to be considered non-NA, *any* of the columns for that group must be non-NA)
  # This can be and is used when doing comparisons and percentages, etc. 
  # Caveat: Assumes that each of the new columns are logical columns, no code yet for handling factors...could probably expand those into a series
  #         of logical vectors as well
  # use of disease_columns is deprecated; its use can be sufficiently replaced by additional_columns now
  
  # date_limits is INCLUSIVE (i.e. [2008, 2017])
  
  include_additional <- c(! identical('', additional_columns))
  
  # generate names if there are additional column groups without names
  if(include_additional) {
    if(! identical(class(additional_columns), 'list')) stop("The 'additional_columns' variable must be input as a list of different column name vectors")
    current_names <- names(additional_columns)
    default_names <- paste0('additional_group_', seq(additional_columns))
    additional_names <- default_names
    additional_names <- additional_names[current_names != ''] <- current_names[current_names != '']
  }
  
  # default_cols <- 
  #   c('nct_id', 'study_first_submitted_date', 'industry_any3', 
  #     'NorthAmerica', 'Europe', 'EastAsia', 'all_regions',
  #     'NorthAmerica_any_facilities', 'NorthAmerica_only_facilities', 'USA_any_facilities', 'USA_only_facilities', 
  #     'br_good3c_single_random', 'br_good4c_double_random', 'br_good5c_single_random_dmc', 'br_good6c_double_random_dmc')
  
  default_cols <- 'year_trial'
  
  # if(include_additional) {
  #   default_cols <- c(default_cols, unlist(additional_columns))
  # }
  
  if('year_trial' %nin% colnames(inputdata)) stop("There must be a year column named 'Year Trial' for this function to work...")
  
  spec_trial_p1 <- 
    inputdata %>%
    # select(one_of(default_cols)) %>% #*# original 
    select(one_of(c(default_cols, unlist(additional_columns)))) %>% #*# new
    mutate(total_trials = TRUE)
  
  spec_trial_yeartotal <- 
    spec_trial_p1 %>%
    count(year_trial, 
          name = 'total_peryear')
  
  if(include_additional) {
    print("Calculating data for custom columns...")
    
    # I use some purrr stuff here...
    additional_data <- 
      lapply(seq(additional_columns), function(i) {
        i_name <- additional_names[i]
        i_cols <- additional_columns[[i]]
        
        i_df_freq <- 
          spec_trial_p1 %>%
          # select(nct_id, year_trial, one_of(i_cols)) %>%
          select(year_trial, one_of(i_cols)) %>%
          mutate(all_i_nas = pmap_lgl(list(!!! rlang::syms(i_cols)),
                                      function(...) all(sapply(list(...), function(i) is.na(i))))) %>%
          mutate(!! rlang::sym(paste0('total_trials_',i_name)) := TRUE) %>%
          filter(!all_i_nas) %>%
          # select(-nct_id, -all_i_nas) %>%
          select(-all_i_nas) %>%
          group_by(year_trial) %>%
          summarise_all(.funs = list( ~sum(., na.rm = TRUE)))
        
        i_df_final_freq <- 
          i_df_freq %>%
          rename_at(vars(
            -one_of('year_trial', paste0('total_trials_',i_name))
          ),
          function(i)
            paste0(i, '_freq'))
        
        i_df_pct <-
          i_df_freq %>%
          mutate_at(vars(-one_of('year_trial', paste0('total_trials_',i_name))),
                    .funs = list( ~. / !! rlang::sym(paste0('total_trials_',i_name))))
        
        i_df_final_pct <- 
          i_df_pct %>% 
          rename_at(vars(
            -one_of('year_trial', paste0('total_trials_',i_name))
          ),
          function(i)
            paste0(i, '_pct')) %>%
          select(- !! rlang::sym(paste0('total_trials_', i_name)))
        
        return(list(i_df_freq, i_df_pct, final_freq = i_df_final_freq, final_pct = i_df_final_pct))
      })
    
    additional_data_raw <- 
      additional_data %>%
      purrr::map(magrittr::extract, c(1,2)) # I don't think we actually use any of these again...
    
    additional_data_final <- 
      additional_data %>%
      purrr::map(magrittr::extract, c('final_freq','final_pct')) # I don't think we actually use any of this...
    
    
  }
  
  spec_trial_growth_total_all <- 
    inputdata %>% 
    # mutate(year_trial = year(study_first_submitted_date)) %>%
    group_by(year_trial) %>%
    summarise(total_trials_all = n())
  
  list_growth_data <- list(
    spec_trial_growth_total_all
  )
  
  if(include_additional) {
    list_growth_data <- 
      c(list_growth_data, 
        Reduce(f = function(a,b) c(a,b),
               x = additional_data_final))
  }
  
  spec_trial_growth_data <- 
    Reduce(f = function(a, b) left_join(a, b, by = 'year_trial'),
           x = list_growth_data
    ) %>%
    filter(year_trial >= date_limits[1],
           year_trial <= date_limits[2])
  
  
  aag_spec_trials <- 
    spec_trial_growth_data %>%
    mutate_at(vars(-year_trial), 
              function(x) (x - lag(x))/lag(x)) %>%
    summarise_at(vars(-year_trial), 
                 function(x) mean(x, na.rm = TRUE)) %>%
    t()
  
  cagr_spec_trials <- 
    spec_trial_growth_data %>%
    summarise_at(vars(-year_trial),
                 function(x) ((last(x)/first(x))^ (1/(length(x) - 1))) - 1) %>%
    t()
  
  kendall_spec_trials <- 
    spec_trial_growth_data %>%
    summarise_at(vars(-year_trial), 
                 function(x) Kendall::MannKendall(x)$sl) %>%
    t()
  
  bonferroni_kendall_spec_trials_pct <- # add a correction by multiplying pval by # of vars we're performing this on (this is probably too stringent)
    spec_trial_growth_data %>%
    summarise_at(vars(-year_trial), 
                 function(x) ((ncol(spec_trial_growth_data) - 1)/2) * Kendall::MannKendall(x)$sl) %>% 
    Reduce(f = cbind) %>%
    t()
  
  ols_spec_trials <-
    spec_trial_growth_data %>%
    summarise_at(vars(-year_trial), 
                 function(x) summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)']) %>%
    t()
  
  bonferroni_ols_spec_trials <-
    spec_trial_growth_data %>%
    summarise_at(vars(-year_trial), 
                 function(x) ((ncol(spec_trial_growth_data) - 1)/2) * summary(lm(x ~ year_trial, data = .))$coefficients[2, 'Pr(>|t|)']) %>%
    t()
  
  armitage_freq_groups <- 
    list(group1 = default_cols %>% paste0('_freq'))
  
  if(include_additional) {
    armitage_freq_groups <- c(armitage_freq_groups, 
                              lapply(additional_columns, function(ivec) paste0(ivec, '_freq')))
  }
  
  armitage_freq_groups <- armitage_freq_groups[armitage_freq_groups %>% lengths() %>% {. > 1}] # only keep if there's more than one group! 
  
  armitage_spec_freq <- 
    lapply(armitage_freq_groups, 
           FUN = function(igroup) {
             idf <-
               spec_trial_growth_data %>%
               select(year_trial, one_of(igroup))
             idf %>%
               tidyr::gather(-year_trial, key = 'bvar', value = 'freq') %>%
               xtabs(formula = freq ~ bvar + year_trial) %>%
               coin::chisq_test(scores = list('year_trial' = seq(nrow(idf)))) %>%
               {
                 iresult <- .
                 iresults <-
                   c(
                     'armitage_chisq' = coin::statistic(iresult),
                     'armitage_pval' = coin::pvalue(iresult)
                   )
                 trialvar <- colnames(idf)[-1]
                 ifinal <-
                   as.data.frame(trialvar, stringsAsFactors = F) %>%
                   mutate(armitage_chisq = iresults[1],
                          armitage_pval = iresults[2])
                 return(ifinal)
               }
           }
    ) %>%
    bind_rows()
  
  
  spec_growth_statistics <- 
    cbind.data.frame(aagr = aag_spec_trials,
                     cagr = cagr_spec_trials,
                     kendall_pval = kendall_spec_trials,
                     ols_pval = ols_spec_trials) %>%
    tibble::rownames_to_column('trialvar') %>%
    tbl_df() %>%
    left_join(armitage_spec_freq, # are the industry vs NIH vs other stat sig different from each other?
              by = 'trialvar') %>%
    mutate(notes = "Mann-Kendall is 2-sided pval test for monotonic trend; ols is 2-sided pval for linear trend; armitage is test for sig difference between curves within each group (funding, region)")
  
  return(list("Data" = spec_trial_growth_data, "Statistics" = spec_growth_statistics))
}



download_fdaaatracker_data <- function() {
  
  # this will download data from the FDAAA Tracker API
  
  # for now does not take any parameters
  
  call1 <- new_call <- 'http://fdaaa.trialstracker.net/api/trials/?length=300&start=0'
  
  get_trials <- httr::GET(url = call1)
  
  get_trials_text <- httr::content(get_trials, 'text')
  
  get_trials_list <- jsonlite::fromJSON(get_trials_text, flatten = TRUE) 
  
  total_num_trials <- get_trials_list$recordsTotal
  
  expected_num_loops <- ceiling(total_num_trials / 300) - 1
  
  current_df <- get_trials_list$results
  
  current_trials_list <- get_trials_list
  
  current_next <- current_trials_list[['next']]
  
  current_loop <- 1
  
  while(!is.null(current_next) & current_loop < (expected_num_loops+20)) {
    
    print(paste0("Current iteration is ", current_loop, " and last call was ", new_call))
    
    new_call <- current_trials_list[['next']]
    
    new_trials <- httr::GET(url = new_call)
    
    new_trials_list <- jsonlite::fromJSON(httr::content(x = new_trials, as = 'text'), 
                                          flatten = TRUE)
    
    current_df <- 
      rbind(current_df,
            new_trials_list$results)
    
    current_trials_list <- new_trials_list
    
    current_loop <- current_loop + 1
    
    current_next <- new_trials_list[['next']]
    
  }
  
  print("Download complete")
  print(paste0("Expected ", total_num_trials, " trial results... downloaded ", nrow(current_df), " trial results."))
  
  current_df <- current_df %>% mutate(download_date = Sys.Date())
  
  return(current_df)
  
}





# Some Black Magic To Assign Multiple Variables At Once --------------------------------------------------------------------------------------

# taken from this StackOverflow page: https://stackoverflow.com/questions/7519790/assign-multiple-new-variables-on-lhs-in-a-single-line

# Note that this assignment is kind of slow... I would be wary of using it extensively, would mainly use it only for convenience

# Generic form
'%#%' = function(l, r, ...) UseMethod('%#%')

# Binary Operator
'%#%.lbunch' = function(l, r, ...) {
    Envir = as.environment(-1)
    
    if (length(r) > length(l))
        warning("RHS has more args than LHS. Only first", length(l), "used.")
    
    if (length(l) > length(r))  {
        warning("LHS has more args than RHS. RHS will be repeated.")
        r <- extendToMatch(r, l)
    }
    
    for (II in 1:length(l)) {
        do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
    }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
    s <- length(source)
    d <- length(destin)
    
    # Assume that destin is a length when it is a single number and source is not
    if(d==1 && s>1 && !is.null(as.numeric(destin)))
        d <- destin
    
    dif <- d - s
    if (dif > 0) {
        source <- rep(source, ceiling(d/s))[1:d]
    }
    return (source)
}

# Grouping the left hand side
g = function(...) {
    List = as.list(substitute(list(...)))[-1L]
    class(List) = 'lbunch'
    return(List)
}
