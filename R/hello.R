#################################################################
##                      Funktionsoversigt                      ##
#################################################################

funktioner <- function() {
  cat("\n pakr:     tidyverse, janitor, bannerC \n\n",
      
      "\n ***** TABELLER MV. ***** \n\n",
      "tabl:     Frekvenstabeller og krydstabeller \n",
      "          - tabl(var1) eller tabl(var1, var2) \n",
      "          - ELLER tabl(var1, wt = vægt) \n\n"
      
      "grp:      Grouped means \n",
      "          - grp(group_var, mean_var) \n\n",
      
      "alltabs:  Frekvenstabeller for alle variable i x \n",
      "          (ud over dem med mere end 20 udfald) \n",
      "          - alltabs(x) \n\n",
      
      "allkryds: Krydstabeller for alle variable i x \n",
      "          (ud over dem med mere end 20 udfald) \n",
      "          - allkryds(x, krydsvar) \n\n",
      
      "tabzz:    Mange frekvenstabeller \n",
      "          - tabzz(var1, var2, var3, ...) \n\n",
      
      "kryzz:    Mange krydstabeller \n",
      "          - kryzz(krydsvar, var1, var2 ,...) \n\n",
      
      "multi:    Nyttigt ved afkrydsningsfelter \n",
      "          - multi(prefix, valgt, sort = F, dset = d) \n",
      "          - fx multi(t_, \"Meget enig\") / multi(t_, \"Valgt\", sort = T) \n\n",
      
      "multi2:   Samme, men nu som krydstabel! \n",
      "          - multi2(prefix, valgt, krydsvar, sort = F, dset=d) \n",
      "          - fx multi2(t_, \"Meget enig\", region) \n\n",

      "flipden:  Flipper en tabel \n",
      "          - flipden(x) \n",
      "          - fx tabl(køn, branche) %>% flipden() \n\n",
      
      "excel:    Excel-krydstabeller for alle variable i x \n",
      "          - excel(x, 'filnavn', krydsvar1, krydsvar2, ..., totaler=T) \n",
      "          - fx excel(d, 'Tabeller', køn, region, segment, totaler=F) \n\n",
      
      "\n ***** GRAFER ****** \n\n",
      "bar:      Søjlediagram \n",
      "          - bar(var) \n\n",
      
      "scat:     Scatterplot uden noget \n",
      "          - scat(var1, var2) \n\n",
      
      "scat2:    Scatterplot med jitter og LOESS \n",
      "          - scat2(var1, var2) \n\n",
      
      "\n ***** ANDET ****** \n\n",
      "behold:   Behold kun nogle datasæt \n",
      "          - behold(dd, d2) \n\n",
      
      "get_prefixes:  Find prefixes til multi-tabeller \n",
      "               - get_prefixes() \n\n",
      
      
      "\n PS. Husk, at dit datasæt skal hedde 'd' \n")
}

funktioner()

##################################################################
##                             pakr                             ##
##################################################################

pakr <- function(...) {
  pacman::p_load(tidyverse, janitor, bannerCommenter)
  packages <- rlang::enquos(...)
  for (package in packages) {
    package_name <- rlang::as_name(substitute(package))
    library(package_name, character.only = TRUE)
  }
}

##################################################################
##                             test                             ##
##################################################################

pik <- function() {print("pik")}

##################################################################
##                             tabl                             ##
##################################################################

tabl_backup <- function(var1, var2, dset=d) {
  if (missing(var2)) {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {
      cat(" ", x, "\n\n")
    }
    y <- dset %>% janitor::tabyl(!!rlang::enquo(var1)) %>% janitor::adorn_pct_formatting()
    return(y)
  } else {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    str <- dplyr::as_label(rlang::enquo(var2))
    y <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {
      cat(" ", x, "[X]", y, "\n\n")
    }
    y <- dset %>% janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(var2)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>% janitor::adorn_pct_formatting()
    return(y)
  }}


tabl <- function(var1, var2, dset = d, wt) {
  if (missing(var2)) {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {
      cat(" ", x, "\n\n")
    }
    if (missing(wt)) {
      y <- dset %>%
        janitor::tabyl(!!rlang::enquo(var1)) %>%
        janitor::adorn_pct_formatting()
    } else {
      y <- dset %>%
        janitor::tabyl(!!rlang::enquo(var1), weights = !!rlang::enquo(wt)) %>%
        janitor::adorn_pct_formatting()
    }
    return(y)
  } else {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    str <- dplyr::as_label(rlang::enquo(var2))
    y <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {
      cat(" ", x, "[X]", y, "\n\n")
    }
    if (missing(wt)) {
      y <- dset %>%
        janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(var2)) %>%
        janitor::adorn_totals() %>%
        janitor::adorn_percentages(denominator = "col") %>%
        janitor::adorn_pct_formatting()
    } else {
      y <- dset %>%
        janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(var2), weights = !!rlang::enquo(wt)) %>%
        janitor::adorn_totals() %>%
        janitor::adorn_percentages(denominator = "col") %>%
        janitor::adorn_pct_formatting()
    }
    return(y)
  }
}



#################################################################
##                             grp                             ##
#################################################################

grp <- function(gruppe, ..., dset=d) {
  args <- rlang::enquos(...)
  for (var in args) {
    str <- dplyr::as_label(var)
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")} else {cat(" ", str, "\n")}
    dset %>% dplyr::group_by(!!rlang::enquo(gruppe)) %>%
      dplyr::summarise(mean = mean(!!var, na.rm=T)) %>% print()
    cat("\n\n")
  }
}

##################################################################
##                            Grafer                            ##
##################################################################

# Søjlediagram
bar <- function(var, dset=d) {
  dset %>% ggplot2::ggplot(aes(x = !!rlang::enquo(var))) +
    ggplot2::geom_bar(aes(y = (..count..)/sum(..count..))) +
    ggplot2::scale_y_continuous(labels=scales::percent) +
    ggplot2::ylab("andel")
}

# Scatterplot uden noget
scat <- function(var1, var2, dset=d) {
  dset %>% ggplot2::ggplot(aes(x = !!rlang::enquo(var1), y = !!rlang::enquo(var2))) +
    ggplot2::geom_point()
}

# Scatterplot 2
scat2 <- function(var1, var2, dset=d) {
  dset %>% ggplot2::ggplot(aes(x = !!rlang::enquo(var1), y = !!rlang::enquo(var2))) +
    ggplot2::geom_point() + ggplot2::geom_jitter(alpha=.5) +
    ggplot2::geom_smooth()
}


#################################################################
##                           alltabs                           ##
#################################################################

alltabs <- function(x) {
  navne <- names(x)
  for (var in navne) {
    y <- labelled::var_label(x[[var]])
    if (!is.null(y)) {
      cat(" ", y, "\n\n")
    }
    t <- x %>% janitor::tabyl(!!var) %>% janitor::adorn_pct_formatting()
    if (length(t$n) < 20) {
      print(t)
      cat("\n\n")
    }
  }
}

#################################################################
##                            tabzz                            ##
#################################################################

tabzz <- function(..., dset=d) {
  args <- rlang::enquos(...)
  for (var in args) {
    str <- dplyr::as_label(var)
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n\n")}
    dset %>% janitor::tabyl(!!var) %>%
      janitor::adorn_pct_formatting() %>% print()
    cat("\n\n")
  }
}


#################################################################
##                            kryzz                            ##
#################################################################

kryzz <- function(kryds, ..., dset=d) {
  args <- rlang::enquos(...)
  for (var in args) {
    str <- dplyr::as_label(var)
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n\n")}
    dset %>% janitor::tabyl(!!var, !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
    cat("\n\n")
  }
}


#################################################################
##                           allkryds                          ##
#################################################################

allkryds <- function(x, kryds) {
  z <- x %>% dplyr::select(-!!rlang::enquo(kryds))
  navne <- names(z)
  for (var in navne) {
    y <- labelled::var_label(x[[var]])
    if (!is.null(y)) {cat(" ", y, "\n\n")}
    t <- x %>% janitor::tabyl(!!var) %>% janitor::adorn_pct_formatting()
    u <- x %>% janitor::tabyl(!!sym(var), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting()
    if (length(t$n) < 20) {
      print(u)
      cat("\n\n")
    }
  }
}


##################################################################
##                         get_prefixes                         ##
##################################################################

get_prefixes <- function(dset = d) {
  variable_names = names(dset)
  prefix_parts <- lapply(variable_names, function(x) {
    parts <- strsplit(x, "_", fixed = TRUE)[[1]]
    if (length(parts) > 1) {paste0(parts[1], "_")} else {NA}})
  prefixes <- unlist(prefix_parts)
  prefix_counts <- table(prefixes)
  repeated_prefixes <- names(prefix_counts[prefix_counts > 2])
  a <- repeated_prefixes[!is.na(repeated_prefixes)]
  return(a)
}

get_2udfald <- function(dset = d) {
  a <- get_prefixes(dset)
  liste <- c()
  for (i in a) {
    b <- dset %>% dplyr::select(dplyr::starts_with(i))
    tab <- janitor::tabyl(b[[1]])
    if (length(tab$n)==2) {liste = c(liste, i)}}
  return(liste)
}


##################################################################
##                            multi                             ##
##################################################################

multi <- function(prefix, valgt, sort = F, dset = d, advice=F, maksimum=40) {
  zz <- dset %>%
    dplyr::select(dplyr::starts_with(rlang::quo_text(rlang::enquo(prefix))))
  
  z <- zz %>% tidyr::drop_na()
  if (count(z)[1,1] != count(zz)[1,1]) {
    print("Jeg har droppet NAs")
  }
  
  z <- z %>% dplyr::mutate_all(~ dplyr::if_else(. == valgt, "Valgt", "Ikke valgt"))
  valgt <- "Valgt"
  
  z <- z %>% dplyr::mutate_all(~ stringr::str_replace_all(., valgt, "øøøøø"))
  o <- z %>% names()
  w <- c()
  for (var in o) {
    k <- tabl(!!var, dset=z)
    p <- k %>% dplyr::arrange(!!var) %>% utils::tail(1)
    if (p[1,1]!="øøøøø") {q <- "0%"} else {q <- p[,3]}
    w <- append(w, q)
  }
  
  y <- labelled::var_label(dset[[o[1]]])
  y <- as.character(y)
  if (!is.null(y) & advice==T) {y <- strsplit(y, " - ", fixed = TRUE)[[1]][[1]]}
  cat(" ", y, "\n\n")
  
  if (advice == T) {
    flops <- c()
    for (i in o) {
      flop <- labelled::var_label(dset[[i]])
      flop <- strsplit(flop, " - ", fixed = TRUE)[[1]][[2]] %>% substr(1, maksimum)
      flops <- c(flops, flop)
    }
  } else {flops <- o}
  
  if (sort == T) {
    tibble(name = flops, percent = w) %>%
      dplyr::mutate(percent_numeric = as.numeric(sub("%", "", percent))) %>% 
      dplyr::arrange(dplyr::desc(percent_numeric)) %>% 
      dplyr::select(-percent_numeric) %>% return()
  } else {tibble(name = flops, percent = w) %>% return()}
}

multi2 <- function(prefix, valgt, krydsvar, sort = FALSE, dset = d, advice = FALSE, maksimum = 40) {
  pref_txt  <- rlang::quo_text(rlang::enquo(prefix))
  kryds_txt <- rlang::quo_text(rlang::enquo(krydsvar))

  zz <- dset %>%
    dplyr::select(dplyr::starts_with(pref_txt), !!rlang::sym(kryds_txt))

  z <- zz %>% tidyr::drop_na()
  if (nrow(z) != nrow(zz)) message("Jeg har droppet NAs")

  # --- ÆNDRING 1: recode KUN prefix-kolonner ---
  valgt_chr <- as.character(valgt)
  z <- z %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(pref_txt),
        ~ dplyr::if_else(as.character(.x) == valgt_chr, "Valgt", "Ikke valgt", missing = NA_character_)
      )
    )
  valgt <- "Valgt"

  # --- ÆNDRING 2: string replace KUN på prefix-kolonner ---
  z <- z %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(pref_txt),
        ~ stringr::str_replace_all(.x, stringr::fixed(valgt), "øøøøø")
      )
    )

  o <- z %>% dplyr::select(dplyr::starts_with(pref_txt)) %>% names()
  w <- NULL
  for (var in o) {
    k <- janitor::tabyl(z, !!rlang::sym(var), !!rlang::ensym(krydsvar)) %>%
      janitor::adorn_totals(where = "col") %>%
      janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting()

    p <- k %>% dplyr::arrange(!!rlang::sym(var)) %>% utils::tail(1)
    if (p[1, 1] != "øøøøø") {
      q <- p[, 2:ncol(p)] %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ "0%"))
    } else {
      q <- p[, 2:ncol(p)]
    }
    w <- c(w, list(q))
  }

  if (isTRUE(advice)) {
    flops <- vapply(
      o,
      function(i) {
        lab <- labelled::var_label(dset[[i]])
        lab <- if (is.null(lab)) i else as.character(lab)
        parts <- strsplit(lab, " - ", fixed = TRUE)[[1]]
        if (length(parts) >= 2) substr(parts[2], 1, maksimum) else substr(lab, 1, maksimum)
      },
      FUN.VALUE = character(1)
    )
  } else {
    flops <- o
  }

  w <- dplyr::bind_rows(w)

  # Safe label-overskrift (undgår subscript out of bounds)
  get_first_label <- function(x, default) {
    lab <- labelled::var_label(x)
    if (is.null(lab) || length(lab) == 0) return(default)
    lab_chr <- as.character(lab)[1]
    if (is.na(lab_chr)) return(default)
    if (grepl(" - ", lab_chr, fixed = TRUE)) strsplit(lab_chr, " - ", fixed = TRUE)[[1]][1] else lab_chr
  }

  y <- get_first_label(dset[[o[1]]], default = o[1])
  cat(" ", y, "\n\n")

  a <- cbind(tibble::tibble(" " = flops), w)
  if (isTRUE(sort)) {
    a %>%
      dplyr::mutate(percent_numeric = as.numeric(sub("%", "", Total))) %>%
      dplyr::arrange(dplyr::desc(percent_numeric)) %>%
      dplyr::select(-percent_numeric) %>%
      return()
  } else {
    return(a)
  }
}

multi2_backup <- function(prefix, valgt, krydsvar, sort = F, dset=d, advice=F, maksimum=40) {
  zz <- dset %>% 
    dplyr::select(dplyr::starts_with(rlang::quo_text(rlang::enquo(prefix))),
                  rlang::quo_text(rlang::enquo(krydsvar)))
    
  z <- zz %>% tidyr::drop_na()
  if (count(z)[1,1] != count(zz)[1,1]) {
    print("Jeg har droppet NAs")
  }
  
  z <- z %>% dplyr::mutate_all(~ dplyr::if_else(. == valgt, "Valgt", "Ikke valgt"))
  valgt <- "Valgt"
    
  z <- z %>% dplyr::mutate_all(~ stringr::str_replace_all(., valgt, "øøøøø"))
  o <- z %>% dplyr::select(-rlang::quo_text(rlang::enquo(krydsvar))) %>% names()
  w <- NULL
  for (var in o) {
    k <- tabyl(z, !!sym(var), !!rlang::enquo(krydsvar)) %>% 
      janitor::adorn_totals(where = "col") %>% 
      janitor::adorn_percentages(denominator = "col") %>% 
      janitor::adorn_pct_formatting()
    p <- k %>% dplyr::arrange(!!sym(var)) %>% utils::tail(1)
    if (p[1,1]!="øøøøø") {q <- p[,2:ncol(p)] %>% dplyr::mutate_all(~ "0%")} else {
      q <- p[,2:ncol(p)]}
    w <- c(w, list(q))
  }
  
  if (advice == T) {
    flops <- c()
    for (i in o) {
      flop <- labelled::var_label(dset[[i]])
      flop <- strsplit(flop, " - ", fixed = TRUE)[[1]][[2]] %>% substr(1, maksimum)
      flops <- c(flops, flop)
    }
  } else {flops <- o}
  
  w <- dplyr::bind_rows(w)
  
  y <- labelled::var_label(dset[[o[1]]])
  y <- as.character(y)
  if (!is.null(y) & advice==T) {y <- strsplit(y, " - ", fixed = TRUE)[[1]][[1]]}
  cat(" ", y, "\n\n")
  
  a <- cbind(tibble(" " = flops), w)
  if (sort == T) {
    a %>% 
      dplyr::mutate(percent_numeric = as.numeric(sub("%", "", Total))) %>% 
      dplyr::arrange(dplyr::desc(percent_numeric)) %>% 
      dplyr::select(-percent_numeric) %>% return()
  } else {a %>% return()}
}

#################################################################
##                           flipden                           ##
#################################################################

flipden <- function(x) {
  x %>% mutate(row = row_number()) %>% 
    pivot_longer(-row, names_to = "column", values_to = "value") %>% 
    pivot_wider(names_from = row, values_from = value) %>% 
    row_to_names(row_number = 1)
}


#################################################################
##                            excel                            ##
#################################################################

excel_backup <- function(df, filename, ..., totaler=T) {
  
  selected_vars <- sapply(df, function(k) length(unique(k))) < 20
  selected_df <- df[, selected_vars, drop = FALSE]
  
  if (length(get_2udfald(df))!=0) {selected_df <- selected_df %>% dplyr::select(-starts_with(get_2udfald(df)))}
  
  selected_df <- selected_df %>% 
    dplyr::mutate(dplyr::across(where(expss::is.labelled), as.factor))
  
  navne <- names(selected_df)
  
  x <- selected_df %>% dplyr::mutate(totalvar = "Total")
  
  args <- rlang::enquos(...)
  
  pp <- NULL
  tom <- tibble(" " = NA, Total=NA)
  titleRows <- c(3)
  
  # Frekvenser
  for (var in navne) {
    
    xx <- x
    
    if(is.factor(x[[var]])) {
      all_levels <- unique(c(levels(x[[var]]), as.character(x[[var]])))
      x[[var]] <- factor(x[[var]], levels = all_levels)
    }
    
    a <- x %>% janitor::tabyl(!!sym(var), totalvar) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_percentages(denominator = "col") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate(dplyr::across(everything(), ~if_else(is.na(.), "[NA]", as.character(.)))) %>% 
      dplyr::mutate(Total = as.numeric(Total)) %>% 
      dplyr::rename(" " = !!sym(var)) %>%
      tibble::as_tibble()
    
    z <- labelled::var_label(xx[[var]])
    if (!is.null(z)) {
      ax <- tibble(" " = labelled::var_label(xx[[var]]), Total=NA)
    } else {
      ax <- tibble(" " = var, Total=NA)
    }
    
    hvasså <- tibble(" " = "n =", Total=nrow(x))
    
    if (is.null(pp)) {
      pp <- rbind(hvasså, ax, a, tom)
    } else {
      titleRows <- c(titleRows, nrow(pp)+2)
      pp <- rbind(pp, ax, a, tom)
    }
    
  }
  
  print("Så er vi i gang")
  
  # Kryds
  for (kryds in args) {
    y <- x %>% mutate(tempvar = !!kryds)
    qq <- NULL
    for (var in navne) {
      a <- y %>% janitor::tabyl(!!sym(var), tempvar) %>%
        janitor::adorn_totals() %>%
        janitor::adorn_percentages(denominator = "col") %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::mutate(!!sym(var) := NA) %>%
        dplyr::rename(" " := !!sym(var)) %>%
        tibble::as_tibble()
      tom <- a[1,] %>% dplyr::mutate_all(~if_else(is.na(.), NA, NA))
      
      n1 <- y %>% dplyr::mutate(tempooo = "")
      n2 <- n1 %>% janitor::tabyl(tempooo, tempvar) %>% 
        dplyr::rename(" " := tempooo) %>% 
        tibble::as_tibble()
      
      if (is.null(qq)) {
        qq <- rbind(n2, tom, a)
      } else {
        qq <- rbind(qq, tom, tom, a)
      }
    }
    
    qq <- rbind(qq, tom)
    pp <- cbind(pp, qq)
    print(paste0("Nu har jeg lavet ", rlang::quo_name(kryds)))
  }
  
  # Totaler?
  if (totaler==F) {
    tempnames <- names(pp)
    names(pp)[1] <- "nejtiltotaler"
    names(pp) <- make.unique(names(pp))
    pp <- pp %>% dplyr::filter(nejtiltotaler != "Total" | is.na(nejtiltotaler) | nejtiltotaler == "")
    names(pp) <- tempnames}
  
  # Multi
  navne2 <- get_2udfald(df)
  pp2 <- NULL
  if (length(navne2) != 0) {
    new_df <- df[, selected_vars, drop = FALSE] %>% dplyr::select(starts_with(get_2udfald(df)))
    navne3 <- names(new_df)
    x <- df[, selected_vars, drop = FALSE] %>% dplyr::mutate(dplyr::across(where(expss::is.labelled), as.factor))
    
    tom <- tibble(" " = NA, Total=NA)
  
    print("Åhha, så er der multiselect-tabeller også")
    
    for (var in navne3) {
      if (!"Valgt" %in% tabyl(x, !!sym(var))[,1]) {
        stop("Alle multiselect-variable skal have udfaldene \"Valgt\" og \"Ikke valgt\"")
      }
    }

    for (var in navne2) {
      a <- multi(!!sym(var), "Valgt", dset=x, advice=T, maksimum=1000) %>% dplyr::mutate(Total = as.numeric(sub("%", "", percent))/100) %>% 
        dplyr::select(name, Total) %>% dplyr::rename(" " = name)
      
      flup <- x %>% dplyr::select(dplyr::starts_with(var)) %>% names()
       
      # flops <- c()
      # for (i in flup) {
      #   flop <- labelled::var_label(x[[i]])
      #   flop <- strsplit(flop, " - ", fixed = TRUE)[[1]][[2]]
      #   flops <- c(flops, flop)
      # }
      # 
      # a[1] <- flops
      
      z <- labelled::var_label(x[[flup[1]]])
      z <- strsplit(z, " - ", fixed = TRUE)[[1]][[1]]
      
      if (!is.null(z)) {
        ax <- tibble(" " = z, Total=NA)
      } else {
        ax <- tibble(" " = var, Total=NA)
      }
    
      if (is.null(pp2)) {
        titleRows <- c(titleRows, nrow(pp)+2)
        pp2 <- rbind(ax, a, tom)
      } else {
        titleRows <- c(titleRows, nrow(pp)+2 + nrow(pp2))
        pp2 <- rbind(pp2, ax, a, tom)
      }
    
    }
    
    for (kryds in args) {
      y <- x %>% mutate(tempvar = !!kryds)
      qq2 <- NULL
      
      for (var in navne2) {
        a <- multi2(!!sym(var), "Valgt", tempvar, dset=y, maksimum=1000) %>% dplyr::select(-Total) %>% 
          dplyr::mutate(!!names(.)[1] := NA) %>% 
          dplyr::mutate(dplyr::across(everything(), ~ as.numeric(gsub("%", "", .)) / 100))
        
        tom <- a[1,] %>% dplyr::mutate_all(~if_else(is.na(.), NA, NA))
        
        if (is.null(qq2)) {
          qq2 <- rbind(tom, a)
        } else {
          qq2 <- rbind(qq2, tom, tom, a)
        }
      }
      
      qq2 <- rbind(qq2, tom)
      pp2 <- cbind(pp2, qq2)
    }
  }
  
  pp <- rbind(pp, pp2)
  
  # Formatér og eksportér (ny version)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, "Sheet 1", pp)
  hs <- openxlsx::createStyle(textDecoration = "BOLD", halign="center")
  percentStyle <- openxlsx::createStyle(numFmt = "0%", halign="center")
  nStyle1 <- openxlsx::createStyle(halign="center", fontSize = 9, numFmt = "General", 
                                  border = "bottom")
  nStyle2 <- openxlsx::createStyle(halign="left", fontSize = 9, numFmt = "General",
                                   border = "bottom")
  titleStyle <- openxlsx::createStyle(halign="left", textDecoration = "bold",
                                      fgFill = "#D8BFD8")
  
  openxlsx::addStyle(wb, "Sheet 1", style = hs, rows = 1, cols = 1:ncol(pp), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle1, rows = 2, cols = 2:ncol(pp), gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle2, rows = 2, cols = 1, gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = percentStyle, rows = 3:nrow(pp)+1, cols = 2:ncol(pp), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Sheet 1", style = titleStyle, rows = titleRows, cols = 1:ncol(pp), gridExpand = T)
  
  openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
  
  print("DONE!!")
}

                          
excel <- function(df, filename, ..., totaler=T, wt) {

  wt_q    <- rlang::enquo(wt)
  has_wt  <- !rlang::quo_is_missing(wt_q)
  wt_name <- if (has_wt) rlang::as_name(wt_q) else NULL
  
  selected_vars <- sapply(df, function(k) length(unique(k))) < 20
  selected_df <- df[, selected_vars, drop = FALSE]                        
  if (has_wt && wt_name %in% names(df) && !(wt_name %in% names(selected_df))) {
    selected_df[[wt_name]] <- df[[wt_name]]}
  
  if (length(get_2udfald(df))!=0) {selected_df <- selected_df %>% dplyr::select(-starts_with(get_2udfald(df)))}
  
  selected_df <- selected_df %>%
    dplyr::mutate(dplyr::across(where(expss::is.labelled) & !dplyr::all_of(wt_name) , as.factor))
  
  navne <- if (has_wt) setdiff(names(selected_df), wt_name) else names(selected_df)
  
  x <- selected_df %>% dplyr::mutate(totalvar = "Total")
  
  args <- rlang::enquos(...)
  
  pp <- NULL
  tom <- tibble(" " = NA, Total=NA)
  titleRows <- c(3)
  
  # Frekvenser
  for (var in navne) {
    
    xx <- x
    
    if(is.factor(x[[var]])) {
      all_levels <- unique(c(levels(x[[var]]), as.character(x[[var]])))
      x[[var]] <- factor(x[[var]], levels = all_levels)
    }

    if (missing(wt)) {
      a <- x %>% janitor::tabyl(!!sym(var), totalvar) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_percentages(denominator = "col") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate(dplyr::across(everything(), ~if_else(is.na(.), "[NA]", as.character(.)))) %>% 
      dplyr::mutate(Total = as.numeric(Total)) %>% 
      dplyr::rename(" " = !!sym(var)) %>%
      tibble::as_tibble()
    } else {
      a <- x %>% janitor::tabyl(!!sym(var), totalvar,  weights = !!rlang::enquo(wt)) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_percentages(denominator = "col") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate(dplyr::across(everything(), ~if_else(is.na(.), "[NA]", as.character(.)))) %>% 
      dplyr::mutate(Total = as.numeric(Total)) %>% 
      dplyr::rename(" " = !!sym(var)) %>%
      tibble::as_tibble()
    }
    
    z <- labelled::var_label(xx[[var]])
    if (!is.null(z)) {
      ax <- tibble(" " = labelled::var_label(xx[[var]]), Total=NA)
    } else {
      ax <- tibble(" " = var, Total=NA)
    }
    
    hvasså <- tibble(" " = "n =", Total=nrow(x))
    
    if (is.null(pp)) {
      pp <- rbind(hvasså, ax, a, tom)
    } else {
      titleRows <- c(titleRows, nrow(pp)+2)
      pp <- rbind(pp, ax, a, tom)
    }
    
  }
  
  print("Så er vi i gang")
  
  # Kryds
  for (kryds in args) {
    y <- x %>% mutate(tempvar = !!kryds)
    qq <- NULL
    for (var in navne) {
      if(missing(wt)) {
        a <- y %>% janitor::tabyl(!!sym(var), tempvar) %>%
        janitor::adorn_totals() %>%
        janitor::adorn_percentages(denominator = "col") %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::mutate(!!sym(var) := NA) %>%
        dplyr::rename(" " := !!sym(var)) %>%
        tibble::as_tibble()
      } else {
        a <- y %>% janitor::tabyl(!!sym(var), tempvar, weights = !!rlang::enquo(wt)) %>%
        janitor::adorn_totals() %>%
        janitor::adorn_percentages(denominator = "col") %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::mutate(!!sym(var) := NA) %>%
        dplyr::rename(" " := !!sym(var)) %>%
        tibble::as_tibble()
      }
      
      tom <- a[1,] %>% dplyr::mutate_all(~if_else(is.na(.), NA, NA))
      
      n1 <- y %>% dplyr::mutate(tempooo = "")
      n2 <- n1 %>% janitor::tabyl(tempooo, tempvar) %>% 
        dplyr::rename(" " := tempooo) %>% 
        tibble::as_tibble()
      
      if (is.null(qq)) {
        qq <- rbind(n2, tom, a)
      } else {
        qq <- rbind(qq, tom, tom, a)
      }
    }
    
    qq <- rbind(qq, tom)
    pp <- cbind(pp, qq)
    print(paste0("Nu har jeg lavet ", rlang::quo_name(kryds)))
  }
  
  # Totaler?
  if (totaler==F) {
    tempnames <- names(pp)
    names(pp)[1] <- "nejtiltotaler"
    names(pp) <- make.unique(names(pp))
    pp <- pp %>% dplyr::filter(nejtiltotaler != "Total" | is.na(nejtiltotaler) | nejtiltotaler == "")
    names(pp) <- tempnames}
  
  # Multi
  navne2 <- get_2udfald(df)
  pp2 <- NULL
  if (length(navne2) != 0) {
    print("Jeg kan stadig ikke finde ud af at lave multiselect i excel :(")
  }
  
  pp <- rbind(pp, pp2)
  
  # Formatér og eksportér (ny version)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet 1")
  openxlsx::writeData(wb, "Sheet 1", pp)
  hs <- openxlsx::createStyle(textDecoration = "BOLD", halign="center")
  percentStyle <- openxlsx::createStyle(numFmt = "0%", halign="center")
  nStyle1 <- openxlsx::createStyle(halign="center", fontSize = 9, numFmt = "General", 
                                  border = "bottom")
  nStyle2 <- openxlsx::createStyle(halign="left", fontSize = 9, numFmt = "General",
                                   border = "bottom")
  titleStyle <- openxlsx::createStyle(halign="left", textDecoration = "bold",
                                      fgFill = "#D8BFD8")
  
  openxlsx::addStyle(wb, "Sheet 1", style = hs, rows = 1, cols = 1:ncol(pp), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle1, rows = 2, cols = 2:ncol(pp), gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle2, rows = 2, cols = 1, gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = percentStyle, rows = 3:nrow(pp)+1, cols = 2:ncol(pp), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Sheet 1", style = titleStyle, rows = titleRows, cols = 1:ncol(pp), gridExpand = T)
  
  openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
  
  print("DONE!!")
}

##################################################################
##                            behold                            ##
##################################################################

behold <- function(...) {
  keeps <- sapply(substitute(list(...))[-1], deparse)
  objs <- ls(pos=1)
  
  keeps_exist <- keeps[keeps %in% objs]
  if (length(keeps_exist) == 0) {stop("findes ik")}
  
  objs_remove <- setdiff(objs, keeps_exist)
  if (length(objs_remove)>0) {rm(list=objs_remove, pos=1)}
}













