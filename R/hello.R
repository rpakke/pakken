#################################################################
##                      Funktionsoversigt                      ##
#################################################################

funktioner <- function() {
  cat("\n pakr:     tidyverse, janitor, bannerC \n\n",
      
      "\n ***** TABELLER MV. ***** \n\n",
      "tabl:     Frekvenstabeller og krydstabeller \n",
      "          - tabl(var1) eller tabl(var1, var2) \n\n",
      
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
##                             tabl                             ##
##################################################################

tabl <- function(var1, var2, dset=d) {
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
    y <- labelled::var_label(d[[var]])
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

multi <- function(prefix, valgt, sort = F, dset = d) {
  z <- dset %>%
    dplyr::select(dplyr::starts_with(rlang::quo_text(rlang::enquo(prefix)))) %>%
    dplyr::mutate_all(~ stringr::str_replace_all(., valgt, "øøøøø"))
  o <- z %>% names()
  w <- c()
  for (var in o) {
    k <- tabl(!!var, dset=z)
    p <- k %>% dplyr::arrange(!!var) %>% utils::tail(1)
    if (p[1,1]!="øøøøø") {q <- "0%"} else {q <- p[,3]}
    w <- append(w, q)
  }
  y <- labelled::var_label(dset[[o[1]]])
  if (!is.null(y)) {cat(" ", y, "\n\n")}
  if (sort == T) {
    tibble(name = o, percent = w) %>%
      dplyr::mutate(percent_numeric = as.numeric(sub("%", "", percent))) %>% 
      dplyr::arrange(dplyr::desc(percent_numeric)) %>% 
      dplyr::select(-percent_numeric)
  } else {tibble(name = o, percent = w)}
}

multi2 <- function(prefix, valgt, krydsvar, sort = F, dset=d) {
  z <- dset %>% 
    dplyr::select(dplyr::starts_with(rlang::quo_text(rlang::enquo(prefix))),
                  rlang::quo_text(rlang::enquo(krydsvar))) %>% 
    dplyr::mutate_all(~ stringr::str_replace_all(., valgt, "øøøøø"))
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
  w <- dplyr::bind_rows(w)
  y <- labelled::var_label(dset[[o[1]]])
  if (!is.null(y)) {cat(" ", y, "\n\n")}
  a <- cbind(tibble(" " = o), w)
  if (sort == T) {
    a %>% 
      dplyr::mutate(percent_numeric = as.numeric(sub("%", "", Total))) %>% 
      dplyr::arrange(dplyr::desc(percent_numeric)) %>% 
      dplyr::select(-percent_numeric) %>% return()
  } else {a %>% return()}
}

#################################################################
##                            excel                            ##
#################################################################

excel <- function(df, filename, ..., totaler=T) {
  
  selected_vars <- sapply(df, function(k) length(unique(k))) < 20
  selected_df <- df[, selected_vars, drop = FALSE] %>% 
    dplyr::select(-starts_with(get_2udfald(df)))
  navne <- names(selected_df)
  
  x <- selected_df %>% dplyr::mutate(totalvar = "Total")
  
  args <- rlang::enquos(...)
  
  pp <- NULL
  tom <- tibble(" " = NA, Total=NA)
  
  # Frekvenser
  for (var in navne) {
    a <- x %>% janitor::tabyl(!!sym(var), totalvar) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_percentages(denominator = "col") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate(dplyr::across(everything(), ~if_else(is.na(.), "[NA]", as.character(.)))) %>% 
      dplyr::mutate(Total = as.numeric(Total)) %>% 
      dplyr::rename(" " = !!sym(var)) %>%
      tibble::as_tibble()
    
    z <- labelled::var_label(x[[var]])
    if (!is.null(z)) {
      ax <- tibble(" " = labelled::var_label(x[[var]]), Total=NA)
    } else {
      ax <- tibble(" " = var, Total=NA)
    }
    
    hvasså <- tibble(" " = "n =", Total=nrow(x))
    
    if (is.null(pp)) {
      pp <- rbind(hvasså, ax, a, tom)
    } else {
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
  
  # Formatér og eksportér
  # options("openxlsx.numFmt" = "0%")
  # hs <- openxlsx::createStyle(textDecoration = "BOLD", halign="center")
  # openxlsx::write.xlsx(pp, paste0(filename,".xlsx"), zoom=100, firstActiveRow=3,
  #                      firstActiveCol = 3, headerStyle = hs)
  
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
  
  openxlsx::addStyle(wb, "Sheet 1", style = hs, rows = 1, cols = 1:ncol(pp), gridExpand = TRUE)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle1, rows = 2, cols = 2:ncol(pp), gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = nStyle2, rows = 2, cols = 1, gridExpand = T)
  openxlsx::addStyle(wb, "Sheet 1", style = percentStyle, rows = 3:nrow(pp)+1, cols = 2:ncol(pp), gridExpand = TRUE)
  
  #openxlsx::setActiveSheet(wb, 1)
  #openxlsx::setActiveCell(wb, "Sheet 1", row = 3, col = 3)
  
  openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
  
  print("DONE!!")
  
  # Krydstabeller for at se n for hver kryds-kategori
  # for (kryds in args) {
  #   print(janitor::tabyl(x, !!kryds))
  #   print(cat("\n\n"))
  # }
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










