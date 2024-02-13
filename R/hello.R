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
      "          - multi(prefix, valgt, sort = F, d = d) \n",
      "          - fx multi(t_, Valgt) / multi(t_, Valgt, T) \n\n",

      "excel:    Excel-krydstabeller for alle variable i x \n",
      "          - excel(x, 'filnavn', krydsvar1, krydsvar2, ...) \n",
      "          - fx excel(d, 'Tabeller', køn, region, segment) \n\n",

      "\n ***** GRAFER ****** \n\n",
      "bar:      Søjlediagram \n",
      "          - bar(var) \n\n",

      "scat:     Scatterplot uden noget \n",
      "          - scat(var1, var2) \n\n",

      "scat2:    Scatterplot med jitter og LOESS \n",
      "          - scat2(var1, var2) \n\n",

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
    dset %>% janitor::tabyl(!!rlang::enquo(var1)) %>% janitor::adorn_pct_formatting()
  } else {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    str <- dplyr::as_label(rlang::enquo(var2))
    y <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {
      cat(" ", x, "[X]", y, "\n\n")
    }
    dset %>% janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(var2)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>% janitor::adorn_pct_formatting()
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
##                            multi                             ##
##################################################################

multi <- function(prefix, valgt, sort = F, d = d) {
  z <- d %>%
    dplyr::select(dplyr::starts_with(rlang::quo_text(rlang::enquo(prefix)))) %>%
    dplyr::mutate_all(~ stringr::str_replace_all(., rlang::quo_text(rlang::enquo(valgt)), "øøøøø"))
  o <- z %>% names()
  w <- c()
  for (var in o) {
    k <- tabl(!!var, dset=z)
    p <- k %>% dplyr::arrange(!!var) %>% utils::tail(1)
    if (p[1,1]!="øøøøø") {stop("Hov, det er vist ikke dét, det hedder")}
    q <- p[,3]
    w <- append(w, q)
  }
  y <- labelled::var_label(d[[o[1]]])
  if (!is.null(y)) {cat(" ", y, "\n\n")}
  if (sort == T) {
    tibble(name = o, percent = w) %>% dplyr::arrange(dplyr::desc(percent))
  } else {tibble(name = o, percent = w)}
}


#################################################################
##                            excel                            ##
#################################################################

excel <- function(df, filename, ...) {

  selected_vars <- sapply(df, function(k) length(unique(k))) < 20
  selected_df <- df[, selected_vars, drop = FALSE]
  navne <- names(selected_df)

  x <- selected_df %>% dplyr::mutate(totalvar = "Total")

  args <- rlang::enquos(...)

  pp <- NULL
  tom <- tibble(" " = NA, Total=NA)

  # Totaler
  for (var in navne) {
    a <- x %>% janitor::tabyl(!!sym(var), totalvar) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_percentages(denominator = "col") %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::rename(" " = !!sym(var)) %>%
      tibble::as_tibble()

    z <- labelled::var_label(x[[var]])
    if (!is.null(z)) {
      ax <- tibble(" " = labelled::var_label(x[[var]]), Total=NA)
    } else {
      ax <- tibble(" " = var, Total=NA)
    }

    if (is.null(pp)) {
      pp <- rbind(ax, a, tom)
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

      if (is.null(qq)) {
        qq <- rbind(tom, a)
      } else {
        qq <- rbind(qq, tom, tom, a)
      }
    }

    qq <- rbind(qq, tom)
    pp <- cbind(pp, qq)
    print(paste0("Nu har jeg lavet ", rlang::quo_name(kryds)))
  }

  options("openxlsx.numFmt" = "0%")
  hs <- openxlsx::createStyle(textDecoration = "BOLD", halign="center")
  openxlsx::write.xlsx(pp, paste0(filename,".xlsx"), zoom=100, firstActiveRow=2,
                       firstActiveCol = 3, headerStyle = hs)
  print("DONE!!")

  for (kryds in args) {
    print(janitor::tabyl(x, !!kryds))
    print(cat("\n\n"))
  }
}







