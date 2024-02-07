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

      "tabzz:    Mange frekvenstabeller \n",
      "          - tabzz(var1, var2, var3, ...) \n\n",

      "kryzz:    Mange krydstabeller \n",
      "          - kryzz(krydsvar, var1, var2 ,...) \n\n",

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
    if (!is.null(x)) {cat" ", x, "\n\n"}
    dset %>% dplyr::group_by(!!rlang::enquo(gruppe)) %>%
      dplyr::summarise(mean = mean(!!var, na.rm=T))
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


















