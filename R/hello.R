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

      "alltabs:  Frekvenstabeller for alle variable \n",
      "          (ud over dem med mere end 20 udfald) \n",
      "          - alltabs() \n\n",

      "tabzz:    Mange frekvenstabeller (max 20) \n",
      "          - alltabs(var1, var2, var3, ...) \n\n",

      "kryzz:    Mange krydstabeller (max 20) \n",
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

pakr <- function(p1, p2, p3, p4, p5, p6, p7) {
  pacman::p_load(tidyverse, janitor, bannerCommenter)
  if (!missing(p1)) {
    p1 <- rlang::as_string(rlang::ensym(p1))
    pacman::p_load(get(p1))
  }
}


##################################################################
##                             tabl                             ##
##################################################################

tabl <- function(var1, var2, dset=d) {
  if (missing(var2)) {
    str <- dplyr::as_label(rlang::enquo(var1))
    x <- labelled::var_label(dset[[str]])
    cat(" ", x, "\n")
    dset %>% janitor::tabyl(!!rlang::enquo(var1)) %>% janitor::adorn_pct_formatting()
  } else {
    dset %>% janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(var2)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>% janitor::adorn_pct_formatting()
  }}


#################################################################
##                             grp                             ##
#################################################################

grp <- function(var1, var2, dset=d) {
  dset %>% dplyr::group_by(!!rlang::enquo(var1)) %>%
    dplyr::summarise(mean = mean(!!rlang::enquo(var2), na.rm=T))
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

alltabs <- function() {
  navne <- names(d)
  for (var in navne) {
    x <- labelled::var_label(d[[var]])
    if (!is.null(x)) {
      cat(" ", x, "\n")
    }
    t <- d %>% janitor::tabyl(!!var) %>% janitor::adorn_pct_formatting()
    if (length(t$n) < 20) {
      print(t)
      cat("\n\n")
    }
  }
}

#################################################################
##                            tabzz                            ##
#################################################################

tabzz <- function(var1, var2, var3, var4, var5, var6, var7, var8, var9, var10,
                    var11, var12, var13, var14, var15, var16, var17, var18, var19, var20, dset=d) {
  str <- dplyr::as_label(rlang::enquo(var1))
  x <- labelled::var_label(dset[[str]])
  cat(" ", x, "\n")
  dset %>% janitor::tabyl(!!rlang::enquo(var1)) %>% janitor::adorn_pct_formatting() %>% print()
  if (!missing(var2)) {cat("\n\n")}
  if (!missing(var2)) {
    str <- dplyr::as_label(rlang::enquo(var2))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var2)) %>% janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var3)) {cat("\n\n")}
  if (!missing(var3)) {
    str <- dplyr::as_label(rlang::enquo(var3))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var3)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var4)) {cat("\n\n")}
  if (!missing(var4)) {
    str <- dplyr::as_label(rlang::enquo(var4))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var4)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var5)) {cat("\n\n")}
  if (!missing(var5)) {
    str <- dplyr::as_label(rlang::enquo(var5))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var5)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var6)) {cat("\n\n")}
  if (!missing(var6)) {
    str <- dplyr::as_label(rlang::enquo(var6))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var6)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var7)) {cat("\n\n")}
  if (!missing(var7)) {
    str <- dplyr::as_label(rlang::enquo(var7))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var7)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var8)) {cat("\n\n")}
  if (!missing(var8)) {
    str <- dplyr::as_label(rlang::enquo(var8))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var8)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var9)) {cat("\n\n")}
  if (!missing(var9)) {
    str <- dplyr::as_label(rlang::enquo(var9))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var9)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var10)) {cat("\n\n")}
  if (!missing(var10)) {
    str <- dplyr::as_label(rlang::enquo(var10))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var10)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var11)) {cat("\n\n")}
  if (!missing(var11)) {
    str <- dplyr::as_label(rlang::enquo(var11))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var11)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var12)) {cat("\n\n")}
  if (!missing(var12)) {
    str <- dplyr::as_label(rlang::enquo(var12))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var12)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var13)) {cat("\n\n")}
  if (!missing(var13)) {
    str <- dplyr::as_label(rlang::enquo(var13))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var13)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var14)) {cat("\n\n")}
  if (!missing(var14)) {
    str <- dplyr::as_label(rlang::enquo(var14))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var14)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var15)) {cat("\n\n")}
  if (!missing(var15)) {
    str <- dplyr::as_label(rlang::enquo(var15))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var15)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var16)) {cat("\n\n")}
  if (!missing(var16)) {
    str <- dplyr::as_label(rlang::enquo(var16))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var16)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var17)) {cat("\n\n")}
  if (!missing(var17)) {
    str <- dplyr::as_label(rlang::enquo(var17))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var17)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var18)) {cat("\n\n")}
  if (!missing(var18)) {
    str <- dplyr::as_label(rlang::enquo(var18))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var18)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var19)) {cat("\n\n")}
  if (!missing(var19)) {
    str <- dplyr::as_label(rlang::enquo(var19))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var19)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var20)) {cat("\n\n")}
  if (!missing(var20)) {
    str <- dplyr::as_label(rlang::enquo(var20))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var20)) %>%
      janitor::adorn_pct_formatting() %>% print()
  }
}


#################################################################
##                            kryzz                            ##
#################################################################

kryzz <- function(kryds, var1, var2, var3, var4, var5, var6, var7, var8, var9, var10,
                    var11, var12, var13, var14, var15, var16, var17, var18, var19, var20, dset=d) {

  str <- dplyr::as_label(rlang::enquo(var1))
  x <- labelled::var_label(dset[[str]])
  if (!is.null(x)) {cat(" ", x, "\n")}
  dset %>% janitor::tabyl(!!rlang::enquo(var1), !!rlang::enquo(kryds)) %>%
    janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
    janitor::adorn_pct_formatting() %>% print()

  if (!missing(var2)) {cat("\n\n")}
  if (!missing(var2)) {
    str <- dplyr::as_label(rlang::enquo(var2))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var2), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var3)) {cat("\n\n")}
  if (!missing(var3)) {
    str <- dplyr::as_label(rlang::enquo(var3))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var3), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var4)) {cat("\n\n")}
  if (!missing(var4)) {
    str <- dplyr::as_label(rlang::enquo(va42))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var4), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var5)) {cat("\n\n")}
  if (!missing(var5)) {
    str <- dplyr::as_label(rlang::enquo(var5))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var5), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var6)) {cat("\n\n")}
  if (!missing(var6)) {
    str <- dplyr::as_label(rlang::enquo(var6))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var6), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var7)) {cat("\n\n")}
  if (!missing(var7)) {
    str <- dplyr::as_label(rlang::enquo(var7))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var7), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var8)) {cat("\n\n")}
  if (!missing(var8)) {
    str <- dplyr::as_label(rlang::enquo(var8))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var8), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var9)) {cat("\n\n")}
  if (!missing(var9)) {
    str <- dplyr::as_label(rlang::enquo(var9))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var9), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var10)) {cat("\n\n")}
  if (!missing(var10)) {
    str <- dplyr::as_label(rlang::enquo(var10))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var10), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var11)) {cat("\n\n")}
  if (!missing(var11)) {
    str <- dplyr::as_label(rlang::enquo(var11))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var11), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var12)) {cat("\n\n")}
  if (!missing(var12)) {
    str <- dplyr::as_label(rlang::enquo(var12))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var12), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var13)) {cat("\n\n")}
  if (!missing(var13)) {
    str <- dplyr::as_label(rlang::enquo(var13))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var13), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var14)) {cat("\n\n")}
  if (!missing(var14)) {
    str <- dplyr::as_label(rlang::enquo(var14))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var14), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var15)) {cat("\n\n")}
  if (!missing(var15)) {
    str <- dplyr::as_label(rlang::enquo(var15))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var15), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var16)) {cat("\n\n")}
  if (!missing(var16)) {
    str <- dplyr::as_label(rlang::enquo(var16))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var16), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var17)) {cat("\n\n")}
  if (!missing(var17)) {
    str <- dplyr::as_label(rlang::enquo(var17))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var17), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var18)) {cat("\n\n")}
  if (!missing(var18)) {
    str <- dplyr::as_label(rlang::enquo(var18))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var18), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var19)) {cat("\n\n")}
  if (!missing(var19)) {
    str <- dplyr::as_label(rlang::enquo(var19))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var19), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
  if (!missing(var20)) {cat("\n\n")}
  if (!missing(var20)) {
    str <- dplyr::as_label(rlang::enquo(var20))
    x <- labelled::var_label(dset[[str]])
    if (!is.null(x)) {cat(" ", x, "\n")}
    dset %>% janitor::tabyl(!!rlang::enquo(var20), !!rlang::enquo(kryds)) %>%
      janitor::adorn_totals() %>% janitor::adorn_percentages(denominator = "col") %>%
      janitor::adorn_pct_formatting() %>% print()
  }
}




















