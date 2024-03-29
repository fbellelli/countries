#' Conversion table
#'
#' A table containing country names in different naming conventions
#'
#' @format A data frame with columns corresponding to different country naming conventions.
#' \describe{
#'   \item{simple}{Reference name for the geographic unit. The names in this column contain only ASCII characters. This nomenclature is available for all countries.}
#'   \item{ISO3}{3-letter country codes as defined in ISO standard \code{3166-1 alpha-3}. This nomenclature is available for the territories in the standard (currently 249 territories).}
#'   \item{ISO2}{2-letter country codes as defined in ISO standard \code{3166-1 alpha-2}. This nomenclature is available for the territories in the standard (currently 249 territories).}
#'   \item{ISO_code}{Numeric country codes as defined in ISO standard \code{3166-1 numeric}. This country code is the same as the UN's country number (\href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard}). This nomenclature is available for the territories in the ISO standard (currently 249 countries).}
#'   \item{UN_ar}{Official UN name in \strong{Arabic}.  This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{UN_zh}{Official UN name in \strong{Chinese}. This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{UN_en}{Official UN name in \strong{English}. This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{UN_fr}{Official UN name in \strong{French}. This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{UN_es}{Official UN name in \strong{Spanish}. This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{UN_ru}{Official UN name in \strong{Russian}. This nomenclature is only available for countries in the \href{https://unstats.un.org/unsd/methodology/m49/}{M49 standard} (currently 249 territories).}
#'   \item{WTO_en}{Official WTO name in \strong{English}. This nomenclature is only available for \href{https://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm}{WTO members and observers} (currently 189 entities).}
#'   \item{WTO_fr}{Official WTO name in \strong{French}. This nomenclature is only available for \href{https://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm}{WTO members and observers} (currently 189 entities).}
#'   \item{WTO_es}{Official WTO name in \strong{Spanish}. This nomenclature is only available for \href{https://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm}{WTO members and observers} (currently 189 entities).}
#'   \item{name_ar}{Translation of ISO country names in \strong{Arabic}. (currently 249 territories)}
#'   \item{name_bg}{Translation of ISO country names in \strong{Bulgarian}. (currently 249 territories)}
#'   \item{name_cs}{Translation of ISO country names in \strong{Czech}. (currently 249 territories)}
#'   \item{name_da}{Translation of ISO country names in \strong{Danish}. (currently 249 territories)}
#'   \item{name_de}{Translation of ISO country names in \strong{German}. (currently 249 territories)}
#'   \item{name_el}{Translation of ISO country names in \strong{Greek}. (currently 249 territories)}
#'   \item{name_en}{Translation of ISO country names in \strong{English}. (currently 249 territories)}
#'   \item{name_es}{Translation of ISO country names in \strong{Spanish}. (currently 249 territories)}
#'   \item{name_et}{Translation of ISO country names in \strong{Estonian}. (currently 249 territories)}
#'   \item{name_eu}{Translation of ISO country names in \strong{Basque}. (currently 249 territories)}
#'   \item{name_fi}{Translation of ISO country names in \strong{Finnish}. (currently 249 territories)}
#'   \item{name_fr}{Translation of ISO country names in \strong{French}. (currently 249 territories)}
#'   \item{name_hu}{Translation of ISO country names in \strong{Hungarian}. (currently 249 territories)}
#'   \item{name_it}{Translation of ISO country names in \strong{Italian}. (currently 249 territories)}
#'   \item{name_ja}{Translation of ISO country names in \strong{Japanese}. (currently 249 territories)}
#'   \item{name_ko}{Translation of ISO country names in \strong{Korean}. (currently 249 territories)}
#'   \item{name_lt}{Translation of ISO country names in \strong{Lithuanian}. (currently 249 territories)}
#'   \item{name_nl}{Translation of ISO country names in \strong{Dutch}. (currently 249 territories)}
#'   \item{name_no}{Translation of ISO country names in \strong{Norwegian}. (currently 249 territories)}
#'   \item{name_pl}{Translation of ISO country names in \strong{Polish}. (currently 249 territories)}
#'   \item{name_pt}{Translation of ISO country names in \strong{Portuguese}. (currently 249 territories)}
#'   \item{name_ro}{Translation of ISO country names in \strong{Romanian}. (currently 249 territories)}
#'   \item{name_ru}{Translation of ISO country names in \strong{Russian}. (currently 249 territories)}
#'   \item{name_sk}{Translation of ISO country names in \strong{Slovak}. (currently 249 territories)}
#'   \item{name_sv}{Translation of ISO country names in \strong{Swedish}. (currently 249 territories)}
#'   \item{name_th}{Translation of ISO country names in \strong{Thai}. (currently 249 territories)}
#'   \item{name_uk}{Translation of ISO country names in \strong{Ukranian}. (currently 249 territories)}
#'   \item{name_zh}{Translation of ISO country names in \strong{simplified Chinese}. (currently 249 territories)}
#'   \item{name_zh-tw}{Translation of ISO country names in \strong{traditional Chinese}. (currently 249 territories)}
#'   \item{GTAP}{\href{https://www.gtap.agecon.purdue.edu/databases/regions.aspx?version=9.211}{GTAP} country and region codes.}
#'   \item{Name0}{Other variants of the country name included to improve the matching process}
#'   \item{Name1}{Other variants of the country name included to improve the matching process}
#'   \item{Name2}{Other variants of the country name included to improve the matching process}
#'   \item{Name3}{Other variants of the country name included to improve the matching process}
#'   \item{Name4}{Other variants of the country name included to improve the matching process}
#'   \item{Name5}{Other variants of the country name included to improve the matching process}
#'   \item{Name6}{Other variants of the country name included to improve the matching process}
#'   \item{Name7}{Other variants of the country name included to improve the matching process}
#'   \item{Name8}{Other variants of the country name included to improve the matching process}
#'   \item{Name9}{Other variants of the country name included to improve the matching process}
#'   \item{Name10}{Other variants of the country name included to improve the matching process}
#'   \item{Name11}{Other variants of the country name included to improve the matching process}
#'   \item{Name12}{Other variants of the country name included to improve the matching process}
#'   \item{Name13}{Other variants of the country name included to improve the matching process}
#'   \item{Name14}{Other variants of the country name included to improve the matching process}
#'   \item{Name15}{Other variants of the country name included to improve the matching process}
#'   \item{Name16}{Other variants of the country name included to improve the matching process}
#'   \item{Name17}{Other variants of the country name included to improve the matching process}
#'   \item{Name18}{Other variants of the country name included to improve the matching process}
#'   \item{Name19}{Other variants of the country name included to improve the matching process}
#' }
"country_reference_list"

data(country_reference_list, envir=environment())


