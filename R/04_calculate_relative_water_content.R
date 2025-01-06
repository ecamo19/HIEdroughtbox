#relative_water_deficit = 1 − actual fresh weight − dry weight
#                             saturated weight − dry weight
# Formula from Bueno etal 2019


#relative_water_content <- function(dry_weight_g,
#                                  turgid_weight,
#                                  strain_weight_g ){
#
#        # Formula taken from:
#        # https://prometheusprotocols.net/function/water-relations/
#        ## water-content-and-rwc/plant-water-content-and-relative-water-content/
#
#        return((strain_weight_g - dry_weight_g) / (turgid_weight - dry_weight_g))
#
#}

#' Relative water content
#'
#' @description
#' This function calculates the relative water content of each sample.
#'
#'
#' @param droughtbox_data Dataframe loaded with the function
#' `read_hie_droughtbox_data()`.
#'
#' @param dry_weights Dataframe loaded with the functionv `read_dry_weights_data()`
#'
#' @return
#' @export
#'
#' @examples
relative_water_content <- function(droughtbox_data, dry_weights){

}
