relative_water_content <- function(dry_weight_g,
                                  turgid_weight,
                                  strain_weight_g ){

        # Formula taken from:
        # https://prometheusprotocols.net/function/water-relations/
        ## water-content-and-rwc/plant-water-content-and-relative-water-content/

        return((strain_weight_g - dry_weight_g) / (turgid_weight - dry_weight_g))

}
