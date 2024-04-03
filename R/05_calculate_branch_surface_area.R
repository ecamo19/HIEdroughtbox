#branch_surface_area <- function(branch_lenght,
#                                branch_basal_diameter,
#                                convert_units_from =
#                                convert_units_to = ){
#
#            if(convert_vars_from_cm_to_m == TRUE){
#                cat(crayon::magenta$underline("branch_lenght and branch_basal_diameter converted from centimeters to meterts\n"))
#
#                # Convert cm to meters
#                branch_basal_diameter  <- branch_basal_diameter / 100
#                branch_lenght  <- branch_lenght / 100
#
#            }else(
#                cat(crayon::magenta$underline("Make sure that branch_lenght and branch_basal_diameter were measured in meters\n"))
#                )
#
#            # Is assumed that a branch has the shape of a cone ------------------
#
#            # Get the radius from the diameter
#            branch_radius  <- branch_basal_diameter / 2
#
#            # Calculate the surface area
#            return(pi * branch_radius * (branch_radius + sqrt(branch_radius^2 +
#                                                            branch_lenght^2))
#            )
#
#}
