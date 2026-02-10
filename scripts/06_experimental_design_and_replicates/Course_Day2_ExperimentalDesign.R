# Experimental design basis, PhD course day 2

# This script provides you with a working example of how to visualise your
# experimental design, as well as a couple tips for using package eddible.
# Please use as inspiration for your own code. 

#
#
#

# Packages ----
library(edibble)

#
#
#

# CENTS example ----

design_CENTS <- 
  design("CENTS LTE") |>
  
  # Units assignment
  set_units(site = 2,
            block = nested_in(site, 4),
            main_plot = nested_in(block, 4),
            sub_strip = nested_in(main_plot, 4),
            plot = nested_in(sub_strip, 3)) |> 
  
  # Treatment assignment
  set_trts(location = c("Foulum", "Flakkebjerg"),
           rotation = conditioned_on(location,
                                     "Foulum" ~ c("R2", "R3", "R4", "R5"),
                                     "Flakkebjerg" ~ c("R1", "R2", "R3", "R4")),
           tillage = c("plowed", "harrowed", "no_till_1", "no_till_2"),
           cover = c("bare_soil", "weeds", "cover_crops")) |>
           
  # Allocate treatments to correct unit levels  
  allot_trts(location ~ site, 
             rotation ~ main_plot, 
             tillage ~ sub_strip,
             cover ~ plot) |>
  
  # Assign treatments to lowest level units / randomise
  assign_trts(order = c("random"), seed=42) 

# Look at the design
design_CENTS
plot(design_CENTS)

# Save the design as a data table
served <- serve_table(design_CENTS)
served

# Write out as a csv-file 
write.csv(served, 
          file = "fieldbook.csv", # Change path here to where you want to save the file
          row.names = FALSE)

#
#
#

# Observational study example ----

agri_obs <-
  design(name = "Farm field survey (observational)") |>
  
  # Observational unit assignment 
  set_units(
    farm  = 100,
    field = nested_in(farm, 
                      1:5 ~5, # In farms 1-5, there are 5 fields
                      35:40 ~3, # In farms 35-40 there are 3 fields
                      . ~ 2), # In the rest, there are 2 fields
    plot  = nested_in(field, 10)) |>
  
  # What will be recorded (note: observational studies don't have treatments!)
  # Records are matched to the correct unit level
  set_rcrds_of(
    farm  = c("farming_system"),
    field = c("soil_texture", "previous_crop", "tillage"),
    plot  = c("soil_pH", "soil_OM", "Nmin", "plant_count", 
              "disease_incidence", "yield_t_ha")) 

# Look at the design
agri_obs
plot(agri_obs)


#
#
# Tip for designs in eddible ----

# You can see the menu of different specific designs
scan_menu()

# And if you look at any given one, you will see the code for how to
# build the design
menu_crd()

# 
# END ----