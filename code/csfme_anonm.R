##################
### CFSME 
###############


## Required Library

require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)
require(gplot2)


## A function to check the correctness of the administrative unit related information

IntersectPtWithPoly <- function(x, y) {
  # Extracts values from a SpatialPolygonDataFrame with
  # SpatialPointsDataFrame, and appends table (similar to
  # ArcGIS intersect)
  # Args:
  #   x: SpatialPoints*Frame
  #   y: SpatialPolygonsDataFrame
  # Returns:
  # SpatialPointsDataFrame with appended table of polygon attributes
  
  # Set up overlay with new column of join IDs in x
  z <- overlay(y, x)
  
  # Bind captured data to points dataframe
  x2 <- cbind(x, z)
  
  # Make it back into a SpatialPointsDataFrame
  # Account for different coordinate variable names
  if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) {
    coordinates(x2) <- ~coords.x1 + coords.x2
  } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) {
    coordinates(x2) <- ~x + y
  }
  
  # Reassign its projection if it has one
  if(is.na(CRSargs(x@proj4string)) == "FALSE") {
    x2@proj4string <- x@proj4string
  }
  return(x2)
}


######### Loading the first bit of the data

cfsme1 <- read.csv("data/JOR_CFSME_WFP_15032015.csv", encoding= "WIN1256", stringsAsFactors=FALSE)

### Spatial join on district 


## getting correct district and gov from coordinates
districtgeo <- readOGR("geo/district.geojson", "OGRGeoJSON")
## No need to load govenorate as we will get correct code for gov trhough the district
##governorategeo <- readOGR("geo/admin1.geojson", "OGRGeoJSON")

## extracting coordinates from dataset
## Create a spatial data frame with vaf records
rm(datasp)
datasp <-cfsme1[ , c("Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Longitude", "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Latitude")]
# summary(datasp)

datasp$long <- datasp$"Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Longitude"

datasp$lat <- datasp$"Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Latitude"

plot(datasp)
coords <- cbind(datasp$long, datasp$lat)
datasp0 <- SpatialPointsDataFrame(coords, data= datasp, proj4string=CRS("+proj=longlat"))

plot(datasp0)

### Map Overlay in gplot2

require("ggmap")


googleterrain <- get_map(location = c(lon =36, lat = 32),
                         color = "color", source = "google",maptype = "terrain", zoom = 8)
googleeterrain <- ggmap(googleterrain)

rm(map_cfsme)
map_cfsme <- googleeterrain
map_cfsme <- map_cfsme + 
  geom_point(aes(x = long, y = lat),data=datasp, color="orangered3")+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("CFSME Survey")+
  theme_bw()
ggsave("out/map_cfsme.png", map_cfsme, width=8, height=6,units="in", dpi=300)



## Intersection of points with district 
datasp1 <- IntersectPtWithPoly(datasp0, districtgeo)


datasp1@data$id <- rownames(datasp1@data)
rm(correct)
#View(districtgeo@data)
correct <- datasp1@data[ ,c("id","district_c","district","Gov_NAME","Gov_code" )]
cfsme1$id <- rownames(cfsme1)
cfsme1 <- merge(x=cfsme1, y=correct, by="id")
rm(correct)


names(cfsme1)

## Reduce GPS Accuracy for Anonymisation purpose

cfsme1$lat <- formatC(cfsme1$Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Latitude, format="f", digits=2)
cfsme1$lon <- formatC(cfsme1$Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Longitude, format="f", digits=2)

cfsme1_data <- cfsme1[, c(  
  "SubmissionDate",
  "start",
  "end",
  "deviceid",
  "intro_note",
  "Case_ID",
  "Case_ID_test",
  "note_check_case",
  "Assessment.G1_GENERAL_INFORMATION.Q1_1_Governorate",
  "Assessment.G1_GENERAL_INFORMATION.Q1_2_District",
  "Assessment.G1_GENERAL_INFORMATION.Q1_3_BSU",
  "Assessment.G1_GENERAL_INFORMATION.Q1_3_1_Other_BSU",
  "Assessment.G1_GENERAL_INFORMATION.Q1_4_Are_you_living_in",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_1_Case_live_in_HH_headed_by_non_Syrian",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_2_Case_share_HH_with_other_Syrians",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_3_Num_of_HH_members_including_all_non_Syrians",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_3_1_Num_of_SYR_including_non_registered_VAF",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_3_2_SYR_incl_unaccompanied_or_separated_minors",
  "Assessment.G2_INFORMATION_ON_THE_CASE.G2_3_2.note2_3_2",
  "Assessment.G2_INFORMATION_ON_THE_CASE.G2_3_2.Q2_3_2_1_Num_of_unaccompanied_unaccompanied_minors",
  "Assessment.G2_INFORMATION_ON_THE_CASE.G2_3_2.Q2_3_2_2_Num_of_separated_minors",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_4_Num_of_Cases_VAF",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_4_1_Num_of_Cases_willing_to_interviewed",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_5_Num_of_rooms_exclud_kitchen_sanitary_VAF",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q2_5_1_Living_space_in_m2_exclud_kitchen_sanitary_VAF",
  "Assessment.G2_INFORMATION_ON_THE_CASE.G2_6_count",
  "SET.OF.Assessment.G2_INFORMATION_ON_THE_CASE.G2_6",
  "Assessment.G2_INFORMATION_ON_THE_CASE.count_C_position",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_4_test_Num_of_Cases",
  # "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Latitude",
  # "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Longitude",
  # "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Altitude",
  "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_5_GPS_Coordinates.Accuracy",
  "lat", "lon",
  # "Assessment.G2_INFORMATION_ON_THE_CASE.Q9_6_Case_ID_test",
  "meta.instanceID",
  "district_c",                                                                                     
  "district",                                                                                       
  "Gov_NAME",                                                                                       
  "Gov_code" )]


write.csv(cfsme1_data, file = "out/JOR_CFSME_WFP_15032015_anonymised.csv",na="")




# cfsme2 <- read.csv("data/JOR_CFSME_WFP_15032015_Assessment_G2_INFORMATION_ON_THE_CASE_G2_6.csv", encoding= "UTF-8")

cfsme2 <- read.csv("data/JOR_CFSME_WFP_15032015_Assessment_G2_INFORMATION_ON_THE_CASE_G2_6.csv", encoding= "WIN1256", stringsAsFactors=FALSE)


cfsme2$meta.instanceID <- as.factor(substr(cfsme2$KEY , 1,41))

#View( cfsme1$meta.instanceID)
#View( cfsme2$meta.instanceID)

#View( cfsme2$G_9_QUALITATIVE_EXPLANATORY.G9_3.G_Follow_up.Q9_3_1_Name)

cfsme2_data <- cfsme2[, c(  "C_position",
                            "N_position",
                            "Q2_6_gender_interviewee",
                            "Q2_7_age_interviewee",
                            "Q2_8_head_of_case",
                            "G2_8.Q2_8_1_sex_of_the_registered_head_of_case",
                            "G2_8.Q2_8_2_age_of_the_registered_head_of_case",
                            "Q2_9_marital_status_registered_head_of_case",
                            "Q2_10_education_completed_by_head_of_case",
                            "Q2_11_reg_Head_of_case_disabled_or_visibly_impaired",
                            "Q2_12_head_case_have_valid_UNHCR_asylum_seeker_VAF",
                            # "Q2_12_1_registration_UNHCR_type_VAF",
                            # "Q2_12_1a_registration_number_barcode_VAF",
                            # "Q2_12_1b_registration_number_Manual_VAF",
                            "Q2_13_MoI_card_place_same_of_current_residence_VAF",
                            "Q2_14_Total_Case_members_VAF",
                            "G2_15.note2_16",
                            "G2_15.Q2_15_1_members_of_your_case_first_arrive_from_Syria_VAF",
                            "G2_15.Q2_15_2_members_of_your_case_last_arrive_from_Syria_VAF",
                            "Q2_16_Type_of_WFP_assist_case_currently_benefits_from_VAF",
                            "Q2_16_1_Other_ype_of_WFP_assistance",
                            "Q2_17_how_long_been_receivie_WFP_assistance_VAF",
                            "G2_18_count",
                            "sum_Male",
                            "sum_Female",
                            "sum_Above18",
                            "sum_under18",
                            "sum_from_6_to_18",
                            "count_C_position1",
                            "Q2_19_test_Total_Case_members_VAF",
                            "G3_EDUCATION.Q3_1_accessing_formal_education_services_VAF",
                            "G3_EDUCATION.Q3_2_reasons_not_accessing_education_services_VAF",
                            "G3_EDUCATION.Q3_2_1_other_reasons_not_accessing_education_services_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.Q4_1_meals_eaten_Yesterday_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_1.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_1.note_CEREALS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_1.Q4_2_days_consume_CEREALS_bread_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_1.Q4_3_SOURCE_CEREALS_bread_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_2.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_2.note_WHITE_TUBERS_AND_ROOTS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_2.Q4_2_days_consume_WHITE_TUBERS_ROOTS_potato_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_2.Q4_3_SOURCE_WHITE_TUBERS_AND_ROOTS_potato_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3_1.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3_1.note_VEGETABLES_LEAVES",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3_1.Q4_2_days_consume_VEGETABLES_LEAVES_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3_1.Q4_3_SOURCE_VEGETABLES_LEAVES_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3.note_FRUITS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3.Q4_2_days_consume_FRUITS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_3.Q4_3_SOURCE_FRUITS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_4.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_4.note_MEAT",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_4.Q4_2_days_consume_MEAT_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_4.Q4_3_SOURCE_MEAT_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_5.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_5.note_EGGS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_5.Q4_2_days_consume_EGGS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_5.Q4_3_SOURCE_EGGS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_6.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_6.note_FISH_AND_OTHER_SEAFOOD",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_6.Q4_2_days_consume_FISH_AND_OTHER_SEAFOOD_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_6.Q4_3_SOURCE_FISH_AND_OTHER_SEAFOOD_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_7.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_7.note_PULSES_NUTS_AND_SEEDS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_7.Q4_2_days_consume_PULSES_NUTS_AND_SEEDS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_7.Q4_3_SOURCE_PULSES_NUTS_AND_SEEDS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_8.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_8.note_MILK_AND_DAIRY_PRODUCTS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_8.Q4_2_days_consume_MILK_AND_DAIRY_PRODUCTS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_8.Q4_3_SOURCE_MILK_AND_DAIRY_PRODUCTS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_9.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_9.note_OIL_AND_FATS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_9.Q4_2_days_consume_OIL_AND_FATS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_9.Q4_3_SOURCE_OIL_AND_FATS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_10.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_10.note_SWEETS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_10.Q4_2_days_consume_SWEETS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_10.Q4_3_SOURCE_SWEETS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_11.note_4_2and3",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_11.note_SPICES_AND_CONDIMENTS",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_11.Q4_2_days_consume_SPICES_AND_CONDIMENTS_VAF",
                            "G4_FOOD_CONSUMPTION_AND_FOOD_SOURCES.G4_2_11.Q4_3_SOURCE_SPICES_AND_CONDIMENTS_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.note_Q5_1",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.Q5_1_1_times_Rely_less_preferred_less_expensive_food_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.Q5_1_2_times_Borrow_food_or_relied_on_relative_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.Q5_1_3_times_Reduce_Num_of_meals_eaten_in_day_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.Q5_1_4_times_Limit_portion_size_at_mealtime_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_1.Q5_1_5_times_Restrict_adults_for_small_children_to_eat_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.note_Q5_2",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_1_Spent_savings_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_2_Sell_productive_assets_or_means_of_transport_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_3_Reduced_essential_non_food_expenditures_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_4_Bought_food_on_credit_or_borrowed_money_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_5_Sell_household_goods_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_6_changed_accommodation_loc_to_reduce_rental",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_7_male_accept_socially_degr_or_illegal_job",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_8_female_accept_socially_degr_or_illegal_job",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_9_children_under_18_worked",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_10_Sent_adult_case_members_to_beg_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_11_Sent_children_case_members_to_beg_VAF",
                            "G_5HOUSEHOLD_COPING_STRATEGIES.G5_2.Q5_2_12_member_returned_to_Syria_to_provide_resources",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_1_financially_support_another_case_in_the_HH_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_2_financially_supported_by_another_case_in_the_HH_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3.Note_6_3",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3.Q6_3a_First_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3.Q6_3b_Second_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3.Q6_3c_Third_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3a_Other_First_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3_1_a_JDs_generated_First_source_income_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3b_Other_Second_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3_1_b_JDs_generated_Second_source_income_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3c_Other_Third_source_cash_income",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_3_1.Q6_3_1_c_JDs_generated_Third_source_income_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_3_2_JDs_generated_by_other_sources_of_income_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_4_amount_of_UNHCR_Cash_Assistance_receive_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_5_total_amount_of_debt_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_6_total_savingsJDs_spent_over_last_six_months",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_7_total_of_savingsJDs_do_you_have_now",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.note_6_8",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_1Food_Expenditures_excluding_WFP_vouchers_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_2Rent_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_3Utilities_electricity_gas_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_4Health_related_expenditures_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_5Education_related_expenditures_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_6water_network_tanker_bottled_dislodging_water_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_7Transport_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_8Debt_repayment_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_8.Q6_8_9All_other_expenditures_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.Q6_8_9Specify_other_expenditures_VAF",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.note6_9",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_1_Matresses",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_2_Beds",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_3_Winter_Clothes",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_4_Blankets",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_5_Refrigerator",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_6_Stove_Kitchen",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_7_Kitchen_Utensils",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_8_Water_heater",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_9_Table_Chairs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_10_Sofa_set",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_11_Heating_for_house",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_12_Air_conditioning",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_13_Washing_machine",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_14_TV",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_15_Computer",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9.Q6_9_16_Motorized_vehicle",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.note6_9_1",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_1_Matresses_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_2_Beds_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_3_Winter_Clothes_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_4_Blankets_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_5_Refrigerator_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_6_Stove_Kitchen_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_7_Kitchen_Utensils_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_8_Water_heater_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_9_Table_Chairs_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_10_Sofa_set_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_11_Heating_for_house_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_12_Air_conditioning_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_13_Washing_machine_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_14_TV_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_15_Computer_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G_6_9_1.Q6_9_1_16_Motorized_vehicle_shared",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10.Note_6_10",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10.Q6_10a_First_non_cash_needs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10.Q6_10b_Second_non_cash_needs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10.Q6_10c_Third_non_cash_needs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10_1.Q6_10a_Other_First_non_cash_needs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10_1.Q6_10b_Other_Second_non_cash_needs",
                            "G6_MAIN_INCOMES_AND_MAIN_EXPENDITURES.G6_10_1.Q6_10c_Other_Third_non_cash_needs",
                            "G_7LIVING_CONDITIONS.Q7_1_Type_of_housing_of_case_in_host_country_VAF",
                            "G_7LIVING_CONDITIONS.Q7_1_a_it_is_rent_VAF",
                            "G_7LIVING_CONDITIONS.G_7_1.Q7_1_1_How_much_if_rent_VAF",
                            "G_7LIVING_CONDITIONS.G_7_1.Q7_1_2_Existence_of_rental_contract",
                            "G_7LIVING_CONDITIONS.G_7_1.Q7_1_3_Duration_of_rental_agreement",
                            "G_7LIVING_CONDITIONS.Q7_2_times_forced_move_or_evicted_in_Jordan_VAF",
                            "G_7LIVING_CONDITIONS.Q7_3_Ventilation_VAF",
                            "G_7LIVING_CONDITIONS.Q7_3_1_type_of_Ventilation_VAF",
                            "G_7LIVING_CONDITIONS.Q7_4_Access_to_Electricity_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.note_7_4_1",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_1_Damp_walls_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_2_Leaking_roofs_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_3_Hygienic_concerns_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_4_broken_windows_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_5_Broken_doors_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_6_Privacy_concern_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_7_Pests_VAF",
                            "G_7LIVING_CONDITIONS.G_7_4_1.Q7_4_8_NA_VAF",
                            "G_7LIVING_CONDITIONS.Q7_5_judge_assessed_shelter_VAF",
                            "G_7LIVING_CONDITIONS.Q7_6_access_to_any_private_latrine_toile_VAF",
                            "G_7LIVING_CONDITIONS.Q7_6_1_people_share_latrine_with_VAF",
                            "G_7LIVING_CONDITIONS.Q7_7_kind_of_latrine_toilet_facility_does_your_case_use_VAF",
                            "G_7LIVING_CONDITIONS.Q7_8_latrine_physically_accessible_VAF",
                            "G_7LIVING_CONDITIONS.Q7_9_latrine_in_environment_perceived_to_be_safely_VAF",
                            "G_7LIVING_CONDITIONS.Q7_10_your_case_have_access_to_sufficient_water_VAF",
                            "G_7LIVING_CONDITIONS.Q7_11_days_the_case_not_have_water_in_past_30days_VAF",
                            "G_7LIVING_CONDITIONS.Q7_12_important_sources_of_water_VAF",
                            "G_7LIVING_CONDITIONS.Q7_13_Wastewater_collection_disposal_VAF",
                            "G_7LIVING_CONDITIONS.Q7_13_other_Wastewater_collection_disposal_VAF",
                            "G_8_HEALTH.Q8_1_Have_medical_need_in_past_six_months_VAF",
                            "G_8_HEALTH.Q8_1_1_able_to_access_public_hospitals_clinics_past_six_m_VAF",
                            "G_8_HEALTH.Q8_1_2_where_able_to_access_public_hospitals_clinics_VAF",
                            "G_8_HEALTH.Q8_1_3_kind_of_difficulty_access_public_hospitals_clinics_VAF",
                            "G_8_HEALTH.Q8_1_3_1_other_difficulty_access_public_hospitals_clinics_VAF",
                            "G_9_QUALITATIVE_EXPLANATORY.Q9_1_family_has_reported_safety_or_protection_issues",
                            "G_9_QUALITATIVE_EXPLANATORY.Q9_1_1_details_safety_or_protection_issues",
                            "G_9_QUALITATIVE_EXPLANATORY.Q9_2_noticed_any_other_protection_concerns",
                            "G_9_QUALITATIVE_EXPLANATORY.Q9_2_1_details_other_protection_concerns",
                           # "G_9_QUALITATIVE_EXPLANATORY.G9_3.Q9_3_contact_head_case",
                           # "G_9_QUALITATIVE_EXPLANATORY.G9_3.G_Follow_up.Q9_3_1_Name",
                           # "G_9_QUALITATIVE_EXPLANATORY.G9_3.G_Follow_up.Q9_3_2_Telephone_number",
                            "PARENT_KEY",
                            "KEY",
                            "SET.OF.G2_6",
                            "meta.instanceID")]


names(cfsme2)
write.csv(cfsme2_data, file = "out/JOR_CFSME_WFP_15032015_Assessment_G2_INFORMATION_ON_THE_CASE_G2_6_anonymised.csv",na="")


cfsme3 <- read.csv("data/JOR_CFSME_WFP_15032015_G2_18.csv", encoding= "WIN1256", stringsAsFactors=FALSE)
names(cfsme3)

cfsme3$meta.instanceID <- as.factor(substr(cfsme3$KEY , 1,41))
#View( cfsme3$meta.instanceID)

cfsme3_data <- cfsme3[, c(                              
                             "C_position1",
                             "N_position1",
                             "G_2_18_1.note2_18_1",
                             "G_2_18_1.Q2_18_1_age_y",
                             "G_2_18_1.Q2_18_1_age_m",
                             "Q2_18_2_gender",
                             "Q2_18_3_MoI_card_place_same_of_current_residence_VAF",
                             "G_2_18_4.note2_18_4",
                             "G_2_18_4.Q2_18_4_in_education_VAF",
                             "G_2_18_4.Q2_18_4_employed_VAF",
                             "G_2_18_4.Q2_18_4_in_training_VAF",
                             "Q2_18_5_worked_been_employed_in_the_last_30days",
                             "G_2_18_6to11.note_2_18_6to11",
                             "G_2_18_6to11.Q2_18_6_Visual_hearing_impairment_VAF",
                             "G_2_18_6to11.Q2_18_7_Other_physical_impairment_VAF",
                             "G_2_18_6to11.Q2_18_8_mental_impairment_VAF",
                             "G_2_18_6to11.Q2_18_9_Injured_VAF",
                             "G_2_18_6to11.Q2_18_10_Chronically_ill_or_serious_medical_conditions_VAF",
                             "G_2_18_6to11.Q2_18_11_need_Other_people_support_to_do_daily_activi_VAF",
                             "G_UNDER_18.G_2_18_12to17.label_2_18_12to17",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_12_Finished_10th_grade_in_Syria_or_Jordan_VAF",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_13_Currently_attending_public_school_VAF",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_14_Currently_attending_private_school_VAF",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_15_Other_educational_centres_VAF",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_16_Working_outside_the_home_VAF",
                             "G_UNDER_18.G_2_18_12to17.Q2_18_17_Missed_education_in_school_VAF",
                             "G_UNDER_18.Q2_18_18_years_missed_education_VAF",
                             "G_UNDER_5.G_2_18_19_20.label_2_18_19_20",
                             "G_UNDER_5.G_2_18_19_20.Q2_18_19_child_immunized_for_polio_VAF",
                             "G_UNDER_5.G_2_18_19_20.Q2_18_20_child_immunized_for_measles_VAF",
                             "G_UNDER_23M.Q2_18_21_receive_breast_milk_yesterday_day_or_night",
                             "G_UNDER_23M.Q2_18_22_times_the_child_drink_millk_yesterday",
                             "G_UNDER_23M.Q2_18_23_child_have_vaccination_card",
                             "Q2_18_24_children_breastfed_exclusively",
                             "Male",
                             "Female",
                             "Above18",
                             "under18",
                             "from_6_to_18",
                             "PARENT_KEY",
                             "KEY",
                             "SET.OF.G2_18",
                             "meta.instanceID")]
write.csv(cfsme3_data, file = "out/JOR_CFSME_WFP_15032015_G2_18_anonymised.csv",na="")




### Merging all dataset in one

cfsme <-  merge(x=cfsme1, y=cfsme2, by="meta.instanceID")
cfsme <-  merge(x=cfsme, y=cfsme3, by="KEY.y",  by.y="PARENT_KEY")
write.csv(cfsme, file = "out/JOR_CFSME_WFP_all.csv",na="")

cfsme.variable <- as.data.frame(names(cfsme))
names(cfsme.variable)
cfsme.variable$vaf <- with(cfsme.variable, ifelse(grepl("VAF", 
                                                        #ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,
                                                        cfsme.variable[,1]),
                                  paste0("VAF"), paste0("nonVAF") ))
cfsme.variablevaf <- cfsme.variable[ (cfsme.variable$vaf=="VAF"),1 ]
cfsme.vaf <-cfsme[,!(names(cfsme) %in% cfsme.variablevaf)]
write.csv(cfsme.vaf, file = "out/JOR_CFSME_WFP_all_vafonly.csv",na="")
