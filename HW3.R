library(tidyverse)
library(ggforce)

# reading in data
setwd("/Users/feliciaselbst")
resp <- read_csv("Desktop/DS Capstone/atusresp-2024/atusresp_2024.dat")
rost <- read_csv("Desktop/DS Capstone/atusrost-2024/atusrost_2024.dat")
act <- read_csv("Desktop/DS Capstone/atusact-2024/atusact_2024.dat")
who <- read_csv("Desktop/DS Capstone/atuswho-2024/atuswho_2024.dat")


# renaming variables
respondents <- resp %>%	transmute( 	
						case_id = TUCASEID, 
						employment_status = TELFS, 
						major_industry = TRMJIND1, 
						survey_weight = TUFINLWGT )
roster <- rost %>% transmute(
						case_id = TUCASEID, 
						person_number = TULINENO, 
						age = TEAGE	)
activities <- act %>% transmute(
						case_id = TUCASEID,
    						activity_number = TUACTIVITY_N,
    						start_time = TUSTARTTIM,
    						stop_time = TUSTOPTIME,
    						activity_category = TUTIER1CODE,
    						activity_subcategory = TUTIER2CODE,
    						activity_code = TRCODE )
ppl_present <- who %>% transmute(
    						case_id = TUCASEID,
    						activity_number = TUACTIVITY_N,
    						person_number = TULINENO,
    						relationship_code = TUWHO_CODE )

# getting case_id and respondents ages
respondent_ages <- roster %>% 
						  filter(person_number == 1) %>% 
						  select( case_id, respondent_age = age )

# getting employed ppl older than or equal to 18
employed_adults <- respondents %>% 
							   filter(employment_status %in% c(1, 2)) %>% 
							   left_join( respondent_ages, by = "case_id") %>% 
							   filter(respondent_age >= 18)

# adding industry tags
employed_adults <- employed_adults %>% mutate( industry_group = case_when( major_industry %in% 1:6   ~ "Production",
																		   major_industry %in% 7:9   ~ "Business",
																		   major_industry %in% 10:13 ~ "Other", TRUE ~ NA_character_ ) ) %>% 
									   filter(!is.na(industry_group))
# setting noon
noon <- hms::as_hms("12:00:00")

# getting subset respondents noon-time activity
noon_activities <- activities %>% filter( start_time <= noon, stop_time > noon )

# checking if activity is eating
noon_activities <- noon_activities %>% mutate( eating_at_noon = (activity_category == "11" & activity_subcategory == "01") )

# checking if there is a child present
child_at_noon <- noon_activities %>% select(case_id, activity_number) %>%
									 left_join( ppl_present, by = c("case_id", "activity_number") ) %>%
  									 left_join( roster, by = c("case_id", "person_number") ) %>%
  									 group_by(case_id) %>%
  									 summarise( child_under_10_present = any( age < 10, na.rm = TRUE ), .groups = "drop" )
# checking if there is a child present and that respondent is eating
noon_activities <- noon_activities %>% left_join( child_at_noon, by = "case_id" ) %>%
									   mutate( child_under_10_present = replace_na(child_under_10_present, FALSE),
											   eating_with_child_under_10 = if_else( eating_at_noon & child_under_10_present, "Yes", "No" ) )

noon_status <- noon_activities %>% select( case_id, eating_at_noon, child_under_10_present, eating_with_child_under_10 )

plot_data <- employed_adults %>% inner_join( noon_status, by = "case_id" )

plot_data <- plot_data %>% mutate( industry_group = factor(	industry_group, levels = c( "Business", "Production", "Other" ) ),
								   eating_with_child_under_10 = factor( eating_with_child_under_10, levels = c( "No", "Yes" ) ),
   								   plot_group = interaction( industry_group, eating_with_child_under_10, sep = "_" ),
    								   plot_group = factor( plot_group, levels = c( "Business_No", "Business_Yes",
    								   												"Production_No", "Production_Yes",
    								   												"Other_No", "Other_Yes" ) ) )

ggplot(	plot_data, aes( x = plot_group, y = respondent_age, fill = industry_group ) ) +
		geom_violin( width = 1.0, trim = FALSE, color = "black", linewidth = 0.5 ) +
		geom_sina( aes(color = industry_group), shape = 1, size = 0.2, maxwidth = 0.9, alpha = 0.7 ) +
		geom_boxplot( width = 0.06, fill = "white", color = "black", outlier.shape = NA, linewidth = 0.5 ) +
		scale_x_discrete( labels = c( "No", "Yes", "No", "Yes", "No", "Yes" ) ) +
		scale_fill_manual( values = c( "Business" = "#F2B6BE", "Production" = "#8FB2F0", "Other" = "#FFD36A" ) ) +
  		scale_color_manual( values = c( "Business" = "#E6332A", "Production" = "#0759E6", "Other" = "#D98900" ), guide = "none" ) +
		labs( title = paste0( "Ages of adults with jobs,\n", "by industry and whether they are eating with kids ", "(under 10) at noon" ),
			  x = "Eating with kid (under 10) at noon?",
			  y = "Age",
			  fill = NULL ) +
		theme_classic(base_size = 12) +
		theme( plot.title = element_text( hjust = 0.5, face = "bold", size = 14 ) )
  
  
  
  
  
  
  
  