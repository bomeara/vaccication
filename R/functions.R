# https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk27/health5_week27.xlsx
# This has hesitancy info broken by demographic groups; I summarize it for just those age 18-64 as working age groups
# Uses tutorial from Isabella Velázquez, https://ivelasq.rbind.io/blog/tidying-census-data/
GetHesitancyWorkingAge <- function(week=27, state_only=TRUE) {
    GET(paste0("https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk", week, "/health5_week", week, ".xlsx"), write_disk(path <- tempfile(fileext = ".xlsx")))
    census_list <-
        path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map(~ read_excel(path = path, sheet = .x, skip = 3), .id = "Sheet")

    column_text <- c("Age", "Total", "Total_Some_Vaccinated", "Receiving_All_Doses", "Receving_Some_But_Not_All_Doses", "Yes_But_Did_Not_Report_Dosing", "Total_Not_Vaccinated", "Will_Definitely_Get", "Will_Probably_Get", "Will_Probably_Not_Get", "Will_Definitely_Not_Get", "No_But_Did_Not_Reply_Why", "No_Response_On_Vaccination")
    census_list <- 
        census_list %>% 
        map(~ slice(.x, -1:-6, -11:-89)) %>% 
        map(., set_names, nm = column_text) %>%
        map(~ mutate_at(.x, column_text[-1], list(~ as.numeric(.))))

    census_df <- data.frame()
    for (i in seq_along(census_list))    {
        local_df <- data.frame(Region=names(census_list)[i])

        local_df <- cbind(local_df, as.data.frame(t(apply(census_list[[i]][,-1], 2, sum, na.rm=TRUE))))
        census_df <- rbind(census_df, local_df)
    }
    census_df$PercentageWorkingAgeHesitant <- 100*(census_df$Will_Probably_Not_Get+census_df$Will_Definitely_Not_Get)/census_df$Total
    census_df$PercentageWorkingAgeVeryHesitant <- 100*(census_df$Will_Definitely_Not_Get)/census_df$Total
    census_df$PercentageWorkingAgeVaccinated <- 100*(census_df$Total_Some_Vaccinated)/census_df$Total
    if(state_only) {
        census_df <- subset(census_df, nchar(Region)==2)
        census_df <- subset(census_df, Region!="US")
    }
    return(census_df)
}

GetCountyOnly <- function(x) {
    counties <- rep(NA, length(x))
    x <- gsub("_", ", ", x)
    for (i in seq_along(x)) {
        counties[i] <- strsplit(x[i], ", ")[[1]][1]
    }
    return(counties)
}

GetStateOnly <- function(x) {
    states <- rep(NA, length(x))
    x <- gsub("_", ", ", x)
    for (i in seq_along(x)) {
        states[i] <- strsplit(x[i], ", ")[[1]][2]
    }
    return(states)
}


#This gets hesitancy estimated by county by the CDC: https://aspe.hhs.gov/pdf-report/vaccine-hesitancy
GetHesitancyByCounty <- function() {
    #https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv?accessType=DOWNLOAD
    options(timeout=600) # let things download for at least ten minutes
    hesitancy <- read.csv("https://data.cdc.gov/api/views/q9mh-h2tw/rows.csv")
    hesitancy$Percent.adults.fully.vaccinated.against.COVID.19 <- 100*hesitancy$Percent.adults.fully.vaccinated.against.COVID.19
    hesitancy$Percent.estimated.hesitant <- 100*hesitancy$Estimated.hesitant
    hesitancy$Percent.estimated.strongly.hesitant <- 100*hesitancy$Estimated.strongly.hesitant
    hesitancy$FIPS <- hesitancy$`FIPS.Code`
    hesitancy_simplified <- hesitancy %>% select(FIPS, County.Name, State, Percent.estimated.hesitant, Percent.estimated.strongly.hesitant, Percent.adults.fully.vaccinated.against.COVID.19)
    hesitancy_simplified$StateAbbreviation <- state.abb[match(hesitancy_simplified$State, toupper(state.name))]
    hesitancy_simplified$County <- GetCountyOnly(hesitancy_simplified$County.Name)
    hesitancy_simplified$County <- gsub(" County", "", hesitancy_simplified$County)
    hesitancy_simplified$County_StateAbbreviation <- paste0(hesitancy_simplified$County, "_", hesitancy_simplified$StateAbbreviation)
    return(hesitancy_simplified)
}

GetHospitalData <- function(countycitystate) {
    options(timeout=600) # let things download for at least ten minutes
    hhs_sources <- jsonlite::fromJSON("https://healthdata.gov/data.json?page=0")
    capacity_by_facility_number <- grep("COVID-19 Reported Patient Impact and Hospital Capacity by Facility", hhs_sources$dataset$title)
    capacity_by_facility_url <- hhs_sources$dataset$distribution[capacity_by_facility_number][[1]]$downloadURL[1] #often a week behind though
temp = tempfile(fileext = ".csv")
    utils::download.file(capacity_by_facility_url, temp, method="libcurl")

    hhs_capacity <- read.csv(file=temp)  
    hhs_capacity <- subset(hhs_capacity, state!="PR")
    hhs_capacity$County <- NA
    for(i in sequence(nrow(hhs_capacity))) {
        matching_row <- which(countycitystate$Zip == hhs_capacity$zip[i])
        if(length(matching_row)==1) {
            hhs_capacity$County[i] <- countycitystate$County[matching_row]
        }
    }
    hhs_simplified <- hhs_capacity %>% select(state, city, zip, County, hospital_name, all_adult_hospital_inpatient_beds_7_day_avg, all_adult_hospital_inpatient_bed_occupied_7_day_avg, total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg, total_icu_beds_7_day_avg, icu_beds_used_7_day_avg)
    for (i in 6:11) {
        hhs_simplified[,i]  <- as.numeric(hhs_simplified[,i])
        bad_vals <- which(hhs_simplified[,i]==-999999)
        hhs_simplified <- hhs_simplified[-bad_vals,] # deleting hospitals reporting insufficient data -- typically small hospitals
    }
    hhs_simplified$County_StateAbbreviation <- paste0(hhs_simplified$County, "_", hhs_simplified$state)
    hhs_county <- hhs_simplified %>% group_by(County_StateAbbreviation) %>% summarize(all_adult_hospital_inpatient_beds_7_day_avg=sum(all_adult_hospital_inpatient_beds_7_day_avg), all_adult_hospital_inpatient_bed_occupied_7_day_avg=sum(all_adult_hospital_inpatient_bed_occupied_7_day_avg), total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg=sum(total_adult_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg), total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg=sum(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_avg), total_icu_beds_7_day_avg=sum(total_icu_beds_7_day_avg), icu_beds_used_7_day_avg=sum(icu_beds_used_7_day_avg))
    hhs_county$Percent_ICU_occupied_7_day_avg <- 100*hhs_county$icu_beds_used_7_day_avg/hhs_county$total_icu_beds_7_day_avg
    hhs_county$County  <- GetCountyOnly(hhs_county$County_StateAbbreviation)
    hhs_county$StateAbbreviation  <- GetStateOnly(hhs_county$County_StateAbbreviation)
    return(hhs_county)
}

GetCountyCityState <- function() {
    # From user roccer on https://stackoverflow.com/questions/36685805/how-to-find-the-county-for-each-city-in-a-vector-of-city-names-using-r
    temp <- tempfile()
    download.file("http://download.geonames.org/export/zip/US.zip",temp)
    con <- unz(temp, "US.txt")
    US <- read.delim(con, header=FALSE)
    unlink(temp)
    colnames(US) <- c("Country", "Zip", "City", "State", "StateAbbreviation", "County", "Unknown1", "Unknown2", "Unknown3", "Latitude", "Longitude", "Unknown4")
    US <- US[!duplicated(US$Zip),]
    return(US)
}

# Uses NY Times' data
GetCountyCaseAverages <- function() {
    nyt <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties.csv")
    most_recent_dates <- sort(unique(nyt$date), decreasing=TRUE)[1:2]
    nyt_last2weeks <- subset(nyt, date %in% most_recent_dates)
    nyt_last2weeks$County_State <- paste0(nyt_last2weeks$county, "_", nyt_last2weeks$state)
    nyt_2wk_sum <- nyt_last2weeks %>% group_by(County_State) %>% summarize(cases_avg=sum(cases_avg), cases_avg_per_100k=sum(cases_avg_per_100k), deaths_avg=sum(deaths_avg), deaths_avg_per_100k=sum(deaths_avg_per_100k))
    nyt_2wk_sum$cases_per_day_avg <- nyt_2wk_sum$cases_avg/14
    nyt_2wk_sum$cases_per_day_per_100k_avg <- nyt_2wk_sum$cases_avg_per_100k/14
    nyt_2wk_sum$County  <- GetCountyOnly(nyt_2wk_sum$County_State)
    nyt_2wk_sum$State  <- GetStateOnly(nyt_2wk_sum$County_State)
    nyt_2wk_sum$StateAbbreviation <- state.abb[match(toupper(nyt_2wk_sum$State), toupper(state.name))]
    nyt_2wk_sum$County_StateAbbreviation <- paste0(nyt_2wk_sum$County, "_", nyt_2wk_sum$StateAbbreviation)
    return(nyt_2wk_sum)
}

GetCommunityTransmissionReportLastDate <- function() {
	spreadsheet <- read.csv("https://healthdata.gov/api/views/6hii-ae4f/rows.csv?accessType=DOWNLOAD")	
	attachments <- (jsonlite::fromJSON(spreadsheet[nrow(spreadsheet),]$Metadata.Published))$attachments
	attachments <- attachments[grepl("xlsx", attachments$name),]
	attachments <- attachments[order(attachments$name, decreasing=TRUE),]
	url <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/", attachments$assetId[1],"?download=true&filename=", attachments$name[1], "_Public.xlsx")
	report_date <- gsub("Community_Profile_Report_", "", gsub("_Public.xlsx", "", attachments$name[1]))
	return(list(report_url=url, report_date=report_date))
}

# From https://beta.healthdata.gov/Health/COVID-19-Community-Profile-Report/gqxm-d9w9
GetCommunityTransmissionReport <- function(report_url) {
    #url <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/93e07285-d044-4fc4-b93b-e8100db5e7a1?download=true&filename=Community_Profile_Report_", reportdate, "_Public.xlsx")
    #GET(paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/93e07285-d044-4fc4-b93b-e8100db5e7a1?download=true&filename=Community_Profile_Report_", report_date, "_Public.xlsx"), write_disk(path <- tempfile(fileext = ".xlsx")))
    GET(report_url, write_disk(path <- tempfile(fileext = ".xlsx")))
    cdc_list <-
        path %>% 
        excel_sheets() %>% 
        set_names() %>% 
        map(~ read_excel(path = path, sheet = .x, skip = 1), .id = "Sheet")
    cdc_county <- cdc_list[["Counties"]]
    cdc_state <- cdc_list[["States"]]
    cdc_county$Population_65_Plus <- cdc_county$`People who are fully vaccinated - ages 65+`/cdc_county$`People who are fully vaccinated as % of population - ages 65+`
    cdc_county$Population_Under_65 <- cdc_county$Population - cdc_county$Population_65_Plus
    cdc_county$Proportion_Fully_Vaccinated_Under_65 <- (cdc_county$`People who are fully vaccinated` - cdc_county$`People who are fully vaccinated - ages 65+`)/ cdc_county$Population_Under_65
	cdc_county$Proportion_Fully_Vaccinated_All <- cdc_county$"People who are fully vaccinated as % of total population"
    cdc_county$`% staffed adult ICU beds occupied_County` <- cdc_county$`% staffed adult ICU beds occupied`
	cdc_county$Proportion_Fully_Vaccinated_12_to_17 <- NA
	if("People who are fully vaccinated as % of population - ages 12-17" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_12_to_17 <- cdc_county$"People who are fully vaccinated as % of population - ages 12-17")
	}
	
	cdc_county$Proportion_Fully_Vaccinated_18_plus <- NA

	if("People who are fully vaccinated as % of population - ages 18+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_18_plus <-  cdc_county$"People who are fully vaccinated as % of population - ages 18+")
	}
	
	cdc_county$Proportion_Fully_Vaccinated_65_plus <- NA
	if("People who are fully vaccinated as % of population - ages 65+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Fully_Vaccinated_65_plus <-  cdc_county$"People who are fully vaccinated as % of population - ages 65+")
	}
	
	
	
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_12_to_17 <- NA
	
	if("People initiating vaccination as % of population - last 7 days - ages 12-17" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_12_to_17 <- cdc_county$"People initiating vaccination as % of population - last 7 days - ages 12-17")
	}
	
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_18_plus <- NA
	if("People initiating vaccination as % of population - last 7 days - ages 18+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_18_plus <- cdc_county$"People initiating vaccination as % of population - last 7 days - ages 18+")
	}
	cdc_county$Proportion_Initiating_Vaccination_Last_7_days_65_plus <- NA
	if("People initiating vaccination as % of population - last 7 days - ages 65+" %in% colnames(cdc_county)) {
		try(cdc_county$Proportion_Initiating_Vaccination_Last_7_days_65_plus <-  cdc_county$"People initiating vaccination as % of population - last 7 days - ages 65+")
	}
	

    cdc_state$Population_65_Plus <- cdc_state$`People who are fully vaccinated - ages 65+`/cdc_state$`People who are fully vaccinated as % of population - ages 65+`
    cdc_state$Population_Under_65 <- cdc_state$Population - cdc_state$Population_65_Plus
    cdc_state$Proportion_Fully_Vaccinated_Under_65 <- (cdc_state$`People who are fully vaccinated` - cdc_state$`People who are fully vaccinated - ages 65+`)/ cdc_state$Population_Under_65
    

    # Now deal with counties where there is no vaccination info by county, like all of Texas. Report state estimate as best one
    cdc_county$Vaccination_Data_Is_For_County <- TRUE
    for (county_index in sequence(nrow(cdc_county))) {
        if(is.na(cdc_county$Proportion_Fully_Vaccinated_Under_65[county_index])) {
            cdc_county$Proportion_Fully_Vaccinated_Under_65[county_index] <- cdc_state$Proportion_Fully_Vaccinated_Under_65[which(cdc_state$`State Abbreviation`==cdc_county$`State Abbreviation`[county_index])]
            cdc_county$`People who are fully vaccinated as % of total population`[county_index] <- cdc_state$`People who are fully vaccinated as % of total population`[which(cdc_state$`State Abbreviation`==cdc_county$`State Abbreviation`[county_index])]
            cdc_county$Vaccination_Data_Is_For_County[county_index] <- FALSE
        }
    }

    # Hospital capacity probably is more relevant on a state basis than a county (since hospitals work regionally -- a small rural county might not have many ICU beds but the neighboring city might)
    cdc_state_hospital <- cdc_state %>% select("State Abbreviation", "% staffed adult ICU beds occupied", "% hospitals with a current staffing shortage")
    cdc_county_with_hospital <- cdc_county%>% left_join(cdc_state_hospital, by="State Abbreviation", copy=TRUE, suffix=c("_County", "_State"))
    cdc_county_with_hospital$FIPS <- cdc_county_with_hospital$`FIPS code`
    cdc_county_with_hospital <- cdc_county_with_hospital %>% select(County, "FIPS", "State Abbreviation", "Population", "Cases per 100k - last 7 days", "Deaths per 100k - last 7 days", "Community Transmission Level - last 7 days", "% staffed adult ICU beds occupied_State", "% staffed adult ICU beds occupied_County", "People who are fully vaccinated as % of total population", "Proportion_Fully_Vaccinated_Under_65", "Vaccination_Data_Is_For_County", "% hospitals with a current staffing shortage", "Area of Concern Category", "Proportion_Fully_Vaccinated_12_to_17" , "Proportion_Fully_Vaccinated_18_plus", "Proportion_Fully_Vaccinated_65_plus", "Proportion_Fully_Vaccinated_All", "Proportion_Initiating_Vaccination_Last_7_days_12_to_17", "Proportion_Initiating_Vaccination_Last_7_days_18_plus", "Proportion_Initiating_Vaccination_Last_7_days_65_plus" )
    return(cdc_county_with_hospital)
}

# Min date chosen as the date vaccination data first included
GetAllCommunityTransmissionReportDates <- function(mindate=20210412) {
	spreadsheet <- read.csv("https://healthdata.gov/api/views/6hii-ae4f/rows.csv?accessType=DOWNLOAD")	
	attachments <- (jsonlite::fromJSON(spreadsheet[nrow(spreadsheet),]$Metadata.Published))$attachments
	attachments <- attachments[grepl("xlsx", attachments$name),]
	report_urls <- rep(NA,nrow(attachments)) 
	report_dates <- rep(NA,nrow(attachments)) 
	for (i in seq_along(attachments$assetId)) {
		report_urls[i] <- paste0("https://beta.healthdata.gov/api/views/gqxm-d9w9/files/", attachments$assetId[i],"?download=true&filename=", attachments$name[i], "_Public.xlsx")
		report_dates[i] <- gsub("Community_Profile_Report_", "", gsub("_Public.xlsx", "", attachments$name[i]))
	}
	if(!is.null(mindate)) {
		report_urls <- report_urls[which(as.numeric(report_dates)>=mindate)]	
		report_dates <- report_dates[which(as.numeric(report_dates)>=mindate)]	

	}
	return(list(report_urls=report_urls, report_dates=report_dates))
}

GetAllCommunityTransmissionReports <- function(urls, dates) {
	all_reports <- data.frame()
	for (i in seq_along(urls)) {
		local_report <- GetCommunityTransmissionReport(urls[i])
		local_report$ReportDate <- dates[i]
		all_reports <- rbind(all_reports, local_report)
		Sys.sleep(3)	
	}	
	return(as.data.frame(all_reports))
}

AggregateCountyInformation <- function(hesitancy_by_county, cases_by_county, hospitalization_by_county) {
    step1 <- full_join(hesitancy_by_county, cases_by_county, by="County_StateAbbreviation")
    result <- full_join(step1, hospitalization_by_county, by="County_StateAbbreviation")
    result <- result[!is.na(result$Percent.estimated.hesitant),]
    result <- result[!is.na(result$cases_per_day_avg),]
    result_simplified <- result %>% select(County, County_StateAbbreviation, StateAbbreviation, cases_per_day_avg, cases_per_day_per_100k_avg, Percent_ICU_occupied_7_day_avg, Percent.estimated.hesitant, Percent.estimated.strongly.hesitant, Percent.adults.fully.vaccinated.against.COVID.19, total_icu_beds_7_day_avg)
    result_simplified$County <- GetCountyOnly(result_simplified$County_StateAbbreviation)
    result_simplified$StateAbbreviation <- GetStateOnly(result_simplified$County_StateAbbreviation)
    return(result_simplified)
}

JoinHesitancyWithCDCWeekly <- function(hesitancy_by_county, cdc_weekly) {
    result <- full_join(cdc_weekly, hesitancy_by_county, by="FIPS", copy=TRUE, suffix=c("_CDC_Name", "_Hesitancy_Name"))
    result$Percent_State_ICU_Beds_Filled <- 100*result$`% staffed adult ICU beds occupied_State`
    result$Percent_County_ICU_Beds_Filled <- 100*result$`% staffed adult ICU beds occupied_County`
    result$Percent_Fully_Vaccinated <- 100 * result$`People who are fully vaccinated as % of total population`
    result$Percent_Fully_Vaccinated_Under_65 <- 100 * result$Proportion_Fully_Vaccinated_Under_65
    result$Percent_State_Hospitals_With_Staffing_Shortage <- 100 * result$`% hospitals with a current staffing shortage`
    result$County <- result$County_Hesitancy_Name
    result$Percent_Estimated_Vaccination_Hesitant <- result$Percent.estimated.hesitant
    result$Percent_Estimated_Vaccination_Strongly_Hesitant <- result$Percent.estimated.strongly.hesitant
    result$Community_Transmission_Level <- result$`Community Transmission Level - last 7 days`
    result$Area_of_Concern_Category <- result$`Area of Concern Category`
    result$State <- result$StateAbbreviation
    result_simplified <- result %>% select(County, State, Community_Transmission_Level, Percent_Fully_Vaccinated, Percent_Fully_Vaccinated_Under_65, Percent_Estimated_Vaccination_Hesitant, Percent_Estimated_Vaccination_Strongly_Hesitant, Percent_State_ICU_Beds_Filled, Percent_County_ICU_Beds_Filled, Percent_State_Hospitals_With_Staffing_Shortage, Population, FIPS, Vaccination_Data_Is_For_County, Area_of_Concern_Category)
    result_simplified <- result_simplified[!is.na(result_simplified$County),]
    return(result_simplified)
}

WriteCSVTable <- function(result, file_out="data/aggregation_by_county.csv") {
    write.csv(result, file=file_out, row.names=FALSE)
    return(file_out)
}


#i.e., use proportion vaccinated, with county population as weight
VectorToCurvedGrade <- function(x, wt=NULL, higher_is_A=TRUE) {
    x <- as.numeric(x)
    if(is.null(wt)) {
        wt <- rep(1, length(x))
    } else {
        wt <- as.numeric(wt)
    }
    if(!higher_is_A) {
        x <- max(x,na.rm=TRUE) - x
    }
    mean_val <- Hmisc::wtd.mean(x, wt, normwt=TRUE)
    sd_val <- sqrt(Hmisc::wtd.var(x, wt, normwt=TRUE))
    grades <- rep(NA, length(x))
    grades[x>=mean_val+2.5*sd_val] <- "A"
    grades[x<mean_val+2.5*sd_val & x>=mean_val+1.5*sd_val] <- "B"
    grades[x<mean_val+1.5*sd_val & x>=mean_val-1.5*sd_val] <- "C"
    grades[x<mean_val-1.5*sd_val & x>=mean_val-2.5*sd_val] <- "D"
    grades[x<mean_val-sd_val] <- "F"
    return(grades)
}

GetGeonames <- function() {
    options(timeout=600) # let things download for at least ten minutes
    admin2codes <- read.delim("https://download.geonames.org/export/dump/admin2Codes.txt", header=FALSE, quote="")
    colnames(admin2codes) <- c("countycode", "countyname", "countyasciiname", "countygeonameid")
    featurecodes <- read.delim("https://download.geonames.org/export/dump/featureCodes_en.txt", header=FALSE)
    temp <- tempfile()
    download.file("https://download.geonames.org/export/dump/US.zip",temp)
    us <- read.delim(unz(temp, "US.txt"), header=FALSE)
    colnames(us) <- c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude", "featureclass", "featurecode", "countrycode", "cc2", "admin1code", "admin2code", "admin3code", "admin4code", "population", "elevation", "dem", "timezone", "modificationdate")
    us$featurecombined <- paste0(us$featureclass, ".",us$featurecode)
    focal_features <- c("amusement park", "zoo","museum", "university", "capital of a political entity", "seat of a first-order administrative division", "seat of a second-order administrative division", "seat of a third-order administrative division", "seat of a fourth-order administrative division", "seat of a fifth-order administrative division", "populated place")
    focal_features_names <- focal_features
    focal_features_names[5:11] <- "Populated Place"
    focal_sites <- data.frame()
    for (i in seq_along(focal_features)) {
        feature_index <- subset(featurecodes, V2==focal_features[i])$V1
        local_focal <- subset(us, featurecombined==feature_index)
        if(nrow(local_focal)>0) {
            local_focal$Type <- focal_features_names[i]
			if(i>=7) { #populated places that aren't state capitals
				local_focal <- subset(local_focal, local_focal$population>=5000)
			}
            if(nrow(focal_sites)==0) {
                focal_sites <- local_focal
            } else {
                focal_sites <- rbind(focal_sites, local_focal)
            }
        }
    }
    local_focal <- us[grepl("national park", us$name, ignore.case=TRUE),]
    local_focal$Type <- "national park"
    focal_sites <- rbind(focal_sites, local_focal)

    # big_cities <- us[us$featurecode=="PPL",]
    # big_cities <- big_cities[which(nchar(big_cities$alternatenames)>200),]
    # big_cities$Type <- "city"
    # focal_sites <- rbind(focal_sites, big_cities)

    focal_sites$countycode <- paste0("US", ".", focal_sites$admin1code, ".", focal_sites$admin2code)
    focal_sites <- left_join(focal_sites, admin2codes, by="countycode")
    focal_sites$countyname <- gsub(" County", "", focal_sites$countyname)
    focal_sites <- focal_sites[!is.na(focal_sites$countyname),]

    #focal_sites <- focal_sites[!duplicated(focal_sites$name),]
    focal_sites$State <- focal_sites$admin1code
    focal_sites$County <- focal_sites$countyname
    return(focal_sites)
}

GetBIA_data <- function() {
	# from https://hub.arcgis.com/datasets/2e915ef3df48422283e5b2c7d89dfcba?geometry=-137.461%2C2.092%2C-84.726%2C77.153
	# "BIA Indian Lands Dataset (Indian Lands of the United States)"
	bia <- read.csv("https://opendata.arcgis.com/datasets/2e915ef3df48422283e5b2c7d89dfcba_0.csv")
    bia$CNTY_ST <- gsub("/Quebec & Ontario, Can", "", bia$CNTY_ST)
    bia$CNTY_ST <- gsub("http://www.navajo.o", "", bia$CNTY_ST)
    bia$CNTY_ST <- gsub("/Ottawa", "", bia$CNTY_ST)

    bia_split <- bia[!grepl("/", bia$CNTY_ST),]
    bia_notsplit <- bia[grepl("/", bia$CNTY_ST),]
    for (i in sequence(nrow(bia_notsplit))) {
        split_areas <- strsplit(bia_notsplit$CNTY_ST, '/')[[1]]
        for (j in seq_along(split_areas)) {
           bia_split <- rbind(bia_split, bia_notsplit[i,])
           bia_split$CNTY_ST[nrow(bia_split)] <- split_areas[j]
           bia_split$STATE[nrow(bia_split)] <- substr(split_areas[j], nchar(split_areas[j])-1, nchar(split_areas[j]))
        }
    }
    bia <- bia_split

	bia$County <- stringr::str_squish(stringr::str_trim(stringr::str_sub(bia$CNTY_ST, 1, nchar(bia$CNTY_ST)-3),side="right"))
    for(i in seq_along(bia$County)) {
        county_end <- substr(bia$County[i], nchar(bia$County[i]), nchar(bia$County[i]))
        if(county_end==",") {
            bia$County[i] <- substr(bia$County[i], 1, nchar(bia$County[i])-1)
        }
    }
    bia_done <- bia[!grepl( ',|;|&', bia$County),]
    bia_edit_more <- bia[grepl( ',|;|&', bia$County),]
    SplitCounty <- function(x) {
        x <- gsub('; ', ',', x)
        x <- gsub(', ', ',', x)
        x <- gsub(' & ', ',', x)
        x <- strsplit(x, ',')[[1]]
        return(x)
    }
    fixed <- c()
    for(i in seq_along(bia_edit_more$County)) {
        if(!grepl('/', bia_edit_more$County[i])) {
            counties <- SplitCounty(bia_edit_more$County[i])
            for(county_index in seq_along(counties)) {
                bia_done <- rbind(bia_done, bia_edit_more[i,])
                bia_done$County[nrow(bia_done)] <- counties[county_index]
            }
            fixed <- append(fixed, i)
        }
    }
    bia_done <- bia_done[!grepl("unorganized borough", bia_done$County),]

    #bia_edit_more <- bia_edit_more[-fixed,]

    bia_all <- data.frame(name=bia_done$IND_NAME, County=bia_done$County, State=bia_done$STATE, Type="Populated Place")
    return(bia_all)
}

MergeBIAWithGeoNames <- function(bia_sites, geonames_sites) {
    both_sites <- plyr::rbind.fill(geonames_sites, bia_sites)
    return(both_sites)
}

MergeAllSitesWithCovidInfo <- function(all_sites, aggregation_by_county) {
    all_sites$County_State <- paste0(all_sites$County, "_", all_sites$State)
    aggregation_by_county$County_State <- paste0(aggregation_by_county$County, "_", aggregation_by_county$State)
    all_info <- left_join(all_sites, aggregation_by_county, by="County_State", suffix=c("_geonames", "_covid"))
    all_info$County <- all_info$County_geonames
    all_info$State <- all_info$State_geonames

    all_info <- all_info[!is.na(all_info$State),]
    all_info <- all_info[!is.na(all_info$Community_Transmission_Level),]

    return(all_info)
}

FormatSitesAndCovid <- function(sites_and_covid) {
    sites_and_covid$Location <- sites_and_covid$name 
    sites_and_covid <- sites_and_covid[!grepl("Fire Department", sites_and_covid$Location, ignore.case=TRUE),]
    sites_and_covid$Latitude <- sites_and_covid$latitude
    sites_and_covid$Longitude <- sites_and_covid$longitude
    sites_and_covid$Type <- stringr::str_to_title(sites_and_covid$Type)

    sites_and_covid <- sites_and_covid[order(sites_and_covid$Percent_Fully_Vaccinated_Under_65, sites_and_covid$Percent_Fully_Vaccinated, sites_and_covid$Location, method="radix", decreasing=c(TRUE, TRUE, FALSE)),]


    sites_and_covid$Percent_Fully_Vaccinated <- round(sites_and_covid$Percent_Fully_Vaccinated,0)
    sites_and_covid$Percent_Fully_Vaccinated_Under_65 <- round(sites_and_covid$Percent_Fully_Vaccinated_Under_65,0)
    sites_and_covid <- sites_and_covid%>%select(Location, Type, County, State, Community_Transmission_Level, Percent_Fully_Vaccinated_Under_65, Percent_Estimated_Vaccination_Hesitant, Percent_State_ICU_Beds_Filled, Percent_County_ICU_Beds_Filled, Percent_Fully_Vaccinated, Percent_Estimated_Vaccination_Strongly_Hesitant, Population, Vaccination_Data_Is_For_County, Latitude, Longitude, Area_of_Concern_Category, FIPS)
    return(sites_and_covid)
}

FormatForDT <- function(site_info_formatted, cities=FALSE, do_cities_filter=TRUE, focal_county=NULL, focal_state=NULL) {
    site_info_formatted <- site_info_formatted[which(nchar(site_info_formatted$Location)>2),]
    site_info_formatted$Percent_Fully_Vaccinated[which(site_info_formatted$Percent_Fully_Vaccinated>99)] <- 99
    site_info_formatted$Percent_Fully_Vaccinated_Under_65[which(site_info_formatted$Percent_Fully_Vaccinated_Under_65>99)] <- 99

    #site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"), "% (", site_info_formatted$Percent_Fully_Vaccinated_Graded_On_Curve, ")")
    site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"))
    #site_info_formatted$`Fully Vaccinated (all)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated,2,pad="0"), "%")
   #site_info_formatted$`Fully Vaccinated (<65)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated_Under_65,2,pad="0"), "% (", site_info_formatted$Percent_Fully_Vaccinated_Under_65_Graded_On_Curve, ")")
    site_info_formatted$`Fully Vaccinated (<65)` <- paste0(str_pad(site_info_formatted$Percent_Fully_Vaccinated_Under_65,2,pad="0"))
    site_info_formatted$`Community Transmission` <- site_info_formatted$Community_Transmission_Level
    site_info_formatted$`Vaccine Hesitant` <- paste0(str_pad(site_info_formatted$Percent_Estimated_Vaccination_Hesitant,2,pad="0"))
    site_info_formatted$`State ICU Beds Filled` <- paste0(str_pad(site_info_formatted$Percent_State_ICU_Beds_Filled,2,pad="0"))
    site_info_formatted$`County ICU Beds Filled` <- paste0(str_pad(site_info_formatted$Percent_County_ICU_Beds_Filled,2,pad="0"))
    site_info_formatted$`Area of Concern` <- site_info_formatted$Area_of_Concern_Category
    site_info_formatted$`County, State` <- paste0(site_info_formatted$County, ", ", site_info_formatted$State)

    site_info_formatted$TransmissionRank <- 0
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="High")] <- 4
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Substantial")] <- 3
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Moderate")] <- 2
    site_info_formatted$TransmissionRank[which(site_info_formatted$`Community Transmission`=="Low")] <- 1
    site_info_formatted <- site_info_formatted[order(site_info_formatted$TransmissionRank, site_info_formatted$Percent_Fully_Vaccinated_Under_65, site_info_formatted$Percent_Fully_Vaccinated, method="radix", decreasing=c(FALSE, TRUE, TRUE)),]
	site_info_summary <- site_info_formatted
    if(do_cities_filter) {
        if(cities) {
            site_info_summary <- site_info_summary[which(site_info_summary$Type=="Populated Place"),]%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", "County, State", "Vaccine Hesitant", "State ICU Beds Filled")
        } else {
            site_info_summary <- site_info_summary[which(site_info_summary$Type!="Populated Place"),]%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", Type, "County, State", "Vaccine Hesitant", "State ICU Beds Filled")
        }
    } else {
        site_info_summary <- subset(site_info_summary, State==focal_state & County==focal_county)%>%select(Location, "Community Transmission", "Fully Vaccinated (all)", "Fully Vaccinated (<65)", Type, "County, State", "Vaccine Hesitant", "State ICU Beds Filled", "County ICU Beds Filled", "Area of Concern", "Latitude", "Longitude", "FIPS")
    }
    return(site_info_summary)
}

PredictFutureVaccinationForACounty <- function(FIPS_code, cdc_all_reports) {
    time_data <- subset(cdc_all_reports, FIPS==FIPS_code)
    time_data$ReportDate <- as.Date(time_data$ReportDate, "%Y%m%d")
    time_data$`Under 65` <- 100*time_data$Proportion_Fully_Vaccinated_Under_65
    time_data$`All Ages` <- 100*time_data$`People who are fully vaccinated as % of total population`
	time_data <- time_data[!duplicated(time_data$ReportDate),]
    ts <- as_tsibble(time_data, index=ReportDate) %>% select(ReportDate, `All Ages`, `Under 65`) %>% tsibble::fill_gaps() %>% imputeTS::na_interpolation()
    county_forecast_all_ages <- suppressWarnings(ts  %>% model(
    ets = ETS(`All Ages`)
    )  %>%  forecast( h = "6 months") ) 
    county_forecast_under_65 <- suppressWarnings(ts  %>% model(
    ets = ETS(`Under 65`)
    )  %>%  forecast( h = "6 months") )
    #county_forecast[county_forecast$ReportDate=="2021-07-01",]
    final_forecast <- data.frame(ReportDate = c(time_data$ReportDate, county_forecast_all_ages$ReportDate, time_data$ReportDate, county_forecast_under_65$ReportDate), PercentFullyVaccinated = c(time_data$`All Ages`, county_forecast_all_ages$.mean, time_data$`Under 65`, county_forecast_under_65$.mean), Type=c(rep("All Ages (observed)", nrow(time_data)), rep("All Ages (potential)", nrow(county_forecast_all_ages)), rep("Under 65 (observed)", nrow(time_data)), rep("Under 65 (potential)", nrow(county_forecast_under_65))))
    final_forecast$FIPS <- FIPS_code
	final_forecast$PercentFullyVaccinated[final_forecast$PercentFullyVaccinated>100] <- 100
    print(paste0(FIPS_code, ": ", max(final_forecast$PercentFullyVaccinated, na.rm=TRUE)))
    return(final_forecast)
}

TimeSeriesVaccinationForACounty <- function(FIPS_code, cdc_all_reports) {
    time_data <- subset(cdc_all_reports, FIPS==FIPS_code)
	time_data <- time_data[order(time_data$ReportDate),]
    time_data$ReportDate <- as.Date(time_data$ReportDate, "%Y%m%d")
	time_data <- time_data[!duplicated(time_data$ReportDate),]
	#pivot_wider, manually
	time_data_formatted <- data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Fully_Vaccinated_65_plus, Type="FullyVaccinated", Group="65+")
	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Fully_Vaccinated_18_plus, Type="FullyVaccinated", Group="18+"))
	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Fully_Vaccinated_12_to_17, Type="FullyVaccinated", Group="12-17"))
	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Fully_Vaccinated_All, Type="FullyVaccinated", Group="All"))

	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Initiating_Vaccination_Last_7_days_65_plus, Type="InitiatingVaccination", Group="65+"))
	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Initiating_Vaccination_Last_7_days_18_plus, Type="InitiatingVaccination", Group="18+"))
	time_data_formatted <- rbind(time_data_formatted, data.frame(Date=time_data$ReportDate, Percentage=100*time_data$Proportion_Initiating_Vaccination_Last_7_days_12_to_17, Type="InitiatingVaccination", Group="12-17"))

	time_data_formatted$FIPS <- FIPS_code
    return(time_data_formatted)
}

PredictFutureVaccinationForAllCounties <- function(cdc_all_reports) {
    FIPS_codes <- unique(cdc_all_reports$FIPS)
    all_results <- do.call("rbind", lapply(FIPS_codes, PredictFutureVaccinationForACounty, cdc_all_reports=cdc_all_reports))
    return(all_results)
}

GetAllTimeSeries <- function(cdc_all_reports) {
    FIPS_codes <- unique(cdc_all_reports$FIPS)
    all_results <- do.call("rbind", lapply(FIPS_codes, TimeSeriesVaccinationForACounty, cdc_all_reports=cdc_all_reports))
    return(all_results)
}


RenderCountyPages <- function(site_info_formatted, nationwide_info, aggregation_by_county, date_community, all_predictions, all_time_series) {
    masking_cdf <- Hmisc::wtd.Ecdf(aggregation_by_county$Percent_Fully_Vaccinated, weights=aggregation_by_county$Population, normwt=TRUE)
    masking_approxfun <- suppressWarnings(approxfun(x=masking_cdf$x, y=masking_cdf$ecdf)) # There are some duplicated values which are being collapsed
    site_info_formatted <- left_join(site_info_formatted, nationwide_info, by='Community_Transmission_Level', copy=TRUE)
    site_info_formatted$Percent_Of_People_In_Areas_With_Higher_Vaccination <- 100*(1-masking_approxfun(site_info_formatted$Percent_Fully_Vaccinated))
	county_state <- data.frame(County=site_info_formatted$County, State=site_info_formatted$State, County_State = gsub(" ", "_", paste0(site_info_formatted$County, "_", site_info_formatted$State)), Percent_Of_People_In_Areas_With_Higher_Vaccination=site_info_formatted$Percent_Of_People_In_Areas_With_Higher_Vaccination, PercentPopulationAtLowerTransmission=site_info_formatted$PercentPopulationAtLowerTransmission, FIPS=site_info_formatted$FIPS, County_State_FIPS=paste(site_info_formatted$County, site_info_formatted$State, site_info_formatted$FIPS), Percent_Fully_Vaccinated=site_info_formatted$Percent_Fully_Vaccinated)
	county_state_unique = county_state[!duplicated(county_state$County_State),]

	for (i in sequence(nrow(county_state_unique))) {
	#for (i in sequence(4)) {
		rmarkdown::render(input="county.Rmd", output_file=paste0("counties/", county_state_unique$County_State[i], ".html"), 
        params = list(
            county_table = FormatForDT(
                site_info_formatted, do_cities_filter=FALSE, focal_county=county_state_unique$County[i], focal_state=county_state_unique$State[i]
            ),
            county_name=county_state_unique$County[i], 
            state_name=county_state_unique$State[i], percent_population_at_lower_transmission=county_state_unique$PercentPopulationAtLowerTransmission[i], 
            percent_population_at_higher_vaccination=county_state_unique$Percent_Of_People_In_Areas_With_Higher_Vaccination[i],
            county_state_unique=county_state_unique,
            focal_fips=county_state_unique$FIPS[i],
            aggregation_by_county=aggregation_by_county,
            update_date=format(Sys.time(), "%a, %b %d, %Y"),
            data_date=format(as.Date(date_community, "%Y%m%d"), "%a, %b %d, %Y"),
			county_full_history=subset(all_predictions, FIPS==county_state_unique$FIPS[i]),
			state_level_only=!site_info_formatted$Vaccination_Data_Is_For_County[which(site_info_formatted$FIPS==county_state_unique$FIPS[i])][1],
			time_series=subset(all_time_series, FIPS==county_state_unique$FIPS[i])
        ))
	}
}

ComputeNationwideNumbers <- function(aggregation_by_county) {
    PeoplePerCommunityTransmissionLevel <- aggregation_by_county %>% group_by(Community_Transmission_Level) %>% dplyr::summarize(Population=sum(Population))
    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels <- 0
    
    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]

    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Substantial")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]

    PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="High")] <- PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Substantial")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Moderate")] + PeoplePerCommunityTransmissionLevel$Population[which(PeoplePerCommunityTransmissionLevel$Community_Transmission_Level=="Low")]
  
    PeoplePerCommunityTransmissionLevel$PercentPopulationPopulationAtThisTransmission <- 100*PeoplePerCommunityTransmissionLevel$Population/sum(PeoplePerCommunityTransmissionLevel$Population)

    PeoplePerCommunityTransmissionLevel$PercentPopulationAtLowerTransmission <- 100*PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels/sum(PeoplePerCommunityTransmissionLevel$PopulationInLowerLevels)

    return(PeoplePerCommunityTransmissionLevel %>% select(Community_Transmission_Level, PercentPopulationAtLowerTransmission, PercentPopulationPopulationAtThisTransmission))
}