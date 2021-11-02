library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

source("R/functions.R")
source("_packages.R")


# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")


# End this file with a list of target objects.
list(
  tar_target(hesitancy_by_county, GetHesitancyByCounty()),
  tar_target(cdc_weekly_metadata, GetCommunityTransmissionReportLastDate()),
  tar_target(cdc_weekly, GetCommunityTransmissionReport(report_url=cdc_weekly_metadata$report_url)),
  tar_target(cdc_all_metadata, GetAllCommunityTransmissionReportDates()),
  tar_target(cdc_all_reports, GetAllCommunityTransmissionReports(cdc_all_metadata$report_urls, cdc_all_metadata$report_dates)),
  tar_target(aggregation_by_county, JoinHesitancyWithCDCWeekly(hesitancy_by_county, cdc_weekly)),
  tar_target(aggregation_table_csv, WriteCSVTable(aggregation_by_county), format="file"),
  tar_target(geonames_sites, GetGeonames()),
  #tar_target(bia_sites, GetBIA_data()),
  #tar_target(all_sites, MergeBIAWithGeoNames(geonames_sites, bia_sites)),
  tar_target(all_sites, geonames_sites), #until dataset is available again
  tar_target(sites_and_covid, MergeAllSitesWithCovidInfo(all_sites, aggregation_by_county)),
  tar_target(site_info_formatted, FormatSitesAndCovid(sites_and_covid)),
  tar_target(site_info_formatted_csv, WriteCSVTable(site_info_formatted, file_out="data/site_info.csv"), format="file"),
  tar_target(nationwide_info, ComputeNationwideNumbers(site_info_formatted)),
  tar_target(all_predictions, PredictFutureVaccinationForAllCounties(cdc_all_reports)),
  tar_target(all_time_series, GetAllTimeSeries(cdc_all_reports)),
  tar_render(report, "index.Rmd", params = list(attractions_table = FormatForDT(site_info_formatted, cities=FALSE), cities_table = FormatForDT(site_info_formatted, cities=TRUE), date_community=cdc_weekly_metadata$report_date, all_predictions=all_predictions, all_time_series=all_time_series)),
  tar_render(animations, "animations.Rmd", params = list(all_predictions=all_predictions)),
  tar_target(all_time_series_saved, write.csv(all_time_series, file=gzfile("data/all_time_series.csv.gz"))),
  tar_target(site_info_formatted_saved, write.csv(site_info_formatted, file=gzfile("data/site_info_formatted.csv.gz"))),
  tar_target(nationwide_info_saved, write.csv(nationwide_info, file=gzfile("data/nationwide_info.csv.gz"))),
  tar_target(aggregation_by_county_saved, write.csv(aggregation_by_county, file=gzfile("data/aggregation_by_county.csv.gz"))),
  tar_target(cdc_all_reports_saved, write.csv(cdc_all_reports, file=gzfile("data/cdc_all_reports.csv.gz")))
)
