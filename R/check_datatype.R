#' Check if the required and recommended datatype-specific SHARK system fields (different between different datatypes) are present.
#'
#' Missing or empty required fields are reported as errors,
#' missing or empty recommended fields are reported as warnings.
#'
#' @param data The data frame.
#' @param level The level of error reporting, i.e. "error" or "warning". Recommended fields are only checked in case of "warning".
#' @return Any warnings or errors.
#' @export
#' modified from https://iobis.github.io/obistools
#' Provoost P and Bosch S (2018). “obistools: Tools for data enhancement and quality control.” Ocean Biogeographic Information System. Intergovernmental Oceanographic Commission of UNESCO. https://cran.r-project.org/package=obistools.

check_Bacterioplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_year",
"visit.visit_date",
"visit.reported_station_name",
"visit.platform_code",
"visit.water_land_station_type_code",
"visit.water_depth_m",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"sample.sample_id",
"sample.sample_series",
"sample.sample_time",
"sample.sample_depth_m",
"sample.sample_project_code",
"sample.sample_orderer_code",
"sample.monitoring_program_code",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.method_reference_code",
"sample.sample_comment",
"sample.sampler_type_code",
"sample.sampled_volume_l",
"sample.sample_reported_latitude",
"sample.sample_reported_longitude",
"variable.analysis_method_code",
"variable.analytical_laboratory_code",
"variable.analytical_laboratory_accreditated",
"variable.analysed_by",
"variable.analysis_date",
"variable.analysed_volume_cm3",
"variable.variable_comment",
"variable.counted_portions",
"variable.coefficient",
"variable.sample_part_id",
"variable.preservation_method_code",
"variable.QFLAG.Bacterial concentration",
"variable.QFLAG.Bacterial production",
"variable.QFLAG.Bacterial cell volume",
"variable.COPY_VARIABLE.Bacterial concentration.cells/l",
"variable.COPY_VARIABLE.Bacterial production.cells/l/d",
"variable.COPY_VARIABLE_MULTIPLY.Bacterial concentration.cells/l.1000",
"variable.COPY_VARIABLE.Bacterial cell volume.um3/cell",
"variable.COPY_VARIABLE.Bacterial cell carbon content.fg C/cell"
)
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}

check_chlorophyll <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_id",
"visit.visit_year",
"visit.visit_date",
"sample.sample_date",
"visit.reported_station_name",
"visit.visit_comment",
"visit.expedition_id",
"visit.platform_code",
"visit.water_land_station_type_code",
"visit.monitoring_station_type_code",
"visit.water_depth_m",
"visit.wind_speed_ms",
"visit.wave_observation_code",
"visit.wind_direction_code",
"visit.cloud_observation_code",
"visit.ice_observation_code",
"visit.weather_observation_code",
"visit.air_temperature_degc",
"visit.air_pressure_hpa",
"visit.secchi_depth_m",
"visit.secchi_depth_quality_flag",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"visit.positioning_system_code",
"sample.sample_id",
"sample.sample_time",
"sample.sample_enddate",
"sample.sample_endtime",
"sample.sample_min_depth_m",
"sample.sample_max_depth_m",
"sample.sample_project_code",
"sample.sample_orderer_code",
"sample.monitoring_program_code",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.method_reference_code",
"sample.method_documentation",
"sample.sample_comment",
"sample.sampler_type_code",
"sample.sampled_volume_l",
"variable.analysis_method_code",
"variable.analytical_laboratory_code",
"variable.analytical_laboratory_accreditated",
"variable.analysis_date",
"variable.estimation_uncertainty",
"variable.method_calculation_uncertainty",
"variable.detection_limit",
"variable.quantification_limit",
"variable.analysis_range",
"variable.variable_comment",
"variable.COPY_VARIABLE.Chlorophyll-a.ug/l",
"TEMP.QFLAG.Chlorophyll-a",
"variable.SFLAG.Chlorophyll-a",
"visit.monitoring_purpose_code"
)
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_epibenthos <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("dataset.reporting_institute_code",
"dataset.reported_by",
"visit.visit_id",
"visit.visit_year",
"visit.visit_date",
"visit.reported_station_name",
"visit.visit_comment",
"visit.TEMP.add_to_visit_comment",
"visit.expedition_id",
"visit.station_N2000_code",
"visit.platform_code",
"visit.station_marking",
"visit.station_photo",
"visit.station_cluster",
"visit.water_land_station_type_code",
"visit.sea_area",
"visit.monitoring_purpose_code",
"visit.wind_speed_ms",
"visit.wave_height_m",
"visit.wind_direction_code",
"visit.water_level_deviation_m",
"visit.wave_exposure_fetch",
"visit.station_exposure",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"visit.positioning_system_code",
"sample.sample_time",
"sample.sample_reported_latitude",
"sample.sample_reported_longitude",
"sample.sample_id",
"variable.sample_part_id",
"sample.sample_series",
"sample.sample_depth_m",
"sample.sample_project_code",
"sample.project_purpose",
"sample.sample_orderer_code",
"sample.monitoring_program_code",
"sample.sampling_laboratory_code",
"sample.method_documentation",
"sample.sample_comment",
"sample.sample_cluster",
"sample.bottom_slope_deg",
"sample.sample_substrate_cover_softbottom",
"sample.sample_substrate_cover_gravel",
"sample.sample_substrate_cover_rock",
"sample.sample_substrate_comnt_rock",
"sample.sample_substrate_comnt_gravel",
"sample.sample_substrate_comnt_softbottom",
"TEMP.substrate",
"TEMP.substrate_code",
"TEMP.substrate_cover",
"TEMP.substrate_comment",
"visit.transect_id",
"sample.transect_id",
"sample.transect_start_latitude_dm",
"sample.transect_start_longitude_dm",
"sample.transect_end_latitude_dm",
"sample.transect_end_longitude_dm",
"sample.transect_start_latitude_dd",
"sample.transect_start_longitude_dd",
"sample.transect_end_latitude_dd",
"sample.transect_end_longitude_dd",
"sample.transect_direction_deg",
"sample.transect_min_distance_m",
"sample.transect_max_distance_m",
"sample.transect_min_depth_m",
"sample.transect_max_depth_m",
"sample.transect_start_depth_m",
"sample.transect_stop_depth_m",
"sample.transect_length_m",
"visit.transect_width_m",
"sample.diver_name",
"sample.transect_video",
"sample.transect_video_name",
"sample.transect_video_interpreter",
"sample.transect_protocol_writer",
"sample.transect_interpretation_time",
"sample.transect_cover_soft_bottom",
"sample.transect_cover_hard_bottom",
"sample.transect_comment",
"sample.section_start_latitude_dm",
"sample.section_start_longitude_dm",
"sample.section_start_latitude_dd",
"sample.section_start_longitude_dd",
"sample.section_end_latitude_dd",
"sample.section_end_longitude_dd",
"sample.section_start_depth_m",
"sample.section_end_depth_m",
"sample.section_distance_start_m",
"sample.section_distance_end_m",
"sample.section_start_interpretation",
"sample.section_stop_interpretation",
"sample.section_fauna_flora_found",
"sample.section_debris_cover",
"sample.section_bare_substrate",
"sample.section_transect",
"sample.section_hard_clay_cover_class",
"sample.section_silt_soft_clay_cover_class",
"sample.section_sand_cover_class",
"sample.section_gravel_cover_class",
"sample.section_stone_cover_class",
"sample.section_boulder_cover_class",
"sample.section_rock_cover_class",
"sample.section_shell_gravel_cover_class",
"sample.section_shell_cover_class",
"sample.section_bare_substrate_cover_class",
"sample.section_debris_cover_class",
"sample.section_epi_zostera_cover_class",
"sample.section_unidentified_plantae_cover_class",
"sample.section_nassarius_tracks_cover_class",
"sample.section_paguridae_tracks_cover_class",
"sample.section_animalia_burrows_cover_class",
"sample.section_animalia_tracks_cover_class",
"sample.section_unidentified_algae_cover_class",
"TEMP.sect_substrate",
"TEMP.sect_substrate_code",
"TEMP.sect_substrate_cover",
"TEMP.sect_substrate_comment",
"sample.section_comment",
"sample.sampler_type_code",
"sample.sampler_area_m2",
"sample.sampler_area_cm2",
"sample.video_interpreted",
"sample.sediment_deposition_code",
"sample.fauna_flora_found",
"sample.image_name",
"sample.image_stop_time",
"sample.sample_photo_code",
"sample.sample_reported_latitude",
"sample.sample_reported_longitude",
"sample.method_comment",
"variable.analysis_method_code",
"variable.analytical_laboratory_code",
"variable.analysed_by",
"variable.variable_comment",
"variable.dyntaxa_id",
"variable.size_class",
"variable.species_flag_code",
"variable.stratum_code",
"variable.reported_scientific_name",
"variable.taxonomist",
"variable.size_class_range_min",
"variable.size_class_range_max",
"variable.epibiont",
"variable.degree_biofouling",
"variable.bitemark",
"variable.reproductive_organs",
"variable.recruits",
"variable.detached",
"variable.taxon_photo",
"variable.preservation_method_code",
"visit.monitoring_station_type_code",
"variable.COPY_VARIABLE.# counted.ind",
"variable.COPY_VARIABLE_MULTIPLY.Abundance.ind/m2.4",
"variable.COPY_VARIABLE.Cover class.class",
"sample.CREATE_VARIABLE.Cover class filamentous algae.class",
"variable.COPY_VARIABLE.Cover (%).%",
"sample.CREATE_VARIABLE.Total cover of all species (%).%",
"sample.CREATE_VARIABLE.Temperature.C",
"sample.CREATE_VARIABLE.Turbidity.NTU",
"sample.CREATE_VARIABLE.Salinity.ppt",
"variable.COPY_VARIABLE.Maximum height.m?",
"variable.COPY_VARIABLE.Average height.cm?",
"variable.COPY_VARIABLE.Reproductive organs.class",
"variable.COPY_VARIABLE.Bite marks.class",
"variable.COPY_VARIABLE.Recruitment.class",
"variable.COPY_VARIABLE.Shoot density.ind/m2",
"variable.COPY_VARIABLE.Shoot biomass.g dry weight/m2",
"variable.COPY_VARIABLE.Min shoot length.cm",
"variable.COPY_VARIABLE.Max shoot length.cm",
"variable.COPY_VARIABLE.Average shoot length.cm",
"variable.COPY_VARIABLE.Rhizome biomass.g dry weight/m2",
"variable.COPY_VARIABLE.Rhizome juice sugar content. % by volume",
"visit.secchi_depth_quality_flag",
"visit.secchi_depth_m",
"sample.CREATE_VARIABLE.Temperature.C",
"sample.CREATE_VARIABLE.Salinity.",
"sample.CREATE_VARIABLE.Sediment deposition cover (%).%",
"variable.COPY_VARIABLE.Species distribution min depth.m",
"variable.COPY_VARIABLE.Species distribution max depth.m",
"variable.COPY_VARIABLE.Dry weight.g",
"variable.COPY_VARIABLE.Abundance class.class",
"variable.COPY_VARIABLE.Depth distribution (max depth).m",
"variable.COPY_VARIABLE.Density in covered area.%",
"variable.COPY_VARIABLE.Substrate specific cover.%",
)
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_EpibenthosDropvideo <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_year",
"visit.visit_date",
"visit.reported_station_name",
"visit.station_N2000_code",
"sample.sample_project_code",
"sample.sample_orderer_code",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.method_documentation",
"visit.visit_comment",
"sample.transect_start_latitude_dd",
"sample.transect_start_longitude_dd",
"sample.transect_end_latitude_dd",
"sample.transect_end_longitude_dd",
"sample.transect_start_depth_m",
"sample.transect_stop_depth_m",
"visit.positioning_system_code",
"sample.transect_video_name",
"sample.transect_video_interpreter",
"sample.transect_protocol_writer",
"sample.transect_interpretation_time",
"variable.section_cover_soft_bottom",
"variable.section_cover_hard_bottom",
"variable.section_hard_clay_cover_class",
"variable.section_silt_soft_clay_cover_class",
"variable.section_sand_cover_class",
"variable.section_gravel_cover_class",
"variable.section_stone_cover_class",
"variable.section_stone_cover_20_60_class",
"variable.section_stone_cover_60_200_class",
"variable.section_boulder_cover_class",
"variable.section_rock_cover_class",
"variable.section_shell_gravel_cover_class",
"variable.section_shell_cover_class",
"variable.section_bare_substrate_cover_class",
"variable.section_debris_cover_class",
"variable.section_epi_zostera_cover_class",
"variable.section_unidentified_plantae_cover_class",
"variable.section_nassarius_tracks_cover_class",
"variable.section_paguridae_tracks_cover_class",
"variable.section_animalia_burrows_cover_class",
"variable.section_animalia_tracks_cover_class",
"variable.section_unidentified_algae_cover_class",
"variable.section_comment",
"sample.sampler_type_code",
"variable.image_sequence",
"variable.image_stop_time",
"sample.CREATE_VARIABLE.Sediment depos cover (class).class",
"visit.secchi_depth_m",
"visit.secchi_depth_quality_flag",
"sample.CREATE_VARIABLE.Salinity."
)
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}

check_GreySeal <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_year",
"visit.visit_date",
"visit.reported_station_name",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"visit.county",
"visit.obspoint",
"visit.wind_speed_ms",
"visit.wind_direction_code",
"visit.weather_observation_code",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"sample.sample_id",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.sampled_by",
"sample.observation_distance_m",
"variable.variable_comment",
"variable.reported_scientific_name",
"variable.COPY_VARIABLE.# counted.ind",
"variable.COPY_VARIABLE.Total # counted on land.ind",
"variable.COPY_VARIABLE.Total # counted in water.ind",
"variable.COPY_VARIABLE.# pups counted on land.ind",
"visit.visit_year",
"visit.reported_station_name",
"sample.sample_project_code",
"sample.sample_orderer_code",
"visit.expedition_id",
"visit.visit_date",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"visit.positioning_system_code",
"visit.county",
"visit.water_depth_m",
"visit.monitoring_station_type_code",
"visit.monitoring_purpose_code",
"sample.monitoring_program_code",
"visit.visit_comment",
"visit.wind_direction_code",
"visit.wind_speed_ms",
"visit.air_temperature_degc",
"visit.air_pressure_hpa",
"visit.weather_observation_code",
"visit.cloud_observation_code",
"visit.wave_observation_code",
"visit.ice_observation_code",
"sample.sample_time",
"sample.fauna_flora_found",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.sampler_type_code",
"sample.sampled_by",
"visit.obspoint",
"visit.observation_distance_m",
"sample.image_id",
"sample.sample_id",
"sample.sample_comment",
"variable.reported_scientific_name",
"variable.COPY_VARIABLE.# counted.ind",
"variable.COPY_VARIABLE.# counted.ind",
"variable.COPY_VARIABLE.Total # counted on land.ind",
"variable.COPY_VARIABLE.Total # counted in water.ind",
"variable.COPY_VARIABLE.# pups counted on land.ind",
"variable.analytical_laboratory_code",
"variable.analytical_laboratory_accreditated",
"variable.analysis_date",
"sample.method_documentation",
"sample.method_reference_code",
"variable.variable_comment"
)
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_HarbourPorpoise <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_year",
"visit.reported_station_name",
"sample.sample_project_code",
"sample.sample_orderer_code",
"visit.visit_date",
"visit.visit_enddate",
"sample.sample_time",
"sample.sample_endtime",
"visit.positioning_system_code",
"visit.monitoring_purpose_code",
"sample.monitoring_program_code",
"visit.visit_comment",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.sampler_type_code",
"sample.method_documentation",
"sample.sample_comment",
"variable.reported_scientific_name",
"variable.observation_date",
"variable.observation_time",
"variable.analytical_laboratory_code",
"variable.analytical_laboratory_accreditated",
"sample.unanalysed_original_data",
"variable.variable_comment",
"visit.country_code",
"sample.sample_reported_latitude",
"sample.sample_reported_longitude",
"variable.COPY_VARIABLE.Porpoise positive minute.Y/N")
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_HarbourSeal <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit.visit_year",
"visit.visit_date",
"visit.reported_station_name",
"sample.sample_comment",
"visit.sea_region",
"visit.visit_reported_latitude",
"visit.visit_reported_longitude",
"visit.obspoint",
"sample.sample_time",
"sample.sample_project_code",
"sample.sample_orderer_code",
"sample.sampling_laboratory_code",
"sample.sampling_laboratory_accreditated",
"sample.method_documentation",
"sample.sampler_type_code",
"sample.fauna_flora_found",
"sample.image_id",
"variable.analytical_laboratory_code",
"variable.analytical_laboratory_accreditated",
"variable.variable_comment",
"variable.species_flag_code",
"variable.reported_scientific_name",
"variable.taxonomist",
"variable.COPY_VARIABLE.# counted.ind")
    recommended <- c()

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_PhysicalChemical <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_Phytoplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_Picoplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_PrimaryProduction <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_RingedSeal <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_SealPathology <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_Sedimentation <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_Zoobenthos <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}


check_Zooplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("visit_year", "station_name", "sample_project_name_sv", "sample_orderer_name_sv", "platform_code", "sample_date", "sample_latitude_dd", "sample_longitude_dd", "positioning_system_code", "water_depth_m")
    recommended <- c("monitoring_station_type_code", "monitoring_purpose_code", "monitoring_program_code", "reporting_institute_name_sv", "analytical_laboratory_name_sv")

    # find missing required fields

    fields <- missing_fields(data, required)
    if (length(fields) > 0) {
      errors <- bind_rows(errors, data_frame(
        level = "error",
        field = fields,
        row = NA,
        message = paste0("Required field ", fields, " is missing")
      ))
    }

    # find empty values for required fields

    for (field in required) {
      if (field %in% names(data)) {
        rows <- missing_values(data[,field])
        if (length(which(rows)) > 0) {
          errors <- bind_rows(errors, data_frame(
            level = "error",
            field = field,
            row = which(rows),
            message = paste0("Empty value for required field ", field)
          ))
        }
      }
    }

    # recommended fields

    if (level == "warning") {

      # find missing recommended fields

      fields <- missing_fields(data, recommended)
      if (length(fields) > 0) {
        errors <- bind_rows(errors, data_frame(
          field = fields,
          level = "warning",
          message = paste0("Recommended field ", fields, " is missing")
        ))
      }

      # find empty values for recommended fields

      for (field in recommended) {
        if (field %in% names(data)) {
          rows <- missing_values(data[,field])
          if (length(which(rows)) > 0) {
            errors <- bind_rows(errors, data_frame(
              level = "warning",
              field = field,
              row = which(rows),
              message = paste0("Empty value for recommended field ", field)
            ))
          }
        }
      }

    }

    return(errors)
}

