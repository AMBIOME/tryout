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
    required <- c()
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
    required <- c()
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
    required <- c()
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
    required <- c()
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
    required <- c()
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
    required <- c()
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
    required <- c()
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
    required <- c()
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


check_Phytoplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","expedition_id","shark_sample_id_md5","sample_date","sample_time","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","wind_direction_code","wind_speed_ms","air_temperature_degc","air_pressure_hpa","weather_observation_code","cloud_observation_code","wave_observation_code","wave_exposure_fetch","ice_observation_code","secchi_depth_m","secchi_depth_quality_flag","visit_comment","sample_series","sample_id","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","plankton_sampling_method_code","sample_comment","scientific_name","species_flag_code","dyntaxa_id","parameter","value","unit","quality_flag","calc_by_dc","trophic_type_code","size_class","size_min_um","size_max_um","size_class_ref_list","reported_cell_volume_um3","sample_part_id","taxonomist","analysis_method_code","counter_program","method_documentation","method_reference_code","method_comment","variable_comment","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","preservation_method_code","mesh_size_um","sedimentation_volume_ml","sedimentation_time_h","coefficient","magnification","station_viss_eu_id","water_land_station_type_code","monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code","reported_scientific_name","reported_parameter","reported_value","reported_unit","reporting_institute_name_sv","data_holding_centre","internet_access","dataset_name","dataset_file_name")
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


check_Picoplankton <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","expedition_id","shark_sample_id_md5","sample_date","sample_time","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","wind_direction_code","wind_speed_ms","air_temperature_degc","air_pressure_hpa","weather_observation_code","cloud_observation_code","wave_observation_code","wave_exposure_fetch","ice_observation_code","secchi_depth_m","secchi_depth_quality_flag","visit_comment","sample_series","sample_id","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sampled_volume_l","plankton_sampling_method_code","flowmeter_length_m","sample_comment","scientific_name","species_flag_code","dyntaxa_id","parameter","value","unit","quality_flag","calc_by_dc","trophic_type_code","size_class","size_min_um","size_max_um","size_class_ref_list","reported_cell_volume_um3","sample_part_id","taxonomist","analysis_method_code","counter_program","method_documentation","method_reference_code","method_comment","variable_comment","analytical_laboratory_name_sv","analytical_laboratory_accreditated","analysis_date","preservation_method_code","mesh_size_um","sedimentation_volume_ml","sedimentation_time_h","wavelength_excitation","wavelength_emission","coefficient","magnification","station_viss_eu_id","water_land_station_type_code","monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code","reported_scientific_name","reported_parameter","reported_value","reported_unit","reporting_institute_name_sv","data_holding_centre","internet_access","dataset_name","dataset_file_name")
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


check_PrimaryProduction <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","expedition_id","shark_sample_id_md5","sample_date","sample_time","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","wind_direction_code","wind_speed_ms","weather_observation_code","cloud_observation_code","wave_observation_code","ice_observation_code","visit_comment","sample_series","sample_id","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sample_comment","DPM_added","DPM_sample","DPM_darkness","parameter","value","unit","quality_flag","calc_by_dc","analysis_method_code","method_documentation","method_reference_code","variable_comment","analytical_laboratory_name_sv","analytical_laboratory_accreditated","incubation_start_time","incubation_end_time","incubation_time_h","insolation_air","incubation_radiation","station_viss_eu_id","water_land_station_type_code","monitoring_station_type_code","monitoring_purpose_code","monitoring_program_code","reporting_institute_name_sv","data_holding_centre","internet_access","dataset_name","dataset_file_name")
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


check_RingedSeal <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","visit_id","visit_date","shark_sample_id_md5","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","water_depth_m","visit_comment","sample_id","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sample_comment","scientific_name","species_flag_code","dyntaxa_id","parameter","value","unit","quality_flag","calc_by_dc","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","variable_comment","analytical_laboratory_name_sv","station_viss_eu_id","reported_scientific_name","reported_parameter","reported_value","reported_unit","dataset_name","dataset_file_name")
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


check_SealPathology <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","visit_id","visit_date","shark_sample_id_md5","sample_date","sample_time","sample_enddate","sample_endtime","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","water_depth_m","visit_comment","sample_id","sample_min_depth_m","sample_max_depth_m","sampling_laboratory_name_sv","sample_comment","scientific_name","species_flag_code","dyntaxa_id","parameter","value","unit","quality_flag","calc_by_dc","sex_code","dev_stage_code","trophic_type_code","size_class","method_documentation","method_reference_code","variable_comment","analytical_laboratory_name_sv","station_viss_eu_id","reported_scientific_name","reported_parameter","reported_value","reported_unit","dataset_name","dataset_file_name")
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


check_Sedimentation <- function(data, level = "error") {

    errors <- data_frame()
    required <- c("delivery_datatype","check_status_sv","data_checked_by_sv","visit_year","station_name","reported_station_name","sample_project_name_sv","sample_orderer_name_sv","platform_code","visit_id","expedition_id","shark_sample_id_md5","sample_date","sample_time","sample_enddate","sample_latitude_dm","sample_longitude_dm","sample_latitude_dd","sample_longitude_dd","positioning_system_code","water_depth_m","wind_direction_code","wind_speed_ms","weather_observation_code","cloud_observation_code","wave_observation_code","ice_observation_code","visit_comment","sample_id","sample_min_depth_m","sample_max_depth_m","sample_depth_quality_flag","sampling_laboratory_name_sv","sampling_laboratory_accreditated","sampler_type_code","sample_comment","parameter","value","unit","quality_flag","calc_by_dc","method_documentation","method_reference_code","image_id","factors_influencing_code","aggregated_subsamples","variable_comment","analytical_laboratory_name_sv","analytical_laboratory_accreditated","preservation_method_code","mesh_size_um","method_incubation","incubation_start_time","incubation_end_time","incubation_time_h","insolation_air","salinity_correction","station_viss_eu_id","water_land_station_type_code","monitoring_station_type_code","monitoring_program_code","reporting_institute_name_sv","data_holding_centre","internet_access","dataset_name","dataset_file_name")
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

