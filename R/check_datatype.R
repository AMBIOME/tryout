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


check_GreySeal <- function(data, level = "error") {

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


check_HarbourPorpoise <- function(data, level = "error") {

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


check_HarbourSeal <- function(data, level = "error") {

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

