#' Load and Validate Configuration Parameters From Yaml
#'
#' @param path Path to config file (default: tempdir()/config.yaml)
#' @returns List of validated parameters from the yaml
#' @export
load_config <- function(path = file.path(tempdir(), "config.yaml")) {
# Check config file exists
checkmate::assert_file_exists(path, access = "r", .var.name = "config_file")

config <- yaml::read_yaml(path)

 # validate the yaml top structure
checkmate::assert_list(config, names = "named", .var.name = "config")
checkmate::assert_subset(
  names(config),
  c("data", "disease_name", "severity"),
  .var.name = "config_sections"
  )

  # Initialize(empty  variables to store)
params <- list(
  data = NULL,
  disease_name = NA_character_,
  severity = list(
    total_cases = NULL,
    total_deaths = NULL,
    death_in_confirmed = NA_real_,
    account_for_delay = TRUE,
    interval = NA_character_,
    epidist = NULL,
    epidist_params = list(
      type = NA_character_,
      distribution = NA_character_,
      parameters = list(
        meanlog = NA_real_,
        sdlog = NA_real_,
        shape = NA_real_,
        scale = NA_real_
      )
    )
  )
)

# populate and validate params --------

# Load data if path provided
 if (!is.null(config[["data"]]) && !is.na(config[["data"]])) {
   checkmate::assert_string(config[["data"]], na.ok = FALSE, .var.name = "data_path")
   checkmate::assert_file_exists(config[["data"]], access = "r", .var.name = "data_file")
  tryCatch({
    params[["data"]] <-  rio::import(config[["data"]])
  }, error = function(e) {
    stop("Failed to load data file: ", e$message)
  })

 }

# Disease name if provided
if (!is.null(config[["disease_name"]])) {
  checkmate::assert_string(config[["disease_name"]], na.ok = TRUE, .var.name = "disease_name")
  params[["disease_name"]] <- config[["disease_name"]]
}

# Validate severity structure
if (!is.null(config[["severity"]])) {
  checkmate::assert_list(config[["severity"]], names = "named", .var.name = "severity")
  checkmate::assert_subset(
    names(config[["severity"]]),
    c("total_cases", "total_deaths", "death_in_confirmed",
      "account_for_delay", "interval", "epidist", "epidist_params"),
    .var.name = "severity_sections"
  )

  # Populate severity params -----------
  severity_fields <- names(config[["severity"]])

  # Numeric params in severity
  if ("total_cases" %in% severity_fields) {
    checkmate::assert_number(config[["severity"]][["total_cases"]], na.ok = TRUE, .var.name = "total_cases")
    params[["severity"]][["total_cases"]] <- config[["severity"]][["total_cases"]]
  }
  if ("total_deaths" %in% severity_fields) {
    checkmate::assert_number(config[["severity"]][["total_deaths"]], na.ok = TRUE, .var.name = "total_deaths")
    params[["severity"]][["total_deaths"]] <- config[["severity"]][["total_deaths"]]
  }
  if ("death_in_confirmed" %in% severity_fields) {
    checkmate::assert_number( config[["severity"]][["death_in_confirmed"]], lower = 0, upper = 1, na.ok = TRUE, .var.name = "death_in_confirmed"
    )
    params[["severity"]][["death_in_confirmed"]] <- config[["severity"]][["death_in_confirmed"]]
  }

  # Logical param in severity
  if ("account_for_delay" %in% severity_fields) {
    checkmate::assert_logical(config[["severity"]][["account_for_delay"]], any.missing = FALSE, .var.name = "account_for_delay")
    params[["severity"]][["account_for_delay"]] <- config[["severity"]][["account_for_delay"]]
  }

  # Character params in severity
  if ("interval" %in% severity_fields) {
    checkmate::assert_string(config[["severity"]][["interval"]], na.ok = TRUE, .var.name = "interval")
    params[["severity"]][["interval"]] <- config[["severity"]][["interval"]]
  }

  # Epidist params (structural validation) in severity
  if ("epidist_params" %in% severity_fields) {
    checkmate::assert_list(config[["severity"]][["epidist_params"]], names = "named", .var.name = "epidist_params")
    checkmate::assert_subset(
      names(config[["severity"]][["epidist_params"]]), c("type", "distribution", "parameters"),
      .var.name = "epidist_params_sections"
    )
  if("type" %in% names(config[["severity"]][["epidist_params"]])){
      checkmate::assert_string(config[["severity"]][["epidist_params"]][["type"]], na.ok = TRUE, .var.name = "epidist.type")
      params[["severity"]][["epidist_params"]][["type"]] <- config[["severity"]][["epidist_params"]][["type"]]
    }
  if ("distribution" %in% names(config[["severity"]][["epidist_params"]])) {
      checkmate::assert_string(config[["severity"]][["epidist_params"]][["distribution"]],
      na.ok = TRUE, .var.name = "epidist.distribution")
      params[["severity"]][["epidist_params"]][["distribution"]] <- config[["severity"]][["epidist_params"]][["distribution"]]
    }

    # validate epidist_params structure
  if ("parameters" %in% names(config[["severity"]][["epidist_params"]])) {
      params_list <- config[["severity"]][["epidist_params"]][["parameters"]]
      checkmate::assert_list(params_list, names = "named", .var.name = "epidist.parameters")
      checkmate::assert_subset(
        names(params_list),
        c("meanlog", "sdlog", "shape", "scale"),
        .var.name = "epidist.parameter_names"
      )

      # Validate its prams
  if ("meanlog" %in% names(params_list)) {
        checkmate::assert_number(params_list[["meanlog"]], na.ok = TRUE, .var.name = "meanlog")
    params[["severity"]][["epidist_params"]][["parameters"]][["meanlog"]] <- params_list[["meanlog"]]
      }
  if ("sdlog" %in% names(params_list)) {
        checkmate::assert_number(params_list[["sdlog"]], lower = 0, na.ok = TRUE, .var.name = "sdlog")
    params[["severity"]][["epidist_params"]][["parameters"]][["sdlog"]] <- params_list[["sdlog"]]
      }
   if ("shape" %in% names(params_list)) {
        checkmate::assert_number(params_list[["shape"]], lower = 0, na.ok = TRUE, .var.name = "shape")
     params[["severity"]][["epidist_params"]][["parameters"]][["shape"]] <- params_list[["shape"]]
      }
   if ("scale" %in% names(params_list)) {
        checkmate::assert_number(params_list[["scale"]], lower = 0, na.ok = TRUE, .var.name = "scale")
     params[["severity"]][["epidist_params"]][["parameters"]][["scale"]] <- params_list[["scale"]]
      }

        }
      }
    }
    return(params)

    }



