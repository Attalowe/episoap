#' Load and Validate Configuration Parameters From Yaml
#'
#' @param path Path to config file (default: tempdir()/config.yaml)
#' @returns List of validated parameters from the yaml
#' @export
load_config <- function(path = file.path(tempdir(), "config.yaml")) {
  # Check if config file exists
  checkmate::assert_file_exists(path, access = "r", .var.name = "config_file")

  # read in the config
  config <- yaml::read_yaml(path)

  # validate the yaml top structure
  checkmate::assert_list(config, names = "named", .var.name = "config")
  checkmate::assert_subset(
    names(config),
    c("data", "disease_name", "severity"),
    .var.name = "config_sections"
  )

  # Initialize the default parameters (empty  variables to store)
  params <- list(
    data = NA_character_,
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

  # modify the default values with those in the config file and validate params
  params <- utils::modifyList(params, config, keep.null = TRUE)

  # check the data argument
  if (!is.na(params[["data"]])) {
    checkmate::assert_string(config[["data"]], na.ok = FALSE,
                             .var.name = "data_path")
    checkmate::assert_file_exists(config[["data"]], access = "r",
                                  .var.name = "data_file")
  }

  # check disease name if provided
  if (!is.na(params[["disease_name"]])) {
    checkmate::assert_string(params[["disease_name"]], na.ok = TRUE,
                             .var.name = "disease_name")
  }

  # Validate severity structure
  if ("severity" %in% names(params)) {
    checkmate::assert_list(params[["severity"]], names = "named",
                           .var.name = "severity")
    checkmate::assert_subset(
      names(params[["severity"]]),
      c("total_cases", "total_deaths", "death_in_confirmed",
        "account_for_delay", "interval", "epidist", "epidist_params"),
      .var.name = "severity_sections"
    )

    # Populate severity params -----------
    severity_fields <- names(params[["severity"]])

    # check if total_cases is provided as numeric
    if ("total_cases" %in% severity_fields) {
      checkmate::assert_number(params[["severity"]][["total_cases"]],
                               na.ok = TRUE, .var.name = "total_cases")
    }

    # check if total_deaths is provided as numeric
    if ("total_deaths" %in% severity_fields) {
      checkmate::assert_number(params[["severity"]][["total_deaths"]],
                               na.ok = TRUE, .var.name = "total_deaths")
    }

    # check if death_in_confirmed is provided as numeric
    if ("death_in_confirmed" %in% severity_fields) {
      checkmate::assert_number(params[["severity"]][["death_in_confirmed"]],
                               lower = 0, upper = 1, na.ok = TRUE,
                               .var.name = "death_in_confirmed")
    }

    # check if account_for_delay is provided as logical
    if ("account_for_delay" %in% severity_fields) {
      checkmate::assert_logical(params[["severity"]][["account_for_delay"]],
                                any.missing = FALSE,
                                .var.name = "account_for_delay")
    }

    # check if account_for_delay is provided as character
    if ("interval" %in% severity_fields) {
      checkmate::assert_string(params[["severity"]][["interval"]],
                               na.ok = TRUE, .var.name = "interval")
    }

    # check if epidist_params is provided as list
    if ("epidist_params" %in% severity_fields) {
      checkmate::assert_list(params[["severity"]][["epidist_params"]],
                             names = "named", .var.name = "epidist_params")
      checkmate::assert_subset(
        names(params[["severity"]][["epidist_params"]]),
        c("type", "distribution", "parameters"),
        .var.name = "epidist_params_sections"
      )

      if ("type" %in% names(params[["severity"]][["epidist_params"]])) {
        checkmate::assert_string(
          params[["severity"]][["epidist_params"]][["type"]],
          na.ok = TRUE, .var.name = "epidist.type"
        )
      }

      if ("distribution" %in% names(params[["severity"]][["epidist_params"]])) {
        checkmate::assert_string(
          params[["severity"]][["epidist_params"]][["distribution"]],
          na.ok = TRUE, .var.name = "epidist.distribution"
        )
      }

      # validate epidist_params structure
      if ("parameters" %in% names(params[["severity"]][["epidist_params"]])) {
        params_list <- params[["severity"]][["epidist_params"]][["parameters"]]
        checkmate::assert_list(params_list, names = "named",
                               .var.name = "epidist.parameters")
        checkmate::assert_subset(
          names(params_list),
          c("meanlog", "sdlog", "shape", "scale"),
          .var.name = "epidist.parameter_names"
        )

        # Validate its prams
        if ("meanlog" %in% names(params_list)) {
            checkmate::assert_number(params_list[["meanlog"]], na.ok = TRUE,
                                     .var.name = "meanlog")
        }
        if ("sdlog" %in% names(params_list)) {
          checkmate::assert_number(params_list[["sdlog"]], lower = 0,
                                   na.ok = TRUE, .var.name = "sdlog")
        }
        if ("shape" %in% names(params_list)) {
          checkmate::assert_number(params_list[["shape"]], lower = 0,
                                   na.ok = TRUE, .var.name = "shape")
        }
        if ("scale" %in% names(params_list)) {
          checkmate::assert_number(params_list[["scale"]], lower = 0,
                                   na.ok = TRUE, .var.name = "scale")
        }
      }
    }
  }
  return(params)
}



