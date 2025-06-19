#' Create and Open a Configuration YAML File Template
#'
#' This function generates a configuration file in a YAML format with a
#' structured template for epidemiological severity parameters. If the file does
#' not exist at the specified path, it creates one with placeholder values. The
#' file is then opened in the system's default application for YAML/text files.
#'
#' @param path A character with the file path where the configuration file
#'    should be created/opened. When not provided, default to a file named
#'    `config.yaml` in the system's temporary directory `tempdir()`.
#'
#' @returns Invisibly returns the `path` to the created or existing
#'    configuration file.
#'
#' @examples
#' # Create/open config in temp directory
#' create_config()
#'
#' # Create/open in working directory
#' create_config("my_config.yaml")
#'
#' @note
#' - Users need to replace `NA` with appropriate values before using the
#' configuration.
#' - The function does not validate YAML content.
#'
#' @export
create_config <- function(path = file.path(tempdir(), "config.yaml")) {

  # create the config file template
  if (!file.exists(path)) {
    config_template <- list(
      data = NA_character_,
      disease_name = NA_character_,
      severity = list(
        total_cases = NA_real_,
        total_deaths = NA_real_,
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
        ))
      )
    )
    yaml::write_yaml(config_template, path)


    comments <- c(
      "data" = "This is the path to your input data file (CSV,Excel,etc.)",
      "disease_name" = "Name of disease (e.g., 'COVID-19')",
      "total_cases" = "total cases to scale against in your population",
      "total_deaths" = "total deaths due to the disease",
      "death_in_confirmed" = "deaths amongst lab-confirmed cases",
      "account_for_delay" = "Whether to account for delay from onset to death(TRUE or FALSE.)",
      "interval" = "Time interval for estimates.(e.g., 'day','week')",
      "epidist" = "Epidemiological delay distribution. NULL or custom",
      "type" = "Distribution to use.(e.g., 'gamma' or 'lognormal')",
      "distribution" = "Synonymous with 'type'",
      "meanlog" = "For lognormal. Mean of log.(e.g., 1.5)",
      "sdlog" = "For lognormal. SD of log.(e.g., 0.5)",
      "shape" = "For gamma. Shape.(e.g., 2)",
      "scale" = "For gamma. Scale. (e.g., 5)"

    )
    lines <- readLines(path)
    for (i in seq_along(lines)){
      key <- sub("^([ ]*)([a-zA-Z0-9_\\-]+):.*$", "\\2", lines[i])
      if (key %in% names(comments)){
        if (!grepl("#", lines[i], fixed = TRUE)){
          lines[i] <- paste0(lines[i], "  # ", comments[[key]])

        }

      }


    }

    writeLines(lines, path)


  }

  # open the file in your editor
  if (interactive()) file.edit(path)
}



