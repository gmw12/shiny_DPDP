get_pamlims_data <- function(order_id) {
  cat(file = stderr(), "Function get_pamlims_data...", "\n")
  library(httr2)
  library(askpass)
  library(rvest)
  library(stringr)
  
  #------------------------------------------------------------
  # Credential Storage Functions
  #------------------------------------------------------------
  
  # Path where credentials will be stored (in your home directory)
  get_cred_path <- function() {
    file.path(Sys.getenv("HOME"), ".pamlims_credentials.rds")
  }
  
  # Save credentials to file
  save_credentials <- function(email, password) {
    cred_path <- get_cred_path()
    creds <- list(email = email, password = password)
    saveRDS(creds, cred_path)
    message("Credentials saved to: ", cred_path)
  }
  
  # Load credentials from file
  load_credentials <- function() {
    #cred_path <- get_cred_path()
    cred_path <- Sys.getenv("PAMLIMS_LOGIN_PATH")
    if (file.exists(cred_path)) {
      return(readRDS(cred_path))
    } else {
      return(NULL)
    }
  }
  
  # Delete stored credentials
  delete_credentials <- function() {
    cred_path <- get_cred_path()
    if (file.exists(cred_path)) {
      file.remove(cred_path)
      message("Credentials deleted.")
    } else {
      message("No credentials found to delete.")
    }
  }
  
  #------------------------------------------------------------
  # 1. Login to PAM LIMS (POST /log_in_for_visitor)
  #------------------------------------------------------------
  login_pamlims <- function(use_stored = TRUE, save_new = TRUE) {
    
    # Try to load stored credentials first
    if (use_stored) {
      creds <- load_credentials()
      if (!is.null(creds)) {
        email <- creds$email
        password <- creds$password
        message("Using stored credentials for: ", email)
      } else {
        message("No stored credentials found.")
        creds <- NULL
      }
    } else {
      creds <- NULL
    }
    
    # If no stored credentials, prompt for them
    if (is.null(creds)) {
      email    <- readline("PAM LIMS email: ")
      password <- askpass("PAM LIMS password: ")
      
      # Optionally save the new credentials
      if (save_new) {
        save_credentials(email, password)
      }
    }
    
    cookie_path <- tempfile(fileext = ".cookies")
    
    resp <- request("https://staff.pamlims.net") |>
      req_cookie_preserve(cookie_path) |>
      req_url_path("log_in_for_visitor") |>
      req_method("POST") |>
      req_body_json(list(
        email_address = email,
        password      = password
      )) |>
      req_perform()
    
    if (resp_status(resp) >= 400) {
      stop("Login failed: ", resp_status_desc(resp))
    }
    
    message("Login successful.")
    cookie_path
  }
  
  #------------------------------------------------------------
  # 2. Fetch an order page using the login cookies
  #------------------------------------------------------------
  get_order_html <- function(cookie_path, order_id) {
    request("https://staff.pamlims.net/order") |>
      req_cookie_preserve(cookie_path) |>
      req_url_query(id = order_id) |>
      req_perform() |>
      resp_body_html()
  }
  
  #------------------------------------------------------------
  # 3. Extract fields from embedded JSON-ish blob
  #------------------------------------------------------------
  
  # Numeric field like ...projectNumber:11168...
  get_project_number <- function(html) {
    raw <- as.character(html)
    m <- str_match(raw, "projectNumber[^:]*:(\\d+)")
    if (nrow(m) == 0 || is.na(m[1, 2])) {
      warning("Could not find projectNumber in page source.")
      return(NA_integer_)
    }
    as.integer(m[1, 2])
  }
  
  # Customer name from something like \"customerName\":\"String(Erik Soderblom)\"
  get_customer_name <- function(html) {
    raw <- as.character(html)
    
    # Look for: customerName ... String(Erik Soderblom)
    m <- str_match(
      raw,
      regex("customerName.*?String\\(([^)]*)\\)", dotall = TRUE)
    )
    
    if (nrow(m) == 0 || is.na(m[1, 2])) {
      warning("customerName not found in page source.")
      return(NA_character_)
    }
    
    m[1, 2]   # the bit inside String(...)
  }
  
  get_all_names <- function(html) {
    raw <- as.character(html)
    
    matches <- stringr::str_match_all(
      raw,
      stringr::regex("name.*?String\\(([^)]*)\\)", dotall = TRUE)
    )[[1]]
    
    if (nrow(matches) == 0) {
      warning("No name fields found.")
      return(character(0))
    }
    
    matches[, 2]
  }
  
  get_pi_name <- function(html) {
    names <- get_all_names(html)
    
    # PI appears as the 3rd "name" in your example
    if (length(names) >= 3) {
      return(names[3])
    } else {
      return(NA_character_)
    }
  }
  
  get_project_name <- function(html) {
    raw <- as.character(html)
    
    # Look for: label ... String(Neuroma vs. Proximal Stump)
    m <- str_match(
      raw,
      regex("label.*?String\\(([^)]*)\\)", dotall = TRUE)
    )
    
    if (nrow(m) == 0 || is.na(m[1, 2])) {
      warning("project label not found in page source.")
      return(NA_character_)
    }
    
    m[1, 2]   # e.g. "Neuroma vs. Proximal Stump"
  }
  
  #------------------------------------------------------------
  # 4. Example usage
  #------------------------------------------------------------
  
  # First time: will prompt for credentials and save them
  # Subsequent times: will use stored credentials automatically
  cookie_path <- login_pamlims()
  
  html_output  <- get_order_html(cookie_path, order_id)
  
  project_num   <- get_project_number(html_output)
  customer_name <- get_customer_name(html_output)
  pi_name       <- get_pi_name(html_output)
  project_name  <- get_project_name(html_output)
  
  customer_lastname <- tail(str_split(customer_name, " ")[[1]], 1)
  pi_lastname <- tail(str_split(pi_name, " ")[[1]], 1)
  
  cat(file = stderr(), "Function get_pamlims_data...end", "\n")
  
  return(list(project_number = project_num, customer_name = customer_name, pi_name = pi_name, 
              project_name =project_name, customer_lastname = customer_lastname, pi_lastname = pi_lastname))

}