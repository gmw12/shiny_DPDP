library(askpass)

# Manual credential file creator
# Run this once to create your credentials file

# Get the path where credentials will be stored
cred_path <- file.path(Sys.getenv("HOME"), ".pamlims_credentials.rds")

cat("Creating PAM LIMS credentials file\n")
cat("==================================\n\n")

# Method 1: Enter directly in the script (least secure but easiest)
# Uncomment and fill in these lines:
# email <- "your.email@example.com"
# password <- "your_password"

# Method 2: Enter them when prompted
email <- "greg.waitt@duke.edu"  # Replace this with your actual email
password <- "kebayit9"         # Replace this with your actual password

# Save credentials
creds <- list(email = email, password = password)
saveRDS(creds, cred_path)

cat("\n✓ Credentials saved to:", cred_path, "\n")
cat("\nYou can now run your main script and it will use these credentials.\n")

# Show what was saved (for verification)
cat("\nSaved credentials:\n")
cat("  Email:", email, "\n")
cat("  Password: [", nchar(password), "characters ]\n")
