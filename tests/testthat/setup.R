# Setup script for CAWET tests

# Create a temporary working directory for tests
wd <- tempfile("cawet-")
suppressWarnings(setup_CAWET(wd))

# Ensure the temporary directory is removed after tests
withr::defer(unlink(wd, recursive = TRUE), teardown_env())
