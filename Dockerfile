# Use the same base image as the Wings Gallery repo
FROM rocker/shiny-verse:latest

# Create app directory
RUN mkdir -p /srv/shiny-server/

# Copy app sources
COPY Ikiam_DB_app.R /srv/shiny-server/
COPY download_data.R /srv/shiny-server/

# Install R packages not included in rocker/shiny-verse
# Note: shiny, dplyr, tidyr, stringr, readr are already in the base image
RUN R -e "install.packages(c( \
    'shinyjs', \
    'shinyWidgets', \
    'DT', \
    'rhandsontable', \
    'rlang', \
    'googlesheets4', \
    'cellranger', \
    'jsonlite', \
    'digest' \
  ), repos = 'https://cloud.r-project.org')"

# Download data during build (same pattern as Wings Gallery)
RUN R -e "source('/srv/shiny-server/download_data.R')"

# Ensure shiny user owns the app dir
RUN chown -R shiny:shiny /srv/shiny-server

# App runs on 8080 inside the container (we'll map host:8081 -> container:8080)
EXPOSE 8080

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/Ikiam_DB_app.R', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8080)))"]
