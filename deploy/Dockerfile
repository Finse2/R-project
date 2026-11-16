# MusicBrainz Explorer - the Shiny app (app.R) in a container.
# The 11 GB database and the recommender model are NOT copied into the image;
# docker-compose.yml mounts them read-only from the server's disk.
# Build context is the project folder:  docker compose -f deploy/docker-compose.yml build

FROM rocker/r-ver:4.5.3

# Current binary packages. duckdb must be at least the version that wrote the
# database (2.0.0 on the PC that built it): newer versions read older files,
# older versions cannot.
RUN echo 'options(repos = c(CRAN = "https://p3m.dev/cran/__linux__/noble/latest"))' \
      >> /usr/local/lib/R/etc/Rprofile.site && \
    install2.r --error --skipinstalled \
      shiny bslib plotly ggplot2 dplyr tidyr scales DBI duckdb httr jsonlite && \
    Rscript -e 'stopifnot(packageVersion("duckdb") >= "2.0.0")' && \
    rm -rf /tmp/downloaded_packages

RUN useradd --create-home --uid 1001 shiny
WORKDIR /app
COPY app.R ./
COPY music-recommender/model.R music-recommender/catalog.R \
     music-recommender/llm.R music-recommender/recommendations.R ./music-recommender/
COPY data/analysis ./data/analysis
COPY output/audit.rds ./output/audit.rds
RUN chown -R shiny:shiny /app
USER shiny

EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838, launch.browser = FALSE)"]
