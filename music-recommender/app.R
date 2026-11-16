library(shiny)

recommender <- new.env()
for (file in c("model.R", "catalog.R", "llm.R", "recommendations.R")) {
  source(file, local = recommender)
}

shinyApp(
  fluidPage(title = "MusicBrainz recommendations", recommender$recommendations_ui("recommendations")),
  function(input, output, session) recommender$recommendations_server("recommendations", "model")
)
