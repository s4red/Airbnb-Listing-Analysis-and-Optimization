#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


server <- function(input, output, session) {
  
  # Assuming airbnb_data is available globally, if not, you can load it inside server
  
  # Text analysis output
  output$text_plot <- renderPlotly({
    # Assuming you've created a word frequency analysis
    # Replace 'top_non_stop_words' with the result of your word frequency analysis
    plot_ly(top_non_stop_words, x = ~reorder(Var1, Freq), y = ~Freq, type = 'bar', 
            marker = list(color = ~Freq, colorscale = 'Blues')) %>%
      layout(xaxis = list(title = "Keyword"), 
             yaxis = list(title = "Frequency"),
             title = "Top 20 Non-Stop Words in Airbnb Listing Data")
  })
  
  # Amenities analysis output
  output$amenities_plot <- renderPlot({
    # Plot the results with ggplot and ggiraph
    gg <- ggplot(top_amenities_df, aes(x = reorder(amenity, frequency), y = frequency)) +
      geom_bar(stat = "identity", fill = viridis(10)) +  # Using viridis color palette
      xlab("Amenity") +
      ylab("Frequency") +
      coord_flip() + # Flip the coordinates to make it easier to read long amenity names
      theme_minimal()
    ggiraph(ggplot_object = gg)
  })
  
  # Sentiment analysis output
  output$sentiment_plot <- renderPlot({
    # Assuming 'airbnb_data_sentiment' is a data frame with sentiment analysis
    # Replace 'airbnb_data_sentiment' with your sentiment analysis result
    ggplot(airbnb_data_sentiment, aes(x = review_scores_rating, y = sentiment_score)) +
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Review Score Rating") +
      ylab("Sentiment Score") +
      theme_minimal()
  })
  
  # Correlation between price and sentiment score
  output$price_plot <- renderPlotly({
    # Replace 'price_sentiment_correlation' with the actual data you want to plot
    plot_ly(airbnb_data, x = ~price_numeric, y = ~sentiment_score, type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2))) %>%
      layout(title = "Price vs Sentiment Score")
  })
  
  # Data table output
  output$data_table <- renderDataTable({
    airbnb_data
  })
  

}
