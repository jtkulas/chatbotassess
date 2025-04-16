
library(shiny)
library(httr)
library(jsonlite)

# GPT-4 interaction function
gpt4_chat <- function(user_message, history) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  url <- "https://api.openai.com/v1/chat/completions"
  
  # Append user message to conversation history
  messages <- c(
    history,
    list(list(role = "user", content = user_message))
  )
  
  # API request body
  body <- list(
    model = "gpt-4",  # Use "gpt-3.5-turbo" if you don't have GPT-4 access
    messages = messages,
    temperature = 0.7
  )
  
  # Send request to OpenAI
  res <- POST(
    url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = body,
    encode = "json"
  )
  
  stop_for_status(res)
  
  # Extract assistant reply
  res_content <- content(res, as = "parsed")
  reply <- res_content$choices[[1]]$message$content
  
  # Append assistant reply to history
  messages <- c(messages, list(list(role = "assistant", content = reply)))
  
  list(reply = reply, updated_history = messages)
}

# UI
ui <- fluidPage(
  titlePanel("🧠 GPT-4 Chatbot (R + Shiny)"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Your message:", ""),
      actionButton("send_btn", "Send"),
      br(), br(),
      helpText("This chatbot is powered by OpenAI's GPT-4 via their API.")
    ),
    mainPanel(
      verbatimTextOutput("chat_output")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize conversation history with system message
  conversation_history <- reactiveVal(
    list(list(role = "system", content = "You are a helpful assistant."))
  )
  
  # Initialize chat log
  chat_log <- reactiveVal("")
  
  observeEvent(input$send_btn, {
    user_msg <- input$user_input
    current_history <- conversation_history()
    
    # Call GPT-4 and update conversation
    result <- gpt4_chat(user_msg, current_history)
    bot_reply <- result$reply
    new_history <- result$updated_history
    
    # Append to chat log
    new_chat <- paste0(
      chat_log(),
      "You: ", user_msg, "\n",
      "Bot: ", bot_reply, "\n\n"
    )
    
    chat_log(new_chat)
    conversation_history(new_history)
    
    # Clear input
    updateTextInput(session, "user_input", value = "")
  })
  
  # Output chat log
  output$chat_output <- renderText({
    chat_log()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
