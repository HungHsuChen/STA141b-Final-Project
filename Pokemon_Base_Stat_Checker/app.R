library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(plotly)

#To get the exist generations
url_generation = str_glue("https://pokeapi.co/api/v2/{endpoint}/",
                          endpoint = "generation")
generations = fromJSON(url_generation)

#get the exist types
url_type = str_glue("https://pokeapi.co/api/v2/{endpoint}/",
                    endpoint = "type")
types = fromJSON(url_type)

ui <- fluidPage(

    navbarPage("Pokemon Base Stat Checker",

               #Allow user to sellect the generation, types, and pokemon
               tabPanel("Stats Checker",fluidPage(theme = shinytheme("united")),
                        sidebarPanel(width = 4,
                                     selectInput("gen", "Select a Generation", c("-", generations$results$name)),
                                     selectInput("type1", "Select a Type", c("-", types$results$name)),
                                     selectInput("type2", "Select a Second Type (if needed)", "-"),
                                     selectInput("pok", "Choose a Pokemon", "-"),
                                     tags$style(".selectize-input {font-size: 20px;}"),
                                     tags$style(".selectize-dropdown {font-size: 20px;}")
                        ),
                        mainPanel(
                            textOutput("Info"),
                            tags$head(tags$style("#Info{color: orange;font-size: 40px;}")),
                            textOutput("Info2"),
                            tags$head(tags$style("#Info2{color: orange;font-size: 40px;}")),
                            column(1, uiOutput("picFront")),
                            column(1, uiOutput("picBack")),
                            column(1, uiOutput("picFrontfemale")),
                            column(1, uiOutput("picBackfemale")),
                            column(1, uiOutput("picFrontShiny")),
                            column(1, uiOutput("picBackShiny")),
                            column(1, uiOutput("picFrontfemaleShiny")),
                            column(1, uiOutput("picBackfemaleShiny")),
                            column(8, tableOutput("table")),
                            tags$style("#table{font-size: 20px;}"),
                            column(8, plotlyOutput("polar", width = 600, height = 600))
                        )
               ),

               tabPanel("README",
                        p("This is a base stat checker for pokemons since the generation-7.
             I use the data from", a("pokeapi.co", href="https://pokeapi.co/docs/v2", target="_blank"),
                          ". The site contains most of the infomration for pokemon; however, there
               are some pokemons' data from gen-3 to gen-7 are missing, so the app
               aren't have all the pokemons' available since gen-3 to gen-7. (If you selected
                a pokemon that do not has data, the app you display only errors.)", style = "font-size:25px")
               )
    )
)


server <- function(input, output, session) {

    table = reactive({
        out = NULL
        if (input$pok != "-"){
            out = fromJSON(str_glue("https://pokeapi.co/api/v2/pokemon/{name}/",
                                    name = input$pok))
        }
        out
    })

    #Stat Checker
    #To update the second type base on the selected type1
    observeEvent(input$type1, {
        req(input$type1 != "-")
        new_type = types$results$name
        new_type = new_type[new_type != input$type1]
        updateSelectInput(session, "type2", choices = c("-", new_type))
    })

    #Let user select pokemon with selected genration
    observeEvent(input$gen, {

        req(input$gen != "-")

        url_pokemon = str_glue("https://pokeapi.co/api/v2/generation/{name}/",
                               name = input$gen)
        pok = fromJSON(url_pokemon)$pokemon_species$name

        if(input$type1 == "-"){
            updateSelectInput(session, "pok", choices = c("-", pok))
        } else {

            url_pokemontype1 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                        name = input$type1)
            pok_type1 = fromJSON(url_pokemontype1)$pokemon$pokemon$name
            pok = intersect(pok, pok_type1)

            if(input$type2 == "-"){
                updateSelectInput(session, "pok", choices = c("-", pok))
            } else {

                url_pokemontype2 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                            name = input$type2)
                pok_type2 = fromJSON(url_pokemontype2)$pokemon$pokemon$name
                pok = intersect(pok, pok_type2)
                updateSelectInput(session, "pok", choices = c("-", pok))
            }
        }
    })

    #Let user select pokemon with selected type1
    observeEvent(input$type1, {

        req(input$type1 != "-")

        url_pokemontype1 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                    name = input$type1)
        pok = fromJSON(url_pokemontype1)$pokemon$pokemon$name

        if(input$gen == "-"){
            updateSelectInput(session, "pok", choices = c("-", pok))
        } else {

            url_pokemon = str_glue("https://pokeapi.co/api/v2/generation/{name}/",
                                   name = input$gen)
            pok_gen = fromJSON(url_pokemon)$pokemon_species$name

            pok = intersect(pok, pok_gen)
            updateSelectInput(session, "pok", choices = c("-", pok))
        }
    })

    #Let user select pokemon with selected type2
    observeEvent(input$type2, {
        req(input$type2 != "-")

        url_pokemontype1 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                    name = input$type1)
        pok = fromJSON(url_pokemontype1)$pokemon$pokemon$name

        url_pokemontype2 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                    name = input$type2)
        pok_type2 = fromJSON(url_pokemontype2)$pokemon$pokemon$name
        pok = intersect(pok, pok_type2)

        if(input$gen == "-"){
            updateSelectInput(session, "pok", choices = c("-", pok))
        } else {
            url_pokemon = str_glue("https://pokeapi.co/api/v2/generation/{name}/",
                                   name = input$gen)
            pok_gen = fromJSON(url_pokemon)$pokemon_species$name

            pok = intersect(pok, pok_gen)
            updateSelectInput(session, "pok", choices = c("-", pok))
        }

    })

    #Update pok when user unselected type2
    observeEvent(input$type2, {
        req(input$type1 != "-")
        req(input$type2 == "-")

        url_pokemontype1 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                    name = input$type1)
        pok = fromJSON(url_pokemontype1)$pokemon$pokemon$name

        if(input$gen == "-"){
            updateSelectInput(session, "pok", choices = c("-", pok))
        } else {
            url_pokemon = str_glue("https://pokeapi.co/api/v2/generation/{name}/",
                                   name = input$gen)
            pok_gen = fromJSON(url_pokemon)$pokemon_species$name

            pok = intersect(pok, pok_gen)
            updateSelectInput(session, "pok", choices = c("-", pok))
        }
    })

    #Update pok when user unselected type1
    observeEvent(input$type1, {
        req(input$gen != "-")
        req(input$type1 == "-")

        updateSelectInput(session, "type2", choices = "-")

        url_pokemon = str_glue("https://pokeapi.co/api/v2/generation/{name}/",
                               name = input$gen)
        pok = fromJSON(url_pokemon)$pokemon_species$name
        updateSelectInput(session, "pok", choices = c("-", pok))
    })
    observeEvent(input$type1, {
        req(input$type2 != "-")
        req(input$gen == "-")

        updateSelectInput(session, "type2", choices = "-")
    })

    #Update pok when user unselected gen
    observeEvent(input$gen, {
        req(input$type1 != "-")
        req(input$gen == "-")

        url_pokemontype1 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                    name = input$type1)
        pok = fromJSON(url_pokemontype1)$pokemon$pokemon$name

        if(input$type2 == "-"){
            updateSelectInput(session, "pok", choices = c("-", pok))
        } else{
            url_pokemontype2 = str_glue("https://pokeapi.co/api/v2/type/{name}/",
                                        name = input$type2)
            pok_type2 = fromJSON(url_pokemontype2)$pokemon$pokemon$name
            pok = intersect(pok, pok_type2)
            updateSelectInput(session, "pok", choices = c("-", pok))
        }
    })

    #plot the result
    output$polar = renderPlotly({
        req(input$pok != "-")

        plot_ly(
            type = 'scatterpolar',
            mode = "closest",
            fill = "toself"
        ) %>%
            add_trace(
                r = table()$stats$base_stat,
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = "markers",
                name = "stat"
            ) %>%
            add_trace(
                r = c(64.21, 72.91, 68.23, 67.14, 66.09, 69.07),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = "markers",
                visible= "legendonly",
                name = "generation-i"
            ) %>%
            add_trace(
                r = c(70.98, 68.26, 69.69, 64.5, 72.34, 61.41),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-ii"
            ) %>%
            add_trace(
                r = c(65.78, 72.54, 69.15, 67.25, 66.59, 60.96),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-iii"
            ) %>%
            add_trace(
                r = c(72.23, 80.04, 74.44, 72.71, 73.5, 69.31),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-iv"
            ) %>%
            add_trace(
                r = c(69.5, 79.9, 71.11, 67.58, 66.68, 64.93),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-v"
            ) %>%
            add_trace(
                r = c(69.32, 73.03, 73.5, 73.35, 73.81, 65.15),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-vi"
            ) %>%
            add_trace(
                r = c(71.06, 85.3, 78.9, 75.58, 75.3, 64.54),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "generation-vii"
            ) %>%
            add_trace(
                r = c(69.01, 76, 72.15, 69.73, 70.62, 65.05),
                theta = c("hp", "attack", "defense",
                          "Sp.attack", "Sp.defense", "speed"),
                showlegend = TRUE,
                mode = 'markers',
                visible= "legendonly",
                name = "Avg All"
            ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,255)
                    )
                ),
                showlegend = TRUE
            )
    })

    #print the pokemon's image
    output$picFront <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$front_default
        tags$img(src = link)
    })
    output$picBack <- renderUI({
        link = table()$sprites$back_default
        tags$img(src = link)
    })
    output$picBackfemale <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$back_female
        tags$img(src = link)
    })
    output$picFrontfemale <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$front_female
        tags$img(src = link)
    })
    output$picFrontShiny <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$front_shiny
        tags$img(src = link)
    })
    output$picBackShiny <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$back_shiny
        tags$img(src = link)
    })
    output$picBackfemaleShiny <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$back_shiny_female
        tags$img(src = link)
    })
    output$picFrontfemaleShiny <- renderUI({
        req(input$pok != "-")
        link = table()$sprites$front_shiny_female
        tags$img(src = link)
    })

    #print the stat as a table
    output$table = renderTable({
        req(input$pok != "-")
        table = table()$stats
        tibble(name = table$stat$name, stat = table$base_stat) %>%
            pivot_wider(names_from = name, values_from = stat)
    })

    #give the general information about the pokemon choosen
    output$Info = renderText({
        req(input$pok != "-")
        name = table()$name
        height = table()$height
        weight = table()$weight
        paste(name, "------height:", height, " weight:", weight, sep = "")
    })
    output$Info2 = renderText({
        req(input$pok != "-")
        type = table()$type$type$name
        if(length(type) == 1){
            paste("type:", type, sep = " ")
        } else {
            paste("type:", type[1], type[2], sep = " ")
        }
    })
}

shinyApp(ui = ui, server = server)
