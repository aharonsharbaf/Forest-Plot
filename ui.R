library(shiny)
ui<-navbarPage("Forest Plot",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    uiOutput("choose_sheet"),
                                                    uiOutput("format_one"),
                                                    uiOutput("column_names"),
                                                    uiOutput("remove_row")
                                                    
                        ),
                        mainPanel(uiOutput("tb1"))
                        )),
               tabPanel("Create graph",
                        sidebarLayout(sidebarPanel(
                          
                          h5(helpText("")),
                          uiOutput("box_size_num"),
                          
                          h5(),
                          uiOutput("xtick_name"),
                          uiOutput("graph_pos"),
                          uiOutput("title_name"),
                          uiOutput("xlab_name"),
                          uiOutput("limit_val"),
                          uiOutput("is_summary"),
                          uiOutput("gray_row"),
                          uiOutput("gray_height"),
                          uiOutput("x_log"),
                          uiOutput("graph_width_val"),
                          uiOutput("file_type"),
                          uiOutput("height_size"),
                          uiOutput("width_size"),
                          uiOutput("res_size"),
                          uiOutput("name_file"),
                          uiOutput("save_file")
                        ),

                        
                          
                          mainPanel( helpText("forest plot"),
                                     uiOutput("size_plot_ui"))))
)