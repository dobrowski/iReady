
# Uses Exports from iReady to create analysis


library(tidyverse)
library(here)
library(MCOE)
library(janitor)
library(readxl)
library(vroom)


options(scipen=999)


vroom(files
      )


files <- fs::dir_ls(here("data","soledad"))

print(files)

output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = "snake"), id = "subject")) %>% 
    mutate(subject = case_when( str_detect(subject, "math") ~ "math",
                                      str_detect(subject, "ela") ~ "ela"
                                      ))

sum.tab <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(overall_relative_placement)) %>%
    rename(relative_placement = overall_relative_placement) %>%
    mutate(domain = "overall")



sum.tab.phono <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(phonological_awareness_relative_placement))%>%
    rename(relative_placement = phonological_awareness_relative_placement) %>%
    mutate(domain = "phonological awareness")



sum.tab.phonics <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
        #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(phonics_relative_placement))%>%
    rename(relative_placement = phonics_relative_placement) %>%
    mutate(domain = "phonics")



sum.tab.high <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(high_frequency_words_relative_placement))%>%
    rename(relative_placement = high_frequency_words_relative_placement) %>%
    mutate(domain = "high frequency words")


sum.tab.vocab <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(vocabulary_relative_placement))%>%
    rename(relative_placement = vocabulary_relative_placement) %>%
    mutate(domain = "vocabulary")


sum.tab.compreh.overall <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(comprehension_overall_relative_placement))%>%
    rename(relative_placement = comprehension_overall_relative_placement) %>%
    mutate(domain = "comprehension overall")



sum.tab.compreh.lit <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(comprehension_literature_relative_placement))%>%
    rename(relative_placement = comprehension_literature_relative_placement) %>%
    mutate(domain = "comprehension literature")



sum.tab.compreh.info <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "ela") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(comprehension_informational_text_relative_placement))%>%
    rename(relative_placement = comprehension_informational_text_relative_placement) %>%
    mutate(domain = "comprehension informational text")




sum.tab.num <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "math") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(number_and_operations_relative_placement))%>%
    rename(relative_placement = number_and_operations_relative_placement) %>%
    mutate(domain = "number and operations")





sum.tab.alg <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "math") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(algebra_and_algebraic_thinking_relative_placement))%>%
    rename(relative_placement = algebra_and_algebraic_thinking_relative_placement) %>%
    mutate(domain = "algebra and algebraic thinking")



sum.tab.meas <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "math") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(measurement_and_data_relative_placement))%>%
    rename(relative_placement = measurement_and_data_relative_placement) %>%
    mutate(domain = "measurement and data")



sum.tab.geom <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           #   student_grade <= 2,
           subject == "math") %>%
    group_by(subject, school, student_grade) %>%
    reframe(tabyl(geometry_relative_placement)) %>%
    rename(relative_placement = geometry_relative_placement) %>%
    mutate(domain = "geometry")




compiled <- bind_rows(sum.tab,)


data_frames <- ls(pattern = "^sum\\.tab")

# Use map_dfr to combine these data frames
combined_data <- data_frames %>%
    map_dfr(~ get(.x) ) %>%
    mutate(student_grade = factor(student_grade),
           relative_placement = factor(relative_placement, levels = c("3 or More Grade Levels Below",
                                                                      "2 Grade Levels Below",         
                                                                      "1 Grade Level Below",        
                                                                      "Early On Grade Level",
                                                                      "Mid or Above Grade Level"    
                                                                      )
                                       ),
           school = str_to_title(school) %>% str_remove_all("School|Middle|Elementary|Elem|Primary") %>% str_trim()
           )



combined_data %>%
    filter(school == "Frank Ledesma",
           domain == "high frequency words") %>%
    ggplot(aes(student_grade, y = percent, fill = relative_placement ,group = fct_rev(relative_placement), label = scales::percent(percent, accuracy = .1) ) ) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5)) +
    mcoe_theme +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
    labs(title = paste0("Percentage of students by grade at relative grade level for " , "high frequency words"," "),
         subtitle = paste0("Frank Ledesma"),
         x = "Grade"
         )

ggsave(here("output", paste0("FRANK LEDESMA", " - ", "high frequency words", ".png" )), width = 8, height = 6)



combined_data %>%
    filter(school == "Jack Franscioni",
           domain == "geometry") %>%
    ggplot(aes(student_grade, y = percent, fill = relative_placement ,group = fct_rev(relative_placement), label = scales::percent(percent, accuracy = .1) ) ) +
    geom_col() +
    geom_text(position = position_stack(vjust = 0.5)) +
    mcoe_theme +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
    labs(title = paste0("Percentage of students by grade at relative grade level for " , "geometry"," "),
         subtitle = paste0("Jack Franscioni"),
         x = "Grade"
    )

ggsave(here("output", paste0("Jack Franscioni", " - ", "geometry", ".png" )), width = 8, height = 6)




for (i in unique(combined_data$school) ) {
    
    for (j in  unique(combined_data$domain) ) {
        
        combined_data %>%
            filter(school == i,
                   domain == j) %>%
            ggplot(aes(student_grade, y = percent, fill = relative_placement ,group = fct_rev(relative_placement), label = scales::percent(percent, accuracy = .1) ) ) +
            geom_col() +
            geom_text(position = position_stack(vjust = 0.5)) +
            mcoe_theme +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
            labs(title = paste0("Percentage of students at relative grade level for " , j," "),
                 subtitle = paste0(i),
                 caption = "Source: iReady Fall Diagnostics",
                 x = "Grade"
            )
        
        ggsave(here("output", paste0(i, " - ", j, ".png" )), width = 8, height = 6)
    
    }
    
}



### Demos 

el.sum.tab <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           english_language_learner == "Y"
           ) %>%
     group_by(subject, school, student_grade) %>%
     reframe(tabyl(overall_relative_placement))  %>%
     rename(relative_placement = overall_relative_placement) %>%
     mutate(domain = "overall") %>%
    mutate(student_grade = factor(student_grade),
           relative_placement = factor(relative_placement, levels = c("3 or More Grade Levels Below",
                                                                      "2 Grade Levels Below",         
                                                                      "1 Grade Level Below",        
                                                                      "Early On Grade Level",
                                                                      "Mid or Above Grade Level"    
           )
           ),
           school = str_to_title(school) %>% str_remove_all("School|Middle|Elementary|Elem|Primary") %>% str_trim()
    )



swd.sum.tab <- output %>%
    filter(most_recent_diagnostic_ytd_y_n == "Y",
           special_education == "Y"
    ) %>%
    group_by(subject,  student_grade) %>%
    reframe(tabyl(overall_relative_placement))  %>%
    rename(relative_placement = overall_relative_placement) %>%
    mutate(domain = "overall") %>%
    mutate(student_grade = factor(student_grade),
           relative_placement = factor(relative_placement, levels = c("3 or More Grade Levels Below",
                                                                      "2 Grade Levels Below",         
                                                                      "1 Grade Level Below",        
                                                                      "Early On Grade Level",
                                                                      "Mid or Above Grade Level"    
           )
           ),
      #     school = str_to_title(school) %>% str_remove_all("School|Middle|Elementary|Elem|Primary") %>% str_trim()
    )




for (i in unique(el.sum.tab$school) ) {
    
  #  for (j in  unique(combined_data$domain) ) {
        
    el.sum.tab %>%
            filter(school == i,
                  # domain == j
                   ) %>%
            ggplot(aes(student_grade, y = percent, fill = relative_placement ,group = fct_rev(relative_placement), label = scales::percent(percent, accuracy = .1) ) ) +
            geom_col() +
            geom_text(position = position_stack(vjust = 0.5)) +
            mcoe_theme +
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
            labs(title = paste0("Percentage of EL students at relative grade level for " ,"overall"," "),
                 subtitle = paste0(i),
                 caption = "Source: iReady Fall Diagnostics",
                 x = "Grade"
            )
        
        ggsave(here("output", paste0(i, " ELs - ", "overall", ".png" )), width = 8, height = 6)
        
 #   }
    
}


#for (i in unique(swd.sum.tab$school) ) {
    
    #  for (j in  unique(combined_data$domain) ) {
    
    swd.sum.tab %>%
        filter(subject == "ela",
               # domain == j
        ) %>%
        ggplot(aes(student_grade, y = percent, fill = relative_placement ,group = fct_rev(relative_placement), label = scales::percent(percent, accuracy = .1) ) ) +
        geom_col() +
        geom_text(position = position_stack(vjust = 0.5)) +
        mcoe_theme +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        guides(fill = guide_legend(nrow = 2, byrow = FALSE)) +
        labs(title = paste0("Percentage of SWD students at relative grade level for " ,"overall"," "),
             subtitle = paste0(i),
             caption = "Source: iReady Fall Diagnostics",
             x = "Grade"
        )
    
    ggsave(here("output", paste0(i, " SWDs - ", "overall", ".png" )), width = 8, height = 6)
    
    #   }
    
#}
