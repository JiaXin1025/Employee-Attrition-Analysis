#POR JIA XIN
#TP062856


# DATA IMPORT & LIBRARY IMPORT #

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(treemap)

assignment_csv<- fread("C:\\Users\\Asus\\Documents\\APU Year 2\\PFDA\\Assignment\\employee_attrition.csv", header = TRUE)



# DATA CLEANING #
colSums(is.na(assignment_csv))


# DATA PRE_PROCESSING #

assignment_csv$gender_short <- NULL # Remove duplicated gender column

# Replacing illogical Termination dates with NA value 
assignment_csv$terminationdate_key <- ifelse(assignment_csv$terminationdate_key == "1/1/1900", NA, assignment_csv$terminationdate_key)


# Remove duplicated entry while only keeping the latest entry
assignment_csv <- assignment_csv %>%
  arrange(desc(STATUS_YEAR))
assignment_csv <- assignment_csv %>%
  distinct(EmployeeID, .keep_all = TRUE)


# Only keep necessary value before delimiter in a job_title columns
assignment_csv <- assignment_csv %>%
  mutate(job_title = if_else(str_detect(job_title, "Director"), "Director", job_title))

assignment_csv <- assignment_csv %>%
  mutate(job_title = if_else(str_detect(job_title, "Exec Assistant"), "Exec Assistant", job_title))

assignment_csv <- assignment_csv %>%
  mutate(job_title = if_else(str_detect(job_title, "VP"), "VP", job_title))

# DATA EXPLORATION #
head(assignment_csv)
tail(assignment_csv)
nrow(assignment_csv)
ncol(assignment_csv) 
names(assignment_csv)
glimpse(assignment_csv)
summary(assignment_csv)

# QUESTION & ANALYSIS #

# QUESTION 1: Why do employees leave the organisation?


# ANALYSIS 1-1: What is the attrition rate (% of employees who left) for the entire dataset

  # Convert to data.table explicitly
  status_counts <- data.table(assignment_csv)[, .N, by = STATUS]
  
  # Calculate the percentage
  status_counts[, Percentage := N / sum(N) * 100]

  # Create the pie chart
    ggplot(status_counts, aes(x = "", y = Percentage, fill = STATUS)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
    labs(fill = "Status", title = "Ratio of Terminated Employees to Active Employees") +
    scale_fill_discrete(name = "Status") +
    theme_minimal()+
    theme(axis.title = element_blank(),  # Remove axis titles
            axis.text = element_blank(), # Remove axis text
            axis.ticks = element_blank()) # Remove axis ticks
    
# ANALYSIS 1-2: Find the relationship between age with attrition (vs Terminated Employees)
  
    # Filter the data for employees with "TERMINATED" status
    terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
    
    # Define the age ranges
    age_ranges <- cut(terminated_data$age, breaks = c(0, 20, 30, 40, 50, 60, Inf), 
                      labels = c("<20", "20-29", "30-39", "40-49", "50-59", ">=60"))
    
    # Create a new column for the age range
    terminated_data$age_range <- age_ranges
    
    # Create the histogram with bars stuck together
    ggplot(terminated_data, aes(x = age_range, fill = age_range)) +
      geom_histogram(stat = "count", position = "identity", width = 1) +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +  # Add labels
      labs(title = "Age Distribution of Terminated Employees",
           x = "Age Range",
           y = "Count") +
      theme_minimal()
  
# ANALYSIS 1-3: Find the relationship between job title with attrition (vs Terminated Employees)
    
  # Filter the data for employees with "terminate" status
  terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
  
  # Calculate the count of employees per job title
  jobtitle_counts <- terminated_data[, .N, by = job_title]

  # Create the bar graph
    ggplot(jobtitle_counts, aes(x = job_title, y = N)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = N), vjust = -0.5, size = 3, color = "black")+
    labs(title = "Number of Terminated Employee by Job Title") +
    xlab("Job Title") +
    ylab("Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
    
# ANALYSIS 1-4: Find the relationship between department with attrition (vs Terminated Employees)
    
    # Filter the data for employees with "TERMINATED" status
    terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
    
    # Calculate the count of terminated employees per department
    department_counts <- terminated_data[, .N, by = department_name]

    # Create the stacked bar graph
      ggplot(department_counts, aes(x = department_name, y = N, fill = department_name)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = N), vjust = -0.5, size = 3, color = "black") +
      labs(title = "Number of Terminated Employees by Department",
           x = "Department",
           y = "Count",
           fill = "Departments") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    
# ANALYSIS 1-5: Find the relationship between gender with attrition (vs Terminated Employees)
      
    # Filter the data for employees with "TERMINATE" status
    terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
    
    # Calculate the count of terminated employees by gender
    gender_counts <- terminated_data[, .N, by = gender_full]
    
    # Calculate the percentage for each gender
    gender_counts[, Percentage := N / sum(N) * 100]
  
    custom_colors <- c("#0FF000", "#F0F000")
    
    # Create the pie chart with labels
      ggplot(gender_counts, aes(x = "", y = N, fill = gender_full)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
      labs(title = "Percentage of Terminated Employees by Gender",
           fill = "Gender") +
      scale_fill_manual(values = custom_colors) + 
      theme_void()

# ANALYSIS 1-6: Find the relationship between length of service with attrition (vs Terminated Employees)
      
      # Filter the data for employees with "TERMINATE" status
      terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]

      
      # Convert the length_of_service column to numeric
      terminated_data$length_of_service <- as.numeric(as.character(terminated_data$length_of_service))
      
      # Create a new column for the length of service range
      terminated_data$length_of_service <- cut(terminated_data$length_of_service, 
                                               breaks = c(0, 1, 5, 10, 15, 20, 25, Inf), 
                                               labels = c("0-1","1-5","5-10","10-15", "15-20", "20-25", ">25"))
      
      # Calculate the count of employees with the same length of service
      length_of_service_count <- terminated_data[, .N, by = length_of_service]
      
      # Generate random colors for each length of service range
      num_ranges <- length(unique(length_of_service_count$length_of_service))
      colors <- sample(colors(), num_ranges)
      
      # Create the histogram
        ggplot(length_of_service_count, aes(x = length_of_service, y = N, fill = length_of_service)) +
        geom_histogram(stat = "identity", position = "stack", color = "black") +
        geom_text(aes(label = N), vjust = -0.5, size = 3) +
        labs(title = "Count of Employees with Same Length of Service",
             x = "Length of Service Range",
             y = "Employee Count") +
        scale_fill_manual(values = colors, guide = FALSE) +
        theme_minimal()
       
# ANALYSIS 1-7: Find the relationship between store name with attrition (vs Terminated Employees)
        
        # Filter the data for employees with "TERMINATE" status
        terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
        
        
        # Calculate the count of employees for each store name
        store_counts <- terminated_data %>%
          group_by(store_name) %>%
          summarize(Count = n())
        
        # Create the heatmap
        ggplot(store_counts, aes(x = reorder(store_name, Count), y = Count, fill = Count)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Add labels
          labs(title = "Relationship between Store Name and Terminated Employees",
               x = "Store Name",
               y = "Employee Count") +
          scale_fill_gradient(low = "white", high = "red") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        

# ANALYSIS 1-8: Find the relationship between city with attrition (vs Terminated Employees)

        # Prepare the data
        terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
        city_counts <- terminated_data %>%
          group_by(city_name) %>%
          summarize(EmployeeCount = n())
        
        ggplot(city_counts, aes(x = EmployeeCount, y = city_name)) +
          geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
          geom_text(aes(label = EmployeeCount), vjust = -0.5)+
          labs(title = "Relationship between City and Terminated Employees",
               x = "Employee Count",
               y = "City") +
          theme_minimal()
        
        # Create the bar graph with percentage labels
        ggplot(city_counts, aes(x = reorder(city_name, EmployeeCount), y = EmployeeCount)) +
          geom_bar(stat = "identity", fill = "cyan") +
          labs(title = "Relationship between City and Terminated Employees",
               x = "City",
               y = "Employee Count") +
          theme_minimal() +
          geom_text(aes(label = paste0(round(EmployeeCount / sum(EmployeeCount) * 100, 1), "%")),
                    vjust = -0.5, color = "white")
# ANALYSIS 1-9: Termination types impacting attrition rates.
        
        terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
        termination_counts <- terminated_data %>%
          group_by(termtype_desc) %>%
          summarize(Count = n())
        
        # Create the treemap
        treemap(termination_counts, index = "termtype_desc", vSize = "Count",
                title = "Relationship between Termination Reasons and Terminated Employees",
                palette = "Set3")
        
# ANALYSIS 1-10: Termination reasons impacting attrition rates
        # Filter the data for employees with "Terminated" status
        terminated_data <- assignment_csv[assignment_csv$STATUS == "TERMINATED", ]
        
        # Calculate the count for each termination reason
        termination_counts <- terminated_data %>%
          group_by(termreason_desc) %>%
          summarize(Count = n())
        
        # Create the pie chart
        ggplot(termination_counts, aes(x = "", y = Count, fill = termreason_desc)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          labs(title = "Relationship between Terminated Reasons and Terminated Employees",
               fill = "Termination Reason") +
          theme_minimal()+
          theme(axis.title = element_blank(),  # Remove axis titles
                axis.text = element_blank(), # Remove axis text
                axis.ticks = element_blank()) # Remove axis ticks
        
# ANALYSIS 1-11: Employee termination by year
        
        filtered_data <- assignment_csv %>%
          filter(STATUS == "TERMINATED")
        
        # Extract the year from the termination date
        filtered_data$YEAR <- format(as.Date(filtered_data$terminationdate_key, "%m/%d/%Y"), "%Y")
        
        termination_counts <- filtered_data %>%
          group_by(YEAR, STATUS) %>%
          summarise(Count = n())
        
        # Arrange the data in ascending order of year
        termination_counts <- termination_counts %>%
          arrange(YEAR)
        
        ggplot(termination_counts, aes(x = YEAR, y = Count, color = STATUS, group = STATUS)) +
          geom_line() +
          geom_point() +
          geom_rug() +
          geom_ribbon(aes(ymin = 0, ymax = Count), alpha = 0.3) +
          geom_text(aes(label = YEAR), vjust = 0, nudge_y = 10, color = "black") +
          geom_text(aes(label = Count), vjust = 0, nudge_y = -20, color = "red") +
          labs(title = "Relationship between Termination Status and Year",
               x = "Year",
               y = "Count") +
          scale_color_manual(values = c("Terminated" = "red")) +
          theme_minimal()
        

# QUESTION 2: What are the reasons for employee layoff?

# ANALYSIS 2-1: Find the relationship between age with layoff
        
        # Filter the data for employees with termination type "layoff"
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff",]
        
        # Convert character data type to numeric
        assignment_csv$age <- as.numeric(assignment_csv$age)
        
        # Create the violin plot
        ggplot(layoff_data, aes(x = "", y = age, fill = termreason_desc)) +
          geom_violin(trim = FALSE) +
          geom_boxplot(width=0.1, fill = "white", color = "black" )+
          labs(title = "Relationship between Age and Employee Layoff",
               x = NULL,
               y = "Age") +
          scale_fill_manual(values = "orange", 
                            name = "Termination Reason",  # Modify the legend title
                            labels = c("Layoff")) +  # Modify the legend labels
          theme_minimal()
        
# ANALYSIS 2-2: Find the relationship between job title with layoff
        
        # Filter the data for employees with termination reason "layoff"
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Calculate the count of employees for each job title
        job_counts <- layoff_data %>%
          group_by(job_title) %>%
          summarize(EmployeeCount = n())
        
        # Sort the job titles by employee count in descending order
        job_counts <- job_counts[order(job_counts$EmployeeCount, decreasing = TRUE), ]
        
        # Create the lollipop plot
        ggplot(job_counts, aes(x = reorder(job_title, EmployeeCount), y = EmployeeCount)) +
          geom_segment(aes(xend = job_title, yend = 0), color = "blue") +
          geom_point(color = "blue", size = 3) +
          geom_text(aes(label = EmployeeCount), vjust = -1.5, color = "black", size = 3) +
          labs(title = "Relationship between Job Title and Number of Employees with Layoff Termination",
               x = "Job Title",
               y = "Employee Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
# ANALYSIS 2-3: Find the relationship between department with layoff
        
        # Filter the data for employees with termination reason "layoff"
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Calculate the count of employees for each department
        department_counts <- layoff_data %>%
          group_by(department_name) %>%
          summarize(EmployeeCount = n())
        
        # Sort the departments by employee count in descending order
        department_counts <- department_counts[order(department_counts$EmployeeCount, decreasing = TRUE), ]
        
        # Create the lollipop plot
        ggplot(department_counts, aes(x = reorder(department_name, EmployeeCount), y = EmployeeCount)) +
          geom_segment(aes(xend = department_name, yend = 0), color = "blue") +
          geom_point(color = "blue", size = 3) +
          geom_text(aes(label = EmployeeCount), vjust = -1.5, color = "black", size = 3) +
          labs(title = "Relationship between Department and Employee Layoff",
               x = "Department",
               y = "Employee Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
# ANALYSIS 2-4: Find the relationship between gender with layoff
        
        # Filter the data for employees with termination reason "layoff"
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Calculate the count of employees for each gender
        gender_counts <- layoff_data %>%
          group_by(gender_full) %>%
          summarize(EmployeeCount = n())
        
        # Create the bar chart
        ggplot(gender_counts, aes(x = gender_full, y = EmployeeCount, fill = gender_full)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = EmployeeCount), vjust = -0.5) +
          labs(title = "Relationship between Gender and  Employee Layoff",
               x = "Gender",
               y = "Employee Count") +
          scale_fill_manual(values = c("pink", "cyan")) +
          theme_minimal()
        
# ANALYSIS 2-5: Find the relationship between store name with layoff
        
        # Filter the data for employees with "LAYOFF" termination reason
        layoff_employees <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Count the number of employees for each combination of termination reason and store name
        layoff_counts <- table(layoff_employees$termreason_desc, layoff_employees$store_name)
        
        # Convert the count table to a data frame
        layoff_df <- as.data.frame(layoff_counts, stringsAsFactors = FALSE)
        colnames(layoff_df) <- c("TerminationReason", "StoreName", "Count")
        
        # Create the bar plot
        ggplot(layoff_df, aes(x = StoreName, y = Count, fill = TerminationReason)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(title = "Number of Employees with 'Layoff' Termination Reason by Store Name",
               x = "Store Name",
               y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
        
# ANALYSIS 2-6: Find the relationship between business unit with layoff
        
        layoff_counts <- assignment_csv %>%
          filter(termreason_desc == "Layoff") %>%
          count(BUSINESS_UNIT)
        
        # Sort the data by the count in descending order
        layoff_counts <- layoff_counts[order(layoff_counts$n, decreasing = TRUE), ]
        
        # Create the stacked bar chart
        ggplot(layoff_counts, aes(x = reorder(BUSINESS_UNIT, -n), y = n, fill = BUSINESS_UNIT)) +
          geom_bar(stat = "identity") +
          labs(title = "Relationship between Layoffs and Business Units",
               x = "Business Unit",
               y = "Count of Layoffs") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
# ANALYSIS 2-7: Find the relationship between city with layoff
        
        # Filter the data for employees with termination type "layoff"
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Calculate the count of layoffs by city
        city_counts <- layoff_data %>%
          count(city_name)
        
        # Generate random colors for each city
        num_cities <- length(unique(city_counts$city_name))
        colors <- sample(colors(), num_cities)
        
        # Create the count plot
        ggplot(city_counts, aes(x = n, y = reorder(city_name, n), fill = city_name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = n), hjust = -0.5, size = 3) +
          labs(title = "Employee Layoffs by City",
               x = "Count",
               y = "City Name") +
          scale_fill_manual(values = colors) +
          theme_minimal()
        
# ANALYSIS 2-8 Find the relationship between termination type with layoff
        
        # Filter the data for employees with "layoff" termination reason
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Get all unique termination types
        all_termination_types <- unique(assignment_csv$termtype_desc)
        
        # Calculate the count of termination types for employees with "layoff" termination reason
        termination_counts <- table(layoff_data$termtype_desc)
        
        # Create a data frame with all termination types and their counts
        termination_data <- data.frame(TerminationType = all_termination_types,
                                       Count = 0)
        
        # Update the count for the termination types with non-zero counts
        termination_data$Count[match(names(termination_counts), termination_data$TerminationType)] <- termination_counts
        
        # Convert the Count column to numeric
        termination_data$Count <- as.numeric(as.character(termination_data$Count))
        
        # Calculate the percentage if there are employees with "layoff" termination reason
        termination_data$Percentage <- termination_data$Count / sum(termination_data$Count) * 100
        
        # Sort the data by count in descending order
        termination_data <- termination_data[order(termination_data$Count, decreasing = TRUE), ]
        
        # Calculate the cumulative percentage
        termination_data$CumulativePercentage <- cumsum(termination_data$Percentage)
        
        # Create the donut chart
        ggplot(termination_data, aes(fill = TerminationType, x = "", y = Percentage, width = 1)) +
          geom_bar(stat = "identity", color = "white") +
          geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 4, color="white") +
          coord_polar("y", start = 0) +
          labs(title = "Termination Types for Employees with 'Layoff' Termination Reason",
               x = NULL,
               y = NULL) +
          scale_fill_manual(values = c("orange", "green", "purple")) +
          theme_void() +
          theme(axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                plot.title = element_text(hjust = 0.5)) +
          guides(fill = guide_legend(override.aes = list(width = 1, alpha = 1))) +
          annotate("text", x = 0, y = 0, label = "", size = 8, fontface = "bold") +
          annotate("text", x = 0, y = 0, label = "", size = 6, fontface = "bold", vjust = 0.7)
        
        
# ANALYSIS 2-9: Employee layoff by year
        
        # Filter the data for employees with "layoff" termination reason
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Extract the year from the termination date
        layoff_data$YEAR <- format(as.Date(layoff_data$terminationdate_key, "%m/%d/%Y"), "%Y")
        
        # Calculate the count of employees by year
        layoff_count <- table(layoff_data$YEAR)
        
        # Convert the count table to a data frame
        layoff_df <- as.data.frame(layoff_count)
        colnames(layoff_df) <- c("Year", "Count")
        
        colors <- c("blue", "orange")
        
        # Create the bar plot
        ggplot(layoff_df, aes(x = Year, y = Count, fill = Year)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = Count), vjust = -0.5, size = 3) +
          labs(title = "Employees Layoff by Year",
               x = "Year",
               y = "Count") +
          scale_fill_manual(values = colors) +
          theme_minimal()
        
        
# ANALYSIS 2-10: Find the relationship between Employee layoff with length of service
        
        # Filter the data for employees with "layoff" termination reason
        layoff_data <- assignment_csv[assignment_csv$termreason_desc == "Layoff", ]
        
        # Convert character data type to numeric
        assignment_csv$length_of_service <- as.numeric(assignment_csv$length_of_service)
        
        # Create the violin plot
        ggplot(layoff_data, aes(x = "", y = length_of_service, fill = termreason_desc)) +
          geom_violin(trim = FALSE) +
          geom_boxplot(width=0.1, fill = "white", color = "black" )+
          labs(title = "Relationship between Length of Service and Employee Layoff",
               x = NULL,
               y = "Length of Service") +
          scale_fill_manual(values = "pink", 
                            name = "Termination Reason",  # Modify the legend title
                            labels = c("Layoff")) +  # Modify the legend labels
          theme_minimal()
        

# QUESTION 3: Why do employees stay in the organisation? 
        
# ANALYSIS 3-1: Find the relationship between age with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Define the age ranges
        age_ranges <- cut(active_data$age, breaks = c(0, 20, 30, 40, 50, 60, Inf), 
                          labels = c("<20", "20-29", "30-39", "40-49", "50-59", ">=60"))
        
        # Create a new column for the age range
        active_data$age_range <- age_ranges
        
        # Create the histogram
        ggplot(active_data, aes(x = age_range, fill = age_range)) +
          geom_bar() +
          labs(title = "Age Distribution of Active Employees",
               x = "Age Range",
               y = "Count") +
          theme_minimal()
        
# ANALYSIS 3-2: Find the relationship between job title with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Calculate the count of employees per job title
        jobtitle_counts <- active_data[, .N, by = job_title]
        
        # Create the bar graph
        ggplot(jobtitle_counts, aes(x = job_title, y = N)) +
          geom_bar(stat = "identity", fill = "pink") +
          geom_text(aes(label = N), vjust = -0.5, size = 3, color = "black")+
          labs(title = "Number of Active Employee by Job Title") +
          xlab("Job Title") +
          ylab("Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
        
# ANALYSIS 3-3: Find the relationship between department with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Calculate the count of terminated employees per department
        department_counts <- active_data[, .N, by = department_name]
        
        # Create the stacked bar graph
        ggplot(department_counts, aes(x = department_name, y = N, fill = department_name)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = N), vjust = -0.5, size = 3, color = "black") +
          labs(title = "Number of Active Employees by Department",
               x = "Department",
               y = "Count",
               fill = "Departments") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
# ANALYSIS 3-4: Find the relationship between gender with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Calculate the count of terminated employees by gender
        gender_counts <- active_data[, .N, by = gender_full]
        
        # Calculate the percentage for each gender
        gender_counts[, Percentage := N / sum(N) * 100]
        
        custom_colors <- c("pink", "cyan")
        
        # Create the pie chart with labels
        ggplot(gender_counts, aes(x = "", y = N, fill = gender_full)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar("y", start = 0) +
          geom_text(aes(label = paste0(round(Percentage), "%")), position = position_stack(vjust = 0.5)) +
          labs(title = "Percentage of Active Employees by Gender",
               fill = "Gender") +
          scale_fill_manual(values = custom_colors) + 
          theme_void()
        
# ANALYSIS 3-5: Find the relationship between store name with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Calculate the count of employees for each store name
        store_counts <- active_data %>%
          group_by(store_name) %>%
          summarize(Count = n())
        
        # Create the heatmap
        ggplot(store_counts, aes(x = reorder(store_name, Count), y = Count, fill = Count)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = Count), vjust = -0.5, color = "black") +  # Add labels
          labs(title = "Relationship between Store Name and Active Employees",
               x = "Store Name",
               y = "Employee Count") +
          scale_fill_gradient(low = "white", high = "skyblue") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ANALYSIS 3-6: Find the relationship between business unit with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        # Calculate the count of employees for each BusinessUnit
        businessunit_counts <- active_data %>%
          group_by(BUSINESS_UNIT) %>%
          summarize(Count = n())
        
        # Sort the data by count in descending order
        businessunit_counts <- businessunit_counts[order(-businessunit_counts$Count), ]
        
        # Create the stacked bar plot using ggplot2
        ggplot(businessunit_counts, aes(x = "", y = Count, fill = BUSINESS_UNIT)) +
          geom_bar(stat = "identity") +
          labs(title = "Relationship between Active Employee and Business Unit",
               x = NULL,
               y = "Employee Count") +
          scale_fill_brewer(palette = "Set3") +
          theme_minimal()
        
# ANALYSIS 3-7: Find the relationship between city with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        city_counts <- active_data %>%
          group_by(city_name) %>%
          summarize(EmployeeCount = n())
        
        # Create the bar graph with percentage labels
        ggplot(city_counts, aes(x = reorder(city_name, EmployeeCount), y = EmployeeCount)) +
          geom_bar(stat = "identity", fill = "yellow") +
          labs(title = "Relationship between City and Active Employees",
               x = "City",
               y = "Employee Count") +
          theme_minimal() +
          geom_text(aes(label = paste0(round(EmployeeCount / sum(EmployeeCount) * 100, 1), "%")),
                    vjust = -0.5, color = "white")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
        
# ANALYSIS 3-8: Find the relationship between length of service with active employee
        
        # Filter the data for employees with "ACTIVE" status
        active_data <- assignment_csv[assignment_csv$STATUS == "ACTIVE", ]
        
        
        # Convert the length_of_service column to numeric
        active_data$length_of_service <- as.numeric(as.character(active_data$length_of_service))
        
        # Create a new column for the length of service range
        active_data$length_of_service <- cut(active_data$length_of_service, 
                                             breaks = c(0, 1, 5, 10, 15, 20, 25, Inf), 
                                             labels = c("0-1","1-5","5-10","10-15", "15-20", "20-25", ">25"))
        
        # Calculate the count of employees with the same length of service
        length_of_service_count <- active_data[, .N, by = length_of_service]
        
        # Generate random colors for each length of service range
        num_ranges <- length(unique(length_of_service_count$length_of_service))
        colors <- sample(colors(), num_ranges)
        
        # Create the histogram
        ggplot(length_of_service_count, aes(x = length_of_service, y = N, fill = length_of_service)) +
          geom_histogram(stat = "identity", position = "stack", color = "black") +
          geom_text(aes(label = N), vjust = -0.5, size = 3) +
          labs(title = "Active Employees by Length of Service",
               x = "Length of Service (Years)",
               y = "Employee Count") +
          scale_fill_manual(values = colors, guide = FALSE) +
          theme_minimal() 


# QUESTION 4: What are the factors influencing Employee promotion opportunities?

# ANALYSIS 4-1: Find the relationship between age with job title
        
        # Create a numeric identifier for job titles
        job_title_numeric <- as.numeric(factor(mean_age_by_jobtitle$job_title))
        
        # Create the scatter plot with colored dots
        ggplot(mean_age_by_jobtitle, aes(x = job_title_numeric, y = mean_age, color = job_title)) +
          geom_point() +
          labs(title = "Mean Age vs Job Title",
               x = "Job Title",
               y = "Mean Age") +
          theme_minimal() +
          scale_x_continuous(breaks = unique(job_title_numeric),
                             labels = unique(mean_age_by_jobtitle$job_title)) +
          geom_text(aes(label = round(mean_age, 1)), vjust = -0.5, size = 3)+
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
# ANALYSIS 4-2: Find the relationship between gender with job title
        
        # Convert the count table to a data frame
        gender_job_df <- as.data.frame(gender_job_counts)
        colnames(gender_job_df) <- c("Gender", "JobTitle", "Count")
        
        # Create the grouped bar plot with color and labels
        ggplot(gender_job_df, aes(x = Gender, y = Count, fill = JobTitle)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Count of Employees by Gender and Job Title",
               x = "Gender",
               y = "Count") +
          scale_fill_discrete(name = "Job Title") +  # Add legend title
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.5, size = 3)
        
# ANALYSIS 4-3: Find the relationship between store name with job title
        
        # Create the stacked bar plot
        ggplot(store_job_df, aes(x = StoreName, y = Count, fill = JobTitle)) +
          geom_bar(stat = "identity") +
          labs(title = "Count of Employees by Store Name and Job Title",
               x = "Store Name",
               y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
# ANALYSIS 4-4: Find the relationship between business unit with job title

        bu_job_counts <- table(assignment_csv$BUSINESS_UNIT, assignment_csv$job_title)
        
        # Convert the count table to a data frame
        bu_job_df <- as.data.frame(bu_job_counts)
        colnames(bu_job_df) <- c("BusinessUnit", "JobTitle", "Count")
        
        # Create the grouped bar plot
        ggplot(bu_job_df, aes(x = BusinessUnit, y = Count, fill = JobTitle)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = "Count of Employees by Business Unit and Job Title",
               x = "Business Unit",
               y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
# ANALYSIS 4-5  : Find the relationship between city with job title
          
          # Create a data frame with the counts for each combination of city and job title
          city_job_counts <- table(assignment_csv$city_name, assignment_csv$job_title)
          
          # Convert the count table to a data frame
          city_job_df <- as.data.frame(city_job_counts)
          colnames(city_job_df) <- c("City", "JobTitle", "Count")
          
          # Create the stacked bar plot
          ggplot(city_job_df, aes(x = City, y = Count, fill = JobTitle)) +
            geom_bar(stat = "identity") +
            labs(title = "Count of Employees by City and Job Title",
                 x = "City",
                 y = "Count") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
# ANALYSIS 4-6: Find the relationship between length of service with job title
          
          # Convert the "length_of_service" column to numeric
          assignment_csv$length_of_service <- as.numeric(assignment_csv$length_of_service)
          
          # Create the box plot
          ggplot(assignment_csv, aes(x = job_title, y = length_of_service)) +
            geom_boxplot() +
            geom_text(stat = "boxplot", aes(label = round(..y.., 2), y = ..y..), vjust = -0.5) +
            labs(title = "Length of Service with Job Title",
                 x = "Job Title",
                 y = "Length of Service") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          

# QUESTION 5: Are the number of terminated employees replaced by an equal number of new hires?

# ANALYSIS 5-1: (terminated) Count the number of unique employee IDs or occurrences in the "Termination Date" column.
        
        # Convert "Termination date" column to date format and extract the Year
        assignment_csv$terminationdate_key <- as.Date(assignment_csv$terminationdate_key, format = "%m/%d/%Y")
        assignment_csv$TerminationYear <- format(assignment_csv$terminationdate_key, "%Y")
        
        # Count the number of terminations by year
        termination_counts <- table(assignment_csv$TerminationYear)
        
        # Convert the count table to a data frame
        termination_df <- as.data.frame(termination_counts)
        colnames(termination_df) <- c("Year", "Count")
        
        # Create a vector of colors
        colors <- rainbow(nrow(termination_df))
        
        # Create the bar plot with colors and labels
        ggplot(termination_df, aes(x = Year, y = Count, fill = Year)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = colors) +
          labs(title = "Number of Terminations by Year",
               x = "Year",
               y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_text(aes(label = Count), vjust = -0.5, size = 3)
        
# ANALYSIS 5-2: (new hires): Count the number of unique employee IDs or occurrences in the "Hired Date" column.
        
        # Convert "Hire Date" column to date format and extract the Year
        assignment_csv$orighiredate_key <- as.Date(assignment_csv$orighiredate_key, format = "%m/%d/%Y")
        assignment_csv$Year <- format(assignment_csv$orighiredate_key, "%Y")
        
        # Count the number of new hires by year
        new_hires_count <- table(assignment_csv$Year)
        
        # Convert the count table to a data frame
        new_hires_df <- as.data.frame(new_hires_count)
        colnames(new_hires_df) <- c("Year", "Count")
        
        # Create a vector of colors
        colors <- rainbow(nrow(new_hires_df))
        
        # Create the bar plot with assigned colors
        ggplot(new_hires_df, aes(x = Year, y = Count, fill = Year)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = colors) +
          labs(title = "Number of New Hires by Year",
               x = "Year",
               y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          geom_text(aes(label = Count), vjust = -0.5, size = 3)
        

        
# ANALYSIS 5-3: Compare the count of terminated employees with the count of new hires by years
        
        # Convert "Termination date" column to date format and extract the Year
        assignment_csv$terminationdate_key <- as.Date(assignment_csv$terminationdate_key, format = "%m/%d/%Y")
        assignment_csv$TerminationYear <- format(assignment_csv$terminationdate_key, "%Y")
        
        # Count the number of terminations by year
        termination_counts <- table(assignment_csv$TerminationYear)
        
        # Convert the count table to a data frame
        termination_df <- as.data.frame(termination_counts)
        colnames(termination_df) <- c("Year", "TerminationCount")
        
        # Convert "Hire Date" column to date format and extract the Year
        assignment_csv$orighiredate_key <- as.Date(assignment_csv$orighiredate_key, format = "%m/%d/%Y")
        assignment_csv$Year <- format(assignment_csv$orighiredate_key, "%Y")
        
        # Count the number of new hires by year
        new_hires_count <- table(assignment_csv$Year)
        
        # Convert the count table to a data frame
        new_hires_df <- as.data.frame(new_hires_count)
        colnames(new_hires_df) <- c("Year", "NewHiresCount")
        
        # Combine termination and new hires data frames
        combined_counts <- merge(termination_df, new_hires_df, by = "Year", all = TRUE)
        
        # Set the count values to 0 for missing years
        combined_counts[is.na(combined_counts)] <- 0
        
        # Convert Year to factor and arrange levels in ascending order
        combined_counts$Year <- factor(combined_counts$Year, levels = sort(unique(combined_counts$Year)))
        
        # Create the line plot with combined data
        ggplot(combined_counts, aes(x = Year)) +
          geom_line(aes(y = TerminationCount, color = "Termination"), linetype = "dashed") +
          geom_line(aes(y = NewHiresCount, color = "New Hires")) +
          geom_point(aes(y = TerminationCount, color = "Termination")) +
          geom_point(aes(y = NewHiresCount, color = "New Hires")) +
          geom_text(aes(y = TerminationCount, label = TerminationCount), vjust = -0.5, size = 3, color = "red") +
          geom_text(aes(y = NewHiresCount, label = NewHiresCount), vjust = -0.5, size = 3, color = "blue") +
          labs(title = "Comparison of Terminations and New Hires by Year",
               x = "Year",
               y = "Count") +
          scale_color_manual(values = c("Termination" = "red", "New Hires" = "blue")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
