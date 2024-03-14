# Exploratory Data Analysis Function

data(mtcars)
df <- mtcars
df$gear <- as.factor(df$gear)


# Added for loop from Bing Copilot, Thank you!!
eda <- function(df, vars, graphs = FALSE) {
  for (var in vars) {# This is the line Bing Pilot added, everything else was mostly me!!
    cat("Variable: ", var, "\n")
    cat("(n): ", length(df[[var]]), "\n")
    cat('Mis: ', sum(is.na(df[[var]])), "\n")
    if (is.numeric(df[[var]])) {
      cat("Avg: ", mean(df[[var]]), "\n")
      cat("Std: ", sd(df[[var]]), "\n")
      cat("Min: ", min(df[[var]]), "\n")
      cat("Med: ", median(df[[var]]), "\n")
      cat("Max: ", max(df[[var]]), "\n")
      norm <- shapiro.test(df[[var]])
      if (length(df[[var]]) < 5000 ) {
      cat("Shapiro p Value:", norm$p.value, "\n")
      }
      cat('\n')
      if (isTRUE(graphs)) {
        par( mfrow = c (1,2))
        histplot <- hist(df[[var]], main = paste(var))
        qq <- qqnorm(df[[var]], main = paste(var))
        qqline(df[[var]]) ## Consider adding par() to have graphs take up one space
        par(mfrow = c(1,1))
      }
    } else if (is.factor(df[[var]])) {
      cat('Var Type: ', class(df[[var]]), '\n')
      cat('Table of', var, '\n')
      tab <- table(df[[var]])
      new <- as.data.frame(tab)
      new$pct <- new$Freq / length(df[[var]])
      print(new)

      # Print $'s for each categroy

      if(isTRUE(graphs)) {
        barplot(table(df[[var]]),main = paste(var))
    }
   }
  }
}

eda(df, 'gear') # Factor

eda(df, "am", graphs = TRUE) # Numeric with Graphs

eda(df, c("mpg", "gear", "wt"), graphs = TRUE)

# Make this a package: https://web.mit.edu/insong/www/pdf/rpackage_instructions.pdf




######################################################


# Original eda funcition, created entirely by me with no AI!
og_eda <- function(var) {
  if (is.numeric(var)) {
    name <- print(deparse(substitute(var)))
    cat(name, "\n")
    cat("Head:",head(var), "\n")
    cat("Mean: ", mean(var), "\n")
    cat("Std: ", sd(var), "\n")
    cat("Min: ", min(var), "\n")
    cat("Med: ", median(var), "\n")
    cat("Max:", max(var), "\n")
    norm <- shapiro.test(var)
    cat("Shapiro p Value:", norm$p.value, "\n")
    hist(var)
  } else if (is.factor(var)) {
    print(table(var))
    barplot(table(var))
  }
}

og_eda(df$gear)
og_eda(df$mpg)
