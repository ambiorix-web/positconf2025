# Ambiorix Data Dashboard Demo

A complete example showing how Ambiorix can serve both **HTML interfaces** and **JSON APIs** from the same application.

## What This Demo Shows

Perfect foundation for showing Ambiorix capabilities to the R community!

### 🎯 **Key Concepts**

- **Same functions, different formats**: HTML and JSON routes use identical R analysis functions
- **Multi-page applications**: Unlike Shiny's SPA approach, each route is a separate page
- **Web-native patterns**:
  - Browser links work for GET requests, no JavaScript needed
  - Request parameters
  - Query parameters
  - HTTP error responses
- **Familiar R workflow**: Working with datasets, statistics, and data analysis

### 🏗️ **Architecture**

```
📁 demo/
├── 📄 app.R              # Main Ambiorix application
├── 📄 data_analysis.R    # Shared R functions (the "business logic")
├── 📄 html_helpers.R     # HTML generation with htmltools + Bootstrap
└── 📄 README.md          # This file
```

## Running the Demo

### Prerequisites

```r
install.packages(c("ambiorix", "htmltools"))
```

### Start the App

```r
# In R console, from the demo directory:
source("app.R")
```

```bash
# or in the terminal:
Rscript app.R
```

The app will start at `http://localhost:3000`

