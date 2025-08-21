# Ambiorix Data Dashboard Demo

A complete example showing how Ambiorix can serve both **HTML interfaces** and **JSON APIs** from the same application.

## What This Demo Shows

### 🎯 **Key Concepts**
- **Same functions, different formats**: HTML and JSON routes use identical R analysis functions
- **Multi-page applications**: Unlike Shiny's SPA approach, each route is a separate page
- **Web-native patterns**: Browser links work for GET requests, no JavaScript needed
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

The app will start at `http://localhost:3000`

## Demo Flow for Presentation

### 1. **HTML Interface** (`http://localhost:3000/`)
- Beautiful Bootstrap dashboard
- Click through dataset cards
- View statistical summaries and sample data
- Multiple pages with navigation

### 2. **JSON API** (click "Explore API" link)
- Same data, different format
- Browser shows raw JSON
- Demonstrate different endpoints:
  - `/api/datasets` - List all datasets
  - `/api/datasets/mtcars` - Dataset summary
  - `/api/datasets/mtcars/data?limit=5` - Raw data

### 3. **Key Demo Points**

**"Same Functions, Different Responses"**
```r
# HTML route
app$get("/datasets/:name", function(req, res) {
  data <- get_dataset_summary(req$params$name)  # Same function
  res$send(create_dataset_page(data))           # Returns HTML
})

# API route  
app$get("/api/datasets/:name", function(req, res) {
  data <- get_dataset_summary(req$params$name)  # Same function
  res$json(data)                                # Returns JSON
})
```

**"Multi-Page, Web-Native"**
- Each dataset gets its own URL: `/datasets/mtcars`, `/datasets/iris`
- Browser back/forward buttons work
- Shareable URLs for specific data views
- No JavaScript required for navigation

**"R-Centric Data Analysis"**
- Uses familiar R datasets (`mtcars`, `iris`, `airquality`)
- Statistical summaries (`mean`, `median`, `sd`)
- Data manipulation with base R functions
- Results served in multiple formats

## File Structure Explained

### `data_analysis.R`
- Pure R functions for data analysis
- No web dependencies
- Reusable across different interfaces
- Functions: `get_dataset_summary()`, `get_available_datasets()`, etc.

### `html_helpers.R`
- HTML generation using `htmltools`
- Bootstrap styling for professional UI
- Functions: `create_homepage()`, `create_dataset_page()`, etc.

### `app.R`
- Ambiorix application setup
- Route definitions (HTML + API)
- Error handling
- Connects data functions to web interfaces

## Presentation Talking Points

1. **"One App, Two Interfaces"** - Same Ambiorix app serves humans (HTML) and machines (JSON)

2. **"R-Familiar Workflow"** - Working with datasets, statistics, data frames - all the R stuff you know

3. **"Web-Native Patterns"** - Each dataset is a page, URLs are shareable, browser navigation works

4. **"No JavaScript Required"** - GET requests work with simple browser links

5. **"Extensible"** - Easy to add new datasets, statistics, or visualizations

## Next Steps

This demo can be extended with:
- More datasets
- Data visualization endpoints
- POST endpoints for data upload
- Authentication middleware
- Database integration
- Real-time updates with WebSockets

Perfect foundation for showing Ambiorix capabilities to the R community!