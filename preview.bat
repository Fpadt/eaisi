#!/bin/bash

# Execute the R script before rendering
echo "Running pre-render script..."
Rscript update_def.R

# Render the Quarto project
echo "Rendering Quarto project..."
quarto preview --render all --no-watch-inputs --no-browse