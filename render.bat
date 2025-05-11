#!/bin/bash

# Execute the R script before rendering
echo "Running pre-render script..."
Rscript update_def.R

# Render the Quarto project
echo "Rendering Quarto project..."
quarto render --no-cache

git add . 
git commit -m "pythia latest (script)"
git push origin main