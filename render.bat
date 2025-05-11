#!/bin/bash

rem # Execute the R script before rendering
echo "Running pre-render script..."
Rscript update_def.R

rem # Render the Quarto project
echo "Rendering Quarto project..."
quarto render --no-cache

rem # Add and commit changes to Git
git add . 
git commit -m "pythia latest (script)"
git push origin main