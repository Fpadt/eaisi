@echo off

REM # Execute the R script before rendering
echo "Running pre-render script..."
REM Rscript update_def.R

REM # Render the Quarto project
echo "Rendering Quarto project..."
quarto preview --render all --no-watch-inputs --no-browse