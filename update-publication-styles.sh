#!/bin/bash

# Script to update publication links styling
# This script can be run to convert all old-style publication links to the new class-based styling

echo "Updating publication link styles..."

# Replace old style with new class in publications.html
sed -i 's/style="text-decoration:underline;color:#145A8C"/class="publication-link"/g' /home/smajumdar/shubhobm.github.io/publications.html

echo "Publication link styles updated!"
echo "All links now use the academic-enhancements.css styling."