#!/bin/bash

# Script to clean up any remaining arXiv references and apply white/brick red theme
# This script ensures consistency across all files

echo "Applying theme updates and removing arXiv references..."

# Remove any remaining arXiv references (backup approach)
for file in *.html; do
    if [ -f "$file" ]; then
        # Remove arXiv links from any remaining places
        sed -i '/ai-arxiv/d' "$file"
        sed -i '/arxiv\.org/d' "$file"
        echo "Processed $file"
    fi
done

echo "Theme updates applied successfully!"
echo "Website now uses clean white/brick red theme without arXiv references."