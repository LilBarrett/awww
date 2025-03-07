import requests
import re
from bs4 import BeautifulSoup
from datetime import datetime
import sys

# Markdown file path
MARKDOWN_FILE_PATH = "_posts/2025-03-06-tiobe-index.md"

def log_message(message):
    """Log messages with timestamp"""
    current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    print(f"[{current_time}] {message}")

def get_wikipedia_info(language):
    """Get Wikipedia URL and year of creation for a programming language"""
    log_message(f"Processing language: {language}")
    
    # Try direct URLs first - these are the most common formats
    url_formats = [
        f"https://en.wikipedia.org/wiki/{language}_(programming_language)",
        f"https://en.wikipedia.org/wiki/{language}"
    ]
    
    wikipedia_url = None
    year_of_creation = None
    
    for url in url_formats:
        log_message(f"Trying URL: {url}")
        response = requests.get(url)
        
        if response.status_code == 200:
            wikipedia_url = url
            log_message(f"Found valid URL: {wikipedia_url}")
            
            # Parse the page for year of creation
            soup = BeautifulSoup(response.text, 'html.parser')
            infobox = soup.find("table", {"class": "infobox"})
            
            if infobox:
                rows = infobox.find_all("tr")
                for row in rows:
                    th = row.find("th")
                    if th and ("First" in th.text or "appeared" in th.text or "Released" in th.text):
                        td = row.find("td")
                        if td:
                            # Extract year using regex (4 digits that look like a year)
                            year_match = re.search(r'19[0-9]{2}|20[0-9]{2}', td.text)
                            if year_match:
                                year_of_creation = year_match.group(0)
                                log_message(f"Found year of creation: {year_of_creation}")
                                break
            break
    
    return wikipedia_url, year_of_creation

def enrich_markdown_file():
    """Enrich the Markdown file with Wikipedia information"""
    log_message(f"Opening file: {MARKDOWN_FILE_PATH}")
    
    try:
        with open(MARKDOWN_FILE_PATH, 'r') as file:
            content = file.readlines()
    except FileNotFoundError:
        log_message(f"Error: File '{MARKDOWN_FILE_PATH}' not found.")
        sys.exit(1)
    
    log_message(f"File loaded successfully with {len(content)} lines")
    
    enriched_content = []
    language = None
    
    for i, line in enumerate(content):
        enriched_content.append(line)
        
        # Extract language name from header line (## 1. Python)
        if line.startswith('## '):
            parts = line.strip().split()
            if len(parts) > 2:
                language = parts[2]  # Get language name
                log_message(f"Found language header: {language} (line {i+1})")
            
        # Add Wikipedia info after the rating line
        if language and "**Rating**:" in line:
            wikipedia_url, year_of_creation = get_wikipedia_info(language)
            
            if wikipedia_url:
                enriched_content.append(f"\n**Wikipedia**: [{language} on Wikipedia]({wikipedia_url})\n")
            
            if year_of_creation:
                enriched_content.append(f"\n**Year of Creation**: {year_of_creation}\n")
            
            language = None  # Reset language to avoid duplications
    
    log_message(f"Writing enriched content back to file")
    
    with open(MARKDOWN_FILE_PATH, 'w') as file:
        file.writelines(enriched_content)
    
    log_message(f"File successfully updated with Wikipedia information")

def main():
    log_message("Starting Wikipedia enrichment process")
    enrich_markdown_file()
    log_message("Wikipedia enrichment completed")

if __name__ == "__main__":
    main()
