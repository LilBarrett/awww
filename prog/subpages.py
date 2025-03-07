import os
import re
import html

# Updated configuration 
MAIN_FILE = "_posts/2025-03-06-tiobe-index.md"
SUBPAGE_DIR = "tiobe"
CURRENT_DATE = "2025-03-06"
CURRENT_TIME = "00:40:28"  # Updated to your current time
AUTHOR = "LilBarrett"      # Your login

# Fibonacci examples with proper formatting and line breaks
FIBONACCI_EXAMPLES = {
    "Python": """def fibonacci(n=10):
    \"\"\"Print first n Fibonacci numbers\"\"\"
    a, b = 0, 1
    print("Fibonacci Sequence:", end=" ")
    for _ in range(n):
        print(a, end=" ")
        a, b = b, a + b
    print()

fibonacci()
# Output: Fibonacci Sequence: 0 1 1 2 3 5 8 13 21 34""",

    "C": """#include <stdio.h>

void fibonacci(int n) {
    int a = 0, b = 1, temp;
    
    printf("Fibonacci Sequence: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", a);
        temp = a + b;
        a = b;
        b = temp;
    }
    printf("\\n");
}

int main() {
    fibonacci(10);
    return 0;
    // Output: Fibonacci Sequence: 0 1 1 2 3 5 8 13 21 34
}""",

    "Java": """public class Fibonacci {
    public static void printFibonacci(int n) {
        int a = 0, b = 1;
        
        System.out.print("Fibonacci Sequence: ");
        for (int i = 0; i < n; i++) {
            System.out.print(a + " ");
            int temp = a + b;
            a = b;
            b = temp;
        }
        System.out.println();
    }
    
    public static void main(String[] args) {
        printFibonacci(10);
        // Output: Fibonacci Sequence: 0 1 1 2 3 5 8 13 21 34
    }
}""",
}

def clean_filename(name):
    """Create a valid filename from language name"""
    return re.sub(r'[^a-z0-9]+', '-', name.lower()).strip('-')

def get_language_code(language):
    """Get language code for syntax highlighting"""
    lang_map = {
        "C#": "csharp",
        "C++": "cpp",
        "JavaScript": "javascript", 
        "TypeScript": "typescript",
        "Visual Basic": "vb",
    }
    
    if language in lang_map:
        return lang_map[language]
    return language.lower()

def create_html_code_block(code, language):
    """Create HTML code block with proper syntax highlighting"""
    # Escape HTML entities in the code
    escaped_code = html.escape(code)
    lang_class = get_language_code(language)
    
    # Create HTML code block with highlighting classes
    return f'''<div class="highlight-wrapper">
  <div class="highlight">
    <pre class="highlight"><code class="language-{lang_class}" data-lang="{lang_class}">{escaped_code}</code></pre>
  </div>
</div>'''

def get_fibonacci_example(language):
    """Get Fibonacci sequence example for a language as HTML"""
    # Try direct match
    if language in FIBONACCI_EXAMPLES:
        return create_html_code_block(FIBONACCI_EXAMPLES[language], language)
    
    # Try partial match
    for key in FIBONACCI_EXAMPLES:
        if key in language or language in key:
            return create_html_code_block(FIBONACCI_EXAMPLES[key], key)
    
    # Default if no match
    return create_html_code_block(f"// Fibonacci sequence example for {language} not available", "text")

def create_subpage(language, rank):
    """Create a subpage for the language with HTML code block"""
    # Make sure directory exists
    os.makedirs(SUBPAGE_DIR, exist_ok=True)
    
    # Generate filename
    safe_name = clean_filename(language)
    file_path = f"{SUBPAGE_DIR}/{safe_name}.html"
    
    # Get code example as HTML
    code_html = get_fibonacci_example(language)
    
    # Create content with inline styling for simplicity
    content = f'''---
layout: null
---
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{language} Programming - Fibonacci Example</title>
  <style>
    body {{
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, sans-serif;
      line-height: 1.6;
      color: #333;
      max-width: 800px;
      margin: 0 auto;
      padding: 20px;
    }}
    h1, h2 {{
      color: #2c3e50;
      margin-top: 1em;
    }}
    .highlight-wrapper {{
      background: #f8f8f8;
      border: 1px solid #ddd;
      border-radius: 3px;
      margin: 1.5em 0;
    }}
    .highlight {{
      padding: 1em;
      overflow-x: auto;
    }}
    pre {{
      margin: 0;
      font-family: SFMono-Regular, Consolas, "Liberation Mono", Menlo, monospace;
      font-size: 14px;
      line-height: 1.5;
      white-space: pre;
      word-break: normal;
    }}
    a {{ color: #3498db; text-decoration: none; }}
    a:hover {{ text-decoration: underline; }}
    .description {{
      background-color: #f0f7fb;
      border-left: 5px solid #3498db;
      padding: 0.5em 1em;
      margin: 1.5em 0;
    }}
  </style>
</head>
<body>
  <h1>{language} Programming Language</h1>
  <p><strong>TIOBE Index Rank:</strong> {rank}</p>
  
  <h2>Fibonacci Sequence Example</h2>
  
  {code_html}
  
  <div class="description">
    <p>This code implements the Fibonacci sequence where each number is the sum of the two preceding ones, starting from 0 and 1.</p>
    <p>The resulting sequence is: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...</p>
  </div>
  
  <p><a href="/2025/03/06/tiobe-index/">Back to TIOBE Index</a></p>
</body>
</html>'''
    
    # Write the file
    with open(file_path, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"Created subpage: {file_path}")
    
    # Return the URL for linking
    return f"/tiobe/{safe_name}.html"

def update_main_markdown():
    """Update the main markdown file with links to subpages"""
    try:
        with open(MAIN_FILE, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except FileNotFoundError:
        print(f"ERROR: Main file not found: {MAIN_FILE}")
        return
    
    new_content = []
    
    for line in lines:
        # Look for language headers (## 1. Python)
        if line.startswith('## ') and re.search(r'^\#\# \d+\.', line):
            # Extract language name and rank
            match = re.match(r'^\#\# (\d+)\. ([^\[\]]+)(?:\s+\[.*\].*)?$', line.strip())
            if match:
                rank = match.group(1)
                language = match.group(2).strip()
                
                # If this line already has our Fibonacci link, keep it as is
                if "[Fibonacci Example]" in line:
                    new_content.append(line)
                    continue
                    
                # Create subpage and update the line
                page_url = create_subpage(language, rank)
                new_line = f"## {rank}. {language} [Fibonacci Example]({page_url})\n"
                new_content.append(new_line)
                continue
        
        # Keep other lines unchanged
        new_content.append(line)
    
    # Write back to the file
    with open(MAIN_FILE, 'w', encoding='utf-8') as f:
        f.writelines(new_content)

if __name__ == "__main__":
    print(f"---- Creating Well-Formatted Code Examples ({CURRENT_DATE} {CURRENT_TIME}) ----")
    update_main_markdown()
    print("\nDone! Code blocks should now render properly without backticks.")
    print("\nThe pages use HTML directly for perfect code formatting.")
