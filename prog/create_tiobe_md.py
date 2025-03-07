import requests
from bs4 import BeautifulSoup

# Dictionary mapping programming languages to their "Hello, World!" programs
HELLO_WORLD_PROGRAMS = {
    "Python": '```python\nprint("Hello, World!")\n```',
    "C": '```c\n#include <stdio.h>\n\nint main() {\n    printf("Hello, World!\\n");\n    return 0;\n}\n```',
    "C++": '```cpp\n#include <iostream>\n\nint main() {\n    std::cout << "Hello, World!" << std::endl;\n    return 0;\n}\n```',
    "Java": '```java\npublic class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n    }\n}\n```',
    "C#": '```csharp\nusing System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine("Hello, World!");\n    }\n}\n```',
    "JavaScript": '```javascript\nconsole.log("Hello, World!");\n```',
    "SQL": '```sql\nSELECT "Hello, World!";\n```',
    "Go": '```go\npackage main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, World!")\n}\n```',
    "Delphi/Object Pascal": '```pascal\nprogram HelloWorld;\nbegin\n    writeln("Hello, World!");\nend.\n```',
    "Visual Basic": '```vb\nModule Module1\n    Sub Main()\n        Console.WriteLine("Hello, World!")\n    End Sub\nEnd Module\n```',
    "Fortran": '```fortran\nprogram HelloWorld\n    print *, "Hello, World!"\nend program HelloWorld\n```',
    "Scratch": 'No code example available.',
    "Rust": '```rust\nfn main() {\n    println!("Hello, World!");\n}\n```',
    "PHP": '```php\n<?php\necho "Hello, World!";\n?>\n```',
    "R": '```r\ncat("Hello, World!\\n")\n```',
    "MATLAB": '```matlab\ndisp("Hello, World!")\n```',
    "Assembly language": '```assembly\nsection .data\n    hello db "Hello, World!", 0\n\nsection .text\n    global _start\n\n_start:\n    mov rax, 1\n    mov rdi, 1\n    mov rsi, hello\n    mov rdx, 13\n    syscall\n\n    mov rax, 60\n    xor rdi, rdi\n    syscall\n```',
    "COBOL": '```cobol\nIDENTIFICATION DIVISION.\nPROGRAM-ID. HelloWorld.\nPROCEDURE DIVISION.\nDISPLAY "Hello, World!".\nSTOP RUN.\n```',
    "Ruby": '```ruby\nputs "Hello, World!"\n```',
    "Prolog": '```prolog\n:- initialization(main).\n\nmain :- write(\'Hello, World!\'), nl.\n```'
}

# Function to scrape TIOBE Index
def scrape_tiobe_index(url):
    headers = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        "Accept-Language": "en-US,en;q=0.9",
        "Referer": "https://www.google.com/",
        "DNT": "1",
        "Connection": "keep-alive",
    }

    response = requests.get(url, headers=headers)
    
    if response.status_code != 200:
        print(f"Failed to fetch data: {response.status_code}")
        return []

    soup = BeautifulSoup(response.text, 'html.parser')
    languages = []

    # Find the TIOBE index ranking table
    table = soup.find("table", {"class": "table table-striped table-top20"})

    if not table:
        print("Could not find the ranking table.")
        return []

    rows = table.find_all("tr")[1:]  # Skip header row

    for row in rows:
        cols = row.find_all("td")
        if len(cols) < 6:
            continue  # Skip invalid rows

        rank = cols[0].text.strip()
        prev_rank = cols[1].text.strip()
        language = cols[4].text.strip()
        rating = cols[5].text.strip()
        image_tag = cols[3].find('img')
        image_url = image_tag['src'] if image_tag else ''

        if image_url and image_url.startswith('/'):
            image_url = url + image_url

        languages.append({
            "rank": rank,
            "prev_rank": prev_rank,
            "language": language,
            "rating": rating,
            "image_url": image_url,
            "hello_world": HELLO_WORLD_PROGRAMS.get(language, '```plaintext\nNo example available.\n```')
        })

    return languages

# Function to generate Markdown
def generate_markdown(languages, filename):
    md_content = "# TIOBE Index - Top Programming Languages\n\n"
    md_content += "This ranking is based on the popularity of programming languages according to the TIOBE Index.\n\n"

    for lang in languages:
        md_content += f"## {lang['rank']}. {lang['language']}\n"
        md_content += f"**Rating**: {lang['rating']}\n\n"
        md_content += f"**Previous Rank**: {lang['prev_rank']}\n\n"
        #if lang['image_url']:
        #    md_content += f"![{lang['language']}]({lang['image_url']})\n\n"
        md_content += f"**Hello, World! Program**:\n\n{lang['hello_world']}\n\n"
        md_content += "---\n"

    with open(filename, 'w', encoding='utf-8') as file:
        file.write(md_content)

# Main function
def main():
    url = "https://www.tiobe.com/tiobe-index/"
    filename = "_posts/2025-03-06-tiobe-index.md"

    languages = scrape_tiobe_index(url)
    if languages:
        generate_markdown(languages, filename)
        print(f"Markdown file '{filename}' generated successfully.")
    else:
        print("No data found. Website may have changed its structure.")

if __name__ == "__main__":
    main()
