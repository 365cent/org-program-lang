import sys

def remove_comments(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()

    in_multiline_comment = False
    new_lines = []

    for line in lines:
        stripped_line = line.strip()
        if stripped_line.startswith("'''") or stripped_line.startswith('"""'):
            # Toggle the multi-line comment state if not within a string
            in_multiline_comment = not in_multiline_comment
            continue  # Skip adding this line
        if in_multiline_comment:
            continue  # Skip lines within multi-line comments

        # Handle single line comments
        comment_start = line.find('#')
        if comment_start != -1:
            # Check if the '#' is not part of a string
            if not (line.count('"', 0, comment_start) % 2 != 0 or line.count("'", 0, comment_start) % 2 != 0):
                line = line[:comment_start] + "\n"  # Remove the comment part

        new_lines.append(line)

    cleaned = 'cleaned_' + filename
    with open(cleaned, 'w') as file:
        file.writelines(new_lines)
    print(f"Cleaned file saved as: {cleaned}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python comm_rm.py <filename.py>")
    else:
        remove_comments(sys.argv[1])

if __name__ == "__main__":
    main()