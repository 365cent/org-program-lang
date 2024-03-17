def remove_comments(filename, output_filename):
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

    with open(output_filename, 'w') as f:
        f.writelines(new_lines)

# Example usage:
remove_comments('demo.py', 'cleaned_demo.py')
