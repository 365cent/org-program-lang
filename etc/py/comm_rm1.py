import sys

def is_inside_string(line, index):
    single_quote_count = double_quote_count = 0
    for i, char in enumerate(line[:index]):
        if char == "'" and (i == 0 or line[i-1] != '\\'):
            single_quote_count += 1
        elif char == '"' and (i == 0 or line[i-1] != '\\'):
            double_quote_count += 1
            
    return single_quote_count % 2 != 0 or double_quote_count % 2 != 0

def rm(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()

    new_lines = []
    inside_multiline_string = False
    multiline_delimiter = ""

    for line in lines:
        stripped_line = line.strip()
        
        if "#" in stripped_line:
            if stripped_line.startswith("#"):
                continue
            split_index = None
            for i, char in enumerate(line):
                if char == "#" and not is_inside_string(line, i):
                    split_index = i
                    break
            if split_index is not None:
                new_lines.append(line[:split_index].rstrip() + '\n')
                continue

        if inside_multiline_string:
            if multiline_delimiter in stripped_line and (stripped_line.endswith(("'''", '"""')) and stripped_line[len(stripped_line) - 4] != "\\"):
                inside_multiline_string = False
                print(f"1Multiline string removed: {stripped_line}")
                continue
            else:
                print(f"2Multiline string removed: {stripped_line}")
                continue
        else:
            if stripped_line.startswith("'''") or stripped_line.startswith('"""'):
                if (stripped_line.count("'''") == 2 or stripped_line.count('"""') == 2) and len(stripped_line) > 3:
                    continue
                else:
                    inside_multiline_string = True
                    multiline_delimiter = stripped_line[:3]
                    continue
        
        new_lines.append(line)

    cleaned_file_path = 'cleaned_' + file_path
    with open(cleaned_file_path, 'w') as file:
        file.writelines(new_lines)
    print(f"Cleaned file saved as: {cleaned_file_path}")

def main():
    if len(sys.argv) < 2:
        print("Usage: python comm_rm.py <filename.py>")
    else:
        rm(sys.argv[1])

if __name__ == "__main__":
    main()
