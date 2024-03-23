function formatScheme() {
    const input = document.getElementById('input');
    const output = document.getElementById('output');
    const inputCode = input.value;
    let indentLevel = 0;
    let formattedCode = '';
    const increaseIndent = '  '; // Two spaces per indent level

    for (let i = 0; i < inputCode.length; i++) {
        const char = inputCode[i];
        if (char === '(') {
            if (i > 0 && inputCode[i - 1] !== '\n') {
                formattedCode += '\n' + increaseIndent.repeat(indentLevel);
            }
            indentLevel++;
            formattedCode += char;
        } else if (char === ')') {
            indentLevel--;
            if (inputCode[i + 1] !== ')' && inputCode[i + 1] !== '\n') {
                formattedCode += '\n' + increaseIndent.repeat(indentLevel);
            }
            formattedCode += char;
        } else if (char === '\n') {
            formattedCode += char + increaseIndent.repeat(indentLevel);
        } else {
            formattedCode += char;
        }
    }

    console.log(formattedCode);
    output.innerHTML = formattedCode;
}
