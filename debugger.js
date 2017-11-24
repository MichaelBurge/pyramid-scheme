// 1. Create a Google Sheet with 2 tabs named Program and Simulation.
// 2. Paste the output of dissassembler.rkt into Program
// 3. Paste this file into the script editor
// 4. "EVM Simulator"->"Show Sidebar"->"Initialize"
var COL_STEP = 0;
var COL_SYMBOL = 1;
var COL_PC = 2;
var COL_STACK = 3;
var COL_MEMORY = 4;
var COL_INSTRUCTION = 5;

var COL_PROG_PC = 0;

// UI
function onOpen() {
    var ui = SpreadsheetApp.getUi();
    ui.createMenu('EVM Simulator')
      .addItem('Show sidebar', 'evmSidebar')
      .addToUi();
}

function evmSidebar() {
    var html = HtmlService.createHtmlOutputFromFile('evmSidebar.html')
                          .setTitle('EVM Simulator')
                          .setWidth(300);
    SpreadsheetApp.getUi().showSidebar(html);
}

// Server handlers
function initialize() {
    var sheet = simulationSheet();
    sheet.clearContents();
    sheet.clearFormats();
    var pdata = programSheet().getDataRange().getValues()
    sheet.appendRow(["Step", "Symbol", "PC", "Stack", "Memory", "Instruction"]);
    sheet.appendRow([1, pdata[1][1], 0, "[]", "{}", pdata[1][2] ]);
    SpreadsheetApp.flush();
}

function next() { nextn(1); }
function next10() { nextn(10); }
function next50() { nextn(50); }
function next100() { nextn(100); }
function next1000() { nextn(1000); }

function nextn(n) {
    var machine = getCurrentState();
    var sheet = simulationSheet();
    var index = machine.step+2;
    var rows = [];
    try {
        for (var i = 0; i < n; i++) {
            machine = dispatch(machine, machine.instruction);
            updateExtraInfo(machine);
            rows.push(newMachineRow(machine));
        }
    } catch (e) {
        // _signalError aborts, but we still need to write what rows we could.
    }
    insertRowsAt(sheet, index, rows);
    SpreadsheetApp.flush();
}

// Implementation
function sheet(name) {
    var app = SpreadsheetApp.getActiveSpreadsheet();
    var sheet = app.getSheetByName(name);
    return sheet;
}
function simulationSheet() { return sheet("Simulation"); }
function programSheet() { return sheet("Program"); }

function lastRow() {
    var sheet = simulationSheet();
    var range = sheet.getDataRange();
    var rowId = range.getLastRow() - 1;
    var data = range.getValues();
    return data[rowId];
}

function dispatch(machine, i) { // instruction
    var op = i.split(" ");
    switch(op[0]) {
        case "ISZERO": op_unop(machine, function(a) { return (a == 0) ? 1 : 0; }); break;
        case "ADD":    op_binop(machine, function(a,b) { return a + b; }); break;
        case "SUB":    op_binop(machine, function(a,b) { return a - b; }); break;
        case "MUL":    op_binop(machine, function(a,b) { return a * b; }); break;
        case "DIV":    op_binop(machine, function(a,b) { return a / b; }); break;
        case "EQ":     op_binop(machine, function(a,b) { return (a == b) ? 1 : 0; }); break;
        case "LT":     op_binop(machine, function(a,b) { return (a < b) ? 1 : 0; }); break;
        case "GT":     op_binop(machine, function(a,b) { return (a > b) ? 1 : 0; }); break;
        case "POP":    op_pop(machine);     break;
        case "DUP1":   op_dup(machine, 1);  break;
        case "DUP2":   op_dup(machine, 2);  break;
        case "DUP3":   op_dup(machine, 3);  break;
        case "DUP4":   op_dup(machine, 4);  break;
        case "DUP5":   op_dup(machine, 5);  break;
        case "DUP6":   op_dup(machine, 6);  break;
        case "DUP7":   op_dup(machine, 7);  break;
        case "Push1":  op_push(machine, 1,  parseInt(op[1], 16)); break;
        case "Push2":  op_push(machine, 2,  parseInt(op[1], 16)); break;
        case "Push3":  op_push(machine, 3,  parseInt(op[1], 16)); break;
        case "Push4":  op_push(machine, 4,  parseInt(op[1], 16)); break;
        case "Push32": op_push(machine, 32, parseInt(op[1], 16)); break;
        case "SWAP1":  op_swap(machine, 1); break;
        case "SWAP2":  op_swap(machine, 2); break;
        case "SWAP3":  op_swap(machine, 3); break;
        case "SWAP4":  op_swap(machine, 4); break;
        case "MSTORE": op_mstore(machine);  break;
        case "MLOAD":  op_mload(machine);   break;
        case "JUMP":   op_jump(machine);    break;
        case "JUMPI":  op_jumpi(machine);   break;
        case "JUMPDEST": break;
        default:
            Logger.log("Unhandled instruction", op[0]);
            _signalError(machine); break;
    }
    machine.pc = _normalizePc(machine.instructionTable, machine.pc+1);
    machine.step++;
    return machine;
}

function op_push(machine, size, data) {
    _pushStack(machine, data);
}

function op_pop(machine) {
    _popStack(machine);
}

function op_dup(machine, depth) {
    _assert(machine, machine.stack.length >= depth);
    var x = machine.stack[depth-1];
    _pushStack(machine, x);
}

function op_swap(machine, size) {
    var x1 = _popStack(machine);
    var xs = [];
    for (var i = 0; i < size; i++) {
        xs.push(_popStack(machine));
    }
    var x2 = xs.pop();
    _pushStack(machine, x1);
    for (i = 0; i < size-1; i++) {
        _pushStack(machine, xs.pop());
    }
    _pushStack(machine, x2);
}

function op_jump(machine) {
    machine.pc = _popStack(machine)-1;
}

function op_jumpi(machine) {
    var target = _popStack(machine);
    var cond = _popStack(machine);
    if (cond) {
        machine.pc = target;
    }
}

function op_mstore(machine) {
    var address = _popStack(machine);
    var value = _popStack(machine);
    _writeMemory(machine, address, value);
}

function op_mload(machine) {
    var address = _popStack(machine);
    var result = _readMemory(machine, address);
    _pushStack(machine, result);
}

function op_unop(machine, f) {
    var a = _popStack(machine);
    var result = f(a);
    _pushStack(machine, result);
}

function op_binop(machine, f) {
    var a = _popStack(machine);
    var b = _popStack(machine);
    var result = f(a, b);
    _pushStack(machine, result);
}

// Reads the last row in the Simulation section to produce a Machine object.
function getCurrentState() {
    var sheet = simulationSheet();
    var row = lastRow();
    var machine = {
        step: parseInt(row[COL_STEP]),
        pc: parseInt(row[COL_PC],16),
        stack: JSON.parse(row[COL_STACK]),
        memory: JSON.parse(row[COL_MEMORY]),
        instruction: row[COL_INSTRUCTION],
        instructionTable: programSheet().getDataRange().getValues()
    };
    updateExtraInfo(machine);
    return machine;
}

function newMachineRow(machine) {
    return [machine.step, machine.symbol, machine.pc.toString(16), JSON.stringify(machine.stack), JSON.stringify(machine.memory), machine.instruction ];
}

function insertRowsAt(sheet, index, rows) {
    sheet.getRange(index, 1, rows.length, rows[0].length).setValues(rows);
}

function updateExtraInfo(machine) {
    machine.rowId = _pcRowId(machine.instructionTable, machine.pc);
    machine.symbol = machine.instructionTable[machine.rowId-1][1];
    machine.instruction = machine.instructionTable[machine.rowId-1][2];
}

function _find(data, pred) {
    for (var i = 1; i < data.length; i++) {
        var row = data[i];
        if (pred(i, row)) {
            return [i, row];
        }
    }
    return null;
}

// Labels and push instructions create holes in the range of PC.
// Sets PC to the first valid row with PC <= target.
function _normalizePc(instructionTable, target) {
    var result = _find(instructionTable, function(i, row) {
        return parseInt(row[COL_PROG_PC], 16) >= target;
    });
    return parseInt(result[1][COL_PROG_PC], 16);
}

function _pcRowId(instructionTable, pc) {
    var result = _find(instructionTable, function(i, row) {
        return parseInt(row[COL_PROG_PC], 16) >= pc;
    });
    return result[0]+1;
}

function _pushStack(machine, value) {
    machine.stack.unshift(value);
}

function _popStack(machine) {
    _assert(machine, machine.stack.length > 0);
    return machine.stack.shift();
}

function _readMemory(machine, address) {
    var mem = machine.memory;
    if (mem[address] != null) {
        return mem[address];
    } else {
        _signalError(machine);
    }
}

function _writeMemory(machine, address, value) {
    machine.memory[address] = value;
}

function _assert(machine, exp) {
    if (! exp) {
        _signalError(machine);
    }
}

function _signalError(machine) {
    var sheet = simulationSheet();
    var range = sheet.getRange(machine.step+1, 1, 1, 6);
    range.setBackground('red');
    SpreadsheetApp.flush();
    throw "_signalError";
}
