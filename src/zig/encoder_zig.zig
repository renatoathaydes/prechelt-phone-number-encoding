const std = @import("std");

const List = std.ArrayList([]const u8);
const Map = std.AutoHashMap([50]u4, List);
const Allocator = std.mem.Allocator;

const ArgError = error{
    InvalidPrintOrCountArg,
};

const expectEqual = std.testing.expectEqual;

const allocator: Allocator = init: {
    // var buffer: [10 * 1000 * 1024]u8 = undefined;
    // var fba = std.heap.FixedBufferAllocator.init(&buffer);
    // break :init fba.allocator();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    break :init gpa.allocator();
};

fn charToDigit(char: u8) u4 {
    const c = if (std.ascii.isUpper(char)) char + 32 else char;
    return switch (c) {
        'e' => 0,
        'j', 'n', 'q' => 1,
        'r', 'w', 'x' => 2,
        'd', 's', 'y' => 3,
        'f', 't' => 4,
        'a', 'm' => 5,
        'c', 'i', 'v' => 6,
        'b', 'k', 'u' => 7,
        'l', 'o', 'p' => 8,
        'g', 'h', 'z' => 9,
        else => unreachable,
    };
}

fn wordToNumber(word: []const u8) [50]u4 {
    var num: [50]u4 = [_]u4{0} ** 50;
    var index = @as(u8, 0);
    for (word) |char| {
        if (std.ascii.isAlpha(char)) {
            num[index] = charToDigit(char);
            index += 1;
        }
    }
    return num;
}

test "wordToNumber" {
    var expected: [50]u4 = [_]u4{0} ** 50;
    try expectEqual(expected, wordToNumber(""));
    expected[0] = 5;
    try expectEqual(expected, wordToNumber("a"));
    var i = @as(u8, 0);
    while (i < 10) : (i += 1) {
        expected[i] = @intCast(u4, i);
    }
    try expectEqual(expected, wordToNumber("ejrdfacblg"));
    while (i < 50) : (i += 1) {
        expected[i] = @intCast(u4, i % 10);
    }
    try expectEqual(expected, wordToNumber("ejrdfacblgEJRDFACBLGenwstmikohEQXYTMVUPZejrdfacblg"));
}

fn nthDigit(digits: []const u8, index: usize) u4 {
    return @intCast(u4, digits[index] - '0');
}

const handlerFn = fn (phone_num: []const u8, words: *const List) usize;

const stdout = std.io.getStdOut();

fn printResult(phone_num: []const u8, words: *const List) usize {
    const writer = stdout.writer();
    writer.print("{s}:", .{phone_num}) catch unreachable;
    for (words.items) |word| {
        writer.print(" {s}", .{word}) catch unreachable;
    }
    _ = writer.writeAll("\n") catch unreachable;
    return @as(usize, 0);
}

const countResult = struct {
    var __count: usize = 0;

    fn countResult(_: []const u8, words: *const List) usize {
        if (words.items.len > 0) {
            __count += 1;
        }
        return __count;
    }
}.countResult;

fn isNumeric(s: []const u8) bool {
    return s.len == 1 and std.ascii.isDigit(s[0]);
}

fn findTranslations(handler: handlerFn, dictionary: *const Map, phone_num: []const u8, digits: []const u8, start: usize, words: *List) Allocator.Error!void {
    if (start >= digits.len) {
        _ = handler(phone_num, words);
        return;
    }
    var found_word = false;
    // find all translations recursively
    {
        var index = start;
        var num: [50]u4 = [_]u4{0} ** 50;
        while (index < digits.len) : (index += 1) {
            num[index - start] = nthDigit(digits, index);
            if (dictionary.get(num)) |dict_words| {
                found_word = true;
                for (dict_words.items) |word| {
                    try words.append(word);
                    try findTranslations(handler, dictionary, phone_num, digits, index + 1, words);
                    _ = words.pop();
                }
            }
        }
    }
    // if word not found at this level, try to substitute with a digit and recurse
    const word_count = words.items.len;
    if (!found_word and (word_count == 0 or !isNumeric(words.items[word_count - 1]))) {
        // TODO no need to allocate a word for the 0-9 digits! Use static strings.
        const digit = try allocator.alloc(u8, 1);
        defer allocator.free(digit);
        digit[0] = digits[start];
        try words.append(digit);
        try findTranslations(handler, dictionary, phone_num, digits, start + 1, words);
        _ = words.pop();
    }
}

fn loadDictionary(dict: []const u8, size: usize) !Map {
    _ = size; // pre-allocate Map space?
    var result = Map.init(allocator);
    const max_bytes_per_line: usize = 1024;
    var file = try std.fs.cwd().openFile(dict, .{});
    defer file.close();
    var reader = std.io.bufferedReader(file.reader()).reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', max_bytes_per_line)) |line| {
        if (line.len > 0) {
            var entry = try result.getOrPut(wordToNumber(line));
            if (!entry.found_existing) {
                entry.value_ptr.* = try List.initCapacity(allocator, 12);
            }
            try entry.value_ptr.*.append(line);
        }
    }
    return result;
}

fn toDigits(word: []const u8) ![]const u8 {
    var result = try allocator.alloc(u8, word.len);
    var i = @as(usize, 0);
    for (word) |char| {
        if (std.ascii.isDigit(char)) {
            result[i] = char;
            i += 1;
        }
    }
    return result[0..i];
}

pub fn main() !void {
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.next(); // program path

    const print_or_count = args.next() orelse "print";
    const dict = args.next() orelse "tests/words.txt";
    const nums = args.next() orelse "tests/numbers.txt";

    const handler_fn = if (std.mem.eql(u8, "print", print_or_count))
        printResult
    else if (std.mem.eql(u8, "count", print_or_count))
        countResult
    else
        return error.InvalidPrintOrCountArg;

    const dictionary = try loadDictionary(dict, 100);

    // read phone numbers file, handling solutions as we find them
    {
        const max_bytes_per_line = 128;
        var file = try std.fs.cwd().openFile(nums, .{});
        defer file.close();
        var reader = std.io.bufferedReader(file.reader()).reader();
        while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', max_bytes_per_line)) |phone_number| {
            defer allocator.free(phone_number);
            if (phone_number.len > 0) {
                var words = try List.initCapacity(allocator, 12);
                defer words.deinit();
                const digits = try toDigits(phone_number);
                defer allocator.free(digits);
                try findTranslations(handler_fn, &dictionary, phone_number, digits, 0, &words);
            }
        }
    }

    if (std.mem.eql(u8, "count", print_or_count)) {
        const empty = try List.initCapacity(allocator, 0);
        const count = countResult("", &empty);
        try stdout.writer().print("{d}\n", .{count});
    }
}
