#+feature dynamic-literals

package main

import "core:fmt"
import "core:unicode"
import "core:os"
import "core:bufio"
import "core:strings"
import "core:bytes"
import "core:io"

import "core:testing"

key_len :: 56
key_uninit :: 16
KEY_T :: [key_len]byte

Operation :: enum { PRINT, COUNT }

main :: proc() {
    using Operation
    argc := len(os.args)
    op := PRINT if argc < 2 else operation(os.args[1])
    numbers := "tests/numbers.txt" if argc < 4 else os.args[3]
    dict : = load_dictionary("tests/words.txt" if argc < 3 else os.args[2])
    start(op, dict, numbers)
}

start :: proc (op: Operation, dict: map[KEY_T][dynamic][]byte, numbers_path: string) {
    f, err := os.open(numbers_path)
    if err != os.ERROR_NONE {
        fmt.println("Cannot open the numbers file")
        os.exit(1)
    }
    defer os.close(f)
    reader: bufio.Reader
    buffer: [64]byte
    bufio.reader_init_with_buf(&reader, os.stream_from_handle(f), buffer[:])
    defer bufio.reader_destroy(&reader)

    count: u32
    words: [dynamic][]byte
    digits_bytes : [64]byte
    for {
	line, err := bufio.reader_read_slice(&reader, '\n')
	if err != nil {
            if err == io.Error.EOF {
                break
            }
	    fmt.println("error reading number line: %v", err)
            os.exit(2)
	}
        line = line[:len(line) - 1]
        digits := keep_only_digits(line, &digits_bytes)
        print_translations(dict, line, digits, &words, op, &count)
    }
    if op == .COUNT {
        fmt.println(count)
    }
}

operation :: proc (name: string) -> Operation {
    using Operation
    switch name {
    case "print": return PRINT
    case "count": return COUNT
    case: panic("unknown operation, must be 'print' or 'count'") 
    }
}

letter_to_digit :: proc (ch: rune) -> byte {
    switch ch {
    case 'e', 'E':
        return 0
    case 'j', 'J', 'n', 'N', 'q', 'Q':
        return 1
    case 'r', 'R', 'w', 'W', 'x', 'X':
        return 2
    case 'd', 'D', 's', 'S', 'y', 'Y':
        return 3
    case 'f', 'F', 't', 'T':
        return 4
    case 'a', 'A', 'm', 'M':
        return 5
    case 'c', 'C', 'i', 'I', 'v', 'V':
        return 6
    case 'b', 'B', 'k', 'K', 'u', 'U':
        return 7
    case 'l', 'L', 'o', 'O', 'p', 'P':
        return 8
    case 'g', 'G', 'h', 'H', 'z', 'Z':
        return 9
    case:
        panic("cannot handle char")
    }
}

word_to_number :: proc (word: []byte, result: []byte) -> []byte {
    index := 0
    for b in word {
        ch := rune(b)
        if unicode.is_letter(ch) {
            l := letter_to_digit(ch)
            result[index] = l
            index += 1
        }
    }
    return result[0:index]
}

@(test)
word_to_number_test :: proc (t: ^testing.T) {
    expected := []byte{0,1,2,3,4,5,6,7,8,9}
    result : KEY_T
    word := "EjRsfAiULg"
    actual := word_to_number(transmute([]byte) word, result[:])
    testing.expectf(t, bytes.equal(expected, actual), "\nA: %v\nE: %v", actual, expected)
    // check that the result array was modified
    testing.expectf(t, bytes.equal(expected, result[:len(actual)]), "\nA: %v\nE: %v", actual, expected)
}

keep_only_digits :: proc (line: []byte, buffer: ^[64]byte) -> []byte {
    index := 0
    for b in line {
        ch := rune(b)
        if unicode.is_digit(ch) {
            buffer[index] = b
            index += 1
        }
    }
    return buffer[:index]
}

@(test)
keep_only_digits_test :: proc (t: ^testing.T) {
    word := "a1bcd0eFG34H5"
    expected_digits := "10345"
    expected := transmute([]byte) expected_digits
    buffer : [64]byte
    actual := keep_only_digits(transmute([]byte) word, &buffer)
    testing.expectf(t, bytes.equal(expected, actual), "\nA: %v\nE: %v", actual, expected)
}

load_dictionary :: proc (path: string) -> map[KEY_T][dynamic][]byte {
    dict := make(map[KEY_T][dynamic][]byte)
    f, err := os.open(path)
    if err != os.ERROR_NONE {
        fmt.println("Cannot open the dictionary file")
        os.exit(1)
    }
    defer os.close(f)
    reader: bufio.Reader
    buffer: [4096]byte
    bufio.reader_init_with_buf(&reader, os.stream_from_handle(f), buffer[:])
    defer bufio.reader_destroy(&reader)
    for {
	line, err := bufio.reader_read_slice(&reader, '\n')
	if err != nil {
            if err == io.Error.EOF {
                break
            }
	    fmt.println("error reading line: %v", err)
            os.exit(2)
	}
	line = line[:len(line) - 1]

        key := new(KEY_T)
        for _, i in key {
            key[i] = key_uninit
        }
        word_to_number(line, key[:])
        entries, ok := dict[key^]
        if !ok {
            entries = make([dynamic][]byte)
        }
        append(&entries, bytes.clone(line))
        dict[key^] = entries
    }
    return dict
}

ends_with_digit :: proc (words: [][]byte) -> bool {
    if len(words) == 0 {
        return false
    }
    last_word := words[len(words) - 1]
    return len(last_word) == 1 && unicode.is_digit(rune(last_word[0]))
}

print_translations :: proc (dict: map[KEY_T][dynamic][]byte, number, digits: []byte, words: ^[dynamic][]byte, op: Operation, count: ^u32) {
    if len(digits) == 0 {
        show_solution(number, words^[:], count, op)
        return
    }
    key : KEY_T = [?]byte {0..<key_len = key_uninit}
    found_word: bool
    for ch, i in digits {
        key[i] = ch - '0'
        found_words, ok := dict[key]
        if ok {
            found_word = true
            for word in found_words {
                append(words, word)
                print_translations(dict, number, digits[i + 1:], words, op, count)
                pop(words)
            }
        }
    }
    if !found_word && !ends_with_digit(words^[:]) {
        append(words, digits[:1])
        print_translations(dict, number, digits[1:], words, op, count)
        pop(words)
    }
}

show_solution :: proc (number: []byte, words: [][]byte, count: ^u32, op: Operation) {
    switch op {
    case .PRINT:
        fmt.printf("%s: ", number)
        final_index := len(words) - 1
        for word, index in words {
            if index == final_index {
                fmt.printfln("%s", word)
            } else {
                fmt.printf("%s ", word)    
            }
        }
    case .COUNT:
        count^ += 1
    }
}
