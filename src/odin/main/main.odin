package main

import "core:bufio"
import "core:bytes"
import "core:fmt"
import "core:io"
import "core:os"
import "core:slice"
import "core:unicode"

import "core:testing"

key_len :: 56
KEY_T :: [key_len]byte

Operation :: enum {
	PRINT,
	COUNT,
}

/// Custom Hash Map to make looking up entries efficient.
/// The default Odin map does not allow customizing the hash function
/// or the key comparison algorithm.
/// This makes the algorithm 10x slower than in other languages that
/// allow it. In Odin, the same can only be achieve by using a custom
/// Map implementation, as done below:

DictionaryKey :: struct {
	raw_key: KEY_T,
	length:  uint,
	hash:    u64,
}

append_to_key :: #force_inline proc(key: ^DictionaryKey, b: byte) {
	key.raw_key[key.length] = b
	key.length += 1
	key.hash = hasher(b, key.hash)
}

set_key_hash :: proc(key: ^DictionaryKey) {
	for i: uint = 0; i < key.length; i += 1 {
		key.hash = hasher(key.raw_key[i], key.hash)
	}
}

@(test)
append_to_key_test :: proc(t: ^testing.T) {
	k1: DictionaryKey
	append_to_key(&k1, 'b')
	append_to_key(&k1, 'a')
	append_to_key(&k1, 'r')

	k2: DictionaryKey
	append_to_key(&k2, 'b')
	append_to_key(&k2, 'a')
	append_to_key(&k2, 'r')

	testing.expectf(t, k1 == k2, "\nA: %v\nE: %v", k1, k2)
	testing.expectf(t, eq_keys(&k1, &k2), "\nA: %v\nE: %v", k1, k2)

	append_to_key(&k2, 'a')

	testing.expectf(t, k1 != k2, "\nA: %v\nE: %v", k1, k2)
	testing.expectf(t, !eq_keys(&k1, &k2), "\nA: %v\nE: %v", k1, k2)
}

DictionaryEntry :: struct {
	key:   ^DictionaryKey,
	value: [dynamic]string,
}

Dictionary :: struct {
	data: [4096][dynamic]^DictionaryEntry,
}

hasher :: #force_inline proc(b: byte, prev: u64) -> u64 {
	return u64(b) | (prev << 4)
}

eq_keys :: #force_inline proc(k1: ^DictionaryKey, k2: ^DictionaryKey) -> bool {
	return slice.equal(k1.raw_key[:k1.length], k2.raw_key[:k2.length])
}

get_entry :: #force_inline proc(
	key: ^DictionaryKey,
	entries: ^[dynamic]^DictionaryEntry,
) -> ^DictionaryEntry {
	for &entry in entries {
		if eq_keys(entry.key, key) {
			return entry
		}
	}
	return nil
}

get_or_add_entry :: proc(
	key: ^DictionaryKey,
	entries: ^[dynamic]^DictionaryEntry,
) -> ^DictionaryEntry {
	for &entry in entries {
		if eq_keys(entry.key, key) {
			return entry
		}
	}
	entry := new(DictionaryEntry)
	entry.key = key
	entry.value = make([dynamic]string, 0, 8)
	append(entries, entry)
	return entry
}

add_to_dictionary :: proc(dict: ^Dictionary, key: ^DictionaryKey, value: string) {
	index := int(key.hash % len(dict.data))
	entry := get_or_add_entry(key, &dict.data[index])
	append(&entry.value, value)
}

find_in_dictionary :: proc(dict: ^Dictionary, key: ^DictionaryKey) -> []string {
	index := int(key.hash % len(dict.data))
	entry := get_entry(key, &dict.data[index])
	if (entry == nil) {
		return nil
	}
	return entry.value[:]
}

@(test)
dictionary_test :: proc(t: ^testing.T) {
	dict := new(Dictionary)
	defer free(dict)
	k1: DictionaryKey
	append_to_key(&k1, 'b')
	append_to_key(&k1, 'a')
	append_to_key(&k1, 'r')
	add_to_dictionary(dict, &k1, "foo")
	v1 := find_in_dictionary(dict, &k1)
	expected := []string{"foo"}
	testing.expectf(t, slice.equal(expected, v1), "\nA: %v\nE: %v", v1, expected)

	k2: DictionaryKey
	append_to_key(&k2, 'b')
	v2 := find_in_dictionary(dict, &k2)
	testing.expectf(t, v2 == nil, "\nA: %v\nE: %v", v2, nil)
}

///// Phone-Encoder Solution:

main :: proc() {
	using Operation
	argc := len(os.args)
	op := PRINT if argc < 2 else operation(os.args[1])
	numbers := "tests/numbers.txt" if argc < 4 else os.args[3]
	dict := load_dictionary("tests/words.txt" if argc < 3 else os.args[2])
	start(op, dict, numbers)
}

start :: proc(op: Operation, dict: ^Dictionary, numbers_path: string) {
	out := BufferedWriter{}
	defer flush_buffer(&out)
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
	words: [dynamic]string
	digits_bytes: [64]byte
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
		print_translations(dict, line, digits, &words, op, &count, &out)
	}
	if op == .COUNT {
		fmt.println(count)
	}
}

operation :: proc(name: string) -> Operation {
	using Operation
	switch name {
	case "print":
		return PRINT
	case "count":
		return COUNT
	case:
		panic("unknown operation, must be 'print' or 'count'")
	}
}

letter_to_digit :: proc "contextless" (ch: rune) -> byte {
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
		unreachable()
	}
}

word_to_number :: proc(word: []byte, result: []byte) -> []byte {
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
word_to_number_test :: proc(t: ^testing.T) {
	expected := []byte{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
	result: KEY_T
	word := "EjRsfAiULg"
	actual := word_to_number(transmute([]byte)word, result[:])
	testing.expectf(t, bytes.equal(expected, actual), "\nA: %v\nE: %v", actual, expected)
	// check that the result array was modified
	testing.expectf(
		t,
		bytes.equal(expected, result[:len(actual)]),
		"\nA: %v\nE: %v",
		actual,
		expected,
	)
}

keep_only_digits :: proc(line: []byte, buffer: ^[64]byte) -> []byte {
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
keep_only_digits_test :: proc(t: ^testing.T) {
	word := "a1bcd0eFG34H5"
	expected_digits := "10345"
	expected := transmute([]byte)expected_digits
	buffer: [64]byte
	actual := keep_only_digits(transmute([]byte)word, &buffer)
	testing.expectf(t, bytes.equal(expected, actual), "\nA: %v\nE: %v", actual, expected)
}

load_dictionary :: proc(path: string) -> ^Dictionary {
	dict := new(Dictionary)
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

		key := new(DictionaryKey)
		key_slice := word_to_number(line, key.raw_key[:])
		key.length = len(key_slice)
		set_key_hash(key)
		add_to_dictionary(dict, key, string(bytes.clone(line)))
	}
	return dict
}

ends_with_digit :: proc(words: []string) -> bool {
	if len(words) == 0 {
		return false
	}
	last_word := words[len(words) - 1]
	return len(last_word) == 1 && unicode.is_digit(rune(last_word[0]))
}

print_translations :: proc(
	dict: ^Dictionary,
	number, digits: []byte,
	words: ^[dynamic]string,
	op: Operation,
	count: ^u32,
	out: ^BufferedWriter,
) {
	if len(digits) == 0 {
		show_solution(number, words^[:], count, op, out)
		return
	}
	key: DictionaryKey
	found_word: bool
	for ch, i in digits {
		append_to_key(&key, ch - '0')
		found_words := find_in_dictionary(dict, &key)
		if found_words != nil {
			found_word = true
			for word in found_words {
				append(words, word)
				print_translations(dict, number, digits[i + 1:], words, op, count, out)
				pop(words)
			}
		}
	}
	if !found_word && !ends_with_digit(words^[:]) {
		append(words, string(digits[:1]))
		print_translations(dict, number, digits[1:], words, op, count, out)
		pop(words)
	}
}

BufferedWriter :: struct {
	buffer: [8192]byte,
	pos:    int,
}

write_to_buffer :: proc(bw: ^BufferedWriter, data: []byte) {
	if (len(data) > len(bw.buffer)) {
		os.write(os.stdout, data)
		return
	}
	if bw.pos + len(data) > len(bw.buffer) {
		flush_buffer(bw)
	}
	i := bw.pos
	for b in data {
		bw.buffer[i] = b
		i += 1
	}
	bw.pos = i
}

flush_buffer :: proc(bw: ^BufferedWriter) {
	if bw.pos > 0 {
		os.write(os.stdout, bw.buffer[:bw.pos])
		bw.pos = 0
	}
}

show_solution :: proc(
	number: []byte,
	words: []string,
	count: ^u32,
	op: Operation,
	out: ^BufferedWriter,
) {
	switch op {
	case .PRINT:
		// Write number followed by ": "
		write_to_buffer(out, number)
		write_to_buffer(out, {':', ' '})

		final_index := len(words) - 1
		for word, index in words {
			write_to_buffer(out, transmute([]byte)word)
			if index == final_index {
				write_to_buffer(out, {'\n'})
			} else {
				write_to_buffer(out, {' '})
			}
		}
	case .COUNT:
		count^ += 1
	}
}
