/*
	Implements the same solution as MyMain2.java.
*/

#include "word_tree.h"
#include <stdio.h>
#include <string.h>

const int MODE_PRINT = 0;
const int MODE_COUNT = 1;

int mode;
int max_word_length = 0;
int min_word_length = 100;
word_tree_t dict;
char * current_number;
int solution_count;
char print_buffer[4100];
char * print_buffer_end = print_buffer;
vector_t * digit_chars[10];

void solve(char *);

int main(int argc, char * argv[]) {
	wt_init(&dict, 0);

	for (int i = 0; i < 10; i++) {
		vector_t * digit_vec = malloc(sizeof(vector_t));
		vector_init(digit_vec, sizeof(char *), 1);
		char * digit_char = calloc(2, sizeof(char));
		*digit_char = (char)(i + 48);
		vector_push(digit_vec, digit_char);
		digit_chars[i] = digit_vec;
	}

	mode = argc > 1 && strcmp(argv[1], "count") == 0 ? MODE_COUNT : MODE_PRINT;
	char * dict_file_name = argc > 2 ? argv[2] : "tests/words.txt";
	char * numbers_file_name = argc > 3 ? argv[3] : "tests/numbers.txt";

	FILE * dict_file = fopen(dict_file_name, "r");
	if(dict_file == NULL) {
  		perror("Error opening dictionary file");
  		return -1;
	}

	fseek(dict_file, 0, SEEK_END);
	long dict_file_size = ftell(dict_file);
	char * buffer = malloc(dict_file_size + 1);
	fseek(dict_file, 0, SEEK_SET);
	int elements_read = fread(buffer, 1, dict_file_size, dict_file);
	fclose(dict_file);

	char * current_word = buffer;
	int length = 0;
	for (int i = 0; i < dict_file_size; i++) {
		if (buffer[i] == '\r') {
			buffer[i] = '\0';
		} else if (buffer[i] == '\n') {
			buffer[i] = '\0';
			if (length > max_word_length)
				max_word_length = length;
			int clean_length;
			char * clean = wt_clean_word(current_word, &clean_length, length);
			if (clean_length < min_word_length)
				min_word_length = clean_length;
			wt_add(&dict, current_word, clean, clean_length);
			current_word = buffer + i + 1;
			length = 0;
		} else {
			length++;
		}
	}

	char * dict_buffer = buffer;

	FILE * numbers_file = fopen(numbers_file_name, "r");
	if(numbers_file == NULL) {
  		perror("Error opening input file");
 		return -1;
   	}

   	fseek(numbers_file, 0, SEEK_END);
	long numbers_file_size = ftell(numbers_file);
	buffer = malloc(numbers_file_size + 1);
	fseek(numbers_file, 0, SEEK_SET);
	elements_read = fread(buffer, 1, numbers_file_size, numbers_file);
	fclose(numbers_file);

	current_number = buffer;
	for (int i = 0; i < numbers_file_size; i++) {
		if (buffer[i] == '\r') {
			buffer[i] = '\0';
		}
		if (buffer[i] == '\n') {
			buffer[i] = '\0';
			solve(current_number);
			current_number = buffer + i + 1;
		}
	}

	if (mode == MODE_COUNT) printf("%d\n", solution_count);
	else if (mode == MODE_PRINT) {
		*print_buffer_end = '\0';
		fputs(print_buffer, stdout);
	}
}

void load_phone_number(char * phone_number_str, vector_t * phone_number) {
	for (int i = 0; phone_number_str[i] != '\0'; i++) {
		if (phone_number_str[i] > 47 && phone_number_str[i] < 58) {
			int * digit = malloc(sizeof(int));
			*digit = phone_number_str[i] - 48;
			vector_push(phone_number, digit);
		}
	}
}

void print_word_list(char ** word_list, int length) {
	print_buffer_end = stpcpy(print_buffer_end, current_number);
	*(print_buffer_end++) = ':';
	*(print_buffer_end++) = ' ';
	print_buffer_end = stpcpy(print_buffer_end, word_list[0]);
	for (int i = 1; i < length; i++) {
		*(print_buffer_end++) = ' ';
		print_buffer_end = stpcpy(print_buffer_end, word_list[i]);
	}
	*(print_buffer_end++) = '\n';
	if (print_buffer_end - print_buffer > 3900) {
		*print_buffer_end = '\0';
		fputs(print_buffer, stdout);
		print_buffer_end = print_buffer;
	}
}

void print_solutions_rec(unsigned int i, char ** part_solution, vector_t * solution_matrix) {
	if (i == solution_matrix->length) {
		print_word_list(part_solution, solution_matrix->length);
		return;
	}

	vector_t * row = (vector_t *) vector_get(solution_matrix, i);
	for (int j = 0; j < row->length; j++) {
		part_solution[i] = (char *) vector_get(row, j);
		print_solutions_rec(i + 1, part_solution, solution_matrix);
	}
}

void print_solutions(vector_t * solution_matrix) {
	char ** part_solution = malloc(sizeof(char *) * solution_matrix->length);
	print_solutions_rec(0, part_solution, solution_matrix);;
	free(part_solution);
}

void count_solutions_rec(unsigned int i, char ** part_solution, vector_t * solution_matrix) {
	if (i == solution_matrix->length) {
		solution_count++;
		return;
	}

	vector_t * row = (vector_t *) vector_get(solution_matrix, i);
	for (int j = 0; j < row->length; j++) {
		part_solution[i] = (char *) vector_get(row, j);
		count_solutions_rec(i + 1, part_solution, solution_matrix);
	}
}

void count_solutions(vector_t * solution_matrix) {
	/*int count = 1;
	for (int i = 0; i < solution_matrix->length; i++) {
		count *= ((vector_t *) vector_get(solution_matrix, i))->length;
	}
	solution_count += count;*/
	char ** part_solution = malloc(sizeof(char *) * solution_matrix->length);
	count_solutions_rec(0, part_solution, solution_matrix);
	free(part_solution);
}

void solve_rec(vector_t * phone_number, unsigned int digit_pos, vector_t * part_solution,
		unsigned int last_was_digit) {
	vector_t * words[max_word_length - 1];
	wt_find_words(&dict, phone_number, digit_pos, words, max_word_length);
	int words_found = 0;
	for (int i = 0; i < max_word_length - 1; i++) {
		if (words[i] != NULL && words[i]->length > 0) {
			words_found = 1;
			break;
		}
	}

	// If no word was found, we add the digit to the solution
	if (!words_found) {
		// We can't have two consecutive digits in the solution
		if (last_was_digit) return;
		words[0] = digit_chars[* (int *) vector_get(phone_number, digit_pos)];
	}

	for (int i = 0; i < max_word_length - 1; i++) {
		if (words[i] != NULL) {
			vector_push(part_solution, words[i]);
			if (digit_pos + i + 1 == phone_number->length) {
				if (mode == MODE_PRINT) print_solutions(part_solution);
				else if (mode == MODE_COUNT) count_solutions(part_solution);
			} else {
				solve_rec(phone_number, digit_pos + i + 1, part_solution, !words_found);
			}
			vector_pop(part_solution);
		}
	}
}

void solve(char * phone_number_str) {
	vector_t * phone_number = malloc(sizeof(vector_t));
	vector_init(phone_number, sizeof(int *), 50);
	load_phone_number(phone_number_str, phone_number);

	// If the phone number has only one digit and the shortest word has more than
	// one letter, the only solution is the digit itself
	if (phone_number->length == 1 && min_word_length > 1) {
		if (mode == MODE_PRINT)
			printf("%s: %d\n", phone_number_str, * (int *) vector_get(phone_number, 0));
		else if (mode == MODE_COUNT)
			solution_count += 1;
		vector_free(phone_number);
		free(phone_number);
		return;
	}

	// If the phone number is empty or the phone number has more than one digits but
	// less than the length of the shortest word, there is no solution
	if (phone_number->length == 0 || 
		(phone_number->length > 1 && phone_number->length < min_word_length)) {
		vector_free(phone_number);
		free(phone_number);
		return;
	}

	vector_t * part_solution = malloc(sizeof(vector_t));
	vector_init(part_solution, sizeof(vector_t *), 10);
	solve_rec(phone_number, 0, part_solution, 0);

	vector_free(part_solution);
	free(part_solution);
	vector_free(phone_number);
	free(phone_number);
}