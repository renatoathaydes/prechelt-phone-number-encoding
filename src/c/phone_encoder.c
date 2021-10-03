/*
	Implements the same solution as MyMain2.java.
*/

#include "word_tree.h"
#include <stdio.h>
#include <string.h>
#include <time.h>

const int MODE_PRINT = 0;
const int MODE_COUNT = 1;

int mode;
int max_word_length = 0;
int min_word_lenth = 100;
word_tree_t dict;
char * current_number;
int solution_count;

void solve(char *);

int main(int argc, char * argv[]) {
	/*
	clock_t t1, t2;
	t1 = clock();
	*/

	wt_init(&dict, 0);

	mode = argc > 1 && strcmp(argv[1], "count") == 0 ? MODE_COUNT : MODE_PRINT;
	char * dict_file_name = argc > 2 ? argv[2] : "tests/words.txt";
	char * numbers_file_name = argc > 3 ? argv[3] : "tests/numbers.txt";

	FILE * dict_file = fopen(dict_file_name, "r");
	if(dict_file == NULL) {
  		perror("Error opening dictionary file");
  		return(-1);
	}

	fseek(dict_file, 0, SEEK_END);
	long dict_file_size = ftell(dict_file);
	char * buffer = malloc(dict_file_size + 1);
	fseek(dict_file, 0, SEEK_SET);
	int elements_read = fread(buffer, 1, dict_file_size, dict_file);
	fclose(dict_file);

	char * current_word = buffer;
	for (int i = 0; i < dict_file_size; i++) {
		if (buffer[i] == '\n') {
			buffer[i] = '\0';
			int length = strlen(current_word);
			if (length > max_word_length)
				max_word_length = length;
			if (length < min_word_lenth)
				min_word_lenth = length;
			wt_add(&dict, current_word);
			current_word = buffer + i + 1;
		}
	}

	FILE * numbers_file = fopen(numbers_file_name, "r");
	if(numbers_file == NULL) {
  		perror("Error opening input file");
 		return(-1);
   	}

   	fseek(numbers_file, 0, SEEK_END);
	long numbers_file_size = ftell(numbers_file);
	buffer = malloc(numbers_file_size + 1);
	fseek(numbers_file, 0, SEEK_SET);
	elements_read = fread(buffer, 1, numbers_file_size, numbers_file);
	fclose(numbers_file);

	current_number = buffer;
	for (int i = 0; i < numbers_file_size; i++) {
		if (buffer[i] == '\n') {
			buffer[i] = '\0';
			solve(current_number);
			current_number = buffer + i + 1;
		}
	}

	if (mode == MODE_COUNT) printf("%d\n", solution_count);

	/*
   	t2 = clock();
	double cpu_time_used = ((double) (t2 - t1)) / (CLOCKS_PER_SEC / 1000);
	printf("Time: %f ms\n", cpu_time_used);
	*/
}

void load_phone_number(char * phone_number_str, vector_t * phone_number) {
	for (int i = 0; i < strlen(phone_number_str); i++) {
		if (phone_number_str[i] > 47 && phone_number_str[i] < 58) {
			int * digit = malloc(sizeof(int));
			*digit = phone_number_str[i] - 48;
			vector_push(phone_number, digit);
		}
	}
}

void print_word_list(vector_t * word_list) {
	printf("%s: %s", current_number, (char *) vector_get(word_list, 0));
	for (int i = 1; i < word_list->length; i++) {
		printf(" %s", (char *) vector_get(word_list, i));
	}
	printf("\n");
}

void print_solutions_rec(unsigned int i, vector_t * part_solution, vector_t * solution_matrix) {
	if (i == solution_matrix->length) {
		print_word_list(part_solution);
		return;
	}

	vector_t * row = (vector_t *) vector_get(solution_matrix, i);
	for (int j = 0; j < row->length; j++) {
		vector_push(part_solution, (char *) vector_get(row, j));
		print_solutions_rec(i + 1, part_solution, solution_matrix);
		vector_pop(part_solution);
	}
}

void print_solutions(vector_t * solution_matrix) {
	vector_t * part_solution = malloc(sizeof(vector_t));
	vector_init(part_solution, sizeof(char *), 10);
	print_solutions_rec(0, part_solution, solution_matrix);
	vector_free(part_solution);
	free(part_solution);
}

void count_solutions(vector_t * solution_matrix) {
	int count = 1;
	for (int i = 0; i < solution_matrix->length; i++) {
		count *= ((vector_t *) vector_get(solution_matrix, i))->length;
	}
	solution_count += count;
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
		if (last_was_digit)
			return;
		vector_t * word_list = malloc(sizeof(vector_t));
		vector_init(word_list, sizeof(char *), 2);
		char * buffer = calloc(2, sizeof(char));
		sprintf(buffer, "%d", * (int *) vector_get(phone_number, digit_pos));
		vector_push(word_list, buffer);
		words[0] = word_list;
	}

	for (int i = 0; i < max_word_length - 1; i++) {
		if (words[i] != NULL) {
			vector_push(part_solution, words[i]);
			if (digit_pos + i + 1== phone_number->length) {
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
	if (phone_number->length == 1 && min_word_lenth > 1) {
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
		(phone_number->length > 1 && phone_number->length < min_word_lenth)) {
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