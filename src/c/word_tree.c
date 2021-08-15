#include "word_tree.h"
#include <string.h>

void wt_init(word_tree_t * word_tree, unsigned int depth) {
	vector_t * vector = malloc(sizeof(vector_t));
	vector_init(vector, sizeof(char *), 50);
	word_tree->words = vector;
	word_tree->depth = depth;
	for (int i = 0; i < 10; i++) {
		word_tree->children[i] = NULL;
	}
}

char * wt_clean_word(char * word, int * out_cleaned_length) {
	size_t word_length = strlen(word);
	char * clean_word = malloc((word_length) + 1 * sizeof(char));

	int j = 0;
	for (int i = 0; i < word_length; i++) {
		if (word[i] != '\"') {
			if (word[i] > 64 && word[i] < 91) clean_word[j++] = word[i] ^ 0x20;
			else clean_word[j++] = word[i];
		}
	}

	clean_word[j] = '\0';
	*out_cleaned_length = j;
	return clean_word;
}

int letter_to_digit(char letter) {
	switch (letter) {
		case 'j':
		case 'n':
		case 'q':
			return 1;
		case 'r':
		case 'w':
		case 'x':
			return 2;
		case 'd':
		case 's':
		case 'y':
			return 3;
		case 'f':
		case 't':
			return 4;
		case 'a':
		case 'm':
			return 5;
		case 'c':
		case 'i':
		case 'v':
			return 6;
		case 'b':
		case 'k':
		case 'u':
			return 7;
		case 'l':
		case 'o':
		case 'p':
			return 8;
		case 'g':
		case 'h':
		case 'z':
			return 9;
		default:
			return 0;
	}
}

void wt_add_rec(word_tree_t * word_tree, char * word, char * cleaned_word, unsigned int letter_pos, 
		unsigned int cleaned_length) {
	if (letter_pos == cleaned_length) {
		vector_push(word_tree->words, word);
		return;
	}

	int next_index = letter_to_digit(cleaned_word[letter_pos]);
	if (word_tree->children[next_index] == NULL) {
		word_tree_t * child = malloc(sizeof(word_tree_t));
		wt_init(child, word_tree->depth + 1);
		word_tree->children[next_index] = child;
	}
	wt_add_rec(word_tree->children[next_index], word, cleaned_word, 
		letter_pos + 1, cleaned_length);
}

void wt_add(word_tree_t * word_tree, char * word) {
	int cleaned_length;
	char * cleaned_word = wt_clean_word(word, &cleaned_length);
	wt_add_rec(word_tree, word, cleaned_word, 0, cleaned_length);
	free(cleaned_word);
}

void wt_find_words_rec(word_tree_t * word_tree, vector_t * phone_number, unsigned int digit_pos, 
		vector_t ** words) {
	// If we have already used every digit, we return
	if (digit_pos == phone_number->length) {
		return;
	}

	word_tree_t * child = word_tree->children[* (int *) vector_get(phone_number, digit_pos)];
	// If there is no children corresponding to the digit, no more words can be added
	if (child == NULL) {
		return;
	}

	if (child->words->length > 0) {
		words[word_tree->depth] = child->words;
	}
	wt_find_words_rec(child, phone_number, digit_pos + 1, words);
}

void wt_find_words(word_tree_t * word_tree, vector_t * phone_number, unsigned int digit_pos, 
		vector_t * words[], unsigned int max_word_length) {
	for (int i = 0; i < max_word_length - 1; i++) {
		words[i] = NULL;
	}
	wt_find_words_rec(word_tree, phone_number, digit_pos, words);
}