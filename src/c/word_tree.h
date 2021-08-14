#include "vector.h"

typedef struct word_tree_t {
	unsigned int depth;
	struct word_tree_t * children[10];
	vector_t * words;
} word_tree_t;

void wt_init(word_tree_t *, unsigned int);
void wt_add(word_tree_t *, char *);
void wt_find_words(word_tree_t *, vector_t *, unsigned int, vector_t **, unsigned int);