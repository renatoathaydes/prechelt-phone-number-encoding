#include <stdlib.h>

typedef struct {
	void ** array;
	unsigned int length;
	unsigned int allocated;
	size_t element_size;
} vector_t;

void vector_init(vector_t *, size_t, unsigned int);
void vector_push(vector_t *, void *);
void * vector_pop(vector_t *);
void vector_clear(vector_t *);
void * vector_get(vector_t *, unsigned int);
void vector_free(vector_t *);