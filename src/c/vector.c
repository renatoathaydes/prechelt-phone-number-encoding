#include "vector.h"
#include <stdlib.h>
#include <stdio.h>

void vector_init(vector_t * vector, size_t element_size, unsigned int init_allocated) {
	vector->array = malloc(init_allocated * element_size);
	vector->length = 0;
	vector->allocated = init_allocated;
	vector->element_size = element_size;
}

void vector_expand(vector_t * vector) {
	unsigned int allocated_expanded = vector->allocated << 1;
	vector->allocated = allocated_expanded;
	void ** reallocated_array = realloc(vector->array, allocated_expanded  * vector->element_size);

	if (reallocated_array == NULL) {
		reallocated_array = malloc(allocated_expanded  * vector->element_size);
		for (int i = 0; i < vector->length; i++) {
			reallocated_array[i] = vector->array[i]; 
		}
		free(vector->array);
	}

	vector->array = reallocated_array;
}

void vector_shrink(vector_t * vector) {
	unsigned int allocated_shrunken = vector->allocated >> 1;
	vector->allocated = allocated_shrunken;
	void ** reallocated_array = realloc(vector->array, allocated_shrunken  * vector->element_size);

	if (reallocated_array == NULL) {
		reallocated_array = malloc(allocated_shrunken  * vector->element_size);
		for (int i = 0; i < vector->length; i++) {
			reallocated_array[i] = vector->array[i]; 
		}
		free(vector->array);
	}

	vector->array = reallocated_array;
}

void vector_push(vector_t * vector, void * element) {
	if (vector->length == vector->allocated) {
		vector_expand(vector);
	}
	vector->array[vector->length++] = element;
}

void * vector_pop(vector_t * vector) {
	void * element = vector->array[--vector->length];
	if (vector->length < vector->allocated >> 1) {
		vector_shrink(vector);
	}
	return element;
}

void vector_clear(vector_t * vector) {
	vector->length = 0;
}

void * vector_get(vector_t * vector, unsigned int index) {
	return vector->array[index];
}

void vector_free(vector_t * vector) {
	for (int i = 0; i < vector->length; i++) {
		free(vector->array[i]); 
	} 
	free(vector->array);
}