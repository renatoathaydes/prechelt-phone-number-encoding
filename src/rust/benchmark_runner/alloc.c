/*
 * Tiny Program that allocates a number of bytes, then sleeps for an amount of time.
 * Args:
 *    - <byte>    how many bytes to allocate
 *    - <time>    time to sleep in seconds
 *
 * Author: Renato Athaydes
*/
#include <stdio.h>
#include <stdlib.h>
#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#endif

int main(int argc, char **argv) {
    char* ptr;
    int i, mem, time;

    if (argc != 3) {
        printf("Please provide 2 arguments, <memory-to-allocate> <time-to-sleep>\n");
        exit(1);
    }

    mem = atoi(argv[1]);
    time = atoi(argv[2]);
    printf("Allocating %d bytes\n", mem);
    ptr = (char*) malloc(mem * sizeof(char));
    if (ptr == NULL) {
        printf("Memory could not be allocated.\n");
        exit(1);
    }
    for (i = 0; i < mem; ++i) {
        ptr[i] = '0';
    }
    printf("Memory successfully allocated. Sleeping for %d seconds....\n", time);
    sleep(time);
    for (i = 0; i < mem; ++i) {
        if (ptr[i] != '0') {
            printf("Memory corrupted\n");
            exit(2);
        }
    }
    free(ptr);
    printf("Done\n");
    return 0;
}