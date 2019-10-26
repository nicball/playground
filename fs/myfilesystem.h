#ifndef MYFILESYSTEM_H
#define MYFILESYSTEM_H
#include <sys/types.h>
#include <stdint.h>
#include <pthread.h>

// Definitions of FS files.
#define DIRTAB_ENTRY_SIZE 72
#define FILENAME_MAX_SIZE 64 // null charactor included
#define FILE_DATA_BLOCK_SIZE 256
#define HASH_SIZE 16

// Represent FS in memory.
struct fs_object {
    // File descriptors of FS files.
    int file_data_fd;
    int directory_table_fd;
    int hash_data_fd;
    // Size of file_data.
    size_t capacity;
    // Size of unallocated space.
    size_t free_space;
    // Number of dirctory_table entries.
    size_t n_entries;
    // Link entries by increasing order of file offsets.
    // ent_head is the index of the entry of the left most file.
    // ent_tail is the index of the entry of the right most file.
    // ent_link[ent_head] is the index of the second left most, etc.
    // ent_link[ent_tail] is always -1.
    // Unused elements are arbitrary.
    // If there're no entries, ent_head = ent_tail = -1.
    ssize_t * ent_link;
    ssize_t ent_head;
    ssize_t ent_tail;
    // Bitmap for entries. 1 means unused.
    uint32_t * ent_bitmap;
    // Mutex for FS operations.
    pthread_mutex_t global_fs_lock;
};

// Utilily functions.
void truncate_filename(const char* filename, char output[FILENAME_MAX_SIZE]);
struct entry {
    char filename[FILENAME_MAX_SIZE];
    size_t offset;
    size_t length;
};
void read_entry(struct fs_object * fs, size_t index, struct entry * output);
void write_entry(struct fs_object * fs, size_t index, const struct entry * data);
ssize_t * find_file(struct fs_object * fs, const char filename[FILENAME_MAX_SIZE], struct entry * ent);
void read_hash(struct fs_object * fs, size_t index, uint8_t output[HASH_SIZE]);
void write_hash(struct fs_object * fs, size_t index, const uint8_t hash[HASH_SIZE]);

void * init_fs(char * f1, char * f2, char * f3, int n_processors);

void close_fs(void * helper);

int create_file(char * filename, size_t length, void * helper);

int resize_file(char * filename, size_t length, void * helper);

void repack(void * helper);

int delete_file(char * filename, void * helper);

int rename_file(char * oldname, char * newname, void * helper);

int read_file(char * filename, size_t offset, size_t count, void * buf, void * helper);

int write_file(char * filename, size_t offset, size_t count, void * buf, void * helper);

ssize_t file_size(char * filename, void * helper);

void fletcher(uint8_t * buf, size_t length, uint8_t * output);

void compute_hash_tree(void * helper);

void compute_hash_block(size_t block_offset, void * helper);

#endif
