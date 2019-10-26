#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#include <fcntl.h>
#include <unistd.h>

#include "myfilesystem.h"

#define bitsizeof(x) (sizeof(x) * 8)
#define BUFFER_SIZE 8192

// Helper functions.
void load_directory_table(struct fs_object * fs);
void mark_entry_unused(struct fs_object * fs, size_t index);
void mark_entry_used(struct fs_object * fs, size_t index);
ssize_t find_unused_entry(struct fs_object * fs);
void fill_zeros(struct fs_object * fs, size_t offset, size_t length);
void copy_file_data(struct fs_object * fs, size_t offset, size_t length, size_t new_offset);
void swap_file_data(struct fs_object * fs, size_t offset, size_t x_length, size_t y_length);
void compute_hash_tree_level(struct fs_object * fs, int level);
void recompute_block_hashes(struct fs_object * fs, size_t begin, size_t end);
int verify_block_hashes(struct fs_object * fs, size_t begin, size_t end);
int verify_hash_segment(struct fs_object * fs, size_t begin, size_t end);

void * init_fs(char * f1, char * f2, char * f3, int n_processors) {
    struct fs_object * fs = (struct fs_object *) malloc(sizeof(struct fs_object));
    assert(fs);
    fs->file_data_fd = open(f1, O_RDWR);
    assert(fs->file_data_fd != -1);
    fs->directory_table_fd = open(f2, O_RDWR);
    assert(fs->directory_table_fd != -1);
    fs->hash_data_fd = open(f3, O_RDWR);
    assert(fs->hash_data_fd != -1);
    pthread_mutexattr_t mutexattr;
    assert(pthread_mutexattr_init(&mutexattr) == 0);
    assert(pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_RECURSIVE) == 0);
    assert(pthread_mutex_init(&fs->global_fs_lock, &mutexattr) == 0);
    assert(pthread_mutexattr_destroy(&mutexattr) == 0);
    // Get the size of file_data.
    off_t size = lseek(fs->file_data_fd, 0, SEEK_END);
    assert(size != -1);
    fs->capacity = (size_t) size;
    load_directory_table(fs);
    return fs;
}

void close_fs(void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    close(fs->file_data_fd);
    close(fs->directory_table_fd);
    close(fs->hash_data_fd);
    free(fs->ent_link);
    free(fs->ent_bitmap);
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    assert(pthread_mutex_destroy(&fs->global_fs_lock) == 0);
    free(fs);
    return;
}

int create_file(char * filename, size_t length, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    // Check if filename already exists.
    if (find_file(fs, real_filename, 0)) {
        retval = 1;
        goto clean_up;
    }
    // Check if there is insufficient space.
    if (fs->free_space < length) {
        retval = 2;
        goto clean_up;
    }
    ssize_t new_entry_index = find_unused_entry(fs);
    if (new_entry_index == -1) {
        retval = 2;
        goto clean_up;
    }
    // Find first suitable gap.
    size_t offset = 0; // New file's offset.
    ssize_t * p; // Insert new entry before *p.
    for (p = &fs->ent_head; ; p = &fs->ent_link[*p]) {
        if (*p == -1) { // Right most gap.
            if (fs->capacity - offset >= length) break; // Found it.
            else { // No suitable gap.
                repack(fs);
                offset = fs->capacity - fs->free_space; // Next to the right most file.
                break;
            }
        }
        else {
            struct entry ent;
            read_entry(fs, *p, &ent);
            if (ent.offset - offset >= length) break; // Found it.
            else offset = ent.offset + ent.length; // Next loop.
        }
    }
    // Insert new entry, update tail if necessary.
    if (*p == -1) fs->ent_tail = new_entry_index;
    fs->ent_link[new_entry_index] = *p;
    *p = new_entry_index;
    mark_entry_used(fs, new_entry_index);
    // Write new entry.
    struct entry newent;
    memcpy(newent.filename, real_filename, FILENAME_MAX_SIZE);
    newent.offset = offset;
    newent.length = length;
    write_entry(fs, new_entry_index, &newent);
    // Update state.
    fs->free_space -= length;
    fill_zeros(fs, offset, length);
    if (length)
        recompute_block_hashes(
            fs,
            offset / FILE_DATA_BLOCK_SIZE,
            (offset + length - 1) / FILE_DATA_BLOCK_SIZE);
    retval = 0;
    goto clean_up;
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

int resize_file(char * filename, size_t length, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    // Find file entry.
    struct entry ent;
    ssize_t * pindex = find_file(fs, real_filename, &ent);
    if (!pindex) { // File not exist.
        retval = 1;
        goto clean_up;
    }
    // Calculate max file size without moving it.
    size_t max_size;
    ssize_t next_index = fs->ent_link[*pindex];
    if (next_index == -1)
        max_size = fs->capacity - ent.offset;
    else {
        struct entry next_ent;
        read_entry(fs, next_index, &next_ent);
        max_size = next_ent.offset - ent.offset;
    }
    if (length <= max_size) { // Resize in place.
        if (length > ent.length) {
            fill_zeros(fs, ent.offset + ent.length, length - ent.length);
            recompute_block_hashes(
                fs,
                (ent.offset + ent.length) / FILE_DATA_BLOCK_SIZE,
                (ent.offset + length - 1) / FILE_DATA_BLOCK_SIZE);
        }
        fs->free_space = fs->free_space + ent.length - length;
        ent.length = length;
        write_entry(fs, *pindex, &ent);
        retval = 0;
        goto clean_up;
    }
    else if (length - ent.length > fs->free_space) { // Insufficient space.
        retval = 2;
        goto clean_up;
    }
    else {
        repack(fs);
        read_entry(fs, *pindex, &ent); // Reread entry.
        // Move file data to the right.
        size_t next_offset = ent.offset + ent.length;
        size_t rest_size = fs->capacity - fs->free_space - next_offset;
        swap_file_data(fs, ent.offset, ent.length, rest_size);
        // Update entries moved.
        for (ssize_t i = fs->ent_link[*pindex]; i != -1; i = fs->ent_link[i]) {
            struct entry e;
            read_entry(fs, i, &e);
            e.offset -= ent.length;
            write_entry(fs, i, &e);
        }
        ent.offset += rest_size;
        // Move *pindex to the last.
        if (*pindex != fs->ent_tail) {
            fs->ent_link[fs->ent_tail] = *pindex;
            fs->ent_tail = *pindex;
            *pindex = fs->ent_link[fs->ent_tail];
            fs->ent_link[fs->ent_tail] = -1;
        }
        // Expand file.
        fill_zeros(fs, ent.offset + ent.length, length - ent.length);
        fs->free_space -= length - ent.length;
        ent.length = length;
        write_entry(fs, fs->ent_tail, &ent);
        recompute_block_hashes(
            fs,
            (ent.offset - rest_size) / FILE_DATA_BLOCK_SIZE,
            (ent.offset + ent.length - 1) / FILE_DATA_BLOCK_SIZE);
        retval = 0;
        goto clean_up;
    }
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

void repack(void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    size_t top = 0;
    for (ssize_t i = fs->ent_head; i != -1; i = fs->ent_link[i]) {
        struct entry ent;
        read_entry(fs, i, &ent);
        copy_file_data(fs, ent.offset, ent.length, top);
        recompute_block_hashes(
            fs,
            top / FILE_DATA_BLOCK_SIZE,
            (top + ent.length - 1) / FILE_DATA_BLOCK_SIZE);
        ent.offset = top;
        write_entry(fs, i, &ent);
        top += ent.length;
    }
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return;
}

int delete_file(char * filename, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    struct entry ent;
    ssize_t * p = find_file(fs, real_filename, &ent);
    if (p) {
        ent.filename[0] = '\0';
        write_entry(fs, *p, &ent); // Clear entry.
        mark_entry_unused(fs, *p); // Delete node.
        *p = fs->ent_link[*p];
        if (*p == -1) { // Deleting tail.
            ssize_t i = fs->ent_head;
            while (!(i == -1 || fs->ent_link[i] == -1))
                i = fs->ent_link[i];
            fs->ent_tail = i;
        }
        fs->free_space += ent.length;
        retval = 0;
        goto clean_up;
    }
    else {
        retval = 1;
        goto clean_up;
    }
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

int rename_file(char * oldname, char * newname, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_oldname[FILENAME_MAX_SIZE];
    char real_newname[FILENAME_MAX_SIZE];
    truncate_filename(oldname, real_oldname);
    truncate_filename(newname, real_newname);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
	if (strcmp(real_oldname, real_newname) == 0) { // No change. Just return success.
		retval = 0;
		goto clean_up;
	}
    if (find_file(fs, real_newname, 0)) { // Newname already exists.
        retval = 1;
        goto clean_up;
    }
    struct entry ent;
    ssize_t * p = find_file(fs, real_oldname, &ent);
    if (!p) { // File not exist.
        retval = 1;
        goto clean_up;
    }
    memcpy(ent.filename, real_newname, FILENAME_MAX_SIZE);
    write_entry(fs, *p, &ent);
    retval = 0;
    goto clean_up;
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

int read_file(char * filename, size_t offset, size_t count, void * buf, void * helper) {
    if (!count) return 0;
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    struct entry ent;
    ssize_t * p = find_file(fs, real_filename, &ent);
    if (!p) { // File not exist.
        retval = 1;
        goto clean_up;
    }
    if (offset >= ent.length
        || offset + count > ent.length) {
        // Unable to read count bytes.
        retval = 2;
        goto clean_up;
    }
    if (!verify_block_hashes(
            fs,
            (ent.offset + offset) / FILE_DATA_BLOCK_SIZE,
            (ent.offset + offset + count - 1) / FILE_DATA_BLOCK_SIZE)) {
        retval = 3;
        goto clean_up;
    }
    assert(lseek(fs->file_data_fd, ent.offset + offset, SEEK_SET) != -1);
    assert(read(fs->file_data_fd, buf, count) == count);
    retval = 0;
    goto clean_up;
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

int write_file(char * filename, size_t offset, size_t count, void * buf, void * helper) {
    if (!count) return 0;
    struct fs_object * fs = (struct fs_object *) helper;
    int retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    struct entry ent;
    ssize_t * p = find_file(fs, real_filename, &ent);
    if (!p) { // File not exist.
        retval = 1;
        goto clean_up;
    }
    if (offset > ent.length) {
        // offset greater than file size.
        retval = 2;
        goto clean_up;
    }
    size_t new_length = offset + count;
    if (new_length > ent.length) {
        if (resize_file(real_filename, new_length, fs) == 2) {
            // Insufficient space.
            retval = 3;
            goto clean_up;
        }
        assert((p = find_file(fs, real_filename, &ent))); // Reread entry.
    }
    assert(lseek(fs->file_data_fd, ent.offset + offset, SEEK_SET) != -1);
    assert(write(fs->file_data_fd, buf, count) == count);
    recompute_block_hashes(
        fs,
        (ent.offset + offset) / FILE_DATA_BLOCK_SIZE,
        (ent.offset + offset + count - 1) / FILE_DATA_BLOCK_SIZE);
    retval = 0;
    goto clean_up;
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

ssize_t file_size(char * filename, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    ssize_t retval;
    char real_filename[FILENAME_MAX_SIZE];
    truncate_filename(filename, real_filename);
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    struct entry ent;
    if (!find_file(fs, real_filename, &ent)) { // File not exist.
        retval = -1;
        goto clean_up;
    }
    retval = ent.length;
    goto clean_up;
clean_up:
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
    return retval;
}

void fletcher(uint8_t * buf, size_t length, uint8_t * output) {
    size_t n_words = length / 4;
    size_t rest = length % 4;
    uint32_t w[4] = { 0 };
    for (size_t i = 0; i < n_words; ++i) {
        uint32_t word = 0;
        for (size_t j = 0; j < 4; ++j)
            word |= (buf[i * 4 + j] & 0xFF) << (j * 8);
        w[0] = (uint32_t) (((uint64_t) w[0] + (uint64_t) word) % 0xFFFFFFFF);
        w[1] = (uint32_t) (((uint64_t) w[1] + (uint64_t) w[0]) % 0xFFFFFFFF);
        w[2] = (uint32_t) (((uint64_t) w[2] + (uint64_t) w[1]) % 0xFFFFFFFF);
        w[3] = (uint32_t) (((uint64_t) w[3] + (uint64_t) w[2]) % 0xFFFFFFFF);
    }
    if (rest) {
        uint32_t last_word = 0;
        for (size_t i = 0; i < rest; ++i)
            last_word |= (buf[length - rest + i] & 0xFF) << (i * 8);
        w[0] = (uint32_t) (((uint64_t) w[0] + (uint64_t) last_word) % 0xFFFFFFFF);
        w[1] = (uint32_t) (((uint64_t) w[1] + (uint64_t) w[0]) % 0xFFFFFFFF);
        w[2] = (uint32_t) (((uint64_t) w[2] + (uint64_t) w[1]) % 0xFFFFFFFF);
        w[3] = (uint32_t) (((uint64_t) w[3] + (uint64_t) w[2]) % 0xFFFFFFFF);
    }
    for (size_t i = 0; i < 4; ++i)
        for (size_t j = 0; j < 4; ++j)
            output[i * 4 + j] = w[i] >> (j * 8);
}

void compute_hash_tree(void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    compute_hash_tree_level(fs, 0);
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
}

void compute_hash_block(size_t block_offset, void * helper) {
    struct fs_object * fs = (struct fs_object *) helper;
    assert(pthread_mutex_lock(&fs->global_fs_lock) == 0);
    recompute_block_hashes(fs, block_offset, block_offset);
    assert(pthread_mutex_unlock(&fs->global_fs_lock) == 0);
}

// Truncate filename if it's longer than FILENAME_MAX_SIZE - 1 charactors.
void truncate_filename(const char* filename, char output[FILENAME_MAX_SIZE]) {
    strncpy(output, filename, FILENAME_MAX_SIZE);
    output[FILENAME_MAX_SIZE - 1] = '\0';
    for (size_t i = strlen(output); i < FILENAME_MAX_SIZE; ++i)
        output[i] = '\0';
}

// Count and link entries and calculate free space. Abort on error.
void load_directory_table(struct fs_object * fs) {
    // Get the size of directory_table.
    off_t size = lseek(fs->directory_table_fd, 0, SEEK_END);
    assert(size != -1);
    fs->n_entries = (size_t) size / DIRTAB_ENTRY_SIZE;
    fs->free_space = fs->capacity;
    fs->ent_head = fs->ent_tail = -1;
    fs->ent_link = (ssize_t *) malloc(sizeof(ssize_t[fs->n_entries]));
    if (!fs->ent_link) abort();
    size_t elem_size = bitsizeof(*fs->ent_bitmap);
    size_t bitmap_size = (fs->n_entries / elem_size
        + !!(fs->n_entries % elem_size)) * elem_size;
    fs->ent_bitmap = (uint32_t *) malloc(bitmap_size);
    if (!fs->ent_bitmap) abort();
    memset(fs->ent_bitmap, 0xFF, bitmap_size);
    // For every entry.
    assert(lseek(fs->directory_table_fd, 0, SEEK_SET) != -1);
    for (size_t i = 0; i < fs->n_entries; ++i) {
        struct entry ent;
        read_entry(fs, i, &ent);
        if (*ent.filename == '\0') continue; // unused entry
        mark_entry_used(fs, i);
        fs->free_space -= ent.length;
        // Insert entry i into entry list.
        ssize_t * p;
        for (p = &fs->ent_head; *p != -1; p = &fs->ent_link[*p]) {
            struct entry next;
            read_entry(fs, *p, &next);
            if (ent.offset < next.offset) {
                fs->ent_link[i] = *p;
                *p = i;
                break;
            }
        }
        if (*p == -1) { // If reached tail or list is empty, insert after tail.
            fs->ent_link[i] = *p;
            *p = i;
            fs->ent_tail = i;
        }
    }
}

void read_entry(struct fs_object * fs, size_t index, struct entry * output) {
    off_t offset = (off_t) index * DIRTAB_ENTRY_SIZE;
    assert(lseek(fs->directory_table_fd, offset, SEEK_SET) != -1);
    char buffer[DIRTAB_ENTRY_SIZE];
    assert(read(fs->directory_table_fd, buffer, DIRTAB_ENTRY_SIZE)
        == DIRTAB_ENTRY_SIZE); // Errors, interruptions or early EOF is unlikely.
    memcpy(output->filename, buffer, FILENAME_MAX_SIZE);
    output->offset = buffer[FILENAME_MAX_SIZE] & 0x000000FF;
    output->offset |= buffer[FILENAME_MAX_SIZE + 1] << 8 & 0x0000FF00;
    output->offset |= buffer[FILENAME_MAX_SIZE + 2] << 16 & 0x00FF0000;
    output->offset |= buffer[FILENAME_MAX_SIZE + 3] << 24 & 0xFF000000;
    output->length = buffer[FILENAME_MAX_SIZE + 4] & 0x000000FF;
    output->length |= buffer[FILENAME_MAX_SIZE + 5] << 8 & 0x0000FF00;
    output->length |= buffer[FILENAME_MAX_SIZE + 6] << 16 & 0x0000FF0000;
    output->length |= buffer[FILENAME_MAX_SIZE + 7] << 24 & 0xFF000000;
}

void write_entry(struct fs_object * fs, size_t index, const struct entry * data) {
    off_t offset = (off_t) index * DIRTAB_ENTRY_SIZE;
    assert(lseek(fs->directory_table_fd, offset, SEEK_SET) != -1);
    char buffer[DIRTAB_ENTRY_SIZE];
    memcpy(buffer, data->filename, FILENAME_MAX_SIZE);
    buffer[FILENAME_MAX_SIZE] =  data->offset;
    buffer[FILENAME_MAX_SIZE + 1] = data->offset >> 8;
    buffer[FILENAME_MAX_SIZE + 2] = data->offset >> 16;
    buffer[FILENAME_MAX_SIZE + 3] = data->offset >> 24;
    buffer[FILENAME_MAX_SIZE + 4] = data->length;
    buffer[FILENAME_MAX_SIZE + 5] = data->length >> 8;
    buffer[FILENAME_MAX_SIZE + 6] = data->length >> 16;
    buffer[FILENAME_MAX_SIZE + 7] = data->length >> 24;
    assert(write(fs->directory_table_fd, buffer, DIRTAB_ENTRY_SIZE)
        == DIRTAB_ENTRY_SIZE);
}

void mark_entry_unused(struct fs_object * fs, size_t index) {
    fs->ent_bitmap[index / bitsizeof(*fs->ent_bitmap)] |=
        1 << (index % bitsizeof(*fs->ent_bitmap));
}

void mark_entry_used(struct fs_object * fs, size_t index) {
    fs->ent_bitmap[index / bitsizeof(*fs->ent_bitmap)] &=
        ~(1 << (index % bitsizeof(*fs->ent_bitmap)));
}

// Return index of first unused entry.
// Return -1 if full.
ssize_t find_unused_entry(struct fs_object * fs) {
    size_t elem_size = bitsizeof(*fs->ent_bitmap);
    size_t n_full_elems = fs->n_entries / elem_size;
    for (size_t i = 0; i < n_full_elems; ++i) {
        if (fs->ent_bitmap[i]) {
            for (size_t j = 0; j < elem_size; ++j) {
                if (fs->ent_bitmap[i] & (1 << j))
                return (ssize_t) i * elem_size + j;
            }
        }
    }
    if (fs->n_entries % elem_size) {
        for (size_t i = 0; i < fs->n_entries % elem_size; ++i) {
            if (fs->ent_bitmap[n_full_elems] & (1 << i))
                return (ssize_t) n_full_elems * elem_size + i;
        }
    }
    return -1;
}

// Fill [offset, offset + length) in file_data with 0.
void fill_zeros(struct fs_object * fs, size_t offset, size_t length) {
    assert(lseek(fs->file_data_fd, offset, SEEK_SET) != -1);
    size_t left = length;
    static char zeros[BUFFER_SIZE] = { 0 };
    while (left >= BUFFER_SIZE) {
        assert(write(fs->file_data_fd, zeros, BUFFER_SIZE) == BUFFER_SIZE);
        left -= BUFFER_SIZE;
    }
    if (left) assert(write(fs->file_data_fd, zeros, left) == left);
}

// Copy [offset, offset + length) to [new_offset, new_offset + length) in file_data.
void copy_file_data(struct fs_object * fs, size_t offset, size_t length, size_t new_offset) {
    if (offset == new_offset || !length) return;
    size_t n_block = length / BUFFER_SIZE;
    size_t rest = length % BUFFER_SIZE;
    static char buffer[BUFFER_SIZE];
    for (size_t i = 0; i < n_block; ++i) {
        size_t from = offset > new_offset ?
            offset + i * BUFFER_SIZE :
            offset + length - i * BUFFER_SIZE;
        size_t to = offset > new_offset ?
            new_offset + i * BUFFER_SIZE :
            new_offset + length - i * BUFFER_SIZE;
        assert(lseek(fs->file_data_fd, from, SEEK_SET) != -1);
        assert(read(fs->file_data_fd, buffer, BUFFER_SIZE) == BUFFER_SIZE);
        assert(lseek(fs->file_data_fd, to, SEEK_SET) != -1);
        assert(write(fs->file_data_fd, buffer, BUFFER_SIZE) == BUFFER_SIZE);
    }
    if (rest) {
        size_t from = offset > new_offset ?
            offset + n_block * BUFFER_SIZE :
            offset;
        size_t to = offset > new_offset ?
            new_offset + n_block * BUFFER_SIZE :
            new_offset;
        assert(lseek(fs->file_data_fd, from, SEEK_SET) != -1);
        assert(read(fs->file_data_fd, buffer, rest) == rest);
        assert(lseek(fs->file_data_fd, to, SEEK_SET) != -1);
        assert(write(fs->file_data_fd, buffer, rest) == rest);
    }
}

// Move [offset + x_length, offset + x_length + y_length) to [offset, y_length),
// and move [offset, offset + x_length) to [offset + y_length, offset + y_length + x_length) in file_data.
void swap_file_data(struct fs_object * fs, size_t offset, size_t x_length, size_t y_length) {
    if (!x_length || !y_length) return;
    size_t n_block = x_length / BUFFER_SIZE;
    size_t rest = x_length % BUFFER_SIZE;
    static char buffer[BUFFER_SIZE];
    for (size_t i = 0; i < n_block; ++i) {
        size_t from = offset + x_length - (i + 1) * BUFFER_SIZE;
        size_t to = from + y_length;
        assert(lseek(fs->file_data_fd, from, SEEK_SET) != -1);
        assert(read(fs->file_data_fd, buffer, BUFFER_SIZE) == BUFFER_SIZE);
        copy_file_data(fs, from + BUFFER_SIZE, y_length, from);
        assert(lseek(fs->file_data_fd, to, SEEK_SET) != -1);
        assert(write(fs->file_data_fd, buffer, BUFFER_SIZE) == BUFFER_SIZE);
    }
    if (rest) {
        size_t from = offset;
        size_t to = offset + y_length;
        assert(lseek(fs->file_data_fd, from, SEEK_SET) != -1);
        assert(read(fs->file_data_fd, buffer, rest) == rest);
        copy_file_data(fs, from + rest, y_length, from);
        assert(lseek(fs->file_data_fd, to, SEEK_SET) != -1);
        assert(write(fs->file_data_fd, buffer, rest) == rest);
    }
}

// Find entry by filename.
// Return pointer to previous entry's link.
// If ent != NULL, write entry data to *ent.
// Return NULL if filename not found.
ssize_t * find_file(struct fs_object * fs, const char filename[FILENAME_MAX_SIZE], struct entry * ent) {
    for (ssize_t * p = &fs->ent_head; *p != -1; p = &fs->ent_link[*p]) {
        struct entry e;
        read_entry(fs, *p, &e);
        if (strcmp(filename, e.filename) == 0) {
            if (ent) *ent = e;
            return p;
        }
    }
    return 0;
}

// Compute hash tree levels [level, +inf).
void compute_hash_tree_level(struct fs_object * fs, int level) {
    int depth = (int) log2(fs->capacity / FILE_DATA_BLOCK_SIZE);
    size_t start = pow(2, level) - 1;
    if (level > depth) return;
    else if (level == depth) {
        for (size_t i = 0; i < fs->capacity / FILE_DATA_BLOCK_SIZE; ++i) {
            uint8_t file_data[FILE_DATA_BLOCK_SIZE];
            assert(lseek(fs->file_data_fd, i * FILE_DATA_BLOCK_SIZE, SEEK_SET) != -1);
            assert(read(fs->file_data_fd, file_data, sizeof(file_data)) == sizeof(file_data));
            uint8_t hash[HASH_SIZE];
            fletcher(file_data, sizeof(file_data), hash);
            write_hash(fs, start + i, hash);
        }
    }
    else {
        compute_hash_tree_level(fs, level + 1);
        size_t end = start + pow(2, level);
        for (size_t i = start; i < end; ++i) {
            uint8_t children_hash[HASH_SIZE * 2];
            read_hash(fs, i * 2 + 1, children_hash);
            read_hash(fs, i * 2 + 2, children_hash + HASH_SIZE);
            uint8_t this_hash[HASH_SIZE];
            fletcher(children_hash, sizeof(children_hash), this_hash);
            write_hash(fs, i, this_hash);
        }
    }
}

void read_hash(struct fs_object * fs, size_t index, uint8_t output[HASH_SIZE]) {
    assert(lseek(fs->hash_data_fd, index * HASH_SIZE, SEEK_SET) != -1);
    assert(read(fs->hash_data_fd, output, HASH_SIZE) == HASH_SIZE);
}

void write_hash(struct fs_object * fs, size_t index, const uint8_t hash[HASH_SIZE]) {
    assert(lseek(fs->hash_data_fd, index * HASH_SIZE, SEEK_SET) != -1);
    assert(write(fs->hash_data_fd, hash, HASH_SIZE) == HASH_SIZE);
}

// Recompute hash_data [begin, end] and all their ancestors.
// [begin, end] must be of the same depth.
void recompute_hash_segment(struct fs_object * fs, size_t begin, size_t end) {
    assert(begin <= end);
    size_t leaf_begin = fs->capacity / FILE_DATA_BLOCK_SIZE - 1;
    // For leaf nodes.
    if (begin >= leaf_begin) {
        for (size_t i = begin; i <= end; ++i) {
            uint8_t block[FILE_DATA_BLOCK_SIZE];
            assert(lseek(fs->file_data_fd, (i - leaf_begin) * FILE_DATA_BLOCK_SIZE, SEEK_SET) != -1);
            assert(read(fs->file_data_fd, block, sizeof(block)) == sizeof(block));
            uint8_t hash[HASH_SIZE];
            fletcher(block, sizeof(block), hash);
            write_hash(fs, i, hash);
            // // debug
            // printf("computed hash [%d] = %llx\n", (int)i, *(long long unsigned *)hash);
            // // end
        }
        if (begin) recompute_hash_segment(fs, (begin - 1) / 2, (end - 1) / 2);
    }
    // For internal nodes.
    else {
        for (size_t i = begin; i <= end; ++i) {
            uint8_t children_hash[HASH_SIZE * 2];
            read_hash(fs, i * 2 + 1, children_hash);
            read_hash(fs, i * 2 + 2, children_hash + HASH_SIZE);
            uint8_t this_hash[HASH_SIZE];
            fletcher(children_hash, sizeof(children_hash), this_hash);
            write_hash(fs, i, this_hash);
        }
        if (begin) recompute_hash_segment(fs, (begin - 1) / 2, (end - 1) / 2);
    }
}

// Wrapper for recompute_hash_segment().
void recompute_block_hashes(struct fs_object * fs, size_t block_begin, size_t block_end) {
    size_t leaf_begin = fs->capacity / FILE_DATA_BLOCK_SIZE - 1;
    recompute_hash_segment(fs, leaf_begin + block_begin, leaf_begin + block_end);
}

// Verify integrity of file_data blocks [being, end].
int verify_block_hashes(struct fs_object * fs, size_t begin, size_t end) {
    size_t leaf_begin = fs->capacity / FILE_DATA_BLOCK_SIZE - 1;
    return verify_hash_segment(fs, leaf_begin + begin, leaf_begin + end);
}

// Verify hash nodes [begin, end] and all their ancestors.
int verify_hash_segment(struct fs_object * fs, size_t begin, size_t end) {
    assert(begin <= end);
    size_t leaf_begin = fs->capacity / FILE_DATA_BLOCK_SIZE - 1;
    // For leaf nodes.
    if (begin >= leaf_begin) {
        for (size_t i = begin; i <= end; ++i) {
            uint8_t block[FILE_DATA_BLOCK_SIZE];
            assert(lseek(fs->file_data_fd, (i - leaf_begin) * FILE_DATA_BLOCK_SIZE, SEEK_SET) != -1);
            assert(read(fs->file_data_fd, block, sizeof(block)) == sizeof(block));
            uint8_t hash[HASH_SIZE];
            fletcher(block, sizeof(block), hash);
            uint8_t stored_hash[HASH_SIZE];
            read_hash(fs, i, stored_hash);
            // // debug
            // printf("verifying hash [%d] = %llx, expected %llx\n", (int)i, *(long long unsigned *)stored_hash, *(long long unsigned *)hash);
            // // end
            if (memcmp(hash, stored_hash, HASH_SIZE)) return 0;
        }
        if (begin)
            return verify_hash_segment(fs, (begin - 1) / 2, (end - 1) / 2);
        else
            return 1;
    }
    // For internal nodes.
    else {
        for (size_t i = begin; i <= end; ++i) {
            uint8_t children_hash[HASH_SIZE * 2];
            read_hash(fs, i * 2 + 1, children_hash);
            read_hash(fs, i * 2 + 2, children_hash + HASH_SIZE);
            uint8_t this_hash[HASH_SIZE];
            fletcher(children_hash, sizeof(children_hash), this_hash);
            uint8_t stored_this_hash[HASH_SIZE];
            read_hash(fs, i, stored_this_hash);
            if (memcmp(this_hash, stored_this_hash, HASH_SIZE)) return 0;
        }
        if (begin)
            return verify_hash_segment(fs, (begin - 1) / 2, (end - 1) / 2);
        else
            return 1;
    }
}