/* Do not change! */
#define FUSE_USE_VERSION 29
#define _FILE_OFFSET_BITS 64
/******************/

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fuse.h>
#include <errno.h>
#include <stdlib.h>
#include <assert.h>

#include "myfilesystem.h"

char * file_data_file_name = NULL;
char * directory_table_file_name = NULL;
char * hash_data_file_name = NULL;

struct fs_object * get_fs() {
    return (struct fs_object *) fuse_get_context()->private_data;
}

int myfuse_getattr(const char * name, struct stat * result) {
    memset(result, 0, sizeof(struct stat));
    if (strcmp(name, "/") == 0) {
        result->st_mode = S_IFDIR | 0777;
        result->st_size = 0;
        result->st_blocks = 0;
    } else {
        ssize_t size = file_size((char *) name + 1, get_fs());
        if (size == -1) return -ENOENT;
        else result->st_size = size;
        result->st_mode = S_IFREG | 0777;
        result->st_blocks = result->st_size / 512 + !!(result->st_size % 512);
    }
    result->st_nlink = 1;
    result->st_uid = getuid();
    result->st_gid = getgid();
    return 0;
}

int myfuse_readdir(const char * name, void * buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info * fi) {
    if (strcmp(name, "/") == 0) {
        for (ssize_t i = get_fs()->ent_head; i != -1; i = get_fs()->ent_link[i]) {
            struct entry ent;
            read_entry(get_fs(), i, &ent);
            if (filler(buf, ent.filename, 0, 0))
                return -ENOMEM;
            printf("readdir: %s\n", ent.filename);
        }
        return 0;
    }
    else
        return -ENOENT;
}

int myfuse_unlink(const char * name) {
    if (delete_file((char *) name + 1, get_fs()))
        return -ENOENT;
    else
        return 0;
}

int myfuse_rename(const char * oldpath, const char * newpath) {
    if (rename_file((char *) oldpath + 1, (char *) newpath + 1, get_fs()))
        return -ENOENT;
    else
        return 0;
}

int myfuse_truncate(const char * name, off_t newsize) {
    if (newsize < 0 ) return -EINVAL;
    switch (resize_file((char *) name + 1, newsize, get_fs())) {
    case 0: return 0;
    case 1: return -ENOENT;
    case 2: return -EINVAL;
    }
}

int myfuse_open(const char * name, struct fuse_file_info * ffi) {
    char realname[FILENAME_MAX_SIZE];
    truncate_filename(name + 1, realname);
    if (!find_file(get_fs(), realname, 0))
        return -ENOENT;
    else
        return 0;
}

int myfuse_read(const char * name, char * buf, size_t count, off_t offset, struct fuse_file_info * ffi) {
    ssize_t size = file_size((char *) name + 1, get_fs());
    if (size == -1) return -ENOENT;
    if (offset >= size) return 0;
    if (offset + count > size) count = size - offset;
    switch (read_file((char *) name + 1, offset, count, buf, get_fs())) {
    case 0: return count;
    case 1: case 2: assert(0);
    }
}

int myfuse_write(const char * name, const char * buf, size_t count, off_t offset, struct fuse_file_info * ffi) {
retry:
    switch (write_file((char *) name + 1, offset, count, (char *) buf, get_fs())) {
    case 0: return count;
    case 1: return -ENOENT;
    case 2:
        if (resize_file((char *) name + 1, offset, get_fs()))
            return -ENOSPC;
        goto retry;
    case 3:
        count -= offset + count
            - file_size((char *)name + 1, get_fs()) - get_fs()->free_space;
        goto retry;
    }
}

int myfuse_release(const char * name, struct fuse_file_info * ffi){
    return 0;
}

void * myfuse_init(struct fuse_conn_info * fci) {
    return init_fs(file_data_file_name, directory_table_file_name, hash_data_file_name, 1);
}

void myfuse_destroy(void * FS) {
    close_fs(FS);
}

int myfuse_create(const char * name, mode_t mode, struct fuse_file_info * ffi) {
    switch (create_file((char *) name + 1, 0, get_fs())) {
    case 0: return 0;
    case 1: return -EEXIST;
    case 2: return -ENOSPC;
    }
}

struct fuse_operations operations = {
    .init = myfuse_init,
    .destroy = myfuse_destroy,
    .getattr = myfuse_getattr,
    .readdir = myfuse_readdir,
    .unlink = myfuse_unlink,
    .rename = myfuse_rename,
    .truncate = myfuse_truncate,
    .open = myfuse_open,
    .read = myfuse_read,
    .write = myfuse_write,
    .release = myfuse_release,
    .init = myfuse_init,
    .destroy = myfuse_destroy,
    .create = myfuse_create
};

int main(int argc, char * argv[]) {
    if (argc >= 5) {
        if (strcmp(argv[argc-4], "--files") == 0) {
            file_data_file_name = argv[argc-3];
            directory_table_file_name = argv[argc-2];
            hash_data_file_name = argv[argc-1];
            argc -= 4;
        }
    }
    int ret = fuse_main(argc, argv, &operations, NULL);
    return ret;
}
