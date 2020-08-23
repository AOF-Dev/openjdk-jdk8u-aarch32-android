/*
 * Copyright (c) 2008, 2019, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

#include "jni.h"
#include "jni_util.h"
#include "jvm.h"
#include "jlong.h"

#include <stdio.h>
#include <string.h>
#include <dlfcn.h>
#include <errno.h>
#include <mntent.h>

// Copy from glibc 2.31
#ifdef __ANDROID__

struct mntent * getmntent_r (FILE *stream, struct mntent *mp, char *buffer, int bufsiz)
{
  // buf not used by caller (see below)
  flockfile (stream);
  mp = getmntent(stream);
  funlockfile (stream);
  return mp;
}
/* Prepare to begin reading and/or writing mount table entries from the
   beginning of FILE.  MODE is as for `fopen'.  */
FILE * setmntent (const char *file, const char *mode)
{
  /* Extend the mode parameter with "c" to disable cancellation in the
     I/O functions and "e" to set FD_CLOEXEC.  */
  size_t modelen = strlen (mode);
  char newmode[modelen + 3];
  memcpy (memcpy (newmode, mode, modelen) + modelen, "ce", 3);
  FILE *result = fopen (file, newmode);

  return result;
}

int endmntent (FILE *stream)
{
  if (stream)		/* SunOS 4.x allows for NULL stream */
    fclose (stream);
  return 1;		/* SunOS 4.x says to always return 1 */
}

#endif  //__ANDROID__

#include "sun_nio_fs_LinuxNativeDispatcher.h"

typedef size_t fgetxattr_func(int fd, const char* name, void* value, size_t size);
typedef int fsetxattr_func(int fd, const char* name, void* value, size_t size, int flags);
typedef int fremovexattr_func(int fd, const char* name);
typedef int flistxattr_func(int fd, char* list, size_t size);

fgetxattr_func* my_fgetxattr_func = NULL;
fsetxattr_func* my_fsetxattr_func = NULL;
fremovexattr_func* my_fremovexattr_func = NULL;
flistxattr_func* my_flistxattr_func = NULL;

static jfieldID entry_name;
static jfieldID entry_dir;
static jfieldID entry_fstype;
static jfieldID entry_options;

static void throwUnixException(JNIEnv* env, int errnum) {
    jobject x = JNU_NewObjectByName(env, "sun/nio/fs/UnixException",
        "(I)V", errnum);
    if (x != NULL) {
        (*env)->Throw(env, x);
    }
}

JNIEXPORT void JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_init(JNIEnv *env, jclass clazz)
{
    my_fgetxattr_func = (fgetxattr_func*)dlsym(RTLD_DEFAULT, "fgetxattr");
    my_fsetxattr_func = (fsetxattr_func*)dlsym(RTLD_DEFAULT, "fsetxattr");
    my_fremovexattr_func = (fremovexattr_func*)dlsym(RTLD_DEFAULT, "fremovexattr");
    my_flistxattr_func = (flistxattr_func*)dlsym(RTLD_DEFAULT, "flistxattr");

    clazz = (*env)->FindClass(env, "sun/nio/fs/UnixMountEntry");
    CHECK_NULL(clazz);
    entry_name = (*env)->GetFieldID(env, clazz, "name", "[B");
    CHECK_NULL(entry_name);
    entry_dir = (*env)->GetFieldID(env, clazz, "dir", "[B");
    CHECK_NULL(entry_dir);
    entry_fstype = (*env)->GetFieldID(env, clazz, "fstype", "[B");
    CHECK_NULL(entry_fstype);
    entry_options = (*env)->GetFieldID(env, clazz, "opts", "[B");
    CHECK_NULL(entry_options);
}

JNIEXPORT jint JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_fgetxattr0(JNIEnv* env, jclass clazz,
    jint fd, jlong nameAddress, jlong valueAddress, jint valueLen)
{
    size_t res = -1;
    const char* name = jlong_to_ptr(nameAddress);
    void* value = jlong_to_ptr(valueAddress);

    if (my_fgetxattr_func == NULL) {
        errno = ENOTSUP;
    } else {
        /* EINTR not documented */
        res = (*my_fgetxattr_func)(fd, name, value, valueLen);
    }
    if (res == (size_t)-1)
        throwUnixException(env, errno);
    return (jint)res;
}

JNIEXPORT void JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_fsetxattr0(JNIEnv* env, jclass clazz,
    jint fd, jlong nameAddress, jlong valueAddress, jint valueLen)
{
    int res = -1;
    const char* name = jlong_to_ptr(nameAddress);
    void* value = jlong_to_ptr(valueAddress);

    if (my_fsetxattr_func == NULL) {
        errno = ENOTSUP;
    } else {
        /* EINTR not documented */
        res = (*my_fsetxattr_func)(fd, name, value, valueLen, 0);
    }
    if (res == -1)
        throwUnixException(env, errno);
}

JNIEXPORT void JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_fremovexattr0(JNIEnv* env, jclass clazz,
    jint fd, jlong nameAddress)
{
    int res = -1;
    const char* name = jlong_to_ptr(nameAddress);

    if (my_fremovexattr_func == NULL) {
        errno = ENOTSUP;
    } else {
        /* EINTR not documented */
        res = (*my_fremovexattr_func)(fd, name);
    }
    if (res == -1)
        throwUnixException(env, errno);
}

JNIEXPORT jint JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_flistxattr(JNIEnv* env, jclass clazz,
    jint fd, jlong listAddress, jint size)
{
    size_t res = -1;
    char* list = jlong_to_ptr(listAddress);

    if (my_flistxattr_func == NULL) {
        errno = ENOTSUP;
    } else {
        /* EINTR not documented */
        res = (*my_flistxattr_func)(fd, list, (size_t)size);
    }
    if (res == (size_t)-1)
        throwUnixException(env, errno);
    return (jint)res;
}

JNIEXPORT jlong JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_setmntent0(JNIEnv* env, jclass this, jlong pathAddress,
                                                 jlong modeAddress)
{
    FILE* fp = NULL;
    const char* path = (const char*)jlong_to_ptr(pathAddress);
    const char* mode = (const char*)jlong_to_ptr(modeAddress);

    do {
        fp = setmntent(path, mode);
    } while (fp == NULL && errno == EINTR);
    if (fp == NULL) {
        throwUnixException(env, errno);
    }
    return ptr_to_jlong(fp);
}

JNIEXPORT jint JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_getmntent0(JNIEnv* env, jclass this,
    jlong value, jobject entry, jlong buffer, jint bufLen)
{
    struct mntent ent;
    char * buf = (char*)jlong_to_ptr(buffer);
    struct mntent* m;
    FILE* fp = jlong_to_ptr(value);
    jsize len;
    jbyteArray bytes;
    char* name;
    char* dir;
    char* fstype;
    char* options;

    m = getmntent_r(fp, &ent, buf, (int)bufLen);
    if (m == NULL)
        return -1;
    name = m->mnt_fsname;
    dir = m->mnt_dir;
    fstype = m->mnt_type;
    options = m->mnt_opts;

    len = strlen(name);
    bytes = (*env)->NewByteArray(env, len);
    if (bytes == NULL)
        return -1;
    (*env)->SetByteArrayRegion(env, bytes, 0, len, (jbyte*)name);
    (*env)->SetObjectField(env, entry, entry_name, bytes);

    len = strlen(dir);
    bytes = (*env)->NewByteArray(env, len);
    if (bytes == NULL)
        return -1;
    (*env)->SetByteArrayRegion(env, bytes, 0, len, (jbyte*)dir);
    (*env)->SetObjectField(env, entry, entry_dir, bytes);

    len = strlen(fstype);
    bytes = (*env)->NewByteArray(env, len);
    if (bytes == NULL)
        return -1;
    (*env)->SetByteArrayRegion(env, bytes, 0, len, (jbyte*)fstype);
    (*env)->SetObjectField(env, entry, entry_fstype, bytes);

    len = strlen(options);
    bytes = (*env)->NewByteArray(env, len);
    if (bytes == NULL)
        return -1;
    (*env)->SetByteArrayRegion(env, bytes, 0, len, (jbyte*)options);
    (*env)->SetObjectField(env, entry, entry_options, bytes);

    return 0;
}

JNIEXPORT void JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_endmntent(JNIEnv* env, jclass this, jlong stream)
{
    FILE* fp = jlong_to_ptr(stream);
    /* FIXME - man page doesn't explain how errors are returned */
    endmntent(fp);
}


#ifdef __ANDROID__
// FIXME - some codes are deleted
// Copy from OpenBSD


/* Minimum buffer size we create.
 * This should allow config files to fit into our power of 2 buffer growth
 * without the need for a realloc. */
#define MINBUF	128

static ssize_t getdelim(char **__restrict buf, size_t *__restrict buflen, int sep, FILE *__restrict fp) {
	unsigned char *p;
	size_t len, newlen, off;
	char *newb;

	flockfile(fp);

	if (buf == NULL || buflen == NULL) {
		errno = EINVAL;
		goto error;
	}

	/* If buf is NULL, we have to assume a size of zero */
	if (*buf == NULL)
		*buflen = 0;

	off = 0;
	do {

		/* Scan through looking for the separator */
		p = memchr(fp->_p, sep, (size_t)fp->_r);
		if (p == NULL)
			len = fp->_r;
		else
			len = (p - fp->_p) + 1;

		/* Ensure we can handle it */
		if (off > SSIZE_MAX || len + 1 > SSIZE_MAX - off) {
			errno = EOVERFLOW;
			goto error;
		}
		newlen = off + len + 1; /* reserve space for NUL terminator */
		if (newlen > *buflen) {
			if (newlen < MINBUF)
				newlen = MINBUF;
#define powerof2(x) ((((x)-1)&(x))==0)
			if (!powerof2(newlen)) {
				/* Grow the buffer to the next power of 2 */
				newlen--;
				newlen |= newlen >> 1;
				newlen |= newlen >> 2;
				newlen |= newlen >> 4;
				newlen |= newlen >> 8;
				newlen |= newlen >> 16;
#if SIZE_MAX > 0xffffffffU
				newlen |= newlen >> 32;
#endif
				newlen++;
			}

			newb = realloc(*buf, newlen);
			if (newb == NULL)
				goto error;
			*buf = newb;
			*buflen = newlen;
		}

		(void)memcpy((*buf + off), fp->_p, len);
		/* Safe, len is never greater than what fp->_r can fit. */
		fp->_r -= (int)len;
		fp->_p += (int)len;
		off += len;
	} while (p == NULL);

	funlockfile(fp);

	/* POSIX demands we return -1 on EOF. */
	if (off == 0)
		return -1;

	if (*buf != NULL)
		*(*buf + off) = '\0';
	return off;

error:
	fp->_flags |= __SERR;
	funlockfile(fp);
	return -1;
}

static ssize_t getline(char **__restrict buf, size_t *__restrict buflen, FILE *__restrict fp)
{
    return getdelim(buf, buflen, '\n', fp);
}
#endif

/**
 * This function returns line length without NUL terminator or -1 on EOF.
 * Since getline is missing on Solaris10 this function was moved from
 * UnixNativeDispatcher to LinuxNativeDispatcher as part of backport form jdk11.
 */
JNIEXPORT jint JNICALL
Java_sun_nio_fs_LinuxNativeDispatcher_getlinelen(JNIEnv* env, jclass this, jlong stream)
{
    FILE* fp = jlong_to_ptr(stream);
    size_t lineSize = 0;
    char * lineBuffer = NULL;
    int saved_errno;

    ssize_t res = getline(&lineBuffer, &lineSize, fp);
    saved_errno = errno;

    /* Should free lineBuffer no matter result, according to man page */
    if (lineBuffer != NULL)
        free(lineBuffer);

    if (feof(fp))
        return -1;

    /* On successfull return res >= 0, otherwise res is -1 */
    if (res == -1)
        throwUnixException(env, saved_errno);

    if (res > INT_MAX)
        throwUnixException(env, EOVERFLOW);

    return (jint)res;
}

