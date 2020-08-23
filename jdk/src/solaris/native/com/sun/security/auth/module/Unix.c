/*
 * Copyright (c) 2000, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifdef __solaris__
#define _POSIX_C_SOURCE 199506L
#endif

#include <jni.h>
#include "com_sun_security_auth_module_UnixSystem.h"
#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef __ANDROID__
// Copy from bionic libc of Android
static int do_getpw_r(uid_t uid,
                      struct passwd* dst, char* buf, size_t byte_count,
                      struct passwd** result) {
  
  *result = NULL;

  // Our implementation of getpwnam(3) and getpwuid(3) use thread-local
  // storage, so we can call them as long as we copy everything out
  // before returning.
  const struct passwd* src = getpwuid(uid); // NOLINT: see above.

  // POSIX allows failure to find a match to be considered a non-error.
  // Reporting success (0) but with *result NULL is glibc's behavior.
  if (src == NULL) {
    return (errno == ENOENT) ? 0 : errno;
  }

  // Work out where our strings will go in 'buf', and whether we've got
  // enough space.
  size_t required_byte_count = 0;
  dst->pw_name = buf;
  required_byte_count += strlen(src->pw_name) + 1;
  dst->pw_dir = buf + required_byte_count;
  required_byte_count += strlen(src->pw_dir) + 1;
  dst->pw_shell = buf + required_byte_count;
  required_byte_count += strlen(src->pw_shell) + 1;
  if (byte_count < required_byte_count) {
    return ERANGE;
  }

  // Copy the strings.
  snprintf(buf, byte_count, "%s%c%s%c%s", src->pw_name, 0, src->pw_dir, 0, src->pw_shell);

  // pw_passwd is non-POSIX and unused (always NULL) in bionic.
  // pw_gecos is non-POSIX and missing in bionic.
  dst->pw_passwd = NULL;

  // Copy the integral fields.
  dst->pw_gid = src->pw_gid;
  dst->pw_uid = src->pw_uid;

  *result = dst;
  return 0;
}

static inline int getpwuid_r(uid_t uid, struct passwd* pwd,
               char* buf, size_t byte_count, struct passwd** result) {
  return do_getpw_r(uid, pwd, buf, byte_count, result);
}
#endif

JNIEXPORT void JNICALL
Java_com_sun_security_auth_module_UnixSystem_getUnixInfo
                                                (JNIEnv *env, jobject obj) {

    int i;
    char pwd_buf[1024];
    struct passwd *pwd;
    struct passwd resbuf;
    jfieldID userNameID;
    jfieldID userID;
    jfieldID groupID;
    jfieldID supplementaryGroupID;

    jstring jstr;
    jlongArray jgroups;
    jlong *jgroupsAsArray;
    jsize numSuppGroups;
    gid_t *groups;
    jclass cls;

    numSuppGroups = getgroups(0, NULL);
    groups = (gid_t *)calloc(numSuppGroups, sizeof(gid_t));
    if (groups == NULL) {
        jclass cls = (*env)->FindClass(env,"java/lang/OutOfMemoryError");
        if (cls != NULL)
            (*env)->ThrowNew(env, cls, NULL);
        return;
    }

    cls = (*env)->GetObjectClass(env, obj);

    memset(pwd_buf, 0, sizeof(pwd_buf));

    if (getpwuid_r(getuid(), &resbuf, pwd_buf, sizeof(pwd_buf), &pwd) == 0 &&
        pwd != NULL &&
        getgroups(numSuppGroups, groups) != -1) {

        userNameID = (*env)->GetFieldID(env, cls, "username", "Ljava/lang/String;");
        if (userNameID == 0)
            goto cleanUpAndReturn;

        userID = (*env)->GetFieldID(env, cls, "uid", "J");
        if (userID == 0)
            goto cleanUpAndReturn;

        groupID = (*env)->GetFieldID(env, cls, "gid", "J");
        if (groupID == 0)
            goto cleanUpAndReturn;

        supplementaryGroupID = (*env)->GetFieldID(env, cls, "groups", "[J");
        if (supplementaryGroupID == 0)
            goto cleanUpAndReturn;

        jstr = (*env)->NewStringUTF(env, pwd->pw_name);
        if (jstr == NULL)
            goto cleanUpAndReturn;
        (*env)->SetObjectField(env, obj, userNameID, jstr);

        (*env)->SetLongField(env, obj, userID, pwd->pw_uid);

        (*env)->SetLongField(env, obj, groupID, pwd->pw_gid);

        jgroups = (*env)->NewLongArray(env, numSuppGroups);
        if (jgroups == NULL)
            goto cleanUpAndReturn;
        jgroupsAsArray = (*env)->GetLongArrayElements(env, jgroups, 0);
        if (jgroupsAsArray == NULL)
            goto cleanUpAndReturn;
        for (i = 0; i < numSuppGroups; i++)
            jgroupsAsArray[i] = groups[i];
        (*env)->ReleaseLongArrayElements(env, jgroups, jgroupsAsArray, 0);
        (*env)->SetObjectField(env, obj, supplementaryGroupID, jgroups);
    }
cleanUpAndReturn:
    free(groups);
    return;
}
