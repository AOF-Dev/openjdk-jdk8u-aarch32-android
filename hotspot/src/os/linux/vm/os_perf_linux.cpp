/*
 * Copyright (c) 2012, 2018, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
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
 *
 */

#include "precompiled.hpp"
#include "jvm.h"
#include "memory/allocation.inline.hpp"
#include "os_linux.inline.hpp"
#include "runtime/os.hpp"
#include "runtime/os_perf.hpp"

#ifdef TARGET_ARCH_aarch32
# include "vm_version_ext_aarch32.hpp"
#endif
#ifdef TARGET_ARCH_x86
# include "vm_version_ext_x86.hpp"
#endif
#ifdef TARGET_ARCH_sparc
# include "vm_version_ext_sparc.hpp"
#endif
#ifdef TARGET_ARCH_zero
# include "vm_version_ext_zero.hpp"
#endif
#ifdef TARGET_ARCH_arm
# include "vm_version_ext_arm.hpp"
#endif
#ifdef TARGET_ARCH_ppc
# include "vm_version_ext_ppc.hpp"
#endif

#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <pthread.h>
#include <limits.h>
#include <ifaddrs.h>
#include <fcntl.h>

/**
   /proc/[number]/stat
              Status information about the process.  This is used by ps(1).  It is defined in /usr/src/linux/fs/proc/array.c.

              The fields, in order, with their proper scanf(3) format specifiers, are:

              1. pid %d The process id.

              2. comm %s
                     The filename of the executable, in parentheses.  This is visible whether or not the executable is swapped out.

              3. state %c
                     One  character  from  the  string "RSDZTW" where R is running, S is sleeping in an interruptible wait, D is waiting in uninterruptible disk
                     sleep, Z is zombie, T is traced or stopped (on a signal), and W is paging.

              4. ppid %d
                     The PID of the parent.

              5. pgrp %d
                     The process group ID of the process.

              6. session %d
                     The session ID of the process.

              7. tty_nr %d
                     The tty the process uses.

              8. tpgid %d
                     The process group ID of the process which currently owns the tty that the process is connected to.

              9. flags %lu
                     The flags of the process.  The math bit is decimal 4, and the traced bit is decimal 10.

              10. minflt %lu
                     The number of minor faults the process has made which have not required loading a memory page from disk.

              11. cminflt %lu
                     The number of minor faults that the process's waited-for children have made.

              12. majflt %lu
                     The number of major faults the process has made which have required loading a memory page from disk.

              13. cmajflt %lu
                     The number of major faults that the process's waited-for children have made.

              14. utime %lu
                     The number of jiffies that this process has been scheduled in user mode.

              15. stime %lu
                     The number of jiffies that this process has been scheduled in kernel mode.

              16. cutime %ld
                     The number of jiffies that this process's waited-for children have been scheduled in user mode. (See also times(2).)

              17. cstime %ld
                     The number of jiffies that this process' waited-for children have been scheduled in kernel mode.

              18. priority %ld
                     The standard nice value, plus fifteen.  The value is never negative in the kernel.

              19. nice %ld
                     The nice value ranges from 19 (nicest) to -19 (not nice to others).

              20. 0 %ld  This value is hard coded to 0 as a placeholder for a removed field.

              21. itrealvalue %ld
                     The time in jiffies before the next SIGALRM is sent to the process due to an interval timer.

              22. starttime %lu
                     The time in jiffies the process started after system boot.

              23. vsize %lu
                     Virtual memory size in bytes.

              24. rss %ld
                     Resident Set Size: number of pages the process has in real memory, minus 3 for administrative purposes. This is just the pages which  count
                     towards text, data, or stack space.  This does not include pages which have not been demand-loaded in, or which are swapped out.

              25. rlim %lu
                     Current limit in bytes on the rss of the process (usually 4294967295 on i386).

              26. startcode %lu
                     The address above which program text can run.

              27. endcode %lu
                     The address below which program text can run.

              28. startstack %lu
                     The address of the start of the stack.

              29. kstkesp %lu
                     The current value of esp (stack pointer), as found in the kernel stack page for the process.

              30. kstkeip %lu
                     The current EIP (instruction pointer).

              31. signal %lu
                     The bitmap of pending signals (usually 0).

              32. blocked %lu
                     The bitmap of blocked signals (usually 0, 2 for shells).

              33. sigignore %lu
                     The bitmap of ignored signals.

              34. sigcatch %lu
                     The bitmap of catched signals.

              35. wchan %lu
                     This  is the "channel" in which the process is waiting.  It is the address of a system call, and can be looked up in a namelist if you need
                     a textual name.  (If you have an up-to-date /etc/psdatabase, then try ps -l to see the WCHAN field in action.)

              36. nswap %lu
                     Number of pages swapped - not maintained.

              37. cnswap %lu
                     Cumulative nswap for child processes.

              38. exit_signal %d
                     Signal to be sent to parent when we die.

              39. processor %d
                     CPU number last executed on.



 ///// SSCANF FORMAT STRING. Copy and use.

field:        1  2  3  4  5  6  7  8  9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38 39
format:       %d %s %c %d %d %d %d %d %lu %lu %lu %lu %lu %lu %lu %ld %ld %ld %ld %ld %ld %lu %lu %ld %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %d %d


*/

/**
 * For platforms that have them, when declaring
 * a printf-style function,
 *   formatSpec is the parameter number (starting at 1)
 *       that is the format argument ("%d pid %s")
 *   params is the parameter number where the actual args to
 *       the format starts. If the args are in a va_list, this
 *       should be 0.
 */
#ifndef PRINTF_ARGS
#  define PRINTF_ARGS(formatSpec,  params) ATTRIBUTE_PRINTF(formatSpec, params)
#endif

#ifndef SCANF_ARGS
#  define SCANF_ARGS(formatSpec,   params) ATTRIBUTE_SCANF(formatSpec, params)
#endif

#ifndef _PRINTFMT_
#  define _PRINTFMT_
#endif

#ifndef _SCANFMT_
#  define _SCANFMT_
#endif


struct CPUPerfTicks {
  uint64_t  used;
  uint64_t  usedKernel;
  uint64_t  total;
};

typedef enum {
  CPU_LOAD_VM_ONLY,
  CPU_LOAD_GLOBAL,
} CpuLoadTarget;

enum {
  UNDETECTED,
  UNDETECTABLE,
  LINUX26_NPTL,
  BAREMETAL
};

struct CPUPerfCounters {
  int   nProcs;
  CPUPerfTicks jvmTicks;
  CPUPerfTicks* cpus;
};

static double get_cpu_load(int which_logical_cpu, CPUPerfCounters* counters, double* pkernelLoad, CpuLoadTarget target);

/** reads /proc/<pid>/stat data, with some checks and some skips.
 *  Ensure that 'fmt' does _NOT_ contain the first two "%d %s"
 */
static int SCANF_ARGS(2, 0) vread_statdata(const char* procfile, _SCANFMT_ const char* fmt, va_list args) {
  FILE*f;
  int n;
  char buf[2048];

  if ((f = fopen(procfile, "r")) == NULL) {
    return -1;
  }

  if ((n = fread(buf, 1, sizeof(buf), f)) != -1) {
    char *tmp;

    buf[n-1] = '\0';
    /** skip through pid and exec name. */
    if ((tmp = strrchr(buf, ')')) != NULL) {
      // skip the ')' and the following space
      // but check that buffer is long enough
      tmp += 2;
      if (tmp < buf + n) {
        n = vsscanf(tmp, fmt, args);
      }
    }
  }

  fclose(f);

  return n;
}

static int SCANF_ARGS(2, 3) read_statdata(const char* procfile, _SCANFMT_ const char* fmt, ...) {
  int   n;
  va_list args;

  va_start(args, fmt);
  n = vread_statdata(procfile, fmt, args);
  va_end(args);
  return n;
}

static FILE* open_statfile(void) {
  FILE *f;

  if ((f = fopen("/proc/stat", "r")) == NULL) {
    static int haveWarned = 0;
    if (!haveWarned) {
      haveWarned = 1;
    }
  }
  return f;
}

static void
next_line(FILE *f) {
  int c;
  do {
    c = fgetc(f);
  } while (c != '\n' && c != EOF);
}

/**
 * Return the total number of ticks since the system was booted.
 * If the usedTicks parameter is not NULL, it will be filled with
 * the number of ticks spent on actual processes (user, system or
 * nice processes) since system boot. Note that this is the total number
 * of "executed" ticks on _all_ CPU:s, that is on a n-way system it is
 * n times the number of ticks that has passed in clock time.
 *
 * Returns a negative value if the reading of the ticks failed.
 */
static OSReturn get_total_ticks(int which_logical_cpu, CPUPerfTicks* pticks) {
  FILE*         fh;
  uint64_t      userTicks, niceTicks, systemTicks, idleTicks;
  uint64_t      iowTicks = 0, irqTicks = 0, sirqTicks= 0;
  int           logical_cpu = -1;
  const int     expected_assign_count = (-1 == which_logical_cpu) ? 4 : 5;
  int           n;

  if ((fh = open_statfile()) == NULL) {
    return OS_ERR;
  }
  if (-1 == which_logical_cpu) {
    n = fscanf(fh, "cpu " UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " "
            UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT,
            &userTicks, &niceTicks, &systemTicks, &idleTicks,
            &iowTicks, &irqTicks, &sirqTicks);
  } else {
    // Move to next line
    next_line(fh);

    // find the line for requested cpu faster to just iterate linefeeds?
    for (int i = 0; i < which_logical_cpu; i++) {
      next_line(fh);
    }

    n = fscanf(fh, "cpu%u " UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " "
               UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT,
               &logical_cpu, &userTicks, &niceTicks,
               &systemTicks, &idleTicks, &iowTicks, &irqTicks, &sirqTicks);
  }

  fclose(fh);
  if (n < expected_assign_count || logical_cpu != which_logical_cpu) {
#ifdef DEBUG_LINUX_PROC_STAT
    vm_fprintf(stderr, "[stat] read failed");
#endif
    return OS_ERR;
  }

#ifdef DEBUG_LINUX_PROC_STAT
  vm_fprintf(stderr, "[stat] read "
          UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " "
          UINT64_FORMAT " " UINT64_FORMAT " " UINT64_FORMAT " \n",
          userTicks, niceTicks, systemTicks, idleTicks,
          iowTicks, irqTicks, sirqTicks);
#endif

  pticks->used       = userTicks + niceTicks;
  pticks->usedKernel = systemTicks + irqTicks + sirqTicks;
  pticks->total      = userTicks + niceTicks + systemTicks + idleTicks +
                       iowTicks + irqTicks + sirqTicks;

  return OS_OK;
}


static int get_systemtype(void) {
  static int procEntriesType = UNDETECTED;
  DIR *taskDir;

  if (procEntriesType != UNDETECTED) {
    return procEntriesType;
  }

  // Check whether we have a task subdirectory
  if ((taskDir = opendir("/proc/self/task")) == NULL) {
    procEntriesType = UNDETECTABLE;
  } else {
    // The task subdirectory exists; we're on a Linux >= 2.6 system
    closedir(taskDir);
    procEntriesType = LINUX26_NPTL;
  }

  return procEntriesType;
}

/** read user and system ticks from a named procfile, assumed to be in 'stat' format then. */
static int read_ticks(const char* procfile, uint64_t* userTicks, uint64_t* systemTicks) {
  return read_statdata(procfile, "%*c %*d %*d %*d %*d %*d %*u %*u %*u %*u %*u " UINT64_FORMAT " " UINT64_FORMAT,
    userTicks, systemTicks);
}

/**
 * Return the number of ticks spent in any of the processes belonging
 * to the JVM on any CPU.
 */
static OSReturn get_jvm_ticks(CPUPerfTicks* pticks) {
  uint64_t userTicks;
  uint64_t systemTicks;

  if (get_systemtype() != LINUX26_NPTL) {
    return OS_ERR;
  }

  if (read_ticks("/proc/self/stat", &userTicks, &systemTicks) != 2) {
    return OS_ERR;
  }

  // get the total
  if (get_total_ticks(-1, pticks) != OS_OK) {
    return OS_ERR;
  }

  pticks->used       = userTicks;
  pticks->usedKernel = systemTicks;

  return OS_OK;
}

/**
 * Return the load of the CPU as a double. 1.0 means the CPU process uses all
 * available time for user or system processes, 0.0 means the CPU uses all time
 * being idle.
 *
 * Returns a negative value if there is a problem in determining the CPU load.
 */
static double get_cpu_load(int which_logical_cpu, CPUPerfCounters* counters, double* pkernelLoad, CpuLoadTarget target) {
  uint64_t udiff, kdiff, tdiff;
  CPUPerfTicks* pticks;
  CPUPerfTicks  tmp;
  double user_load;

  *pkernelLoad = 0.0;

  if (target == CPU_LOAD_VM_ONLY) {
    pticks = &counters->jvmTicks;
  } else if (-1 == which_logical_cpu) {
    pticks = &counters->cpus[counters->nProcs];
  } else {
    pticks = &counters->cpus[which_logical_cpu];
  }

  tmp = *pticks;

  if (target == CPU_LOAD_VM_ONLY) {
    if (get_jvm_ticks(pticks) != OS_OK) {
      return -1.0;
    }
  } else if (get_total_ticks(which_logical_cpu, pticks) != OS_OK) {
    return -1.0;
  }

  // seems like we sometimes end up with less kernel ticks when
  // reading /proc/self/stat a second time, timing issue between cpus?
  if (pticks->usedKernel < tmp.usedKernel) {
    kdiff = 0;
  } else {
    kdiff = pticks->usedKernel - tmp.usedKernel;
  }
  tdiff = pticks->total - tmp.total;
  udiff = pticks->used - tmp.used;

  if (tdiff == 0) {
    return 0.0;
  } else if (tdiff < (udiff + kdiff)) {
    tdiff = udiff + kdiff;
  }
  *pkernelLoad = (kdiff / (double)tdiff);
  // BUG9044876, normalize return values to sane values
  *pkernelLoad = MAX2<double>(*pkernelLoad, 0.0);
  *pkernelLoad = MIN2<double>(*pkernelLoad, 1.0);

  user_load = (udiff / (double)tdiff);
  user_load = MAX2<double>(user_load, 0.0);
  user_load = MIN2<double>(user_load, 1.0);

  return user_load;
}

static int SCANF_ARGS(1, 2) parse_stat(_SCANFMT_ const char* fmt, ...) {
  FILE *f;
  va_list args;

  va_start(args, fmt);

  if ((f = open_statfile()) == NULL) {
    va_end(args);
    return OS_ERR;
  }
  for (;;) {
    char line[80];
    if (fgets(line, sizeof(line), f) != NULL) {
      if (vsscanf(line, fmt, args) == 1) {
        fclose(f);
        va_end(args);
        return OS_OK;
      }
    } else {
        fclose(f);
        va_end(args);
        return OS_ERR;
    }
  }
}

static int get_noof_context_switches(uint64_t* switches) {
  return parse_stat("ctxt " UINT64_FORMAT "\n", switches);
}

/** returns boot time in _seconds_ since epoch */
static int get_boot_time(uint64_t* time) {
  return parse_stat("btime " UINT64_FORMAT "\n", time);
}

static int perf_context_switch_rate(double* rate) {
  static pthread_mutex_t contextSwitchLock = PTHREAD_MUTEX_INITIALIZER;
  static uint64_t      lastTime;
  static uint64_t      lastSwitches;
  static double        lastRate;

  uint64_t lt = 0;
  int res = 0;

  if (lastTime == 0) {
    uint64_t tmp;
    if (get_boot_time(&tmp) < 0) {
      return OS_ERR;
    }
    lt = tmp * 1000;
  }

  res = OS_OK;

  pthread_mutex_lock(&contextSwitchLock);
  {

    uint64_t sw;
    s8 t, d;

    if (lastTime == 0) {
      lastTime = lt;
    }

    t = os::javaTimeMillis();
    d = t - lastTime;

    if (d == 0) {
      *rate = lastRate;
    } else if (!get_noof_context_switches(&sw)) {
      *rate      = ( (double)(sw - lastSwitches) / d ) * 1000;
      lastRate     = *rate;
      lastSwitches = sw;
      lastTime     = t;
    } else {
      *rate = 0;
      res   = OS_ERR;
    }
    if (*rate <= 0) {
      *rate = 0;
      lastRate = 0;
    }
  }
  pthread_mutex_unlock(&contextSwitchLock);

  return res;
}

class CPUPerformanceInterface::CPUPerformance : public CHeapObj<mtInternal> {
  friend class CPUPerformanceInterface;
 private:
  CPUPerfCounters _counters;

  int cpu_load(int which_logical_cpu, double* cpu_load);
  int context_switch_rate(double* rate);
  int cpu_load_total_process(double* cpu_load);
  int cpu_loads_process(double* pjvmUserLoad, double* pjvmKernelLoad, double* psystemTotalLoad);

 public:
  CPUPerformance();
  bool initialize();
  ~CPUPerformance();
};

CPUPerformanceInterface::CPUPerformance::CPUPerformance() {
  _counters.nProcs = os::active_processor_count();
  _counters.cpus = NULL;
}

bool CPUPerformanceInterface::CPUPerformance::initialize() {
  size_t tick_array_size = (_counters.nProcs +1) * sizeof(CPUPerfTicks);
  _counters.cpus = (CPUPerfTicks*)NEW_C_HEAP_ARRAY(char, tick_array_size, mtInternal);
  if (NULL == _counters.cpus) {
    return false;
  }
  memset(_counters.cpus, 0, tick_array_size);

  // For the CPU load total
  get_total_ticks(-1, &_counters.cpus[_counters.nProcs]);

  // For each CPU
  for (int i = 0; i < _counters.nProcs; i++) {
    get_total_ticks(i, &_counters.cpus[i]);
  }
  // For JVM load
  get_jvm_ticks(&_counters.jvmTicks);

  // initialize context switch system
  // the double is only for init
  double init_ctx_switch_rate;
  perf_context_switch_rate(&init_ctx_switch_rate);

  return true;
}

CPUPerformanceInterface::CPUPerformance::~CPUPerformance() {
  if (_counters.cpus != NULL) {
    FREE_C_HEAP_ARRAY(char, _counters.cpus, mtInternal);
  }
}

int CPUPerformanceInterface::CPUPerformance::cpu_load(int which_logical_cpu, double* cpu_load) {
  double u, s;
  u = get_cpu_load(which_logical_cpu, &_counters, &s, CPU_LOAD_GLOBAL);
  if (u < 0) {
    *cpu_load = 0.0;
    return OS_ERR;
  }
  // Cap total systemload to 1.0
  *cpu_load = MIN2<double>((u + s), 1.0);
  return OS_OK;
}

int CPUPerformanceInterface::CPUPerformance::cpu_load_total_process(double* cpu_load) {
  double u, s;
  u = get_cpu_load(-1, &_counters, &s, CPU_LOAD_VM_ONLY);
  if (u < 0) {
    *cpu_load = 0.0;
    return OS_ERR;
  }
  *cpu_load = u + s;
  return OS_OK;
}

int CPUPerformanceInterface::CPUPerformance::cpu_loads_process(double* pjvmUserLoad, double* pjvmKernelLoad, double* psystemTotalLoad) {
  double u, s, t;

  assert(pjvmUserLoad != NULL, "pjvmUserLoad not inited");
  assert(pjvmKernelLoad != NULL, "pjvmKernelLoad not inited");
  assert(psystemTotalLoad != NULL, "psystemTotalLoad not inited");

  u = get_cpu_load(-1, &_counters, &s, CPU_LOAD_VM_ONLY);
  if (u < 0) {
    *pjvmUserLoad = 0.0;
    *pjvmKernelLoad = 0.0;
    *psystemTotalLoad = 0.0;
    return OS_ERR;
  }

  cpu_load(-1, &t);
  // clamp at user+system and 1.0
  if (u + s > t) {
    t = MIN2<double>(u + s, 1.0);
  }

  *pjvmUserLoad = u;
  *pjvmKernelLoad = s;
  *psystemTotalLoad = t;

  return OS_OK;
}

int CPUPerformanceInterface::CPUPerformance::context_switch_rate(double* rate) {
  return perf_context_switch_rate(rate);
}

CPUPerformanceInterface::CPUPerformanceInterface() {
  _impl = NULL;
}

bool CPUPerformanceInterface::initialize() {
  _impl = new CPUPerformanceInterface::CPUPerformance();
  return NULL == _impl ? false : _impl->initialize();
}

CPUPerformanceInterface::~CPUPerformanceInterface() {
  if (_impl != NULL) {
    delete _impl;
  }
}

int CPUPerformanceInterface::cpu_load(int which_logical_cpu, double* cpu_load) const {
  return _impl->cpu_load(which_logical_cpu, cpu_load);
}

int CPUPerformanceInterface::cpu_load_total_process(double* cpu_load) const {
  return _impl->cpu_load_total_process(cpu_load);
}

int CPUPerformanceInterface::cpu_loads_process(double* pjvmUserLoad, double* pjvmKernelLoad, double* psystemTotalLoad) const {
  return _impl->cpu_loads_process(pjvmUserLoad, pjvmKernelLoad, psystemTotalLoad);
}

int CPUPerformanceInterface::context_switch_rate(double* rate) const {
  return _impl->context_switch_rate(rate);
}

class SystemProcessInterface::SystemProcesses : public CHeapObj<mtInternal> {
  friend class SystemProcessInterface;
 private:
  class ProcessIterator : public CHeapObj<mtInternal> {
    friend class SystemProcessInterface::SystemProcesses;
   private:
    DIR*           _dir;
    struct dirent* _entry;
    bool           _valid;
    char           _exeName[PATH_MAX];
    char           _exePath[PATH_MAX];

    ProcessIterator();
    ~ProcessIterator();
    bool initialize();

    bool is_valid() const { return _valid; }
    bool is_valid_entry(struct dirent* entry) const;
    bool is_dir(const char* name) const;
    int  fsize(const char* name, uint64_t& size) const;

    char* allocate_string(const char* str) const;
    void  get_exe_name();
    char* get_exe_path();
    char* get_cmdline();

    int current(SystemProcess* process_info);
    int next_process();
  };

  ProcessIterator* _iterator;
  SystemProcesses();
  bool initialize();
  ~SystemProcesses();

  //information about system processes
  int system_processes(SystemProcess** system_processes, int* no_of_sys_processes) const;
};

bool SystemProcessInterface::SystemProcesses::ProcessIterator::is_dir(const char* name) const {
  struct stat mystat;
  int ret_val = 0;

  ret_val = stat(name, &mystat);
  if (ret_val < 0) {
    return false;
  }
  ret_val = S_ISDIR(mystat.st_mode);
  return ret_val > 0;
}

int SystemProcessInterface::SystemProcesses::ProcessIterator::fsize(const char* name, uint64_t& size) const {
  assert(name != NULL, "name pointer is NULL!");
  size = 0;
  struct stat fbuf;

  if (stat(name, &fbuf) < 0) {
    return OS_ERR;
  }
  size = fbuf.st_size;
  return OS_OK;
}

// if it has a numeric name, is a directory and has a 'stat' file in it
bool SystemProcessInterface::SystemProcesses::ProcessIterator::is_valid_entry(struct dirent* entry) const {
  char buffer[PATH_MAX];
  uint64_t size = 0;

  if (atoi(entry->d_name) != 0) {
    jio_snprintf(buffer, PATH_MAX, "/proc/%s", entry->d_name);
    buffer[PATH_MAX - 1] = '\0';

    if (is_dir(buffer)) {
      jio_snprintf(buffer, PATH_MAX, "/proc/%s/stat", entry->d_name);
      buffer[PATH_MAX - 1] = '\0';
      if (fsize(buffer, size) != OS_ERR) {
        return true;
      }
    }
  }
  return false;
}

// get exe-name from /proc/<pid>/stat
void SystemProcessInterface::SystemProcesses::ProcessIterator::get_exe_name() {
  FILE* fp;
  char  buffer[PATH_MAX];

  jio_snprintf(buffer, PATH_MAX, "/proc/%s/stat", _entry->d_name);
  buffer[PATH_MAX - 1] = '\0';
  if ((fp = fopen(buffer, "r")) != NULL) {
    if (fgets(buffer, PATH_MAX, fp) != NULL) {
      char* start, *end;
      // exe-name is between the first pair of ( and )
      start = strchr(buffer, '(');
      if (start != NULL && start[1] != '\0') {
        start++;
        end = strrchr(start, ')');
        if (end != NULL) {
          size_t len;
          len = MIN2<size_t>(end - start, sizeof(_exeName) - 1);
          memcpy(_exeName, start, len);
          _exeName[len] = '\0';
        }
      }
    }
    fclose(fp);
  }
}

// get command line from /proc/<pid>/cmdline
char* SystemProcessInterface::SystemProcesses::ProcessIterator::get_cmdline() {
  FILE* fp;
  char  buffer[PATH_MAX];
  char* cmdline = NULL;

  jio_snprintf(buffer, PATH_MAX, "/proc/%s/cmdline", _entry->d_name);
  buffer[PATH_MAX - 1] = '\0';
  if ((fp = fopen(buffer, "r")) != NULL) {
    size_t size = 0;
    char   dummy;

    // find out how long the file is (stat always returns 0)
    while (fread(&dummy, 1, 1, fp) == 1) {
      size++;
    }
    if (size > 0) {
      cmdline = NEW_C_HEAP_ARRAY(char, size + 1, mtInternal);
      if (cmdline != NULL) {
        cmdline[0] = '\0';
        if (fseek(fp, 0, SEEK_SET) == 0) {
          if (fread(cmdline, 1, size, fp) == size) {
            // the file has the arguments separated by '\0',
            // so we translate '\0' to ' '
            for (size_t i = 0; i < size; i++) {
              if (cmdline[i] == '\0') {
                cmdline[i] = ' ';
              }
            }
            cmdline[size] = '\0';
          }
        }
      }
    }
    fclose(fp);
  }
  return cmdline;
}

// get full path to exe from /proc/<pid>/exe symlink
char* SystemProcessInterface::SystemProcesses::ProcessIterator::get_exe_path() {
  char buffer[PATH_MAX];

  jio_snprintf(buffer, PATH_MAX, "/proc/%s/exe", _entry->d_name);
  buffer[PATH_MAX - 1] = '\0';
  return realpath(buffer, _exePath);
}

char* SystemProcessInterface::SystemProcesses::ProcessIterator::allocate_string(const char* str) const {
  if (str != NULL) {
    size_t len = strlen(str);
    char* tmp = NEW_C_HEAP_ARRAY(char, len+1, mtInternal);
    strncpy(tmp, str, len);
    tmp[len] = '\0';
    return tmp;
  }
  return NULL;
}

int SystemProcessInterface::SystemProcesses::ProcessIterator::current(SystemProcess* process_info) {
  if (!is_valid()) {
    return OS_ERR;
  }

  process_info->set_pid(atoi(_entry->d_name));

  get_exe_name();
  process_info->set_name(allocate_string(_exeName));

  if (get_exe_path() != NULL) {
     process_info->set_path(allocate_string(_exePath));
  }

  char* cmdline = NULL;
  cmdline = get_cmdline();
  if (cmdline != NULL) {
    process_info->set_command_line(allocate_string(cmdline));
    FREE_C_HEAP_ARRAY(char, cmdline, mtInternal);
  }

  return OS_OK;
}

int SystemProcessInterface::SystemProcesses::ProcessIterator::next_process() {
  if (!is_valid()) {
    return OS_ERR;
  }

  do {
    _entry = os::readdir(_dir);
    if (_entry == NULL) {
      // Error or reached end.  Could use errno to distinguish those cases.
      _valid = false;
      return OS_ERR;
    }
  } while(!is_valid_entry(_entry));

  _valid = true;
  return OS_OK;
}

SystemProcessInterface::SystemProcesses::ProcessIterator::ProcessIterator() {
  _dir = NULL;
  _entry = NULL;
  _valid = false;
}

bool SystemProcessInterface::SystemProcesses::ProcessIterator::initialize() {
  _dir = os::opendir("/proc");
  _entry = NULL;
  _valid = true;
  next_process();

  return true;
}

SystemProcessInterface::SystemProcesses::ProcessIterator::~ProcessIterator() {
  if (_dir != NULL) {
    os::closedir(_dir);
  }
}

SystemProcessInterface::SystemProcesses::SystemProcesses() {
  _iterator = NULL;
}

bool SystemProcessInterface::SystemProcesses::initialize() {
  _iterator = new SystemProcessInterface::SystemProcesses::ProcessIterator();
  return NULL == _iterator ? false : _iterator->initialize();
}

SystemProcessInterface::SystemProcesses::~SystemProcesses() {
  if (_iterator != NULL) {
    delete _iterator;
  }
}

int SystemProcessInterface::SystemProcesses::system_processes(SystemProcess** system_processes, int* no_of_sys_processes) const {
  assert(system_processes != NULL, "system_processes pointer is NULL!");
  assert(no_of_sys_processes != NULL, "system_processes counter pointers is NULL!");
  assert(_iterator != NULL, "iterator is NULL!");

  // initialize pointers
  *no_of_sys_processes = 0;
  *system_processes = NULL;

  while (_iterator->is_valid()) {
    SystemProcess* tmp = new SystemProcess();
    _iterator->current(tmp);

    //if already existing head
    if (*system_processes != NULL) {
      //move "first to second"
      tmp->set_next(*system_processes);
    }
    // new head
    *system_processes = tmp;
    // increment
    (*no_of_sys_processes)++;
    // step forward
    _iterator->next_process();
  }
  return OS_OK;
}

int SystemProcessInterface::system_processes(SystemProcess** system_procs, int* no_of_sys_processes) const {
  return _impl->system_processes(system_procs, no_of_sys_processes);
}

SystemProcessInterface::SystemProcessInterface() {
  _impl = NULL;
}

bool SystemProcessInterface::initialize() {
  _impl = new SystemProcessInterface::SystemProcesses();
  return NULL == _impl ? false : _impl->initialize();
}

SystemProcessInterface::~SystemProcessInterface() {
  if (_impl != NULL) {
    delete _impl;
  }
}

CPUInformationInterface::CPUInformationInterface() {
  _cpu_info = NULL;
}

bool CPUInformationInterface::initialize() {
  _cpu_info = new CPUInformation();
  if (NULL == _cpu_info) {
    return false;
  }
  _cpu_info->set_number_of_hardware_threads(VM_Version_Ext::number_of_threads());
  _cpu_info->set_number_of_cores(VM_Version_Ext::number_of_cores());
  _cpu_info->set_number_of_sockets(VM_Version_Ext::number_of_sockets());
  _cpu_info->set_cpu_name(VM_Version_Ext::cpu_name());
  _cpu_info->set_cpu_description(VM_Version_Ext::cpu_description());

  return true;
}

CPUInformationInterface::~CPUInformationInterface() {
  if (_cpu_info != NULL) {
    if (_cpu_info->cpu_name() != NULL) {
      const char* cpu_name = _cpu_info->cpu_name();
      FREE_C_HEAP_ARRAY(char, cpu_name, mtInternal);
      _cpu_info->set_cpu_name(NULL);
    }
    if (_cpu_info->cpu_description() != NULL) {
       const char* cpu_desc = _cpu_info->cpu_description();
       FREE_C_HEAP_ARRAY(char, cpu_desc, mtInternal);
      _cpu_info->set_cpu_description(NULL);
    }
    delete _cpu_info;
  }
}

int CPUInformationInterface::cpu_information(CPUInformation& cpu_info) {
  if (_cpu_info == NULL) {
    return OS_ERR;
  }

  cpu_info = *_cpu_info; // shallow copy assignment
  return OS_OK;
}

class NetworkPerformanceInterface::NetworkPerformance : public CHeapObj<mtInternal> {
  friend class NetworkPerformanceInterface;
 private:
  NetworkPerformance();
  NetworkPerformance(const NetworkPerformance& rhs); // no impl
  NetworkPerformance& operator=(const NetworkPerformance& rhs); // no impl
  bool initialize();
  ~NetworkPerformance();
  int64_t read_counter(const char* iface, const char* counter) const;
  int network_utilization(NetworkInterface** network_interfaces) const;
};

NetworkPerformanceInterface::NetworkPerformance::NetworkPerformance() {

}

bool NetworkPerformanceInterface::NetworkPerformance::initialize() {
  return true;
}

NetworkPerformanceInterface::NetworkPerformance::~NetworkPerformance() {
}

int64_t NetworkPerformanceInterface::NetworkPerformance::read_counter(const char* iface, const char* counter) const {
  char buf[128];

  snprintf(buf, sizeof(buf), "/sys/class/net/%s/statistics/%s", iface, counter);

  int fd = open(buf, O_RDONLY);
  if (fd == -1) {
    return -1;
  }

  ssize_t num_bytes = read(fd, buf, sizeof(buf));
  close(fd);
  if ((num_bytes == -1) || (num_bytes >= static_cast<ssize_t>(sizeof(buf))) || (num_bytes < 1)) {
    return -1;
  }

  buf[num_bytes] = '\0';
  int64_t value = strtoll(buf, NULL, 10);

  return value;
}

#ifdef __ANDROID__

/*
Copyright (c) 2013, Kenneth MacKay
All rights reserved.
Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "ifaddrs.h"

#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>
#include <net/if_arp.h>
#include <netinet/in.h>
#include <linux/netlink.h>
#include <linux/rtnetlink.h>

typedef struct NetlinkList
{
    struct NetlinkList *m_next;
    struct nlmsghdr *m_data;
    unsigned int m_size;
} NetlinkList;

static int netlink_socket(void)
{
    int l_socket = socket(PF_NETLINK, SOCK_RAW, NETLINK_ROUTE);
    if(l_socket < 0)
    {
        return -1;
    }
    
    struct sockaddr_nl l_addr;
    memset(&l_addr, 0, sizeof(l_addr));
    l_addr.nl_family = AF_NETLINK;
    if(bind(l_socket, (struct sockaddr *)&l_addr, sizeof(l_addr)) < 0)
    {
        close(l_socket);
        return -1;
    }
    
    return l_socket;
}

static int netlink_send(int p_socket, int p_request)
{
    char l_buffer[NLMSG_ALIGN(sizeof(struct nlmsghdr)) + NLMSG_ALIGN(sizeof(struct rtgenmsg))];
    memset(l_buffer, 0, sizeof(l_buffer));
    struct nlmsghdr *l_hdr = (struct nlmsghdr *)l_buffer;
    struct rtgenmsg *l_msg = (struct rtgenmsg *)NLMSG_DATA(l_hdr);
    
    l_hdr->nlmsg_len = NLMSG_LENGTH(sizeof(*l_msg));
    l_hdr->nlmsg_type = p_request;
    l_hdr->nlmsg_flags = NLM_F_ROOT | NLM_F_MATCH | NLM_F_REQUEST;
    l_hdr->nlmsg_pid = 0;
    l_hdr->nlmsg_seq = p_socket;
    l_msg->rtgen_family = AF_UNSPEC;
    
    struct sockaddr_nl l_addr;
    memset(&l_addr, 0, sizeof(l_addr));
    l_addr.nl_family = AF_NETLINK;
    return (sendto(p_socket, l_hdr, l_hdr->nlmsg_len, 0, (struct sockaddr *)&l_addr, sizeof(l_addr)));
}

static int netlink_recv(int p_socket, void *p_buffer, size_t p_len)
{
    struct msghdr l_msg;
    struct iovec l_iov = { p_buffer, p_len };
    struct sockaddr_nl l_addr;
    int l_result;

    for(;;)
    {
        l_msg.msg_name = (void *)&l_addr;
        l_msg.msg_namelen = sizeof(l_addr);
        l_msg.msg_iov = &l_iov;
        l_msg.msg_iovlen = 1;
        l_msg.msg_control = NULL;
        l_msg.msg_controllen = 0;
        l_msg.msg_flags = 0;
        int l_result = recvmsg(p_socket, &l_msg, 0);
        
        if(l_result < 0)
        {
            if(errno == EINTR)
            {
                continue;
            }
            return -2;
        }
        
        if(l_msg.msg_flags & MSG_TRUNC)
        { // buffer was too small
            return -1;
        }
        return l_result;
    }
}

static struct nlmsghdr *getNetlinkResponse(int p_socket, int *p_size, int *p_done)
{
    size_t l_size = 4096;
    void *l_buffer = NULL;
    
    for(;;)
    {
        free(l_buffer);
        l_buffer = malloc(l_size);
        
        int l_read = netlink_recv(p_socket, l_buffer, l_size);
        *p_size = l_read;
        if(l_read == -2)
        {
            free(l_buffer);
            return NULL;
        }
        if(l_read >= 0)
        {
            pid_t l_pid = getpid();
            struct nlmsghdr *l_hdr;
            for(l_hdr = (struct nlmsghdr *)l_buffer; NLMSG_OK(l_hdr, (unsigned int)l_read); l_hdr = (struct nlmsghdr *)NLMSG_NEXT(l_hdr, l_read))
            {
                if((pid_t)l_hdr->nlmsg_pid != l_pid || (int)l_hdr->nlmsg_seq != p_socket)
                {
                    continue;
                }
                
                if(l_hdr->nlmsg_type == NLMSG_DONE)
                {
                    *p_done = 1;
                    break;
                }
                
                if(l_hdr->nlmsg_type == NLMSG_ERROR)
                {
                    free(l_buffer);
                    return NULL;
                }
            }
            return (nlmsghdr*)l_buffer;
        }
        
        l_size *= 2;
    }
}

static NetlinkList *newListItem(struct nlmsghdr *p_data, unsigned int p_size)
{
    NetlinkList *l_item = (NetlinkList*)malloc(sizeof(NetlinkList));
    l_item->m_next = NULL;
    l_item->m_data = p_data;
    l_item->m_size = p_size;
    return l_item;
}

static void freeResultList(NetlinkList *p_list)
{
    NetlinkList *l_cur;
    while(p_list)
    {
        l_cur = p_list;
        p_list = p_list->m_next;
        free(l_cur->m_data);
        free(l_cur);
    }
}

static NetlinkList *getResultList(int p_socket, int p_request)
{
    if(netlink_send(p_socket, p_request) < 0)
    {
        return NULL;
    }

    NetlinkList *l_list = NULL;
    NetlinkList *l_end = NULL;
    int l_size;
    int l_done = 0;
    while(!l_done)
    {
        struct nlmsghdr *l_hdr = getNetlinkResponse(p_socket, &l_size, &l_done);
        if(!l_hdr)
        { // error
            freeResultList(l_list);
            return NULL;
        }
        
        NetlinkList *l_item = newListItem(l_hdr, l_size);
        if(!l_list)
        {
            l_list = l_item;
        }
        else
        {
            l_end->m_next = l_item;
        }
        l_end = l_item;
    }
    return l_list;
}

static size_t maxSize(size_t a, size_t b)
{
    return (a > b ? a : b);
}

static size_t calcAddrLen(sa_family_t p_family, int p_dataSize)
{
    switch(p_family)
    {
        case AF_INET:
            return sizeof(struct sockaddr_in);
        case AF_INET6:
            return sizeof(struct sockaddr_in6);
        case AF_PACKET:
            return maxSize(sizeof(struct sockaddr_ll), offsetof(struct sockaddr_ll, sll_addr) + p_dataSize);
        default:
            return maxSize(sizeof(struct sockaddr), offsetof(struct sockaddr, sa_data) + p_dataSize);
    }
}

static void makeSockaddr(sa_family_t p_family, struct sockaddr *p_dest, void *p_data, size_t p_size)
{
    switch(p_family)
    {
        case AF_INET:
            memcpy(&((struct sockaddr_in*)p_dest)->sin_addr, p_data, p_size);
            break;
        case AF_INET6:
            memcpy(&((struct sockaddr_in6*)p_dest)->sin6_addr, p_data, p_size);
            break;
        case AF_PACKET:
            memcpy(((struct sockaddr_ll*)p_dest)->sll_addr, p_data, p_size);
            ((struct sockaddr_ll*)p_dest)->sll_halen = p_size;
            break;
        default:
            memcpy(p_dest->sa_data, p_data, p_size);
            break;
    }
    p_dest->sa_family = p_family;
}

static void addToEnd(struct ifaddrs **p_resultList, struct ifaddrs *p_entry)
{
    if(!*p_resultList)
    {
        *p_resultList = p_entry;
    }
    else
    {
        struct ifaddrs *l_cur = *p_resultList;
        while(l_cur->ifa_next)
        {
            l_cur = l_cur->ifa_next;
        }
        l_cur->ifa_next = p_entry;
    }
}

static void interpretLink(struct nlmsghdr *p_hdr, struct ifaddrs **p_links, struct ifaddrs **p_resultList)
{
    struct ifinfomsg *l_info = (struct ifinfomsg *)NLMSG_DATA(p_hdr);

    size_t l_nameSize = 0;
    size_t l_addrSize = 0;
    size_t l_dataSize = 0;
    
    size_t l_rtaSize = NLMSG_PAYLOAD(p_hdr, sizeof(struct ifinfomsg));
    struct rtattr *l_rta;
    for(l_rta = (struct rtattr *)(((char *)l_info) + NLMSG_ALIGN(sizeof(struct ifinfomsg))); RTA_OK(l_rta, l_rtaSize); l_rta = RTA_NEXT(l_rta, l_rtaSize))
    {
        void *l_rtaData = RTA_DATA(l_rta);
        size_t l_rtaDataSize = RTA_PAYLOAD(l_rta);
        switch(l_rta->rta_type)
        {
            case IFLA_ADDRESS:
            case IFLA_BROADCAST:
                l_addrSize += NLMSG_ALIGN(calcAddrLen(AF_PACKET, l_rtaDataSize));
                break;
            case IFLA_IFNAME:
                l_nameSize += NLMSG_ALIGN(l_rtaSize + 1);
                break;
            case IFLA_STATS:
                l_dataSize += NLMSG_ALIGN(l_rtaSize);
                break;
            default:
                break;
        }
    }
    
    struct ifaddrs *l_entry = (struct ifaddrs *)malloc(sizeof(struct ifaddrs) + l_nameSize + l_addrSize + l_dataSize);
    memset(l_entry, 0, sizeof(struct ifaddrs));
    l_entry->ifa_name = "";
    
    char *l_name = ((char *)l_entry) + sizeof(struct ifaddrs);
    char *l_addr = l_name + l_nameSize;
    char *l_data = l_addr + l_addrSize;
    
    l_entry->ifa_flags = l_info->ifi_flags;
    
    l_rtaSize = NLMSG_PAYLOAD(p_hdr, sizeof(struct ifinfomsg));
    for(l_rta = (struct rtattr *)(((char *)l_info) + NLMSG_ALIGN(sizeof(struct ifinfomsg))); RTA_OK(l_rta, l_rtaSize); l_rta = RTA_NEXT(l_rta, l_rtaSize))
    {
        void *l_rtaData = RTA_DATA(l_rta);
        size_t l_rtaDataSize = RTA_PAYLOAD(l_rta);
        switch(l_rta->rta_type)
        {
            case IFLA_ADDRESS:
            case IFLA_BROADCAST:
            {
                size_t l_addrLen = calcAddrLen(AF_PACKET, l_rtaDataSize);
                makeSockaddr(AF_PACKET, (struct sockaddr *)l_addr, l_rtaData, l_rtaDataSize);
                ((struct sockaddr_ll *)l_addr)->sll_ifindex = l_info->ifi_index;
                ((struct sockaddr_ll *)l_addr)->sll_hatype = l_info->ifi_type;
                if(l_rta->rta_type == IFLA_ADDRESS)
                {
                    l_entry->ifa_addr = (struct sockaddr *)l_addr;
                }
                else
                {
                    l_entry->ifa_broadaddr = (struct sockaddr *)l_addr;
                }
                l_addr += NLMSG_ALIGN(l_addrLen);
                break;
            }
            case IFLA_IFNAME:
                strncpy(l_name, (const char*)l_rtaData, l_rtaDataSize);
                l_name[l_rtaDataSize] = '\0';
                l_entry->ifa_name = l_name;
                break;
            case IFLA_STATS:
                memcpy(l_data, l_rtaData, l_rtaDataSize);
                l_entry->ifa_data = l_data;
                break;
            default:
                break;
        }
    }
    
    addToEnd(p_resultList, l_entry);
    p_links[l_info->ifi_index - 1] = l_entry;
}

static void interpretAddr(struct nlmsghdr *p_hdr, struct ifaddrs **p_links, struct ifaddrs **p_resultList)
{
    struct ifaddrmsg *l_info = (struct ifaddrmsg *)NLMSG_DATA(p_hdr);

    size_t l_nameSize = 0;
    size_t l_addrSize = 0;
    
    int l_addedNetmask = 0;
    
    size_t l_rtaSize = NLMSG_PAYLOAD(p_hdr, sizeof(struct ifaddrmsg));
    struct rtattr *l_rta;
    for(l_rta = (struct rtattr *)(((char *)l_info) + NLMSG_ALIGN(sizeof(struct ifaddrmsg))); RTA_OK(l_rta, l_rtaSize); l_rta = RTA_NEXT(l_rta, l_rtaSize))
    {
        void *l_rtaData = RTA_DATA(l_rta);
        size_t l_rtaDataSize = RTA_PAYLOAD(l_rta);
        if(l_info->ifa_family == AF_PACKET)
        {
            continue;
        }
        
        switch(l_rta->rta_type)
        {
            case IFA_ADDRESS:
            case IFA_LOCAL:
                if((l_info->ifa_family == AF_INET || l_info->ifa_family == AF_INET6) && !l_addedNetmask)
                { // make room for netmask
                    l_addrSize += NLMSG_ALIGN(calcAddrLen(l_info->ifa_family, l_rtaDataSize));
                    l_addedNetmask = 1;
                }
            case IFA_BROADCAST:
                l_addrSize += NLMSG_ALIGN(calcAddrLen(l_info->ifa_family, l_rtaDataSize));
                break;
            case IFA_LABEL:
                l_nameSize += NLMSG_ALIGN(l_rtaSize + 1);
                break;
            default:
                break;
        }
    }
    
    struct ifaddrs *l_entry = (struct ifaddrs*)malloc(sizeof(struct ifaddrs) + l_nameSize + l_addrSize);
    memset(l_entry, 0, sizeof(struct ifaddrs));
    l_entry->ifa_name = p_links[l_info->ifa_index - 1]->ifa_name;
    
    char *l_name = ((char *)l_entry) + sizeof(struct ifaddrs);
    char *l_addr = l_name + l_nameSize;
    
    l_entry->ifa_flags = l_info->ifa_flags | p_links[l_info->ifa_index - 1]->ifa_flags;
    
    l_rtaSize = NLMSG_PAYLOAD(p_hdr, sizeof(struct ifaddrmsg));
    for(l_rta = (struct rtattr *)(((char *)l_info) + NLMSG_ALIGN(sizeof(struct ifaddrmsg))); RTA_OK(l_rta, l_rtaSize); l_rta = RTA_NEXT(l_rta, l_rtaSize))
    {
        void *l_rtaData = RTA_DATA(l_rta);
        size_t l_rtaDataSize = RTA_PAYLOAD(l_rta);
        switch(l_rta->rta_type)
        {
            case IFA_ADDRESS:
            case IFA_BROADCAST:
            case IFA_LOCAL:
            {
                size_t l_addrLen = calcAddrLen(l_info->ifa_family, l_rtaDataSize);
                makeSockaddr(l_info->ifa_family, (struct sockaddr *)l_addr, l_rtaData, l_rtaDataSize);
                if(l_info->ifa_family == AF_INET6)
                {
                    if(IN6_IS_ADDR_LINKLOCAL((struct in6_addr *)l_rtaData) || IN6_IS_ADDR_MC_LINKLOCAL((struct in6_addr *)l_rtaData))
                    {
                        ((struct sockaddr_in6 *)l_addr)->sin6_scope_id = l_info->ifa_index;
                    }
                }
                
                if(l_rta->rta_type == IFA_ADDRESS)
                { // apparently in a point-to-point network IFA_ADDRESS contains the dest address and IFA_LOCAL contains the local address
                    if(l_entry->ifa_addr)
                    {
                        l_entry->ifa_dstaddr = (struct sockaddr *)l_addr;
                    }
                    else
                    {
                        l_entry->ifa_addr = (struct sockaddr *)l_addr;
                    }
                }
                else if(l_rta->rta_type == IFA_LOCAL)
                {
                    if(l_entry->ifa_addr)
                    {
                        l_entry->ifa_dstaddr = l_entry->ifa_addr;
                    }
                    l_entry->ifa_addr = (struct sockaddr *)l_addr;
                }
                else
                {
                    l_entry->ifa_broadaddr = (struct sockaddr *)l_addr;
                }
                l_addr += NLMSG_ALIGN(l_addrLen);
                break;
            }
            case IFA_LABEL:
                strncpy(l_name, (const char*)l_rtaData, l_rtaDataSize);
                l_name[l_rtaDataSize] = '\0';
                l_entry->ifa_name = l_name;
                break;
            default:
                break;
        }
    }
    
    if(l_entry->ifa_addr && (l_entry->ifa_addr->sa_family == AF_INET || l_entry->ifa_addr->sa_family == AF_INET6))
    {
        unsigned l_maxPrefix = (l_entry->ifa_addr->sa_family == AF_INET ? 32 : 128);
        unsigned l_prefix = (l_info->ifa_prefixlen > l_maxPrefix ? l_maxPrefix : l_info->ifa_prefixlen);
        char l_mask[16] = {0};
        unsigned i;
        for(i=0; i<(l_prefix/8); ++i)
        {
            l_mask[i] = 0xff;
        }
        l_mask[i] = 0xff << (8 - (l_prefix % 8));
        
        makeSockaddr(l_entry->ifa_addr->sa_family, (struct sockaddr *)l_addr, l_mask, l_maxPrefix / 8);
        l_entry->ifa_netmask = (struct sockaddr *)l_addr;
    }
    
    addToEnd(p_resultList, l_entry);
}

static void interpret(int p_socket, NetlinkList *p_netlinkList, struct ifaddrs **p_links, struct ifaddrs **p_resultList)
{
    pid_t l_pid = getpid();
    for(; p_netlinkList; p_netlinkList = p_netlinkList->m_next)
    {
        unsigned int l_nlsize = p_netlinkList->m_size;
        struct nlmsghdr *l_hdr;
        for(l_hdr = p_netlinkList->m_data; NLMSG_OK(l_hdr, l_nlsize); l_hdr = NLMSG_NEXT(l_hdr, l_nlsize))
        {
            if((pid_t)l_hdr->nlmsg_pid != l_pid || (int)l_hdr->nlmsg_seq != p_socket)
            {
                continue;
            }
            
            if(l_hdr->nlmsg_type == NLMSG_DONE)
            {
                break;
            }
            
            if(l_hdr->nlmsg_type == RTM_NEWLINK)
            {
                interpretLink(l_hdr, p_links, p_resultList);
            }
            else if(l_hdr->nlmsg_type == RTM_NEWADDR)
            {
                interpretAddr(l_hdr, p_links, p_resultList);
            }
        }
    }
}

static unsigned countLinks(int p_socket, NetlinkList *p_netlinkList)
{
    unsigned l_links = 0;
    pid_t l_pid = getpid();
    for(; p_netlinkList; p_netlinkList = p_netlinkList->m_next)
    {
        unsigned int l_nlsize = p_netlinkList->m_size;
        struct nlmsghdr *l_hdr;
        for(l_hdr = p_netlinkList->m_data; NLMSG_OK(l_hdr, l_nlsize); l_hdr = NLMSG_NEXT(l_hdr, l_nlsize))
        {
            if((pid_t)l_hdr->nlmsg_pid != l_pid || (int)l_hdr->nlmsg_seq != p_socket)
            {
                continue;
            }
            
            if(l_hdr->nlmsg_type == NLMSG_DONE)
            {
                break;
            }
            
            if(l_hdr->nlmsg_type == RTM_NEWLINK)
            {
                ++l_links;
            }
        }
    }
    
    return l_links;
}

static int getifaddrs(struct ifaddrs **ifap)
{
    if(!ifap)
    {
        return -1;
    }
    *ifap = NULL;
    
    int l_socket = netlink_socket();
    if(l_socket < 0)
    {
        return -1;
    }
    
    NetlinkList *l_linkResults = getResultList(l_socket, RTM_GETLINK);
    if(!l_linkResults)
    {
        close(l_socket);
        return -1;
    }
    
    NetlinkList *l_addrResults = getResultList(l_socket, RTM_GETADDR);
    if(!l_addrResults)
    {
        close(l_socket);
        freeResultList(l_linkResults);
        return -1;
    }
    
    unsigned l_numLinks = countLinks(l_socket, l_linkResults) + countLinks(l_socket, l_addrResults);
    struct ifaddrs *l_links[l_numLinks];
    memset(l_links, 0, l_numLinks * sizeof(struct ifaddrs *));
    
    interpret(l_socket, l_linkResults, l_links, ifap);
    interpret(l_socket, l_addrResults, l_links, ifap);

    freeResultList(l_linkResults);
    freeResultList(l_addrResults);
    close(l_socket);
    return 0;
}

static void freeifaddrs(struct ifaddrs *ifa)
{
    struct ifaddrs *l_cur;
    while(ifa)
    {
        l_cur = ifa;
        ifa = ifa->ifa_next;
        free(l_cur);
    }
}
#endif

int NetworkPerformanceInterface::NetworkPerformance::network_utilization(NetworkInterface** network_interfaces) const
{
  ifaddrs* addresses;
  ifaddrs* cur_address;

  if (getifaddrs(&addresses) != 0) {
    return OS_ERR;
  }

  NetworkInterface* ret = NULL;
  for (cur_address = addresses; cur_address != NULL; cur_address = cur_address->ifa_next) {
    if ((cur_address->ifa_addr == NULL) || (cur_address->ifa_addr->sa_family != AF_PACKET)) {
      continue;
    }

    int64_t bytes_in = read_counter(cur_address->ifa_name, "rx_bytes");
    int64_t bytes_out = read_counter(cur_address->ifa_name, "tx_bytes");

    NetworkInterface* cur = new NetworkInterface(cur_address->ifa_name, bytes_in, bytes_out, ret);
    ret = cur;
  }

  freeifaddrs(addresses);
  *network_interfaces = ret;

  return OS_OK;
}

NetworkPerformanceInterface::NetworkPerformanceInterface() {
  _impl = NULL;
}

NetworkPerformanceInterface::~NetworkPerformanceInterface() {
  if (_impl != NULL) {
    delete _impl;
  }
}

bool NetworkPerformanceInterface::initialize() {
  _impl = new NetworkPerformanceInterface::NetworkPerformance();
  return _impl != NULL && _impl->initialize();
}

int NetworkPerformanceInterface::network_utilization(NetworkInterface** network_interfaces) const {
  return _impl->network_utilization(network_interfaces);
}
