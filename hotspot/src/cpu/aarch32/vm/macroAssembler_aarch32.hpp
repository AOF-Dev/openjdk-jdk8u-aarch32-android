/*
 * Copyright (c) 1997, 2012, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, 2015, Red Hat Inc. All rights reserved.
 * Copyright (c) 2015, Linaro Ltd. All rights reserved.
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

#ifndef CPU_AARCH32_VM_MACROASSEMBLER_AARCH32_HPP
#define CPU_AARCH32_VM_MACROASSEMBLER_AARCH32_HPP

#include "asm/assembler.hpp"
#include "nativeInst_aarch32.hpp"

// MacroAssembler extends Assembler by frequently used macros.
//
// Instructions for which a 'better' code sequence exists depending
// on arguments should also go in here.

class MacroAssembler: public Assembler {
  friend class LIR_Assembler;

  using Assembler::mov;

 protected:

  // Support for VM calls
  //
  // This is the base routine called by the different versions of call_VM_leaf. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
#ifdef CC_INTERP
  // c++ interpreter never wants to use interp_masm version of call_VM
  #define VIRTUAL
#else
  #define VIRTUAL virtual
#endif

  VIRTUAL void call_VM_leaf_base(
    address entry_point,               // the entry point
    int     number_of_arguments,        // the number of arguments to pop after the call
    Label *retaddr = NULL
  );

  VIRTUAL void call_VM_leaf_base(
    address entry_point,               // the entry point
    int     number_of_arguments,        // the number of arguments to pop after the call
    Label &retaddr) {
    call_VM_leaf_base(entry_point, number_of_arguments, &retaddr);
  }

  // This is the base routine called by the different versions of call_VM. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
  //
  // If no java_thread register is specified (noreg) than rthread will be used instead. call_VM_base
  // returns the register which contains the thread upon return. If a thread register has been
  // specified, the return value will correspond to that register. If no last_java_sp is specified
  // (noreg) than rsp will be used instead.
  VIRTUAL void call_VM_base(           // returns the register containing the thread upon return
    Register oop_result,               // where an oop-result ends up if any; use noreg otherwise
    Register java_thread,              // the thread if computed before     ; use noreg otherwise
    Register last_java_sp,             // to set up last_Java_frame in stubs; use noreg otherwise
    address  entry_point,              // the entry point
    int      number_of_arguments,      // the number of arguments (w/o thread) to pop after the call
    bool     check_exceptions          // whether to check for pending exceptions after return
  );

  // These routines should emit JVMTI PopFrame and ForceEarlyReturn handling code.
  // The implementation is only non-empty for the InterpreterMacroAssembler,
  // as only the interpreter handles PopFrame and ForceEarlyReturn requests.
  virtual void check_and_handle_popframe(Register java_thread);
  virtual void check_and_handle_earlyret(Register java_thread);

  void call_VM_helper(Register oop_result, address entry_point, int number_of_arguments, bool check_exceptions = true);

 public:
  void init_unseen_bytecodes();
  MacroAssembler(CodeBuffer* code) : Assembler(code) { init_unseen_bytecodes();}

  // Biased locking support
  // lock_reg and obj_reg must be loaded up with the appropriate values.
  // swap_reg is killed.
  // tmp_reg is optional. If it is supplied (i.e., != noreg) it will
  // be killed; if not supplied, push/pop will be used internally to
  // allocate a temporary (inefficient, avoid if possible).
  // Optional slow case is for implementations (interpreter and C1) which branch to
  // slow case directly. Leaves condition codes set for C2's Fast_Lock node.
  // Returns offset of first potentially-faulting instruction for null
  // check info (currently consumed only by C1). If
  // swap_reg_contains_mark is true then returns -1 as it is assumed
  // the calling code has already passed any potential faults.
  int biased_locking_enter(Register lock_reg, Register obj_reg,
                           Register swap_reg, Register tmp_reg,
                           bool swap_reg_contains_mark,
                           Label& done, Label* slow_case = NULL,
                           BiasedLockingCounters* counters = NULL);
  void biased_locking_exit (Register obj_reg, Register temp_reg, Label& done);


  // Helper functions for statistics gathering.
  // Unconditional atomic increment.
  void atomic_inc(Register counter_addr, Register tmp);
  void atomic_inc(Address counter_addr, Register tmp1, Register tmp2) {
    lea(tmp1, counter_addr);
    atomic_inc(tmp1, tmp2);
  }
  // Load Effective Address
  void lea(Register r, const Address &a) {
    InstructionMark im(this);
    code_section()->relocate(inst_mark(), a.rspec());
    a.lea(this, r);
  }

  virtual void _call_Unimplemented(address call_site) {
    mov(rscratch2, call_site);
    stop("HALT");
  }

#define call_Unimplemented() _call_Unimplemented((address)__PRETTY_FUNCTION__)

// macro assembly operations needed for aarch32

// first two private routines for loading 32 bit constants
//TODO Probably add back the 64-bit one as it will be useful for longs
private:

  int push(unsigned int bitset, Register stack);
  int pop(unsigned int bitset, Register stack);

public:

  void mov(Register dst, Address a, Condition cond = C_DFLT);


  void push(RegSet regs, Register stack) { if (regs.bits()) push(regs.bits(), stack); }
  void pop(RegSet regs, Register stack) { if (regs.bits()) pop(regs.bits(), stack); }

  // now mov instructions for loading absolute addresses and 32bit immediates

  inline void mov(Register dst, address addr, Condition cond = C_DFLT) {
    // TODO: Do Address end up as address and then passing through this method, after
    // being marked for relocation elsewhere? If not (as I suspect) then this can
    // be relaxed to mov_immediate to potentially produce shorter code sequences.
    mov_immediate32(dst, (uint32_t)addr, cond, false);
  }

  inline void mov(Register dst, long l, Condition cond = C_DFLT) {
    mov(dst, (uint32_t)l, cond);
  }
  inline void mov(Register dst, unsigned long l, Condition cond = C_DFLT) {
    mov(dst, (uint32_t)l, cond);
  }
  inline void mov(Register dst, int i, Condition cond = C_DFLT) {
    mov(dst, (uint32_t)i, cond);
  }
  inline void mov(Register dst, uint32_t i, Condition cond = C_DFLT) {
    mov_immediate(dst, i, cond, false);
  }

  inline void mov(Register dst, Register src, Condition cond = C_DFLT) {
    Assembler::mov(dst, src, cond);
  }
  inline void mov(Register dst, Register src, shift_op shift,
                  Condition cond = C_DFLT) {
    Assembler::mov(dst, src, shift, cond);
  }
  // TODO add sflag compatibility
  void movptr(Register r, uintptr_t imm32, Condition cond = C_DFLT);

  void ret(Register reg);

  // Both of these are aarch64 instructions that can easily be emulated
  // Note that this does not quite have the same semantics as aarch64
  // version as this updates the s flag.
  void cbz(Register r, Label& l) {
    cmp(r, 0);
    b(l, EQ);
  }
  void cbnz(Register r, Label& l) {
    cmp(r, 0);
    b(l, NE);
  }
  void tbz(Register r, unsigned bit, Label& l) {
    tst(r, 1 << bit);
    b(l, EQ);
  }
  void tbnz(Register r, unsigned bit, Label& l) {
    tst(r, 1 << bit);
    b(l, NE);
  }

  void addmw(Address a, Register incr, Register scratch) {
    ldr(scratch, a);
    add(scratch, scratch, incr);
    str(scratch, a);
  }

  // Add constant to memory word
  void addmw(Address a, int imm, Register scratch) {
    ldr(scratch, a);
    if (imm > 0)
      add(scratch, scratch, (unsigned)imm);
    else
      sub(scratch, scratch, (unsigned)-imm);
    str(scratch, a);
  }

// XXX stubs

  Register tlab_refill(Label& retry, Label& try_eden, Label& slow_case);

  // macro instructions for accessing and updating floating point
  // status register
  //
  // FPSR : op1 == 011
  //        CRn == 0100
  //        CRm == 0100
  //        op2 == 001

  inline void get_fpsr(Register reg = as_Register(0xf)) {
    vmrs(reg);
  }

  inline void set_fpsr(Register reg) {
    vmsr(reg);
  }

  inline void clear_fpsr() {
    mov(rscratch1, 0);
    set_fpsr(rscratch1);
  }

  // Support for NULL-checks
  //
  // Generates code that causes a NULL OS exception if the content of reg is NULL.
  // If the accessed location is M[reg + offset] and the offset is known, provide the
  // offset. No explicit code generation is needed if the offset is within a certain
  // range (0 <= offset <= page_size).

  virtual void null_check(Register reg, int offset = -1);
  static bool needs_explicit_null_check(intptr_t offset);

  static address target_addr_for_insn(address insn_addr, unsigned insn);
  static address target_addr_for_insn(address insn_addr) {
    unsigned insn = *(unsigned*)insn_addr;
    return target_addr_for_insn(insn_addr, insn);
  }

  // Required platform-specific helpers for Label::patch_instructions.
  // They _shadow_ the declarations in AbstractAssembler, which are undefined.
  static int pd_patch_instruction_size(address branch, address target);
  static void pd_patch_instruction(address branch, address target) {
    pd_patch_instruction_size(branch, target);
  }

#ifndef PRODUCT
  static void pd_print_patched_instruction(address branch);
#endif

  static int patch_oop(address insn_addr, address o);

  // The following 4 methods return the offset of the appropriate move instruction

  // Support for fast byte/short loading with zero extension (depending on particular CPU)
  int load_unsigned_byte(Register dst, Address src);
  int load_unsigned_short(Register dst, Address src);

  // Support for fast byte/short loading with sign extension (depending on particular CPU)
  int load_signed_byte(Register dst, Address src);
  int load_signed_short(Register dst, Address src);

  // Support for sign-extension (hi:lo = extend_sign(lo))
  void extend_sign(Register hi, Register lo);

  // Load and store values by size and signed-ness
  void load_sized_value(Register dst, Address src, size_t size_in_bytes, bool is_signed, Register dst2 = noreg);
  void store_sized_value(Address dst, Register src, size_t size_in_bytes, Register src2 = noreg);

  // Support for inc/dec with optimal instruction selection depending on value.
  // increment()/decrement() calls with an address destination will need to use
  // rscratch1 to load the value to be incremented. increment()/decrement()
  // calls which add or subtract a constant value greater than 2^12 will need
  // to use rscratch2 to hold the constant. So, a register increment()/
  // decrement() may trash rscratch2, and an address increment()/decrement()
  // may trash rscratch1 and rscratch2.
  void decrement(Register reg, int value = 1);
  void decrement(Address dst, int value = 1);
  void increment(Register reg, int value = 1);
  void increment(Address dst, int value = 1);

  // Alignment
  void align(int modulus);

  // Stack frame creation/removal
  void enter()
  {
    stmdb(sp, RegSet::of(rfp, lr).bits());
    add(rfp, sp, wordSize);
  }

  void leave()
  {
    sub(sp, rfp, wordSize);
    ldmia(sp, RegSet::of(rfp, lr).bits());
  }

  // Support for getting the JavaThread pointer (i.e.; a reference to thread-local information)
  // The pointer will be loaded into the thread register.
  void get_thread(Register thread);

  enum ret_type { ret_type_void, ret_type_integral, ret_type_float, ret_type_double};
  // Support for VM calls
  //
  // It is imperative that all calls into the VM are handled via the call_VM macros.
  // They make sure that the stack linkage is setup correctly. call_VM's correspond
  // to ENTRY/ENTRY_X entry points while call_VM_leaf's correspond to LEAF entry points.


  void call_VM(Register oop_result,
               address entry_point,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);

  // Overloadings with last_Java_sp
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               int number_of_arguments = 0,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, bool
               check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);

  void get_vm_result  (Register oop_result, Register thread);
  void get_vm_result_2(Register metadata_result, Register thread);

  // These always tightly bind to MacroAssembler::call_VM_base
  // bypassing the virtual implementation
  void super_call_VM(Register oop_result, Register last_java_sp, address entry_point, int number_of_arguments = 0, bool check_exceptions = true);
  void super_call_VM(Register oop_result, Register last_java_sp, address entry_point, Register arg_1, bool check_exceptions = true);
  void super_call_VM(Register oop_result, Register last_java_sp, address entry_point, Register arg_1, Register arg_2, bool check_exceptions = true);
  void super_call_VM(Register oop_result, Register last_java_sp, address entry_point, Register arg_1, Register arg_2, Register arg_3, bool check_exceptions = true);
  void super_call_VM(Register oop_result, Register last_java_sp, address entry_point, Register arg_1, Register arg_2, Register arg_3, Register arg_4, bool check_exceptions = true);

  void call_VM_leaf(address entry_point,
                    int number_of_arguments = 0);
  void call_VM_leaf(address entry_point,
                    Register arg_1);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2, Register arg_3);

  // These always tightly bind to MacroAssembler::call_VM_leaf_base
  // bypassing the virtual implementation
  void super_call_VM_leaf(address entry_point);
  void super_call_VM_leaf(address entry_point, Register arg_1);
  void super_call_VM_leaf(address entry_point, Register arg_1, Register arg_2);
  void super_call_VM_leaf(address entry_point, Register arg_1, Register arg_2, Register arg_3);
  void super_call_VM_leaf(address entry_point, Register arg_1, Register arg_2, Register arg_3, Register arg_4);

  // last Java Frame (fills frame anchor)
  void set_last_Java_frame(Register last_java_sp,
                           Register last_java_fp,
                           address last_java_pc,
                           Register scratch);

  void set_last_Java_frame(Register last_java_sp,
                           Register last_java_fp,
                           Label &last_java_pc,
                           Register scratch);

  void set_last_Java_frame(Register last_java_sp,
                           Register last_java_fp,
                           Register last_java_pc,
                           Register scratch);

  void reset_last_Java_frame(Register thread);

  // thread in the default location (rthread)
  void reset_last_Java_frame(bool clear_fp);

  // Stores
  void store_check(Register obj);                // store check for obj - register is destroyed afterwards
  void store_check(Register obj, Address dst);   // same as above, dst is exact store location (reg. is destroyed)

  void resolve_jobject(Register value, Register thread, Register tmp);
  void clear_jweak_tag(Register possibly_jweak);

#if INCLUDE_ALL_GCS

  void g1_write_barrier_pre(Register obj,
                            Register pre_val,
                            Register thread,
                            Register tmp,
                            bool tosca_live,
                            bool expand_call);

  void g1_write_barrier_post(Register store_addr,
                             Register new_val,
                             Register thread,
                             Register tmp,
                             Register tmp2);

#endif // INCLUDE_ALL_GCS

  // split store_check(Register obj) to enhance instruction interleaving
  void store_check_part_1(Register obj);
  void store_check_part_2(Register obj);

  // oop manipulations
  void load_klass(Register dst, Register src);
  void store_klass(Register dst, Register src);
  void cmp_klass(Register oop, Register trial_klass, Register tmp);

  void load_heap_oop(Register dst, Address src);

  void load_heap_oop_not_null(Register dst, Address src);
  void store_heap_oop(Address dst, Register src);

  // Used for storing NULL. All other oop constants should be
  // stored using routines that take a jobject.
  void store_heap_oop_null(Address dst);

  void load_prototype_header(Register dst, Register src);

  void store_klass_gap(Register dst, Register src);

  // This dummy is to prevent a call to store_heap_oop from
  // converting a zero (like NULL) into a Register by giving
  // the compiler two choices it can't resolve

  void store_heap_oop(Address dst, void* dummy);

  void push_CPU_state();
  void pop_CPU_state() ;

  // Round up to a power of two
  void round_to(Register reg, int modulus);

  // allocation
  void eden_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
  );
  void tlab_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
  );

  void verify_tlab();

  void incr_allocated_bytes(Register thread,
                            Register var_size_in_bytes, int con_size_in_bytes,
                            Register t1 = noreg);

  // interface method calling
  void lookup_interface_method(Register recv_klass,
                               Register intf_klass,
                               RegisterOrConstant itable_index,
                               Register method_result,
                               Register scan_temp,
                               Label& no_such_interface,
                               bool return_method = true);

  // virtual method calling
  // n.b. x86 allows RegisterOrConstant for vtable_index
  void lookup_virtual_method(Register recv_klass,
                             RegisterOrConstant vtable_index,
                             Register method_result);

  // Test sub_klass against super_klass, with fast and slow paths.

  // The fast path produces a tri-state answer: yes / no / maybe-slow.
  // One of the three labels can be NULL, meaning take the fall-through.
  // If super_check_offset is -1, the value is loaded up from super_klass.
  // No registers are killed, except temp_reg.
  void check_klass_subtype_fast_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     Label* L_slow_path,
                RegisterOrConstant super_check_offset = RegisterOrConstant(-1));

  // The rest of the type check; must be wired to a corresponding fast path.
  // It does not repeat the fast path logic, so don't use it standalone.
  // The temp_reg and temp2_reg can be noreg, if no temps are available.
  // Updates the sub's secondary super cache as necessary.
  // If set_cond_codes, condition codes will be Z on success, NZ on failure.
  void check_klass_subtype_slow_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Register temp2_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     bool set_cond_codes = false);

  // Simplified, combined version, good for typical uses.
  // Falls through on failure.
  void check_klass_subtype(Register sub_klass,
                           Register super_klass,
                           Register temp_reg,
                           Label& L_success);

  Address argument_address(RegisterOrConstant arg_slot, int extra_slot_offset = 0);


  // Debugging

  // only if +VerifyOops
  void verify_oop(Register reg, const char* s = "broken oop");
  void verify_oop_addr(Address addr, const char * s = "broken oop addr");

// TODO: verify method and klass metadata (compare against vptr?)
  void _verify_method_ptr(Register reg, const char * msg, const char * file, int line) {}
  void _verify_klass_ptr(Register reg, const char * msg, const char * file, int line){}

#define verify_method_ptr(reg) _verify_method_ptr(reg, "broken method " #reg, __FILE__, __LINE__)
#define verify_klass_ptr(reg) _verify_klass_ptr(reg, "broken klass " #reg, __FILE__, __LINE__)

  // only if +VerifyFPU
  void verify_FPU(int stack_depth, const char* s = "illegal FPU state");

  // prints msg, dumps registers and stops execution
  void stop(const char* msg);

  // prints msg and continues
  void warn(const char* msg);

  static void debug32(char* msg, int32_t pc, int32_t regs[]);

  void untested()                                { stop("untested"); }

  void unimplemented(const char* what = "")      { char* b = new char[1024];  jio_snprintf(b, 1024, "unimplemented: %s", what);  stop(b); }

#define should_not_reach_here() should_not_reach_here_line(__FILE__, __LINE__)
  void should_not_reach_here_line(const char *file, int line) {
#ifdef ASSERT
    mov(rscratch1, line);
    reg_printf_important(file);
    reg_printf_important(": %d", rscratch1);
#endif
    stop("should_not_reach_here");
  }

  // Stack overflow checking
  void bang_stack_with_offset(int offset) {
    // stack grows down, caller passes positive offset
    assert(offset > 0, "must bang with negative offset");
    // bang with random value from r0
    if (operand_valid_for_add_sub_immediate(offset)) {
      sub(rscratch2, sp, offset);
      strb(r0, Address(rscratch2));
    } else {
      mov(rscratch2, offset);
      strb(r0, Address(sp, rscratch2, Assembler::lsl(), Address::SUB));
    }
  }

  // Writes to stack successive pages until offset reached to check for
  // stack overflow + shadow pages.  Also, clobbers tmp
  void bang_stack_size(Register size, Register tmp);

  virtual RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr,
                                                Register tmp,
                                                int offset);

  // Support for serializing memory accesses between threads
  void serialize_memory(Register thread, Register tmp);

  // Arithmetics

  void addptr(Address dst, int32_t src) {
    lea(rscratch2, dst);
    ldr(rscratch1, Address(rscratch2));
    add(rscratch1, rscratch1, src);
    str(rscratch1, Address(rscratch2));
  }

  void cmpptr(Register src1, Address src2);

  void cmpxchgptr(Register oldv, Register newv, Register addr, Register tmp,
                  Label &suceed, Label *fail);

  void cmpxchgw(Register oldv, Register newv, Register addr, Register tmp,
                  Label &suceed, Label *fail);

  void atomic_add(Register prev, RegisterOrConstant incr, Register addr);
  void atomic_addw(Register prev, RegisterOrConstant incr, Register addr);

  void atomic_xchg(Register prev, Register newv, Register addr);
  void atomic_xchgw(Register prev, Register newv, Register addr);

  void orptr(Address adr, RegisterOrConstant src) {
    ldr(rscratch2, adr);
    if (src.is_register())
      orr(rscratch2, rscratch2, src.as_register());
    else
      orr(rscratch2, rscratch2, src.as_constant());
    str(rscratch2, adr);
  }

  // Calls

  void trampoline_call(Address entry, CodeBuffer *cbuf = NULL);

  static bool far_branches() {
    return ReservedCodeCacheSize > branch_range;
  }

  // Jumps that can reach anywhere in the code cache.
  // Trashes tmp.
  void far_call(Address entry, CodeBuffer *cbuf = NULL, Register tmp = rscratch1);
  void far_jump(Address entry, CodeBuffer *cbuf = NULL, Register tmp = rscratch1);

  static int far_branch_size() {
    // TODO performance issue: always generate real far jumps
    if (far_branches()) {
      if (VM_Version::features() & (FT_ARMV7 | FT_ARMV6T2))  {
        return 3 * NativeInstruction::arm_insn_sz;  // movw, movt, br
      } else {
        return 5 * NativeInstruction::arm_insn_sz;  // mov, 3 orr, br
      }
    } else {
      return NativeInstruction::arm_insn_sz; // br
    }
  }

  // Emit the CompiledIC call idiom
  void ic_call(address entry);

  // Data
  void mov_metadata(Register dst, Metadata* obj);
  Address allocate_metadata_address(Metadata* obj);
  Address constant_oop_address(jobject obj);

  void movoop(Register dst, jobject obj, bool immediate = false);

  void far_load(Register dst, address addr);
  void far_load_oop(Register dst, int oop_index);
  void far_load_metadata(Register dst, int metadata_index);
  void far_load_const(Register dst, address const);


  // CRC32 code for java.util.zip.CRC32::updateBytes() instrinsic.
  void kernel_crc32(Register crc, Register buf, Register len,
        Register table0, Register table1, Register table2, Register table3,
        Register tmp, Register tmp2, Register tmp3);

#undef VIRTUAL

  // Stack push and pop individual 64 bit registers
  void push(Register src);
  void pop(Register dst);

  // push all registers onto the stack
  void pusha();
  void popa();

  void repne_scan(Register addr, Register value, Register count,
                  Register scratch);
  void repne_scanw(Register addr, Register value, Register count,
                   Register scratch);

  // Form an address from base + offset in Rd. Rd may or may not actually be
  // used: you must use the Address that is returned. It is up to you to ensure
  // that the shift provided matches the size of your data.
  Address form_address(Register Rd, Register base, long byte_offset, int shift);

 public:

  void ldr_constant(Register dest, const Address &const_addr) {
    if (NearCpool) {
      ldr(dest, const_addr);
    } else {
      mov(dest, InternalAddress(const_addr.target()));
      ldr(dest, dest);
    }
  }

  address read_polling_page(Register r, address page, relocInfo::relocType rtype);
  address read_polling_page(Register r, relocInfo::relocType rtype);

  // CRC32 code for java.util.zip.CRC32::updateBytes() instrinsic.
  void update_byte_crc32(Register crc, Register val, Register table);
  void update_word_crc32(Register crc, Register v, Register tmp, Register tmp2,
        Register table0, Register table1, Register table2, Register table3);

  // Auto dispatch for barriers isb, dmb & dsb.
  void isb() {
    if(VM_Version::features() & FT_ARMV7) {
      Assembler::isb();
    } else {
      cp15isb();
    }
  }

  void dsb(enum barrier option) {
    if(VM_Version::features() & FT_ARMV7) {
      Assembler::dsb(option);
    } else {
      cp15dsb();
    }
  }

  void dmb(enum barrier option) {
    if(VM_Version::features() & FT_ARMV7) {
      Assembler::dmb(option);
    } else {
      cp15dmb();
    }
  }

  void membar(Membar_mask_bits order_constraint) {
    dmb(Assembler::barrier(order_constraint));
  }

  // ISB may be needed because of a safepoint
  void maybe_isb() { MacroAssembler::isb(); }

  // Helper functions for 64-bit multipliction, division and remainder
  // does <Rd+1:Rd> = <Rn+1:Rn> * <Rm+1:Rm>
  void mult_long(Register Rd, Register Rn, Register Rm);
  // does <Rdh:Rd> = <Rnh:Rn> * <Rmh:Rm>
  void mult_long(Register Rd, Register Rdh, Register Rn, Register Rnh, Register Rm, Register Rmh);

 private:
  void divide32(Register res, Register num, Register den, bool want_mod);
 public:
  // <Rd+1:Rd> = <Rn+1:Rn> / <Rm+1:Rm>
  // <Rd+1:Rd> = <Rn+1:Rn> % <Rm+1:Rm>
  // <Rd> = <Rn> / <Rm>
  // <Rd> = <Rn> % <Rm>
  void divide(Register Rd, Register Rn, Register Rm, int width, bool want_remainder);

  void extract_bits(Register dest, Register source, int lsb, int width);

  // These functions require that the src/dst register is an even register
  // and will emit LDREXD/STREXD if there are multiple cores and the procesor
  // supports it. If there's only one core then LDRD/STRD will be emit instead.
  // If the processor has multiple cores and doesn't support LDREXD/STREXD then
  // LDRD/STRD will be emitted and a warning message printed.
  void atomic_ldrd(Register Rt, Register RtII, Register Rbase);
  void atomic_strd(Register Rt, Register RtII, Register Rbase,
                   Register temp, Register tempII);

 private:
  // generic fallback ldrd generator. may need to use temporary register
  // when register collisions are found
  //
  // since double_ld_failed_dispatch can introduce address manipulation instructions
  // it should return offset of first load/store instruction that will be used
  // while constructing implicit null check table
  int double_ld_failed_dispatch(Register Rt, Register Rt2, const Address& adr,
                            void (Assembler::* mul)(unsigned, const Address&, Condition),
                            void (Assembler::* sgl)(Register, const Address&, Condition),
                            Register Rtmp, Condition cond);
  // ldrd/strd generator. can handle all strd cases and those ldrd where there
  // are no register collisions
  void double_ldst_failed_dispatch(Register Rt, Register Rt2, const Address& adr,
                            void (Assembler::* mul)(unsigned, const Address&, Condition),
                            void (Assembler::* sgl)(Register, const Address&, Condition),
                            Condition cond);
public:
  // override ldrd/strd to perform a magic for when Rt + 1 != Rt2 or any other
  // conditions which prevent to use single ldrd/strd insn. a pair of ldr/str
  // is used instead then
  //
  // Since ldrd/strd macro can introduce address manipulation instructions
  // it should return offset of first load/store instruction that will be used
  // while constructing implicit null check table
  using Assembler::ldrd;
  int ldrd(Register Rt, Register Rt2, const Address& adr, Register Rmp = rscratch1, Condition cond = C_DFLT);
  using Assembler::strd;
  int strd(Register Rt, Register Rt2, const Address& adr, Condition cond = C_DFLT);

private:
  void bfc_impl(Register rd, int lsb, int width, Condition cond);
public:
  void bfc(Register Rd, int lsb, int width, Condition cond = C_DFLT) {
    if (VM_Version::features() & (FT_ARMV6T2 | FT_ARMV7))
      Assembler::bfc(Rd, lsb, width, cond);
    else
      bfc_impl(Rd, lsb, width, cond);
  }

  void align_stack() {
    if (StackAlignmentInBytes > 4)
      bic(sp, sp, StackAlignmentInBytes-1);
  }

#ifdef ASSERT
  void verify_stack_alignment();
#endif

  // Debug helper
  void save_machine_state();
  void restore_machine_state();

  static uint32_t bytecodes_until_print;
  static uint32_t bytecodes_executed;
  static int enable_debug;
  static int enable_method_debug;
  static int enable_debugging_static;


  void bytecode_seen(Register bc_reg, Register scratch);
  static void print_unseen_bytecodes();
  void reg_printf_internal(bool important, const char *fmt, Register a = r0, Register b = r0, Register c = r0);
  void reg_printf_important(const char *fmt, Register a = r0, Register b = r0, Register c = r0);
  void reg_printf(const char *fmt, Register a = r0, Register b = r0, Register c = r0);
  void print_method_entry(Register rmethod, bool native);
  void print_method_exit(bool normal = true);
  void get_bytecode(Register bc, Register dst);
  static void print_cpool(InstanceKlass *klass);

  void create_breakpoint();
};


#ifdef ASSERT
inline bool AbstractAssembler::pd_check_instruction_mark() { return false; }
#endif

/**
 * class SkipIfEqual:
 *
 * Instantiating this class will result in assembly code being output that will
 * jump around any code emitted between the creation of the instance and it's
 * automatic destruction at the end of a scope block, depending on the value of
 * the flag passed to the constructor, which will be checked at run-time.
 */
class SkipIfEqual {
 private:
  MacroAssembler* _masm;
  Label _label;

 public:
   SkipIfEqual(MacroAssembler*, const bool* flag_addr, bool value);
   ~SkipIfEqual();
};

struct tableswitch {
  Register _reg;
  int _insn_index;
  jint _first_key;
  jint _last_key;
  Label _after;
  Label _branches;
};

#endif // CPU_AARCH32_VM_MACROASSEMBLER_AARCH32_HPP
