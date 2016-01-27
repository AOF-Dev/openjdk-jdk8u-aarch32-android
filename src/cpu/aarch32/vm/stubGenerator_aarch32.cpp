/*
 * Copyright (c) 2003, 2015, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "nativeInst_aarch32.hpp"
#include "oops/instanceOop.hpp"
#include "oops/method.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/oop.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/top.hpp"
#ifdef COMPILER2
#include "opto/runtime.hpp"
#endif


// Declaration and definition of StubGenerator (no .hpp file).
// For a more detailed description of the stub routine structure
// see the comment in stubRoutines.hpp

#undef __
#define __ _masm->
#define TIMES_OOP lsl(exact_log2(4))

#ifdef PRODUCT
#define BLOCK_COMMENT(str) /* nothing */
#else
#define BLOCK_COMMENT(str) __ block_comment(str)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

// Stub Code definitions

class StubGenerator: public StubCodeGenerator {
 private:

#ifdef PRODUCT
#define inc_counter_np(counter) ((void)0)
#else
  void inc_counter_np_(int& counter) {
    __ lea(rscratch2, ExternalAddress((address)&counter));
    __ ldr(rscratch1, Address(rscratch2));
    __ add(rscratch1, rscratch1, 1);
    __ str(rscratch1, Address(rscratch2));
  }
#define inc_counter_np(counter) \
  BLOCK_COMMENT("inc_counter " #counter); \
  inc_counter_np_(counter);
#endif

  // Call stubs are used to call Java from C
  //
  // There are only four registers available to house arguments and we're expecting eight
  // the layout will be as follows:

  // c_rarg0 = call wrapper address
  // c_rarg1 = result
  // c_rarg2 = result type
  // c_rarg3 = method
  // sp -> [ entry_point
  //         parameters -> java params
  //         parameter size (in words)
  //         thread] (address increasing)
  //
  // We don't
  // NEW!! layout for aarch32 so that save and restore can be collapsed into a single
  // load/store
  // layout of saved registers now is
  // 0   [ saved lr      ] <- rfp
  // -1  [ saved fp      ]
  // -2  [ r12/rthread   ] Thread passed in args
  // -3  [ r10/rmethod   ] NOTE omitted rfp as restored automatically
  // -4  [ r9/rscratch1  ] Platform register?
  // -5  [ r8/thread     ]
  // -6  [ r7/rcpool     ]
  // -7  [ r6/rlocals    ]
  // -8  [ r5/rbcp       ]
  // -9  [ r4/rdispatch  ]
  // -10 [ r2/res type   ]
  // -11 [ r1/result     ]
  // -12 [r0/call wrapper]<- sp (when restored from fp value)
  // -13 [ java arg0     ]
  //   ...
  // -xx [ java argn     ] <- sp on branch into java
  //
  // XXX Note we do not save floating point registers
  // Only floating point registers s16-31 / d8-15 need to be saved
  // these are never touched by template interpreted code.
  // On a sequence such as C -> Java -> C, the C functions will save them if used.

  static const int thread_off = -2 * wordSize; // The offset of the saved thread

  address generate_call_stub(address& return_address) {
    /*assert((int)frame::entry_frame_after_call_words == -(int)sp_after_call_off + 1 &&
           (int)frame::entry_frame_call_wrapper_offset == (int)call_wrapper_off,
           "adjust this code");*/

    StubCodeMark mark(this, "StubRoutines", "call_stub");
    address start = __ pc();
    __ reg_printf("entering call stub with { sp : %p, rfp : %p, lr : %p}\n", sp, rfp, lr);
    __ enter(); //save rfp & lr !!NOTE PUSHES TWO REGISTERS TO STACK

    int frame_header_size = 2 * wordSize;

    int entry_point_arg_off = frame_header_size,
        params_arg_off      = frame_header_size + 1 * wordSize,
        param_sz_arg_off    = frame_header_size + 2 * wordSize,
        thread_arg_off      = frame_header_size + 3 * wordSize;
    // r12 is a scratch register so we can clobber it to save thread
    // which is needed at the end
    __ ldr(r12, Address(sp, thread_arg_off));
    // r0, r2 - r8, r10, r12
    // we save r0 as the call_wrapper_address is needed elsewhere
    // we save r2, r3 as they holds the result type and location
    // which are needed on return
    unsigned c_save_regset = 0b0001011111110111;
    int nsaved = __ count_bits(c_save_regset);
    // Note the extra
    __ stmdb(sp, c_save_regset);

    //Don't count the frame here as already accounted for above
    entry_point_arg_off += wordSize * nsaved;
    params_arg_off      += wordSize * nsaved;
    param_sz_arg_off    += wordSize * nsaved;
    thread_arg_off      += wordSize * nsaved;

    // Offset from rfp to end of stack.
    int rfp_tos_offset_bytes = frame::offset_from_rfp_bytes + nsaved * wordSize;

    // install Java thread in global register now we have saved
    // whatever value it held
    __ mov(rthread, r12);
    // And method
    __ mov(rmethod, c_rarg3);
    //__ reg_printf("Set rthread to %p, rmethod to %p\n", rthread, rmethod);

    __ mov(r3, sp); //save sp for use with current argument offsets

#ifdef ASSERT
    // make sure we have no pending exceptions
    {
      Label L;
      __ ldr(rscratch1, Address(rthread, in_bytes(Thread::pending_exception_offset())));
      __ cmp(rscratch1, (unsigned)NULL_WORD);
      __ b(L, Assembler::EQ);
      __ stop("StubRoutines::call_stub: entered with pending exception");
      __ BIND(L);
    }
#endif
    __ ldr(rscratch1, Address(r3, params_arg_off));
    __ ldr(rscratch2, Address(r3, param_sz_arg_off));

    BLOCK_COMMENT("pass parameters if any");
    Label parameters_done;

    __ reg_printf("call_stub param_off = %p, param_sz = %d\n", rscratch1, rscratch2);
    __ cmp(rscratch2, 0);
    __ b(parameters_done, Assembler::EQ);

    // r14 makes ok temp as saved
    address loop = __ pc();
    __ ldr(r14, Address(__ post(rscratch1, wordSize)));
    __ subs(rscratch2, rscratch2, 1);

    // TODO remove
    __ reg_printf("\tARG SP[%d] : 0x%08x\n", rscratch2, r14);
    __ cmp(rscratch2, 0);
    // END TODO
    __ push(r14);
    __ b(loop, Assembler::GT);

    __ BIND(parameters_done);

    BLOCK_COMMENT("call Java function");
    __ ldr(rscratch1, Address(r3, entry_point_arg_off));

    static int depth = 0;

    __ reg_printf("Calling Java function with rfp = %p, sp = %p\n", rfp, sp);
    __ bl(rscratch1);
    // save current address for use by exception handling code
    return_address = __ pc();

    __ reg_printf("Returned to call_stub with rfp = %p, sp = %p\n", rfp, sp);


    // At this point rfp should be restored to the value it was set to before
    // use it to set the top of stack.
    __ sub(sp, rfp, rfp_tos_offset_bytes);

#ifdef ASSERT
    // verify that threads correspond
    __ ldr(r12, Address(rfp, thread_off));
    //rfp points to register stored in highest memory location - first on
    // stack, that's the saved lr, r12 is just below that
    // stored in r12 at this point
    {
      Label L, S;
      __ cmp(rthread, r12);
      __ b(S, Assembler::NE);
      __ get_thread(r12);
      __ cmp(rthread, r12);
      __ b(L, Assembler::EQ);
      __ BIND(S);
      __ stop("StubRoutines::call_stub: threads must correspond");
      __ BIND(L);
    }
#endif

    if(MacroAssembler::enable_debugging_static){
      // FIXME Remove this hacky debugging code
      Label L;
      __ ldr(rscratch2, Address(rthread, Thread::pending_exception_offset()));
      __ cbnz(rscratch2, L);
      // If we're returning via an exception then we shouldn't report exit,
      // the exception handler will have already reported the exit and reporting
      // via our progress through the call stub will result in an extra method
      // being reported as exited.
      __ print_method_exit();
      __ bind(L);
    }

    // NOTE Horrible tricks here
    // We saved c_rarg0, c_rarg1, c_rarg2, however we don't need to restore
    // c_rarg0, also if it's a long our return value will be in <r1:r0>.
    // this is a problem. Instead we alter the regset so that
    // c_rarg2 = result
    // c_rarg3 = result_type

    assert((c_save_regset & 0xf) == 0b0111, "change me");
    __ add(sp, sp, wordSize);
    int altered_saved_regset = (~0xf & c_save_regset) | 12;
    __ ldmia(sp, altered_saved_regset);

    // store result depending on type (everything that is not
    // T_OBJECT, T_LONG, T_FLOAT or T_DOUBLE is treated as T_INT)
    // n.b. this assumes Java returns an integral result in r0
    // and a floating result in j_farg0

    Label is_object, is_long, is_float, is_double, exit;
    __ cmp(c_rarg3, T_OBJECT);
    __ b(is_object, Assembler::EQ);
    __ cmp(c_rarg3, T_LONG);
    __ b(is_long, Assembler::EQ);
    __ cmp(c_rarg3, T_FLOAT);
    __ b(is_float, Assembler::EQ);
    __ cmp(c_rarg3, T_DOUBLE);
    __ b(is_double, Assembler::EQ);

    // handle T_INT case
    __ str(r0, Address(c_rarg2));

    __ BIND(exit);
    __ leave(); //Restore rfp, sp, lr
    __ reg_printf("leaving call stub with { sp : %p, rfp : %p, lr : %p}\n", sp, rfp, lr);
    // Pop arguments from stack.
    //__ add(sp, sp, 4 * wordSize);

    __ b(lr);

    // handle return types different from T_INT
    __ BIND(is_object);
    __ mov(r1, 0);

    __ BIND(is_long);
    __ strd(r0, r1, Address(c_rarg2, 0));
    __ b(exit, Assembler::AL);

    __ BIND(is_float);
    __ vstr_f32(j_farg0, Address(c_rarg2, 0));
    __ b(exit, Assembler::AL);

    __ BIND(is_double);
    __ vstr_f64(j_farg0, Address(c_rarg2, 0));
    __ b(exit, Assembler::AL);

    return start;
  }

  // Return point for a Java call if there's an exception thrown in
  // Java code.  The exception is caught and transformed into a
  // pending exception stored in JavaThread that can be tested from
  // within the VM.
  //
  // Note: Usually the parameters are removed by the callee. In case
  // of an exception crossing an activation frame boundary, that is
  // not the case if the callee is compiled code => need to setup the
  // rsp.
  //
  // r0: exception oop

  // NOTE: this is used as a target from the signal handler so it
  // needs an x86 prolog which returns into the current simulator
  // executing the generated catch_exception code. so the prolog
  // needs to install rax in a sim register and adjust the sim's
  // restart pc to enter the generated code at the start position
  // then return from native to simulated execution.

  address generate_catch_exception() {
    StubCodeMark mark(this, "StubRoutines", "catch_exception");
    address start = __ pc();

    // same as in generate_call_stub():
    const Address thread(rfp, thread_off);

#ifdef ASSERT
    // verify that threads correspond
    {
      Label L, S;
      __ ldr(rscratch1, thread);
      __ cmp(rthread, rscratch1);
      __ b(S, Assembler::NE);
      __ get_thread(rscratch1);
      __ cmp(rthread, rscratch1);
      __ b(L, Assembler::EQ);
      __ bind(S);
      __ stop("StubRoutines::catch_exception: threads must correspond");
      __ bind(L);
    }
#endif

    // set pending exception
    __ verify_oop(r0);

    __ str(r0, Address(rthread, Thread::pending_exception_offset()));
    __ mov(rscratch1, (address)__FILE__);
    __ str(rscratch1, Address(rthread, Thread::exception_file_offset()));
    __ mov(rscratch1, (int)__LINE__);
    __ str(rscratch1, Address(rthread, Thread::exception_line_offset()));

    // complete return to VM
    assert(StubRoutines::_call_stub_return_address != NULL,
           "_call_stub_return_address must have been generated before");
    __ b(StubRoutines::_call_stub_return_address);

    return start;
  }

  // Continuation point for runtime calls returning with a pending
  // exception.  The pending exception check happened in the runtime
  // or native call stub.  The pending exception in Thread is
  // converted into a Java-level exception.
  //
  // Contract with Java-level exception handlers:
  // r0: exception
  // r3: throwing pc
  //
  // NOTE: At entry of this stub, exception-pc must be in LR !!

  // NOTE: this is always used as a jump target within generated code
  // so it just needs to be generated code wiht no x86 prolog

  address generate_forward_exception() {
    //FIXME NOTE ON ALTERATION TO ARM32 IT WAS ASSUMED THAT rmethod
    // won't be used anymore and set on entry to the handler - is this true?

    Register spare = rmethod;

    StubCodeMark mark(this, "StubRoutines", "forward exception");
    address start = __ pc();

    // Upon entry, LR points to the return address returning into
    // Java (interpreted or compiled) code; i.e., the return address
    // becomes the throwing pc.
    //
    // Arguments pushed before the runtime call are still on the stack
    // but the exception handler will reset the stack pointer ->
    // ignore them.  A potential result in registers can be ignored as
    // well.

#ifdef ASSERT
    // make sure this code is only executed if there is a pending exception
    {
      Label L;
      __ ldr(rscratch1, Address(rthread, Thread::pending_exception_offset()));
      __ cbnz(rscratch1, L);
      __ stop("StubRoutines::forward exception: no pending exception (1)");
      __ bind(L);
    }
#endif

    // compute exception handler into r2

    // call the VM to find the handler address associated with the
    // caller address. pass thread in r0 and caller pc (ret address)
    // in r1. n.b. the caller pc is in lr, unlike x86 where it is on
    // the stack.
    __ mov(c_rarg1, lr);
    // lr will be trashed by the VM call so we move it to R2
    // (callee-saved) because we also need to pass it to the handler
    // returned by this call.
    __ mov(spare, lr); //note rscratch1 is a callee saved register
    BLOCK_COMMENT("call exception_handler_for_return_address");
    __ call_VM_leaf(CAST_FROM_FN_PTR(address,
                         SharedRuntime::exception_handler_for_return_address),
                    rthread, c_rarg1);
    // we should not really care that lr is no longer the callee
    // address. we saved the value the handler needs in r19 so we can
    // just copy it to r3. however, the C2 handler will push its own
    // frame and then calls into the VM and the VM code asserts that
    // the PC for the frame above the handler belongs to a compiled
    // Java method. So, we restore lr here to satisfy that assert.
    __ mov(lr, spare);
    // setup r0 & r3 & clear pending exception
    __ mov(r3, spare);
    __ mov(spare, r0);
    __ ldr(r0, Address(rthread, Thread::pending_exception_offset()));
    __ mov(rscratch1, 0);
    __ str(rscratch1, Address(rthread, Thread::pending_exception_offset()));

#ifdef ASSERT
    // make sure exception is set
    {
      Label L;
      __ cbnz(r0, L);
      __ stop("StubRoutines::forward exception: no pending exception (2)");
      __ bind(L);
    }
#endif
    // continue at exception handler
    // r0: exception
    // r3: throwing pc
    // spare: exception handler

    __ verify_oop(r0);
    __ b(spare);

    return start;
  }

  // Non-destructive plausibility checks for oops
  //
  // Arguments:
  //    r0: oop to verify
  //    rscratch1: error message
  //
  // Stack after saving c_rarg3:
  //    [tos + 0]: saved c_rarg3
  //    [tos + 1]: saved c_rarg2
  //    [tos + 2]: saved lr
  //    [tos + 3]: saved rscratch2
  //    [tos + 4]: saved r0
  //    [tos + 5]: saved rscratch1
  address generate_verify_oop() {
    StubCodeMark mark(this, "StubRoutines", "verify_oop");
    address start = __ pc();

    Label exit, error;

    // save c_rarg2 and c_rarg3
    __ stmdb(sp, RegSet::of(c_rarg2, c_rarg3).bits());

    // __ incrementl(ExternalAddress((address) StubRoutines::verify_oop_count_addr()));
    __ lea(c_rarg2, ExternalAddress((address) StubRoutines::verify_oop_count_addr()));
    __ ldr(c_rarg3, Address(c_rarg2));
    __ add(c_rarg3, c_rarg3, 1);
    __ str(c_rarg3, Address(c_rarg2));

    // object is in r0
    // make sure object is 'reasonable'
    __ cbz(r0, exit); // if obj is NULL it is OK

    // Check if the oop is in the right area of memory
    __ mov(c_rarg3, (intptr_t) Universe::verify_oop_mask());
    __ andr(c_rarg2, r0, c_rarg3);
    __ mov(c_rarg3, (intptr_t) Universe::verify_oop_bits());

    // Compare c_rarg2 and c_rarg3.  We don't use a compare
    // instruction here because the flags register is live.
    __ eor(c_rarg2, c_rarg2, c_rarg3);
    __ cbnz(c_rarg2, error);

    // make sure klass is 'reasonable', which is not zero.
    __ load_klass(r0, r0);  // get klass
    __ cbz(r0, error);      // if klass is NULL it is broken

    // return if everything seems ok
    __ bind(exit);

    __ ldmia(sp, RegSet::of(c_rarg2, c_rarg3).bits());
    __ b(lr);

    // handle errors
    __ bind(error);
    __ ldmia(sp, RegSet::of(c_rarg2, c_rarg3).bits());

    __ pusha();
    // Save old sp
    __ add(c_rarg2, sp, 14 * wordSize);
    __ str(c_rarg2, Address( __ pre(sp, -wordSize)));
    __ mov(c_rarg0, rscratch1);      // pass address of error message
    __ mov(c_rarg1, lr);             // pass return address
    __ mov(c_rarg2, sp);             // pass address of regs on stack
#ifndef PRODUCT
    assert(frame::arg_reg_save_area_bytes == 0, "not expecting frame reg save area");
#endif
    BLOCK_COMMENT("call MacroAssembler::debug");
    __ mov(rscratch1, CAST_FROM_FN_PTR(address, MacroAssembler::debug32));
    __ bl(rscratch1);
    __ hlt(0);

    return start;
  }

  // NOTE : very strange, I changed this but I don't know why the Address:(signed extend word) was here
  //void array_overlap_test(Label& L_no_overlap, Address sf) { __ b(L_no_overlap); }
  void array_overlap_test(Label& L_no_overlap) { __ b(L_no_overlap); }
  //no test being performed ?

  // Generate code for an array write pre barrier
  //
  //     addr    -  starting address
  //     count   -  element count
  //     tmp     - scratch register
  //
  //     Destroy no registers!
  //
  void  gen_write_ref_array_pre_barrier(Register addr, Register count, bool dest_uninitialized) {
    __ stop("werg");
    /*
    BarrierSet* bs = Universe::heap()->barrier_set();
    switch (bs->kind()) {
    case BarrierSet::G1SATBCTLogging:
      // With G1, don't generate the call if we statically know that the target in uninitialized
      if (!dest_uninitialized) {
        __ push(RegSet::range(r0, r29), sp);         // integer registers except lr & sp
        if (count == c_rarg0) {
          if (addr == c_rarg1) {
            // exactly backwards!!
            __ strd(c_rarg0, c_rarg1, __ pre(sp, -2 * wordSize));
            __ ldrd(c_rarg1, c_rarg0, __ post(sp, -2 * wordSize));
          } else {
            __ mov(c_rarg1, count);
            __ mov(c_rarg0, addr);
          }
        } else {
          __ mov(c_rarg0, addr);
          __ mov(c_rarg1, count);
        }
        __ call_VM_leaf(CAST_FROM_FN_PTR(address, BarrierSet::static_write_ref_array_pre), 2);
        __ pop(RegSet::range(r0, r29), sp);         // integer registers except lr & sp        }
        break;
      case BarrierSet::CardTableModRef:
      case BarrierSet::CardTableExtension:
      case BarrierSet::ModRef:
        break;
      default:
        ShouldNotReachHere();

      }
    }*/
  }

  //
  // Generate code for an array write post barrier
  //
  //  Input:
  //     start    - register containing starting address of destination array
  //     end      - register containing ending address of destination array
  //     scratch  - scratch register
  //
  //  The input registers are overwritten.
  //  The ending address is inclusive.
  void gen_write_ref_array_post_barrier(Register start, Register end, Register scratch) {
    __ stop("werg");
    /*
    assert_different_registers(start, end, scratch);
    BarrierSet* bs = Universe::heap()->barrier_set();
    switch (bs->kind()) {
      case BarrierSet::G1SATBCTLogging:

        {
          __ push(RegSet::range(r0, r29), sp);         // integer registers except lr & sp
          // must compute element count unless barrier set interface is changed (other platforms supply count)
          assert_different_registers(start, end, scratch);
          __ lea(scratch, Address(end, BytesPerHeapOop));
          __ sub(scratch, scratch, start);               // subtract start to get #bytes
          __ lsr(scratch, scratch, LogBytesPerHeapOop);  // convert to element count
          __ mov(c_rarg0, start);
          __ mov(c_rarg1, scratch);
          __ call_VM_leaf(CAST_FROM_FN_PTR(address, BarrierSet::static_write_ref_array_post), 2);
          __ pop(RegSet::range(r0, r29), sp);         // integer registers except lr & sp        }
        }
        break;
      case BarrierSet::CardTableModRef:
      case BarrierSet::CardTableExtension:
        {
          CardTableModRefBS* ct = (CardTableModRefBS*)bs;
          assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");

          Label L_loop;

           __ lsr(start, start, CardTableModRefBS::card_shift);
           __ lsr(end, end, CardTableModRefBS::card_shift);
           __ sub(end, end, start); // number of bytes to copy

          const Register count = end; // 'end' register contains bytes count now
          __ mov(scratch, (address)ct->byte_map_base);
          __ add(start, start, scratch);
          __ BIND(L_loop);
          __ mov(scratch, 0);
          __ strb(scratch, Address(start, count));
          __ subs(count, count, 1);
          __ b(L_loop, Assembler::HS);
        }
        break;
      default:
        ShouldNotReachHere();

    }*/
  }

  typedef enum {
    copy_forwards = 1,
    copy_backwards = -1
  } copy_direction;

  // Bulk copy of blocks of 8 words.
  //
  // count is a count of words.
  //
  // Precondition: count >= 2
  //
  // Postconditions:
  //
  // The least significant bit of count contains the remaining count
  // of words to copy.  The rest of count is trash.
  //
  // s and d are adjusted to point to the remaining words to copy
  //
  void generate_copy_longs(Label &start, Register s, Register d, Register count,
                           copy_direction direction) {
    int unit = wordSize * direction;

    // FIXME - This clobbers basically all registers that have java values in,
    // find out when this is called - looks like only from opto, hopefully we're ok
    __ stop("fixme");
    int offset;
    const Register t0 = r3, t1 = r4, t2 = r5, t3 = r6,
      t4 = r7, t5 = r8, t6 = r10, t7 = r11; //!! clobbers rfp?!

    assert_different_registers(rscratch1, t0, t1, t2, t3, t4, t5, t6, t7);
    assert_different_registers(s, d, count, rscratch1);

    Label again, large, small;
    __ align(6);
    __ bind(start);
    __ cmp(count, 8);
    __ b(small, Assembler::LO);
    if (direction == copy_forwards) {
      __ sub(s, s, 2 * wordSize);
      __ sub(d, d, 2 * wordSize);
    }
    __ subs(count, count, 16);
    __ b(large, Assembler::GE);

    // 8 <= count < 16 words.  Copy 8.
    __ ldrd(t0, t1, Address(s, 2 * unit));
    __ ldrd(t2, t3, Address(s, 4 * unit));
    __ ldrd(t4, t5, Address(s, 6 * unit));
    __ ldrd(t6, t7, Address(__ pre(s, 8 * unit)));

    __ strd(t0, t1, Address(d, 2 * unit));
    __ strd(t2, t3, Address(d, 4 * unit));
    __ strd(t4, t5, Address(d, 6 * unit));
    __ strd(t6, t7, Address(__ pre(d, 8 * unit)));

    if (direction == copy_forwards) {
      __ add(s, s, 2 * wordSize);
      __ add(d, d, 2 * wordSize);
    }

    {
      Label L1, L2;
      __ bind(small);
      __ tbz(count, exact_log2(4), L1);
      __ ldrd(t0, t1, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ ldrd(t2, t3, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ strd(t0, t1, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ strd(t2, t3, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ bind(L1);

      __ tbz(count, 1, L2);
      __ ldrd(t0, t1, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ strd(t0, t1, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ bind(L2);
    }

    __ b(lr);

    __ align(6);
    __ bind(large);

    // Fill 8 registers
    __ ldrd(t0, t1, Address(s, 2 * unit));
    __ ldrd(t2, t3, Address(s, 4 * unit));
    __ ldrd(t4, t5, Address(s, 6 * unit));
    __ ldrd(t6, t7, Address(__ pre(s, 8 * unit)));

    __ bind(again);

    if (direction == copy_forwards && PrefetchCopyIntervalInBytes > 0) {
      __ stop("Implement prefetch");
      //__ prfm(Address(s, PrefetchCopyIntervalInBytes), PLDL1KEEP);
    }

    __ strd(t0, t1, Address(d, 2 * unit));
    __ ldrd(t0, t1, Address(s, 2 * unit));
    __ strd(t2, t3, Address(d, 4 * unit));
    __ ldrd(t2, t3, Address(s, 4 * unit));
    __ strd(t4, t5, Address(d, 6 * unit));
    __ ldrd(t4, t5, Address(s, 6 * unit));
    __ strd(t6, t7, Address(__ pre(d, 8 * unit)));
    __ ldrd(t6, t7, Address(__ pre(s, 8 * unit)));

    __ subs(count, count, 8);
    __ b(again, Assembler::HS);

    // Drain
    __ strd(t0, t1, Address(d, 2 * unit));
    __ strd(t2, t3, Address(d, 4 * unit));
    __ strd(t4, t5, Address(d, 6 * unit));
    __ strd(t6, t7, Address(__ pre(d, 8 * unit)));

    if (direction == copy_forwards) {
      __ add(s, s, 2 * wordSize);
      __ add(d, d, 2 * wordSize);
    }

    {
      Label L1, L2;
      __ tbz(count, exact_log2(4), L1);
      __ ldrd(t0, t1, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ ldrd(t2, t3, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ strd(t0, t1, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ strd(t2, t3, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ bind(L1);

      __ tbz(count, 1, L2);
      __ ldrd(t0, t1, Address(__ adjust(s, 2 * unit, direction == copy_backwards)));
      __ strd(t0, t1, Address(__ adjust(d, 2 * unit, direction == copy_backwards)));
      __ bind(L2);
    }

    __ b(lr);

  }

  // Small copy: less than 16 bytes.
  //
  // NB: Ignores all of the bits of count which represent more than 15
  // bytes, so a caller doesn't have to mask them.

  void copy_memory_small(Register s, Register d, Register count, Register tmp, int step) {
    bool is_backwards = step < 0;
    size_t granularity = uabs(step);
    int direction = is_backwards ? -1 : 1;
    int unit = wordSize * direction;

    Label Lpair, Lword, Lint, Lshort, Lbyte;

    assert(granularity
           && granularity <= sizeof (jlong), "Impossible granularity in copy_memory_small");

    const Register t0 = r3, t1 = r4, t2 = r5, t3 = r6;

    // ??? I don't know if this bit-test-and-branch is the right thing
    // to do.  It does a lot of jumping, resulting in several
    // mispredicted branches.  It might make more sense to do this
    // with something like Duff's device with a single computed branch.

    __ tbz(count, 3 - exact_log2(granularity), Lword);
    __ ldr(tmp, Address(__ adjust(s, unit, is_backwards)));
    __ str(tmp, Address(__ adjust(d, unit, is_backwards)));
    __ bind(Lword);

    if (granularity <= sizeof (jint)) {
      __ tbz(count, 2 - exact_log2(granularity), Lint);
      __ ldr(tmp, Address(__ adjust(s, sizeof (jint) * direction, is_backwards)));
      __ str(tmp, Address(__ adjust(d, sizeof (jint) * direction, is_backwards)));
      __ bind(Lint);
    }

    if (granularity <= sizeof (jshort)) {
      __ tbz(count, 1 - exact_log2(granularity), Lshort);
      __ ldrh(tmp, Address(__ adjust(s, sizeof (jshort) * direction, is_backwards)));
      __ strh(tmp, Address(__ adjust(d, sizeof (jshort) * direction, is_backwards)));
      __ bind(Lshort);
    }

    if (granularity <= sizeof (jbyte)) {
      __ tbz(count, 0, Lbyte);
      __ ldrb(tmp, Address(__ adjust(s, sizeof (jbyte) * direction, is_backwards)));
      __ strb(tmp, Address(__ adjust(d, sizeof (jbyte) * direction, is_backwards)));
      __ bind(Lbyte);
    }
  }

  Label copy_f, copy_b;

  // All-singing all-dancing memory copy.
  //
  // Copy count units of memory from s to d.  The size of a unit is
  // step, which can be positive or negative depending on the direction
  // of copy.  If is_aligned is false, we align the source address.
  //

  void copy_memory(bool is_aligned, Register s, Register d,
                   Register count, Register tmp, int step) {
    copy_direction direction = step < 0 ? copy_backwards : copy_forwards;
    bool is_backwards = step < 0;
    int granularity = uabs(step);
    const Register t0 = r3, t1 = r4;

    if (is_backwards) {
      __ lea(s, Address(s, count, lsl(exact_log2(-step))));
      __ lea(d, Address(d, count, lsl(exact_log2(-step))));
    }

    Label done, tail;

    __ cmp(count, 16/granularity);
    __ b(tail, Assembler::LO);

    // Now we've got the small case out of the way we can align the
    // source address on a 2-word boundary.

    Label aligned;

    if (is_aligned) {
      // We may have to adjust by 1 word to get s 2-word-aligned.
      __ tbz(s, exact_log2(wordSize), aligned);
      __ ldr(tmp, Address(__ adjust(s, direction * wordSize, is_backwards)));
      __ str(tmp, Address(__ adjust(d, direction * wordSize, is_backwards)));
      __ sub(count, count, wordSize/granularity);
    } else {
      if (is_backwards) {
        __ andr(rscratch2, s, 2 * wordSize - 1);
      } else {
        __ neg(rscratch2, s);
        __ andr(rscratch2, rscratch2, 2 * wordSize - 1);
      }
      // rscratch2 is the byte adjustment needed to align s.
      __ cbz(rscratch2, aligned);
      __ lsr(rscratch2, rscratch2, exact_log2(granularity));
      __ sub(count, count, rscratch2);

#if 0
      // ?? This code is only correct for a disjoint copy.  It may or
      // may not make sense to use it in that case.

      // Copy the first pair; s and d may not be aligned.
      __ ldrd(t0, t1, Address(s, is_backwards ? -2 * wordSize : 0));
      __ strd(t0, t1, Address(d, is_backwards ? -2 * wordSize : 0));

      // Align s and d, adjust count
      if (is_backwards) {
        __ sub(s, s, rscratch2);
        __ sub(d, d, rscratch2);
      } else {
        __ add(s, s, rscratch2);
        __ add(d, d, rscratch2);
      }
#else
      copy_memory_small(s, d, rscratch2, rscratch1, step);
#endif
    }

    __ cmp(count, 16/granularity);
    __ b(tail, Assembler::LT);
    __ bind(aligned);

    // s is now 2-word-aligned.

    // We have a count of units and some trailing bytes.  Adjust the
    // count and do a bulk copy of words.
    // hacky fix
    if(0 == wordSize/granularity) {
      __ lsr(rscratch2, count, 0U);
    } else {
      __ lsr(rscratch2, count, exact_log2(wordSize/granularity));
    }
    if (direction == copy_forwards)
      __ bl(copy_f);
    else
      __ bl(copy_b);

    // And the tail.

    __ bind(tail);
    copy_memory_small(s, d, count, tmp, step);
  }


  void clobber_registers() {
#ifdef ASSERT
    __ mov(rscratch1, (u_int32_t)0xdeadbeef);
    for (Register r = r3; r <= r14; r++)
      if (r != rscratch1) __ mov(r, rscratch1);
#endif
  }

  // Scan over array at a for count oops, verifying each one.
  // Preserves a and count, clobbers rscratch1 and rscratch2.
  void verify_oop_array (size_t size, Register a, Register count, Register temp) {
    Label loop, end;
    __ mov(rscratch1, a);
    __ mov(rscratch2, 0);
    __ bind(loop);
    __ cmp(rscratch2, count);
    __ b(end, Assembler::HS);
    if (size == (size_t)wordSize) {
      __ ldr(temp, Address(a, rscratch2, lsl(exact_log2(size))));
      __ verify_oop(temp);
    } else {
      //__ ldrw(r16, Address(a, rscratch2, lsl(exact_log2(size))));
     //__ decode_heap_oop(temp); // calls verify_oop
     assert(false, "decode_heap_oop Doesn't exist");
    }
    __ add(rscratch2, rscratch2, size);
    __ b(loop);
    __ bind(end);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  // Side Effects:
  //   disjoint_int_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_int_oop_copy().
  //
  address generate_disjoint_copy(size_t size, bool aligned, bool is_oop, address *entry,
                                  const char *name, bool dest_uninitialized = false) {
    Register s = c_rarg0, d = c_rarg1, count = c_rarg2;
    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();
    if (entry != NULL) {
      *entry = __ pc();
      // caller can pass a 64-bit byte count here (from Unsafe.copyMemory)
      BLOCK_COMMENT("Entry:");
    }
    __ enter();
    if (is_oop) {
      __ push(RegSet::of(d, count), sp);
      // no registers are destroyed by this call
      gen_write_ref_array_pre_barrier(d, count, dest_uninitialized);
    }
    copy_memory(aligned, s, d, count, rscratch1, size);
    if (is_oop) {
      __ pop(RegSet::of(d, count), sp);
      if (VerifyOops)
        __ stop("werg");
        //verify_oop_array(size, d, count, r16);
      __ sub(count, count, 1); // make an inclusive end pointer
      __ lea(count, Address(d, count, lsl(exact_log2(size))));
      gen_write_ref_array_post_barrier(d, count, rscratch1);
    }
    __ leave();
    __ b(lr);
    return start;
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  address generate_conjoint_copy(size_t size, bool aligned, bool is_oop, address nooverlap_target,
                                 address *entry, const char *name,
                                 bool dest_uninitialized = false) {
    Register s = c_rarg0, d = c_rarg1, count = c_rarg2;

    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    __ cmp(d, s);
    __ b(nooverlap_target, Assembler::LS);

    __ enter();
    if (is_oop) {
      __ push(RegSet::of(d, count), sp);
      // no registers are destroyed by this call
      gen_write_ref_array_pre_barrier(d, count, dest_uninitialized);
    }
    copy_memory(aligned, s, d, count, rscratch1, -size);
    if (is_oop) {
      __ pop(RegSet::of(d, count), sp);
      if (VerifyOops)
        __ stop("werg");
        //verify_oop_array(size, d, count, r16);
      __ sub(count, count, 1); // make an inclusive end pointer
      __ lea(count, Address(d, count, lsl(exact_log2(size))));
      gen_write_ref_array_post_barrier(d, count, rscratch1);
    }
    __ leave();
    __ b(lr);
    return start;
}

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-, 2-, or 1-byte boundaries,
  // we let the hardware handle it.  The one to eight bytes within words,
  // dwords or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  // Side Effects:
  //   disjoint_byte_copy_entry is set to the no-overlap entry point  //
  // If 'from' and/or 'to' are aligned on 4-, 2-, or 1-byte boundaries,
  // we let the hardware handle it.  The one to eight bytes within words,
  // dwords or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  // Side Effects:
  //   disjoint_byte_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_byte_copy().
  //
  address generate_disjoint_byte_copy(bool aligned, address* entry, const char *name) {
    const bool not_oop = false;
    return generate_disjoint_copy(sizeof (jbyte), aligned, not_oop, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-, 2-, or 1-byte boundaries,
  // we let the hardware handle it.  The one to eight bytes within words,
  // dwords or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  address generate_conjoint_byte_copy(bool aligned, address nooverlap_target,
                                      address* entry, const char *name) {
    const bool not_oop = false;
    return generate_conjoint_copy(sizeof (jbyte), aligned, not_oop, nooverlap_target, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4- or 2-byte boundaries, we
  // let the hardware handle it.  The two or four words within dwords
  // or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  // Side Effects:
  //   disjoint_short_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_short_copy().
  //
  address generate_disjoint_short_copy(bool aligned,
                                       address* entry, const char *name) {
    const bool not_oop = false;
    return generate_disjoint_copy(sizeof (jshort), aligned, not_oop, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4- or 2-byte boundaries, we
  // let the hardware handle it.  The two or four words within dwords
  // or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  address generate_conjoint_short_copy(bool aligned, address nooverlap_target,
                                       address *entry, const char *name) {
    const bool not_oop = false;
    return generate_conjoint_copy(sizeof (jshort), aligned, not_oop, nooverlap_target, entry, name);

  }
  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  // Side Effects:
  //   disjoint_int_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_int_oop_copy().
  //
  address generate_disjoint_int_copy(bool aligned, address *entry,
                                         const char *name, bool dest_uninitialized = false) {
    const bool not_oop = false;
    return generate_disjoint_copy(sizeof (jint), aligned, not_oop, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  address generate_conjoint_int_copy(bool aligned, address nooverlap_target,
                                     address *entry, const char *name,
                                     bool dest_uninitialized = false) {
    const bool not_oop = false;
    return generate_conjoint_copy(sizeof (jint), aligned, not_oop, nooverlap_target, entry, name);
  }


  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord boundary == 8 bytes
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as size_t, can be zero
  //
  // Side Effects:
  //   disjoint_oop_copy_entry or disjoint_long_copy_entry is set to the
  //   no-overlap entry point used by generate_conjoint_long_oop_copy().
  //
  address generate_disjoint_long_copy(bool aligned, address *entry,
                                          const char *name, bool dest_uninitialized = false) {
    const bool not_oop = false;
    return generate_disjoint_copy(sizeof (jlong), aligned, not_oop, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord boundary == 8 bytes
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as size_t, can be zero
  //
  address generate_conjoint_long_copy(bool aligned,
                                      address nooverlap_target, address *entry,
                                      const char *name, bool dest_uninitialized = false) {
    const bool not_oop = false;
    return generate_conjoint_copy(sizeof (jlong), aligned, not_oop, nooverlap_target, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord boundary == 8 bytes
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as size_t, can be zero
  //
  // Side Effects:
  //   disjoint_oop_copy_entry or disjoint_long_copy_entry is set to the
  //   no-overlap entry point used by generate_conjoint_long_oop_copy().
  //
  address generate_disjoint_oop_copy(bool aligned, address *entry,
                                     const char *name, bool dest_uninitialized = false) {
    const bool is_oop = true;
    const size_t size = UseCompressedOops ? sizeof (jint) : sizeof (jlong);
    return generate_disjoint_copy(size, aligned, is_oop, entry, name);
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord boundary == 8 bytes
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as size_t, can be zero
  //
  address generate_conjoint_oop_copy(bool aligned,
                                     address nooverlap_target, address *entry,
                                     const char *name, bool dest_uninitialized = false) {
    const bool is_oop = true;
    const size_t size = UseCompressedOops ? sizeof (jint) : sizeof (jlong);
    return generate_conjoint_copy(size, aligned, is_oop, nooverlap_target, entry, name);
  }


  // Helper for generating a dynamic type check.
  // Smashes rscratch1.
  void generate_type_check(Register sub_klass,
                           Register super_check_offset,
                           Register super_klass,
                           Label& L_success) {
    assert_different_registers(sub_klass, super_check_offset, super_klass);

    BLOCK_COMMENT("type_check:");

    Label L_miss;

    __ check_klass_subtype_fast_path(sub_klass, super_klass, noreg,        &L_success, &L_miss, NULL,
                                     super_check_offset);
    __ check_klass_subtype_slow_path(sub_klass, super_klass, noreg, noreg, &L_success, NULL);

    // Fall through on failure!
    __ BIND(L_miss);
  }

  //
  //  Generate checkcasting array copy stub
  //
  //  Input:
  //    c_rarg0   - source array address
  //    c_rarg1   - destination array address
  //    c_rarg2   - element count, treated as ssize_t, can be zero
  //    c_rarg3   - size_t ckoff (super_check_offset)
  //    c_rarg4   - oop ckval (super_klass)
  //
  //  Output:
  //    r0 ==  0  -  success
  //    r0 == -1^K - failure, where K is partial transfer count
  //
  address generate_checkcast_copy(const char *name, address *entry,
                                  bool dest_uninitialized = false) {
    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();
    __ stop("werg");
    /*Label L_load_element, L_store_element, L_do_card_marks, L_done, L_done_pop;

    // Input registers (after setup_arg_regs)
    const Register from        = c_rarg0;   // source array address
    const Register to          = c_rarg1;   // destination array address
    const Register count       = c_rarg2;   // elementscount
    const Register ckoff       = c_rarg3;   // super_check_offset
    const Register ckval       = c_rarg4;   // super_klass

    // Registers used as temps (r18, r19, r20 are save-on-entry)
    const Register count_save  = r21;       // orig elementscount
    const Register start_to    = r20;       // destination array start address
    const Register copied_oop  = r18;       // actual oop copied
    const Register r19_klass   = r19;       // oop._klass

    //---------------------------------------------------------------
    // Assembler stub will be used for this call to arraycopy
    // if the two arrays are subtypes of Object[] but the
    // destination array type is not equal to or a supertype
    // of the source type.  Each element must be separately
    // checked.

    assert_different_registers(from, to, count, ckoff, ckval, start_to,
                               copied_oop, r19_klass, count_save);

    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    __ enter(); // required for proper stackwalking of RuntimeStub frame

#ifdef ASSERT
    // caller guarantees that the arrays really are different
    // otherwise, we would have to make conjoint checks
    { Label L;
      array_overlap_test(L);//, TIMES_OOP);
      __ stop("checkcast_copy within a single array");
      __ bind(L);
    }
#endif //ASSERT

    // Caller of this entry point must set up the argument registers.
    if (entry != NULL) {
      *entry = __ pc();
      BLOCK_COMMENT("Entry:");
    }

     // Empty array:  Nothing to do.
    __ cbz(count, L_done);

    __ push(RegSet::of(r18, r19, r20, r21), sp);

#ifdef ASSERT
    BLOCK_COMMENT("assert consistent ckoff/ckval");
    // The ckoff and ckval must be mutually consistent,
    // even though caller generates both.
    { Label L;
      int sco_offset = in_bytes(Klass::super_check_offset_offset());
      __ ldrw(start_to, Address(ckval, sco_offset));
      __ cmpw(ckoff, start_to);
      __ b(L, Assembler::EQ);
      __ stop("super_check_offset inconsistent");
      __ bind(L);
    }
#endif //ASSERT

    // save the original count
    __ mov(count_save, count);

    // Copy from low to high addresses
    __ mov(start_to, to);              // Save destination array start address
    __ b(L_load_element);

    // ======== begin loop ========
    // (Loop is rotated; its entry is L_load_element.)
    // Loop control:
    //   for (; count != 0; count--) {
    //     copied_oop = load_heap_oop(from++);
    //     ... generate_type_check ...;
    //     store_heap_oop(to++, copied_oop);
    //   }
    __ align(OptoLoopAlignment);

    __ BIND(L_store_element);
    __ store_heap_oop(__ post(to, UseCompressedOops ? 4 : 8), copied_oop);  // store the oop
    __ sub(count, count, 1);
    __ cbz(count, L_do_card_marks);

    // ======== loop entry is here ========
    __ BIND(L_load_element);
    __ load_heap_oop(copied_oop, __ post(from, UseCompressedOops ? 4 : 8)); // load the oop
    __ cbz(copied_oop, L_store_element);

    __ load_klass(r19_klass, copied_oop);// query the object klass
    generate_type_check(r19_klass, ckoff, ckval, L_store_element);
    // ======== end loop ========

    // It was a real error; we must depend on the caller to finish the job.
    // Register count = remaining oops, count_orig = total oops.
    // Emit GC store barriers for the oops we have copied and report
    // their number to the caller.

    __ subs(count, count_save, count);     // K = partially copied oop count
    __ inv(count, count);                   // report (-1^K) to caller
    __ b(L_done_pop, Assembler::EQ);

    __ BIND(L_do_card_marks);
    __ add(to, to, -heapOopSize);         // make an inclusive end pointer
    gen_write_ref_array_post_barrier(start_to, to, rscratch1);

    __ bind(L_done_pop);
    __ pop(RegSet::of(r18, r19, r20, r21), sp);
    inc_counter_np(SharedRuntime::_checkcast_array_copy_ctr);

    __ bind(L_done);
    __ mov(r0, count);
    __ leave();
    __ b(lr);
    */
    return start;
  }

  // Perform range checks on the proposed arraycopy.
  // Kills temp, but nothing else.
  // Also, clean the sign bits of src_pos and dst_pos.
  void arraycopy_range_checks(Register src,     // source array oop (c_rarg0)
                              Register src_pos, // source position (c_rarg1)
                              Register dst,     // destination array oo (c_rarg2)
                              Register dst_pos, // destination position (c_rarg3)
                              Register length,
                              Register temp,
                              Label& L_failed) { Unimplemented(); }

  // These stubs get called from some dumb test routine.
  // I'll write them properly when they're called from
  // something that's actually doing something.
  static void fake_arraycopy_stub(address src, address dst, int count) {
    assert(count == 0, "huh?");
  }


  void generate_arraycopy_stubs() {
    address entry;
    address entry_jbyte_arraycopy;
    address entry_jshort_arraycopy;
    address entry_jint_arraycopy;
    address entry_oop_arraycopy;
    address entry_jlong_arraycopy;
    address entry_checkcast_arraycopy;

    generate_copy_longs(copy_f, r0, r1, rscratch2, copy_forwards);
    generate_copy_longs(copy_b, r0, r1, rscratch2, copy_backwards);

    //*** jbyte
    // Always need aligned and unaligned versions
    StubRoutines::_jbyte_disjoint_arraycopy         = generate_disjoint_byte_copy(false, &entry,
                                                                                  "jbyte_disjoint_arraycopy");
    StubRoutines::_jbyte_arraycopy                  = generate_conjoint_byte_copy(false, entry,
                                                                                  &entry_jbyte_arraycopy,
                                                                                  "jbyte_arraycopy");
    StubRoutines::_arrayof_jbyte_disjoint_arraycopy = generate_disjoint_byte_copy(true, &entry,
                                                                                  "arrayof_jbyte_disjoint_arraycopy");
    StubRoutines::_arrayof_jbyte_arraycopy          = generate_conjoint_byte_copy(true, entry, NULL,
                                                                                  "arrayof_jbyte_arraycopy");

    //*** jshort
    // Always need aligned and unaligned versions
    StubRoutines::_jshort_disjoint_arraycopy         = generate_disjoint_short_copy(false, &entry,
                                                                                    "jshort_disjoint_arraycopy");
    StubRoutines::_jshort_arraycopy                  = generate_conjoint_short_copy(false, entry,
                                                                                    &entry_jshort_arraycopy,
                                                                                    "jshort_arraycopy");
    StubRoutines::_arrayof_jshort_disjoint_arraycopy = generate_disjoint_short_copy(true, &entry,
                                                                                    "arrayof_jshort_disjoint_arraycopy");
    StubRoutines::_arrayof_jshort_arraycopy          = generate_conjoint_short_copy(true, entry, NULL,
                                                                                    "arrayof_jshort_arraycopy");

    //*** jint
    // Aligned versions
    StubRoutines::_arrayof_jint_disjoint_arraycopy = generate_disjoint_int_copy(true, &entry,
                                                                                "arrayof_jint_disjoint_arraycopy");
    StubRoutines::_arrayof_jint_arraycopy          = generate_conjoint_int_copy(true, entry, &entry_jint_arraycopy,
                                                                                "arrayof_jint_arraycopy");
    // In 64 bit we need both aligned and unaligned versions of jint arraycopy.
    // entry_jint_arraycopy always points to the unaligned version
    StubRoutines::_jint_disjoint_arraycopy         = generate_disjoint_int_copy(false, &entry,
                                                                                "jint_disjoint_arraycopy");
    StubRoutines::_jint_arraycopy                  = generate_conjoint_int_copy(false, entry,
                                                                                &entry_jint_arraycopy,
                                                                                "jint_arraycopy");

    //*** jlong
    // It is always aligned
    StubRoutines::_arrayof_jlong_disjoint_arraycopy = generate_disjoint_long_copy(true, &entry,
                                                                                  "arrayof_jlong_disjoint_arraycopy");
    StubRoutines::_arrayof_jlong_arraycopy          = generate_conjoint_long_copy(true, entry, &entry_jlong_arraycopy,
                                                                                  "arrayof_jlong_arraycopy");
    StubRoutines::_jlong_disjoint_arraycopy         = StubRoutines::_arrayof_jlong_disjoint_arraycopy;
    StubRoutines::_jlong_arraycopy                  = StubRoutines::_arrayof_jlong_arraycopy;

    //*** oops
    {
      // With compressed oops we need unaligned versions; notice that
      // we overwrite entry_oop_arraycopy.
      bool aligned = !UseCompressedOops;

      StubRoutines::_arrayof_oop_disjoint_arraycopy
        = generate_disjoint_oop_copy(aligned, &entry, "arrayof_oop_disjoint_arraycopy");
      StubRoutines::_arrayof_oop_arraycopy
        = generate_conjoint_oop_copy(aligned, entry, &entry_oop_arraycopy, "arrayof_oop_arraycopy");
      // Aligned versions without pre-barriers
      StubRoutines::_arrayof_oop_disjoint_arraycopy_uninit
        = generate_disjoint_oop_copy(aligned, &entry, "arrayof_oop_disjoint_arraycopy_uninit",
                                     /*dest_uninitialized*/true);
      StubRoutines::_arrayof_oop_arraycopy_uninit
        = generate_conjoint_oop_copy(aligned, entry, NULL, "arrayof_oop_arraycopy_uninit",
                                     /*dest_uninitialized*/true);
    }

    StubRoutines::_oop_disjoint_arraycopy            = StubRoutines::_arrayof_oop_disjoint_arraycopy;
    StubRoutines::_oop_arraycopy                     = StubRoutines::_arrayof_oop_arraycopy;
    StubRoutines::_oop_disjoint_arraycopy_uninit     = StubRoutines::_arrayof_oop_disjoint_arraycopy_uninit;
    StubRoutines::_oop_arraycopy_uninit              = StubRoutines::_arrayof_oop_arraycopy_uninit;

    StubRoutines::_checkcast_arraycopy        = generate_checkcast_copy("checkcast_arraycopy", &entry_checkcast_arraycopy);
    StubRoutines::_checkcast_arraycopy_uninit = generate_checkcast_copy("checkcast_arraycopy_uninit", NULL,
                                                                        /*dest_uninitialized*/true);
  }

  void generate_math_stubs() { Unimplemented(); }

  // Safefetch stubs.
  void generate_safefetch(const char* name, int size, address* entry,
                          address* fault_pc, address* continuation_pc) {
    // safefetch signatures:
    //   int      SafeFetch32(int*      adr, int      errValue);
    //   intptr_t SafeFetchN (intptr_t* adr, intptr_t errValue);
    //
    // arguments:
    //   c_rarg0 = adr
    //   c_rarg1 = errValue
    //
    // result:
    //   PPC_RET  = *adr or errValue

    StubCodeMark mark(this, "StubRoutines", name);

    // Entry point, pc or function descriptor.
    *entry = __ pc();

    // Load *adr into c_rarg1, may fault.
    *fault_pc = __ pc();
    switch (size) {
      case 4:
        // int32_t
        __ ldr(c_rarg0, Address(c_rarg0, 0));
        break;
      case 8:
        __ ldrd(c_rarg0, c_rarg1, Address(c_rarg0, 0));
        break;
      default:
        ShouldNotReachHere();
    }
    __ b(lr);
    // return errValue or *adr
    *continuation_pc = __ pc();
    __ mov(r0, c_rarg1);
    __ b(lr);
  }


  // Continuation point for throwing of implicit exceptions that are
  // not handled in the current activation. Fabricates an exception
  // oop and initiates normal exception dispatching in this
  // frame. Since we need to preserve callee-saved values (currently
  // only for C2, but done for C1 as well) we need a callee-saved oop
  // map and therefore have to make these stubs into RuntimeStubs
  // rather than BufferBlobs.  If the compiler needs all registers to
  // be preserved between the fault point and the exception handler
  // then it must assume responsibility for that in
  // AbstractCompiler::continuation_for_implicit_null_exception or
  // continuation_for_implicit_division_by_zero_exception. All other
  // implicit exceptions (e.g., NullPointerException or
  // AbstractMethodError on entry) are either at call sites or
  // otherwise assume that stack unwinding will be initiated, so
  // caller saved registers were assumed volatile in the compiler.

#undef __
#define __ masm->

  address generate_throw_exception(const char* name,
                                   address runtime_entry,
                                   Register arg1 = noreg,
                                   Register arg2 = noreg) {
    // Information about frame layout at time of blocking runtime call.
    // Note that we only have to preserve callee-saved registers since
    // the compilers are responsible for supplying a continuation point
    // if they expect all registers to be preserved.
    // n.b. aarch32 asserts that frame::arg_reg_save_area_bytes == 0
    enum layout {
      rbp_off = frame::arg_reg_save_area_bytes/BytesPerInt,
      return_off,
      framesize // inclusive of return address
    };

    int insts_size = 512;
    int locs_size  = 64;

    CodeBuffer code(name, insts_size, locs_size);
    OopMapSet* oop_maps  = new OopMapSet();
    MacroAssembler* masm = new MacroAssembler(&code);

    address start = __ pc();

    // This is an inlined and slightly modified version of call_VM
    // which has the ability to fetch the return PC out of
    // thread-local storage and also sets up last_Java_sp slightly
    // differently than the real call_VM

    __ enter(); // Save FP and LR before call

    // lr and fp are already in place
    assert(frame::arg_reg_save_area_bytes == 0, "please modify this code");
    // __ sub(sp, rfp, frame::arg_reg_save_area_bytes + wordSize); // prolog

    int frame_complete = __ pc() - start;

    // Set up last_Java_sp and last_Java_fp
    address the_pc = __ pc();
    __ set_last_Java_frame(sp, rfp, (address)NULL, rscratch1);

    // Call runtime
    if (arg1 != noreg) {
      assert(arg2 != c_rarg1, "clobbered");
      __ mov(c_rarg1, arg1);
    }
    if (arg2 != noreg) {
      __ mov(c_rarg2, arg2);
    }
    __ mov(c_rarg0, rthread);
    BLOCK_COMMENT("call runtime_entry");
    __ mov(rscratch1, runtime_entry);
    __ bl(rscratch1);

    // Generate oop map
    OopMap* map = new OopMap(framesize, 0);

    oop_maps->add_gc_map(the_pc - start, map);

    __ reset_last_Java_frame(true, true);
    __ maybe_isb();

    __ leave();

    // check for pending exceptions
#ifdef ASSERT
    Label L;
    __ ldr(rscratch1, Address(rthread, Thread::pending_exception_offset()));
    __ cbnz(rscratch1, L);
    __ should_not_reach_here();
    __ bind(L);
#endif // ASSERT
    __ far_jump(RuntimeAddress(StubRoutines::forward_exception_entry()));


    // codeBlob framesize is in words (not VMRegImpl::slot_size)
    RuntimeStub* stub =
      RuntimeStub::new_runtime_stub(name,
                                    &code,
                                    frame_complete,
                                    (framesize >> (LogBytesPerWord - LogBytesPerInt)),
                                    oop_maps, false);
    return stub->entry_point();
  }

  // Initialization
  void generate_initial() {
    // Generate initial stubs and initializes the entry points

    // entry points that exist in all platforms Note: This is code
    // that could be shared among different platforms - however the
    // benefit seems to be smaller than the disadvantage of having a
    // much more complicated generator structure. See also comment in
    // stubRoutines.hpp.

    StubRoutines::_forward_exception_entry = generate_forward_exception();

    StubRoutines::_call_stub_entry =
      generate_call_stub(StubRoutines::_call_stub_return_address);

    // is referenced by megamorphic call
    StubRoutines::_catch_exception_entry = generate_catch_exception();

    // Build this early so it's available for the interpreter.
    StubRoutines::_throw_StackOverflowError_entry =
      generate_throw_exception("StackOverflowError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_StackOverflowError));
  }

  void generate_all() {
    // support for verify_oop (must happen after universe_init)
    StubRoutines::_verify_oop_subroutine_entry     = generate_verify_oop();
    StubRoutines::_throw_AbstractMethodError_entry =
      generate_throw_exception("AbstractMethodError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_AbstractMethodError));

    StubRoutines::_throw_IncompatibleClassChangeError_entry =
      generate_throw_exception("IncompatibleClassChangeError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_IncompatibleClassChangeError));

    StubRoutines::_throw_NullPointerException_at_call_entry =
      generate_throw_exception("NullPointerException at call throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_NullPointerException_at_call));

    // arraycopy stubs used by compilers
    generate_arraycopy_stubs();

    // Safefetch stubs.
    generate_safefetch("SafeFetch32", sizeof(int),     &StubRoutines::_safefetch32_entry,
                                                       &StubRoutines::_safefetch32_fault_pc,
                                                       &StubRoutines::_safefetch32_continuation_pc);
    generate_safefetch("SafeFetchN", sizeof(intptr_t), &StubRoutines::_safefetchN_entry,
                                                       &StubRoutines::_safefetchN_fault_pc,
                                                       &StubRoutines::_safefetchN_continuation_pc);
  }

 public:
  StubGenerator(CodeBuffer* code, bool all) : StubCodeGenerator(code) {
    if (all) {
      generate_all();
    } else {
      generate_initial();
    }
  }
}; // end class declaration

void StubGenerator_generate(CodeBuffer* code, bool all) {
  StubGenerator g(code, all);
}
