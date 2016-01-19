/*
 * Copyright (c) 2003, 2011, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, Red Hat Inc. All rights reserved.
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
#include "interp_masm_aarch32.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/method.hpp"
#include "oops/methodData.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

#define __ _masm->


address AbstractInterpreterGenerator::generate_slow_signature_handler() {
  address entry = __ pc();

  // The sp should be aligned on entry to the bottom of where the integer args
  // need to be copied to.

  // rmethod
  // rlocals
  // c_rarg3: first stack arg - wordSize

  __ mov(c_rarg3, sp);
  __ sub(sp, sp, 22 * wordSize);
  __ str(lr, sp);
  __ call_VM(noreg,
             CAST_FROM_FN_PTR(address,
                              InterpreterRuntime::slow_signature_handler),
             rmethod, rlocals, c_rarg3);

  // r0: result handler

  // Stack layout:
  // rsp: return address           <- sp (lowest addr)
  //      1 float/double identifiers
  //      4 integer args (if static first is unused)
  //      8 double args FIXME how many for ARM, need to adjust
  //        stack args              <- sp (on entry)
  //        garbage
  //        expression stack bottom
  //        bcp (NULL)
  //        ...
  // If this changes, update interpreterRt_aarch32.cpp slowpath

  // Restore LR
  __ ldr(lr, sp);

  // Do FP first so we can use c_rarg3 as temp
  __ ldr(c_rarg3, Address(sp, wordSize)); // float/double identifiers

  for (int i = 0; i < Argument::n_float_register_parameters_c; i++) {
    const FloatRegister r = as_FloatRegister(i);

    Label d, done;

    __ tbnz(c_rarg3, i, d);
    __ vldr_f32(r, Address(sp, (6 + 2 * i) * wordSize));
    __ b(done);
    __ bind(d);
    __ vldr_f64(r, Address(sp, (6 + 2 * i) * wordSize));
    __ bind(done);
  }

  // c_rarg0 contains the result from the call of
  // InterpreterRuntime::slow_signature_handler so we don't touch it
  // here.  It will be loaded with the JNIEnv* later.
  __ ldr(c_rarg1, Address(sp, 2 * wordSize));
  __ ldrd(c_rarg2, c_rarg3, Address(sp, 3 * wordSize));

  __ add(sp, sp, 22 * wordSize);
  __ b(lr);

  return entry;
}


//
// Various method entries
//

address InterpreterGenerator::generate_math_entry(AbstractInterpreter::MethodKind kind) {
  // rmethod: Method*
  // r13: sender sp
  // esp: args

  //if (!InlineIntrinsics) return NULL; // Generate a vanilla entry
  // FIXME currently ignoring this flag and inlining anyway

  // These don't need a safepoint check because they aren't virtually
  // callable. We won't enter these intrinsics from compiled code.
  // If in the future we added an intrinsic which was virtually callable
  // we'd have to worry about how to safepoint so that this code is used.

  // mathematical functions inlined by compiler
  // (interpreter must provide identical implementation
  // in order to avoid monotonicity bugs when switching
  // from interpreter to compiler in the middle of some
  // computation)
  //
  // stack:
  //        [ arg ] <-- esp
  //        [ arg ]
  // retaddr in lr

  address entry_point = NULL;
  Register continuation = lr;
  switch (kind) {
  case Interpreter::java_lang_math_abs:
    entry_point = __ pc();
    __ vldr_f64(d0, Address(sp));
    __ vabs_f64(d0, d0);
    __ add(sp, sp, Interpreter::stackElementSize);
    break;
  case Interpreter::java_lang_math_sqrt:
    entry_point = __ pc();
    __ vldr_f64(d0, Address(sp));
    __ vsqrt_f64(d0, d0);
    __ add(sp, sp, Interpreter::stackElementSize);
    break;
  case Interpreter::java_lang_math_sin :
  case Interpreter::java_lang_math_cos :
  case Interpreter::java_lang_math_tan :
  case Interpreter::java_lang_math_log :
  case Interpreter::java_lang_math_log10 :
  case Interpreter::java_lang_math_exp :
    entry_point = __ pc();
    __ vldr_f64(d0, Address(sp));
    __ add(sp, sp, Interpreter::stackElementSize);
    __ mov(r4, lr);
    continuation = r4;  // The first callee-saved register
    generate_transcendental_entry(kind, 1);
    break;
  case Interpreter::java_lang_math_pow :
    entry_point = __ pc();
    __ mov(r4, lr);
    continuation = r4;
    __ vldr_f64(d0, Address(sp, 2 * Interpreter::stackElementSize));
    __ vldr_f64(d1, Address(sp));
    __ add(sp, sp, 2 * Interpreter::stackElementSize);
    generate_transcendental_entry(kind, 2);
    break;
  default:
    ;
  }
  if (entry_point) {
    __ b(continuation);
  }

  return entry_point;
}

  // double trigonometrics and transcendentals
  // static jdouble dsin(jdouble x);
  // static jdouble dcos(jdouble x);
  // static jdouble dtan(jdouble x);
  // static jdouble dlog(jdouble x);
  // static jdouble dlog10(jdouble x);
  // static jdouble dexp(jdouble x);
  // static jdouble dpow(jdouble x, jdouble y);

void InterpreterGenerator::generate_transcendental_entry(AbstractInterpreter::MethodKind kind, int fpargs) {
  address fn = NULL;
  switch (kind) {
  case Interpreter::java_lang_math_sin :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dsin);
    break;
  case Interpreter::java_lang_math_cos :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dcos);
    break;
  case Interpreter::java_lang_math_tan :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dtan);
    break;
  case Interpreter::java_lang_math_log :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dlog);
    break;
  case Interpreter::java_lang_math_log10 :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dlog10);
    break;
  case Interpreter::java_lang_math_exp :
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dexp);
    break;
  case Interpreter::java_lang_math_pow :
    fpargs = 2;
    fn = CAST_FROM_FN_PTR(address, SharedRuntime::dpow);
    break;
  default:
    ShouldNotReachHere();
  }
  const int gpargs = 0, rtype = 3;
  __ mov(rscratch1, fn);
  __ bl(rscratch1);
}

// Jump into normal path for accessor and empty entry to jump to normal entry
// The "fast" optimization don't update compilation count therefore can disable inlining
// for these functions that should be inlined.
address InterpreterGenerator::generate_jump_to_normal_entry(void) {
  address entry_point = __ pc();

  assert(Interpreter::entry_for_kind(Interpreter::zerolocals) != NULL, "should already be generated");
  __ b(Interpreter::entry_for_kind(Interpreter::zerolocals));
  return entry_point;
}

// Abstract method entry
// Attempt to execute abstract method. Throw exception
address InterpreterGenerator::generate_abstract_entry(void) {
  // rmethod: Method*
  // r13: sender SP

  address entry_point = __ pc();

  // abstract method entry

  //  pop return address, reset last_sp to NULL
  __ empty_expression_stack();
  __ restore_bcp();      // bcp must be correct for exception handler   (was destroyed)
  __ restore_locals();   // make sure locals pointer is correct as well (was destroyed)

  // throw exception
  __ call_VM(noreg, CAST_FROM_FN_PTR(address,
                             InterpreterRuntime::throw_AbstractMethodError));
  // the call_VM checks for exception, so we should never return here.
  __ should_not_reach_here();

  return entry_point;
}


void Deoptimization::unwind_callee_save_values(frame* f, vframeArray* vframe_array) {

  // This code is sort of the equivalent of C2IAdapter::setup_stack_frame back in
  // the days we had adapter frames. When we deoptimize a situation where a
  // compiled caller calls a compiled caller will have registers it expects
  // to survive the call to the callee. If we deoptimize the callee the only
  // way we can restore these registers is to have the oldest interpreter
  // frame that we create restore these values. That is what this routine
  // will accomplish.

  // At the moment we have modified c2 to not have any callee save registers
  // so this problem does not exist and this routine is just a place holder.

  assert(f->is_interpreted_frame(), "must be interpreted");
}
