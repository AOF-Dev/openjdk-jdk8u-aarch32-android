/*
 * Copyright (c) 2004, 2012, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/metaspaceShared.hpp"

// Generate the self-patching vtable method:
//
// This method will be called (as any other Klass virtual method) with
// the Klass itself as the first argument.  Example:
//
//      oop obj;
//      int size = obj->klass()->oop_size(this);
//
// for which the virtual method call is Klass::oop_size();
//
// The dummy method is called with the Klass object as the first
// operand, and an object as the second argument.
//

//=====================================================================

// All of the dummy methods in the vtable are essentially identical,
// differing only by an ordinal constant, and they bear no relationship
// to the original method which the caller intended. Also, there needs
// to be 'vtbl_list_size' instances of the vtable in order to
// differentiate between the 'vtable_list_size' original Klass objects.

#define __ masm->

void MetaspaceShared::generate_vtable_methods(void** vtbl_list,
                                                   void** vtable,
                                                   char** md_top,
                                                   char* md_end,
                                                   char** mc_top,
                                                   char* mc_end) {

  intptr_t vtable_bytes = (num_virtuals * vtbl_list_size) * sizeof(void*);
  *(intptr_t *)(*md_top) = vtable_bytes;
  *md_top += sizeof(intptr_t);
  void** dummy_vtable = (void**)*md_top;
  *vtable = dummy_vtable;
  *md_top += vtable_bytes;

  // Get ready to generate dummy methods.

  CodeBuffer cb((unsigned char*)*mc_top, mc_end - *mc_top);
  MacroAssembler* masm = new MacroAssembler(&cb);

  // r12, lr, r9 registers are used as temporary registers
  // callee-saved registers (lr, r9) should be restored
  RegSet rset = RegSet::of(r9, lr);

  Label common_code;
  for (int i = 0; i < vtbl_list_size; ++i) {
    for (int j = 0; j < num_virtuals; ++j) {
      dummy_vtable[num_virtuals * i + j] = (void*)masm->pc();

      // We're called directly from C code.

      // Load r12 with a value indicating vtable/offset pair.
      // -- bits[ 7..0]  (8 bits) which virtual method in table?
      // -- bits[12..8]  (5 bits) which virtual method table?
      __ mov(r12, (i << 8) + j);
      __ b(common_code);
    }
  }

  __ bind(common_code);

  __ push(rset, sp);
  __ lsr(lr, r12, 8);            // isolate vtable identifier.
  __ mov(r9, (address)vtbl_list);      // address of list of vtable pointers.
  __ ldr(r9, Address(r9, lr, lsl(LogBytesPerWord))); // get correct vtable pointer.
  __ str(r9, Address(c_rarg0));        // update vtable pointer in obj.
  __ andr(r12, r12, 0xFF);
  __ add(r12, r9, r12, lsl(LogBytesPerWord)); // address of real method pointer.
  __ ldr(r12, Address(r12)); // get real method pointer.
  __ pop(rset, sp);
  __ b(r12);           // jump to the real method.

  *mc_top = (char*)__ pc();
}
