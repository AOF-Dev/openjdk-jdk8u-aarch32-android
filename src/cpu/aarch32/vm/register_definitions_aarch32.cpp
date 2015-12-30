/*
 * Copyright (c) 2002, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "asm/assembler.hpp"
#include "asm/register.hpp"
#include "register_aarch32.hpp"
# include "interp_masm_aarch32.hpp"

REGISTER_DEFINITION(Register, r0);
REGISTER_DEFINITION(Register, r1);
REGISTER_DEFINITION(Register, r2);
REGISTER_DEFINITION(Register, r3);
REGISTER_DEFINITION(Register, r4);
REGISTER_DEFINITION(Register, r5);
REGISTER_DEFINITION(Register, r6);
REGISTER_DEFINITION(Register, r7);
REGISTER_DEFINITION(Register, r8);
REGISTER_DEFINITION(Register, r9);
REGISTER_DEFINITION(Register, r10);
REGISTER_DEFINITION(Register, r11);
REGISTER_DEFINITION(Register, r12);
REGISTER_DEFINITION(Register, r13);
REGISTER_DEFINITION(Register, r14);
REGISTER_DEFINITION(Register, r15);

REGISTER_DEFINITION(Register, sp);


REGISTER_DEFINITION(FloatRegister, d0);
REGISTER_DEFINITION(FloatRegister, d1);
REGISTER_DEFINITION(FloatRegister, d2);
REGISTER_DEFINITION(FloatRegister, d3);
REGISTER_DEFINITION(FloatRegister, d4);
REGISTER_DEFINITION(FloatRegister, d5);
REGISTER_DEFINITION(FloatRegister, d6);
REGISTER_DEFINITION(FloatRegister, d7);


REGISTER_DEFINITION(Register, c_rarg0);
REGISTER_DEFINITION(Register, c_rarg1);
REGISTER_DEFINITION(Register, c_rarg2);
REGISTER_DEFINITION(Register, c_rarg3);

REGISTER_DEFINITION(FloatRegister, c_farg0);
REGISTER_DEFINITION(FloatRegister, c_farg1);
REGISTER_DEFINITION(FloatRegister, c_farg2);
REGISTER_DEFINITION(FloatRegister, c_farg3);
REGISTER_DEFINITION(FloatRegister, c_farg4);
REGISTER_DEFINITION(FloatRegister, c_farg5);
REGISTER_DEFINITION(FloatRegister, c_farg6);
REGISTER_DEFINITION(FloatRegister, c_farg7);

REGISTER_DEFINITION(Register, j_rarg0);
REGISTER_DEFINITION(Register, j_rarg1);
REGISTER_DEFINITION(Register, j_rarg2);
REGISTER_DEFINITION(Register, j_rarg3);

REGISTER_DEFINITION(FloatRegister, j_farg0);
REGISTER_DEFINITION(FloatRegister, j_farg1);
REGISTER_DEFINITION(FloatRegister, j_farg2);
REGISTER_DEFINITION(FloatRegister, j_farg3);
REGISTER_DEFINITION(FloatRegister, j_farg4);
REGISTER_DEFINITION(FloatRegister, j_farg5);
REGISTER_DEFINITION(FloatRegister, j_farg6);
REGISTER_DEFINITION(FloatRegister, j_farg7);

REGISTER_DEFINITION(Register, rscratch1);
REGISTER_DEFINITION(Register, rscratch2);
REGISTER_DEFINITION(Register, esp);
REGISTER_DEFINITION(Register, rdispatch);
REGISTER_DEFINITION(Register, rcpool);
REGISTER_DEFINITION(Register, rmethod);
REGISTER_DEFINITION(Register, rlocals);
REGISTER_DEFINITION(Register, rbcp);

REGISTER_DEFINITION(Register, lr);
REGISTER_DEFINITION(Register, rfp);
REGISTER_DEFINITION(Register, rthread);
REGISTER_DEFINITION(Register, r15_pc);

