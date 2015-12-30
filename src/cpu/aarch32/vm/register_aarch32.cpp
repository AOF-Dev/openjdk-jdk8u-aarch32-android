/*
 * Copyright (c) 2000, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "register_aarch32.hpp"

const int ConcreteRegisterImpl::max_gpr = RegisterImpl::number_of_registers << 1;

const int ConcreteRegisterImpl::max_fpr
  = ConcreteRegisterImpl::max_gpr + (FloatRegisterImpl::number_of_registers << 1);

const char* RegisterImpl::name() const {
  const char* names[number_of_registers] = {
    "c_rarg0", "c_rarg1", "c_rarg2", "c_rarg3",
    "rdispatch", "rbcp", "rlocals", "rmethod", "rcpool", "rthread", "rfp",
    "rscratch1", "rscratch2",
    "sp", "lr/r14", "r15_pc"
  };
  return is_valid() ? names[encoding()] : "noreg";
}

const char* FloatRegisterImpl::name() const {
  const char* names[number_of_registers] = {
    "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"
  };
  return is_valid() ? names[encoding()] : "noreg";
}
