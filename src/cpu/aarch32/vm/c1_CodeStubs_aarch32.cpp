/*
 * Copyright (c) 1999, 2015, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2014, Red Hat Inc. All rights reserved.
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
// This file is a derivative work resulting from (and including) modifications
// made by Azul Systems, Inc.  The dates of such changes are 2013-2016.
// Copyright 2013-2016 Azul Systems, Inc.  All Rights Reserved.
//
// Please contact Azul Systems, 385 Moffett Park Drive, Suite 115, Sunnyvale,
// CA 94089 USA or visit www.azul.com if you need additional information or
// have any questions.

#include "precompiled.hpp"
#include "c1/c1_CodeStubs.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "nativeInst_aarch32.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_aarch32.inline.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#endif

#define __ ce->masm()->

#define should_not_reach_here() should_not_reach_here_line(__FILE__, __LINE__)

void CounterOverflowStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  ce->store_parameter(_method->as_register(), 1);
  ce->store_parameter(_bci, 0);
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::counter_overflow_id)));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  __ b(_continuation);
}

RangeCheckStub::RangeCheckStub(CodeEmitInfo* info, LIR_Opr index,
                               bool throw_index_out_of_bounds_exception)
  : _throw_index_out_of_bounds_exception(throw_index_out_of_bounds_exception)
  , _index(index)
{
  assert(info != NULL, "must have info");
  _info = new CodeEmitInfo(info);
}

void RangeCheckStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  if (_info->deoptimize_on_exception()) {
    address a = Runtime1::entry_for(Runtime1::predicate_failed_trap_id);
    __ far_call(RuntimeAddress(a));
    ce->add_call_info_here(_info);
    ce->verify_oop_map(_info);
    debug_only(__ should_not_reach_here());
    return;
  }

  if (_index->is_cpu_register()) {
    __ mov(rscratch1, _index->as_register());
  } else {
    __ mov(rscratch1, _index->as_jint());
  }
  Runtime1::StubID stub_id;
  if (_throw_index_out_of_bounds_exception) {
    stub_id = Runtime1::throw_index_exception_id;
  } else {
    stub_id = Runtime1::throw_range_check_failed_id;
  }
  __ far_call(RuntimeAddress(Runtime1::entry_for(stub_id)), NULL, rscratch2);
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  debug_only(__ should_not_reach_here());
}

PredicateFailedStub::PredicateFailedStub(CodeEmitInfo* info) {
  _info = new CodeEmitInfo(info);
}

void PredicateFailedStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  address a = Runtime1::entry_for(Runtime1::predicate_failed_trap_id);
  __ far_call(RuntimeAddress(a));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  debug_only(__ should_not_reach_here());
}

void DivByZeroStub::emit_code(LIR_Assembler* ce) {
  if (_offset != -1) {
    ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
  }
  __ bind(_entry);
  __ far_call(Address(Runtime1::entry_for(Runtime1::throw_div0_exception_id), relocInfo::runtime_call_type));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
#ifdef ASSERT
  __ should_not_reach_here();
#endif
}



// Implementation of NewInstanceStub

NewInstanceStub::NewInstanceStub(LIR_Opr klass_reg, LIR_Opr result, ciInstanceKlass* klass, CodeEmitInfo* info, Runtime1::StubID stub_id) {
  _result = result;
  _klass = klass;
  _klass_reg = klass_reg;
  _info = new CodeEmitInfo(info);
  assert(stub_id == Runtime1::new_instance_id                 ||
         stub_id == Runtime1::fast_new_instance_id            ||
         stub_id == Runtime1::fast_new_instance_init_check_id,
         "need new_instance id");
  _stub_id   = stub_id;
}



void NewInstanceStub::emit_code(LIR_Assembler* ce) {
  assert(__ rsp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  __ mov(r3, _klass_reg->as_register());
  __ far_call(RuntimeAddress(Runtime1::entry_for(_stub_id)));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  assert(_result->as_register() == r0, "result must in r0,");
  __ b(_continuation);
}


// Implementation of NewTypeArrayStub

// Implementation of NewTypeArrayStub

NewTypeArrayStub::NewTypeArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
  _klass_reg = klass_reg;
  _length = length;
  _result = result;
  _info = new CodeEmitInfo(info);
}


void NewTypeArrayStub::emit_code(LIR_Assembler* ce) {
  assert(__ rsp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  assert(_length->as_register() == r6, "length must in r6,");
  assert(_klass_reg->as_register() == r3, "klass_reg must in r3");
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::new_type_array_id)));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  assert(_result->as_register() == r0, "result must in r0");
  __ b(_continuation);
}


// Implementation of NewObjectArrayStub

NewObjectArrayStub::NewObjectArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
  _klass_reg = klass_reg;
  _result = result;
  _length = length;
  _info = new CodeEmitInfo(info);
}


void NewObjectArrayStub::emit_code(LIR_Assembler* ce) {
  assert(__ rsp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  assert(_length->as_register() == r6, "length must in r6");
  assert(_klass_reg->as_register() == r3, "klass_reg must in r3");
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::new_object_array_id)));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  assert(_result->as_register() == r0, "result must in r0");
  __ b(_continuation);
}
// Implementation of MonitorAccessStubs

MonitorEnterStub::MonitorEnterStub(LIR_Opr obj_reg, LIR_Opr lock_reg, CodeEmitInfo* info)
: MonitorAccessStub(obj_reg, lock_reg)
{
  _info = new CodeEmitInfo(info);
}


void MonitorEnterStub::emit_code(LIR_Assembler* ce) {
  assert(__ rsp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  ce->store_parameter(_obj_reg->as_register(),  1);
  ce->store_parameter(_lock_reg->as_register(), 0);
  Runtime1::StubID enter_id;
  if (ce->compilation()->has_fpu_code()) {
    enter_id = Runtime1::monitorenter_id;
  } else {
    enter_id = Runtime1::monitorenter_nofpu_id;
  }
  __ far_call(RuntimeAddress(Runtime1::entry_for(enter_id)));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  __ b(_continuation);
}


void MonitorExitStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  if (_compute_lock) {
    // lock_reg was destroyed by fast unlocking attempt => recompute it
    ce->monitor_address(_monitor_ix, _lock_reg);
  }
  ce->store_parameter(_lock_reg->as_register(), 0);
  // note: non-blocking leaf routine => no call info needed
  Runtime1::StubID exit_id;
  if (ce->compilation()->has_fpu_code()) {
    exit_id = Runtime1::monitorexit_id;
  } else {
    exit_id = Runtime1::monitorexit_nofpu_id;
  }
  __ adr(lr, _continuation);
  __ far_jump(RuntimeAddress(Runtime1::entry_for(exit_id)));
}


// Implementation of patching:
// - Copy the code at given offset to an inlined buffer (first the bytes, then the number of bytes)
// - Replace original code with a call to the stub
// At Runtime:
// - call to stub, jump to runtime
// - in runtime: preserve all registers (rspecially objects, i.e., source and destination object)
// - in runtime: after initializing class, restore original code, reexecute instruction

int PatchingStub::_patch_info_offset = 0;

void PatchingStub::align_patch_site(MacroAssembler* masm) {
}

void PatchingStub::emit_code(LIR_Assembler* ce) {
  // NativeCall::instruction_size is dynamically calculated based on CPU,
  // armv7 -> 3 instructions, armv6 -> 5 instructions. Initialize _patch_info_offset
  // here, when CPU is determined already.
  if (!_patch_info_offset)
    _patch_info_offset = -NativeCall::instruction_size;
  assert(_patch_info_offset == -NativeCall::instruction_size, "must not change");
  assert(NativeCall::instruction_size <= _bytes_to_copy && _bytes_to_copy <= 0xFF, "not enough room for call");

  Label call_patch;

  // static field accesses have special semantics while the class
  // initializer is being run so we emit a test which can be used to
  // check that this code is being executed by the initializing
  // thread.
  address being_initialized_entry = __ pc();
  if (CommentedAssembly) {
    __ block_comment(" patch template");
  }
  address start = __ pc();
  if (_id == load_klass_id) {
    // produce a copy of the load klass instruction for use by the being initialized case
    int metadata_index = -1;
    CodeSection* cs = __ code_section();
    RelocIterator iter(cs, (address)_pc_start, (address)_pc_start+1);
    while (iter.next()) {
      if (iter.type() == relocInfo::metadata_type) {
        metadata_Relocation* r = iter.metadata_reloc();
        assert(metadata_index == -1, "uninitalized yet");
        metadata_index = r->metadata_index();
        break;
      }
    }
    assert(metadata_index != -1, "initialized");
    __ relocate(metadata_Relocation::spec(metadata_index));
    __ patchable_load(_obj, __ pc());
    while ((intx) __ pc() - (intx) start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    for (int i = 0; i < _bytes_to_copy; i++) {
      assert(*(_pc_start + i) == *(start + i), "should be the same code");
    }
#endif
  } else if (_id == load_mirror_id || _id == load_appendix_id) {
    // produce a copy of the load mirror instruction for use by the being
    // initialized case
    int oop_index = -1;
    CodeSection* cs = __ code_section();
    RelocIterator iter(cs, (address)_pc_start, (address)_pc_start+1);
    while (iter.next()) {
      if (iter.type() == relocInfo::oop_type) {
        oop_Relocation* r = iter.oop_reloc();
        assert(oop_index == -1, "uninitalized yet");
        oop_index = r->oop_index();
        break;
      }
    }
    assert(oop_index != -1, "initialized");
    __ relocate(oop_Relocation::spec(oop_index));
    __ patchable_load(_obj, __ pc());
    while ((intx) __ pc() - (intx) start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    for (int i = 0; i < _bytes_to_copy; i++) {
      assert(*(_pc_start + i) == *(start + i), "should be the same code");
    }
#endif
  } else if (_id == access_field_id) {
    // make a copy the code which is going to be patched.
    address const_addr = (address) -1;
    CodeSection* cs = __ code_section();
    RelocIterator iter(cs, (address)_pc_start, (address)_pc_start+1);
    while (iter.next()) {
      if (iter.type() == relocInfo::section_word_type) {
        section_word_Relocation* r = iter.section_word_reloc();
        assert(const_addr == (address) -1, "uninitalized yet");
        const_addr = r->target();
        break;
      }
    }
    assert(const_addr != (address) -1, "initialized");
    __ relocate(section_word_Relocation::spec(const_addr, CodeBuffer::SECT_CONSTS));
    __ patchable_load(rscratch1, const_addr);
    while ((intx) __ pc() - (intx) start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    intptr_t* from = (intptr_t*) start;
    intptr_t* to = (intptr_t*) _pc_start;

    assert(NativeFarLdr::from((address) (from))->data_addr()
        == NativeFarLdr::from((address) (to))->data_addr(),
        "should load from one addr)");

    address next_from = NativeFarLdr::from(start)->next_instruction_address();
    address next_to   = NativeFarLdr::from(_pc_start)->next_instruction_address();

    assert(sizeof(*start) == sizeof(char), "Correct below");
    address end       = start +  _bytes_to_copy;
    while (next_from < end) {
      assert(*next_from == *next_to, "should be the same code");
      next_from++;
      next_to++;
    }
#endif
  } else {
    ShouldNotReachHere();
  }

  int bytes_to_skip = _bytes_to_copy;

  if (_id == load_mirror_id) {
    int offset = __ offset();
    if (CommentedAssembly) {
      __ block_comment(" being_initialized check");
    }
    assert(_obj != noreg, "must be a valid register");
    // Load without verification to keep code size small. We need it because
    // begin_initialized_entry_offset has to fit in a byte. Also, we know it's not null.
    __ ldr(rscratch1, Address(_obj, java_lang_Class::klass_offset_in_bytes()));
    __ ldr(rscratch1, Address(rscratch1, InstanceKlass::init_thread_offset()));
    __ cmp(rthread, rscratch1);
    __ b(call_patch, Assembler::NE);

    // access_field patches may execute the patched code before it's
    // copied back into place so we need to jump back into the main
    // code of the nmethod to continue execution.
    __ b(_patch_site_continuation);
    // make sure this extra code gets skipped
    bytes_to_skip += __ offset() - offset;
  }

  // Now emit the patch record telling the runtime how to find the
  // pieces of the patch.  We only need 3 bytes but it has to be
  // aligned as an instruction so emit 4 bytes.
  int sizeof_patch_record = 4;
  bytes_to_skip += sizeof_patch_record;

  // emit the offsets needed to find the code to patch
  int being_initialized_entry_offset = __ pc() - being_initialized_entry + sizeof_patch_record;

  __ emit_int8(0);
  __ emit_int8(being_initialized_entry_offset);
  __ emit_int8(bytes_to_skip);
  __ emit_int8(0);

  address patch_info_pc = __ pc();

  address entry = __ pc();
  NativeGeneralJump::insert_unconditional((address)_pc_start, entry);
  address target = NULL;
  relocInfo::relocType reloc_type = relocInfo::none;
  switch (_id) {
    case access_field_id:  target = Runtime1::entry_for(Runtime1::access_field_patching_id); reloc_type = relocInfo::section_word_type; break;
    case load_klass_id:    target = Runtime1::entry_for(Runtime1::load_klass_patching_id); reloc_type = relocInfo::metadata_type; break;
    case load_mirror_id:   target = Runtime1::entry_for(Runtime1::load_mirror_patching_id); reloc_type = relocInfo::oop_type; break;
    case load_appendix_id: target = Runtime1::entry_for(Runtime1::load_appendix_patching_id); reloc_type = relocInfo::oop_type; break;
    default: ShouldNotReachHere();
  }
  __ bind(call_patch);

  if (CommentedAssembly) {
    __ block_comment("patch entry point");
  }
  __ mov(rscratch1, RuntimeAddress(target));
  __ bl(rscratch1);
  // pad with nops to globally known upper bound of patch site size
  while (patch_info_pc - __ pc() < _patch_info_offset)
    __ nop();
  assert(_patch_info_offset == (patch_info_pc - __ pc()), "must not change, required by shared code");
  ce->add_call_info_here(_info);
  int jmp_off = __ offset();
  __ b(_patch_site_entry);
  // Add enough nops so deoptimization can overwrite the jmp above with a call
  // and not destroy the world.
  for (int j = __ offset() ; j < jmp_off + NativeCall::instruction_size; j += NativeInstruction::arm_insn_sz) {
    __ nop();
  }

  CodeSection* cs = __ code_section();
  RelocIterator iter(cs, (address)_pc_start, (address)_pc_start+1);
  relocInfo::change_reloc_info_for_address(&iter, (address)_pc_start, reloc_type, relocInfo::none);
}


void DeoptimizeStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::deoptimize_id)));
  ce->add_call_info_here(_info);
  DEBUG_ONLY(__ should_not_reach_here());
}


void ImplicitNullCheckStub::emit_code(LIR_Assembler* ce) {
  address a;
  if (_info->deoptimize_on_exception()) {
    // Deoptimize, do not throw the exception, because it is probably wrong to do it here.
    a = Runtime1::entry_for(Runtime1::predicate_failed_trap_id);
  } else {
    a = Runtime1::entry_for(Runtime1::throw_null_pointer_exception_id);
  }

  ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
  __ bind(_entry);
  __ far_call(RuntimeAddress(a));
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  debug_only(__ should_not_reach_here());
}


void SimpleExceptionStub::emit_code(LIR_Assembler* ce) {
  assert(__ rsp_offset() == 0, "frame size should be fixed");

  __ bind(_entry);
  // pass the object in a scratch register because all other registers
  // must be preserved
  if (_obj->is_cpu_register()) {
    __ mov(rscratch1, _obj->as_register());
  }
  __ far_call(RuntimeAddress(Runtime1::entry_for(_stub)), NULL, rscratch2);
  ce->add_call_info_here(_info);
  debug_only(__ should_not_reach_here());
}


void ArrayCopyStub::emit_code(LIR_Assembler* ce) {
  //---------------slow case: call to native-----------------
  __ bind(_entry);
  // Figure out where the args should go
  // This should really convert the IntrinsicID to the Method* and signature
  // but I don't know how to do that.
  //
  VMRegPair args[5];
  BasicType signature[5] = { T_OBJECT, T_INT, T_OBJECT, T_INT, T_INT};
  SharedRuntime::java_calling_convention(signature, args, 5, true);

  // push parameters
  // (src, src_pos, dest, destPos, length)
  Register r[5];
  r[0] = src()->as_register();
  r[1] = src_pos()->as_register();
  r[2] = dst()->as_register();
  r[3] = dst_pos()->as_register();
  r[4] = length()->as_register();

  // next registers will get stored on the stack
  for (int i = 0; i < 5 ; i++ ) {
    VMReg r_1 = args[i].first();
    if (r_1->is_stack()) {
      int st_off = r_1->reg2stack() * wordSize;
      __ str (r[i], Address(sp, st_off));
    } else {
      assert(r[i] == args[i].first()->as_Register(), "Wrong register for arg ");
    }
  }

  ce->align_call(lir_static_call);

  ce->emit_static_call_stub();
  Address resolve(SharedRuntime::get_resolve_static_call_stub(),
                  relocInfo::static_call_type);
  __ trampoline_call(resolve);
  ce->add_call_info_here(info());

#ifndef PRODUCT
  __ lea(rscratch2, ExternalAddress((address)&Runtime1::_arraycopy_slowcase_cnt));
  __ increment(Address(rscratch2));
#endif

  __ b(_continuation);
}


/////////////////////////////////////////////////////////////////////////////
#if INCLUDE_ALL_GCS

void G1PreBarrierStub::emit_code(LIR_Assembler* ce) {
  // At this point we know that marking is in progress.
  // If do_load() is true then we have to emit the
  // load of the previous value; otherwise it has already
  // been loaded into _pre_val.

  __ bind(_entry);
  assert(pre_val()->is_register(), "Precondition.");

  Register pre_val_reg = pre_val()->as_register();

  if (do_load()) {
    ce->mem2reg(addr(), pre_val(), T_OBJECT, patch_code(), info(), false /*wide*/, false /*unaligned*/);
  }
  __ cbz(pre_val_reg, _continuation);
  ce->store_parameter(pre_val()->as_register(), 0);
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::g1_pre_barrier_slow_id)));
  __ b(_continuation);
}

void G1PostBarrierStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  assert(addr()->is_register(), "Precondition.");
  assert(new_val()->is_register(), "Precondition.");
  Register new_val_reg = new_val()->as_register();
  __ cbz(new_val_reg, _continuation);
  ce->store_parameter(addr()->as_pointer_register(), 0);
  __ far_call(RuntimeAddress(Runtime1::entry_for(Runtime1::g1_post_barrier_slow_id)));
  __ b(_continuation);
}

#endif // INCLUDE_ALL_GCS
/////////////////////////////////////////////////////////////////////////////

#undef __
