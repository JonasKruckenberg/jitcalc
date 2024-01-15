use crate::jit::{ExprDefinitions, SymbolId};
use crate::mmap::Mmap;
use core::ops::Range;
use core::ptr::write_unaligned;
use cranelift_codegen::binemit::Reloc;
use cranelift_codegen::ir::{ExternalName, Function, Signature};
use cranelift_codegen::{FinalizedMachReloc, FinalizedRelocTarget};

pub struct CompiledExpr {
    pub(crate) signature: Signature,
    pub(crate) mmap: Mmap,
    range: Range<usize>,
}

impl CompiledExpr {
    pub(crate) fn new(signature: Signature, bytes: &[u8]) -> Self {
        let (mmap, range) = {
            let mut mmap = Mmap::new(bytes.len());
            let range = 0..bytes.len();
            let slice = unsafe { mmap.slice_mut(range.clone()) };
            slice.copy_from_slice(bytes);

            (mmap, range)
        };

        Self {
            mmap,
            range,
            signature,
        }
    }

    pub(crate) fn make_executable(&mut self) {
        unsafe {
            self.mmap.make_accessible(self.range.clone());
            self.mmap.make_executable(self.range.clone(), true);
        }
    }

    pub fn eval(&self, args: &[f64]) -> f64 {
        let trampoline = self.get_trampoline();
        trampoline(self, args)
    }

    pub(crate) fn get_trampoline(&self) -> impl Fn(&CompiledExpr, &[f64]) -> f64 {
        match self.signature.params.len() {
            0 => trampoline_0,
            1 => trampoline_1,
            2 => trampoline_2,
            3 => trampoline_3,
            4 => trampoline_4,
            5 => trampoline_5,
            6 => trampoline_6,
            7 => trampoline_7,
            _ => unimplemented!(),
        }
    }

    pub(crate) fn perform_reloc(
        &self,
        reloc: &FinalizedMachReloc,
        expr_defs: &ExprDefinitions,
        func: &Function,
    ) {
        let name = match reloc.target {
            FinalizedRelocTarget::ExternalName(ExternalName::User(reff)) => {
                func.params.user_named_funcs()[reff].clone()
            }
            _ => unimplemented!(),
        };

        {
            let at = unsafe {
                self.mmap
                    .as_ptr()
                    .offset(isize::try_from(reloc.offset).unwrap())
            };
            let base = expr_defs.lookup_symbol(SymbolId(name.index)).unwrap();

            match reloc.kind {
                Reloc::Abs4 => {
                    let what = unsafe { base.offset(isize::try_from(reloc.addend).unwrap()) };
                    unsafe {
                        write_unaligned(at as *mut u32, u32::try_from(what as usize).unwrap())
                    };
                }
                Reloc::Abs8 => {
                    let what = unsafe { base.offset(isize::try_from(reloc.addend).unwrap()) };
                    unsafe {
                        write_unaligned(at as *mut u64, u64::try_from(what as usize).unwrap())
                    };
                }
                Reloc::X86PCRel4 | Reloc::X86CallPCRel4 => {
                    let what = unsafe { base.offset(isize::try_from(reloc.addend).unwrap()) };
                    let pcrel = i32::try_from((what as isize) - (at as isize)).unwrap();
                    unsafe { write_unaligned(at as *mut i32, pcrel) };
                }
                Reloc::S390xPCRel32Dbl | Reloc::S390xPLTRel32Dbl => {
                    let what = unsafe { base.offset(isize::try_from(reloc.addend).unwrap()) };
                    let pcrel = i32::try_from(((what as isize) - (at as isize)) >> 1).unwrap();
                    unsafe { write_unaligned(at as *mut i32, pcrel) };
                }
                Reloc::Arm64Call => {
                    let iptr = at as *mut u32;
                    let diff = ((base as isize) - (at as isize)) >> 2;
                    assert!((diff >> 26 == -1) || (diff >> 26 == 0));
                    let chop = 32 - 26;
                    let imm26 = (diff as u32) << chop >> chop;
                    unsafe { modify_inst32(iptr, |inst| inst | imm26) };
                }
                Reloc::RiscvCallPlt => {
                    let what = unsafe { base.offset(isize::try_from(reloc.addend).unwrap()) };
                    let pcrel = i32::try_from((what as isize) - (at as isize)).unwrap() as u32;

                    let hi20 = pcrel.wrapping_add(0x800) & 0xFFFFF000;
                    let lo12 = pcrel.wrapping_sub(hi20) & 0xFFF;

                    unsafe {
                        // Do a R_RISCV_PCREL_HI20 on the `auipc`
                        let auipc_addr = at as *mut u32;
                        modify_inst32(auipc_addr, |auipc| (auipc & 0xFFF) | hi20);

                        // Do a R_RISCV_PCREL_LO12_I on the `jalr`
                        let jalr_addr = at.offset(4) as *mut u32;
                        modify_inst32(jalr_addr, |jalr| (jalr & 0xFFFFF) | (lo12 << 20));
                    }
                }
                _ => unimplemented!(),
            }
        }
    }

    #[cfg(feature = "std")]
    pub fn write(&self, write: &mut dyn std::io::Write) {
        use object::write::Object;
        use object::{Architecture, BinaryFormat, Endianness, SectionKind};

        let mut o = Object::new(BinaryFormat::Elf, Architecture::Aarch64, Endianness::Little);
        let text_section = o.add_section(vec![], b".text".to_vec(), SectionKind::Text);

        unsafe {
            o.section_mut(text_section)
                .append_data(self.mmap.slice(self.range.clone()), 4);
        }

        o.write_stream(write).unwrap();
    }
}

macro_rules! impl_trampoline {
    ($n:ident,) => {
        fn $n(expr: &CompiledExpr, args: &[f64]) -> f64 {
            assert_eq!(args.len(), expr.signature.params.len());
            let ptr = expr.mmap.as_ptr();
            unsafe {
                let func = core::mem::transmute::<_, unsafe extern "C" fn() -> f64>(ptr);
                func()
            }
        }
    };
    ($n:ident, $($ident:ident),*) => {
        fn $n(expr: &CompiledExpr, args: &[f64]) -> f64 {
            assert_eq!(args.len(), expr.signature.params.len());
            let ptr = expr.mmap.as_ptr();
            unsafe {
                let func = core::mem::transmute::<_, unsafe extern "C" fn($($ident : f64),*) -> f64>(ptr);
                let [$($ident),* ,.. ] = args else {
                    unreachable!()
                };
                func($(*$ident),*)
            }
        }
    };
}

impl_trampoline!(trampoline_0,);
impl_trampoline!(trampoline_1, a0);
impl_trampoline!(trampoline_2, a0, a1);
impl_trampoline!(trampoline_3, a0, a1, a2);
impl_trampoline!(trampoline_4, a0, a1, a2, a3);
impl_trampoline!(trampoline_5, a0, a1, a2, a3, a4);
impl_trampoline!(trampoline_6, a0, a1, a2, a3, a4, a5);
impl_trampoline!(trampoline_7, a0, a1, a2, a3, a4, a5, a6);

/// Reads a 32bit instruction at `iptr`, and writes it again after
/// being altered by `modifier`
unsafe fn modify_inst32(iptr: *mut u32, modifier: impl FnOnce(u32) -> u32) {
    let inst = iptr.read_unaligned();
    let new_inst = modifier(inst);
    iptr.write_unaligned(new_inst);
}
