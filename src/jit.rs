use crate::lexer::Lexer;
use crate::mmap::Mmap;
use crate::translator::Translator;
use alloc::collections::BTreeMap;
use alloc::string::{String, ToString};
use alloc::vec;
use alloc::vec::Vec;
use core::mem;
use core::ops::Range;
use cranelift::frontend::FunctionBuilderContext;
use cranelift_codegen::binemit::{CodeOffset, Reloc};
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::dominator_tree::DominatorTree;
use cranelift_codegen::flowgraph::ControlFlowGraph;
use cranelift_codegen::ir::{ExternalName, Function, Signature, UserExternalName};
use cranelift_codegen::isa::OwnedTargetIsa;
use cranelift_codegen::settings::Configurable;
use cranelift_codegen::{FinalizedMachReloc, FinalizedRelocTarget};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExprId(u32);

impl ExprId {
    pub fn as_bits(&self) -> u32 {
        self.0
    }
}

pub struct ExprDefinitions {
    /// Maps human readable names to expression IDs
    names: BTreeMap<String, ExprId>,
    signatures: BTreeMap<ExprId, Signature>,
    compiled_exprs: BTreeMap<ExprId, CompiledExpr>,
}

impl ExprDefinitions {
    pub fn new() -> Self {
        Self {
            names: Default::default(),
            signatures: Default::default(),
            compiled_exprs: Default::default(),
        }
    }

    pub fn declare_expr(&mut self, name: &str, sig: Signature) -> ExprId {
        assert!(
            !self.names.contains_key(name),
            "attempted to redeclare expression"
        );

        let id = ExprId(self.signatures.len() as u32);
        self.names.insert(name.to_string(), id);
        self.signatures.insert(id, sig);
        id
    }

    pub fn define_expr(&mut self, id: ExprId, expr: CompiledExpr) {
        assert!(
            !self.compiled_exprs.contains_key(&id),
            "attempted to redefine expression"
        );
        self.compiled_exprs.insert(id, expr);
    }

    pub fn get_expr_signature(&self, id: ExprId) -> &Signature {
        self.signatures.get(&id).unwrap()
    }

    pub fn get_expr_address(&self, id: ExprId) -> *const u8 {
        let expr = self.compiled_exprs.get(&id).unwrap();
        expr.func.as_ptr()
    }

    pub fn get_expr_data(&self, id: ExprId) -> &CompiledExpr {
        &self.compiled_exprs[&id]
    }

    pub fn lookup_name(&self, name: &str) -> Option<ExprId> {
        self.names.get(name).map(|n| *n)
    }
}

/// The `Compiler` is the global environment that compiles expressions.
pub struct Compiler {
    isa: OwnedTargetIsa,
    func_ctx: FunctionBuilderContext,

    expr_defs: ExprDefinitions,
    exprs_to_finalize: Vec<ExprId>,
}

impl Compiler {
    /// Creates a new `Compiler` for the current host target.
    pub fn new_for_host() -> Self {
        let isa_builder = cranelift::codegen::isa::lookup(target_lexicon::HOST).unwrap();
        let mut b = cranelift::codegen::settings::builder();
        b.set("opt_level", "speed_and_size").unwrap();

        let isa = isa_builder
            .finish(cranelift::codegen::settings::Flags::new(b))
            .unwrap();

        Self {
            isa,
            func_ctx: FunctionBuilderContext::new(),
            expr_defs: ExprDefinitions::new(),
            exprs_to_finalize: vec![],
        }
    }

    /// Compiles a given expression. See the module documentation for allowed syntax.
    pub fn compile(&mut self, str: &str) -> anyhow::Result<CompiledExpr> {
        let lexer = Lexer::new(str);
        let mut translator = Translator::from_tokens(lexer, &self.expr_defs);
        let func = translator.translate(&mut self.func_ctx)?;

        println!("{}", func.display());

        let cfg = ControlFlowGraph::with_function(&func);
        let domtree = DominatorTree::with_function(&func, &cfg);
        let mut ctrl_plane = ControlPlane::default();

        let compiled_expr = self
            .isa
            .compile_function(&func, &domtree, true, &mut ctrl_plane)?;

        let (mmap, range) = unsafe {
            let mut mmap = Mmap::new(compiled_expr.code_buffer().len());
            let range = 0..compiled_expr.code_buffer().len();
            let slice = mmap.slice_mut(range.clone());
            slice.copy_from_slice(compiled_expr.code_buffer());

            mmap.make_accessible(range.clone());
            mmap.make_executable(range.clone(), true);

            (mmap, range)
        };

        let relocs = compiled_expr
            .buffer
            .relocs()
            .iter()
            .map(|reloc| ModuleReloc::from_mach_reloc(reloc, &func))
            .collect();

        Ok(CompiledExpr {
            func: mmap,
            range,
            signature: func.signature.clone(),
            relocs,
        })
    }

    pub fn call(&self, expr: ExprId, args: &[f64]) -> f64 {
        self.expr_defs.get_expr_data(expr).call(args)
    }

    pub fn define_expr(&mut self, name: &str, expr: CompiledExpr) -> anyhow::Result<ExprId> {
        let id = self.expr_defs.declare_expr(name, expr.signature.clone());
        self.expr_defs.define_expr(id, expr);

        self.exprs_to_finalize.push(id);

        Ok(id)
    }

    pub fn finalize_definitions(&mut self) -> anyhow::Result<()> {
        for id in core::mem::take(&mut self.exprs_to_finalize) {
            let func = &self.expr_defs.compiled_exprs[&id];

            func.perform_relocations(|name| self.expr_defs.get_expr_address(name));
        }

        Ok(())
    }
}

pub struct ModuleReloc {
    /// The offset at which the relocation applies, *relative to the
    /// containing section*.
    pub offset: CodeOffset,
    /// The kind of relocation.
    pub kind: Reloc,
    /// The external symbol / name to which this relocation refers.
    pub name: UserExternalName,
    /// The addend to add to the symbol value.
    pub addend: i64,
}

impl ModuleReloc {
    pub fn from_mach_reloc(mach_reloc: &FinalizedMachReloc, func: &Function) -> Self {
        let name = match mach_reloc.target {
            FinalizedRelocTarget::ExternalName(ExternalName::User(reff)) => {
                func.params.user_named_funcs()[reff].clone()
            }
            _ => unimplemented!(),
        };
        Self {
            offset: mach_reloc.offset,
            kind: mach_reloc.kind,
            name,
            addend: mach_reloc.addend,
        }
    }
}

/// A compiled expression ready for execution.
pub struct CompiledExpr {
    func: Mmap,
    range: Range<usize>,
    signature: Signature,
    relocs: Vec<ModuleReloc>,
}

impl CompiledExpr {
    pub fn call(&self, args: &[f64]) -> f64 {
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

    pub(crate) fn perform_relocations(&self, get_address: impl Fn(ExprId) -> *const u8) {
        use core::ptr::write_unaligned;

        for &ModuleReloc {
            kind,
            offset,
            ref name,
            addend,
        } in &self.relocs
        {
            let at = unsafe { self.func.as_ptr().offset(isize::try_from(offset).unwrap()) };
            let base = get_address(ExprId(name.index));
            match kind {
                Reloc::Abs4 => {
                    let what = unsafe { base.offset(isize::try_from(addend).unwrap()) };
                    unsafe {
                        write_unaligned(at as *mut u32, u32::try_from(what as usize).unwrap())
                    };
                }
                Reloc::Abs8 => {
                    let what = unsafe { base.offset(isize::try_from(addend).unwrap()) };
                    unsafe {
                        write_unaligned(at as *mut u64, u64::try_from(what as usize).unwrap())
                    };
                }
                Reloc::X86PCRel4 | Reloc::X86CallPCRel4 => {
                    let what = unsafe { base.offset(isize::try_from(addend).unwrap()) };
                    let pcrel = i32::try_from((what as isize) - (at as isize)).unwrap();
                    unsafe { write_unaligned(at as *mut i32, pcrel) };
                }
                Reloc::S390xPCRel32Dbl | Reloc::S390xPLTRel32Dbl => {
                    let what = unsafe { base.offset(isize::try_from(addend).unwrap()) };
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
                    let what = unsafe { base.offset(isize::try_from(addend).unwrap()) };
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
                .append_data(self.func.slice(self.range.clone()), 4);
        }

        o.write_stream(write).unwrap();
    }
}

/// Reads a 32bit instruction at `iptr`, and writes it again after
/// being altered by `modifier`
unsafe fn modify_inst32(iptr: *mut u32, modifier: impl FnOnce(u32) -> u32) {
    let inst = iptr.read_unaligned();
    let new_inst = modifier(inst);
    iptr.write_unaligned(new_inst);
}

macro_rules! impl_trampoline {
    ($n:ident,) => {
        fn $n(expr: &CompiledExpr, args: &[f64]) -> f64 {
            assert_eq!(args.len(), expr.signature.params.len());
            let ptr = expr.func.as_ptr();
            unsafe {
                let func = mem::transmute::<_, unsafe extern "C" fn() -> f64>(ptr);
                func()
            }
        }
    };
    ($n:ident, $($ident:ident),*) => {
        fn $n(expr: &CompiledExpr, args: &[f64]) -> f64 {
            assert_eq!(args.len(), expr.signature.params.len());
            let ptr = expr.func.as_ptr();
            unsafe {
                let func = mem::transmute::<_, unsafe extern "C" fn($($ident : f64),*) -> f64>(ptr);
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
