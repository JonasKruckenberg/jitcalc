use crate::lexer::Lexer;
use crate::mmap::Mmap;
use crate::translator::Translator;
use core::mem;
use core::ops::Range;
use cranelift::frontend::FunctionBuilderContext;
use cranelift_codegen::control::ControlPlane;
use cranelift_codegen::dominator_tree::DominatorTree;
use cranelift_codegen::flowgraph::ControlFlowGraph;
use cranelift_codegen::isa::OwnedTargetIsa;
use cranelift_codegen::settings::Configurable;
use object::write::Object;
use object::{Architecture, BinaryFormat, Endianness, SectionKind};

/// The `Compiler` is the global environment that compiles expressions.
pub struct Compiler {
    isa: OwnedTargetIsa,
    func_ctx: FunctionBuilderContext,
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
        }
    }

    /// Compiles a given expression. See the module documentation for allowed syntax.
    pub fn compile(&mut self, str: &str) -> CompiledExpr {
        let lexer = Lexer::new(str);
        let mut translator = Translator::from_tokens(lexer);
        let func = translator.translate(&mut self.func_ctx).unwrap();

        let cfg = ControlFlowGraph::with_function(&func);
        let domtree = DominatorTree::with_function(&func, &cfg);
        let mut ctrl_plane = ControlPlane::default();

        let compiled_func = self
            .isa
            .compile_function(&func, &domtree, true, &mut ctrl_plane)
            .unwrap();

        unsafe {
            let mut mmap = Mmap::new(compiled_func.code_buffer().len());
            let range = 0..compiled_func.code_buffer().len();
            let slice = mmap.slice_mut(range.clone());
            slice.copy_from_slice(compiled_func.code_buffer());

            mmap.make_accessible(range.clone());
            mmap.make_executable(range.clone(), true);

            CompiledExpr {
                func: mmap,
                range,
                params: translator.num_params(),
            }
        }
    }
}

/// A compiled expression ready for execution.
pub struct CompiledExpr {
    func: Mmap,
    range: Range<usize>,
    params: usize,
}

impl CompiledExpr {
    pub fn call(&self, args: &[f64]) -> f64 {
        assert_eq!(args.len(), self.params);

        unsafe {
            let func = mem::transmute::<_, unsafe extern "C" fn(args: *const f64) -> f64>(
                self.func.as_ptr(),
            );

            func(args.as_ptr())
        }
    }

    pub fn write(&self, write: &mut dyn std::io::Write) {
        let mut o = Object::new(BinaryFormat::Elf, Architecture::Aarch64, Endianness::Little);
        let text_section = o.add_section(vec![], b".text".to_vec(), SectionKind::Text);

        unsafe {
            o.section_mut(text_section)
                .append_data(self.func.slice(self.range.clone()), 4);
        }

        o.write_stream(write).unwrap();
    }
}
