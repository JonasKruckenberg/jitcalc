use core::fmt;
use core::ops::Range;
use core::ptr;
use core::ptr::NonNull;
use rustix::mm::{mprotect, MprotectFlags};

#[repr(transparent)]
pub struct SendSyncPtr<T: ?Sized>(NonNull<T>);

unsafe impl<T: Send + ?Sized> Send for SendSyncPtr<T> {}
unsafe impl<T: Sync + ?Sized> Sync for SendSyncPtr<T> {}

impl<T: ?Sized> SendSyncPtr<T> {
    /// Creates a new pointer wrapping the non-nullable pointer provided.
    pub fn new(ptr: NonNull<T>) -> SendSyncPtr<T> {
        SendSyncPtr(ptr)
    }

    /// Returns the underlying raw pointer.
    pub fn as_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T: ?Sized, U> From<U> for SendSyncPtr<T>
where
    U: Into<NonNull<T>>,
{
    fn from(ptr: U) -> SendSyncPtr<T> {
        SendSyncPtr::new(ptr.into())
    }
}

impl<T: ?Sized> fmt::Debug for SendSyncPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ptr().fmt(f)
    }
}

#[derive(Debug)]
pub struct Mmap {
    memory: SendSyncPtr<[u8]>,
    len: usize,
}

impl Mmap {
    pub fn new(size: usize) -> Self {
        let ptr = unsafe {
            rustix::mm::mmap_anonymous(
                ptr::null_mut(),
                size,
                rustix::mm::ProtFlags::READ | rustix::mm::ProtFlags::WRITE,
                rustix::mm::MapFlags::PRIVATE,
            )
            .unwrap()
        };
        let memory = ptr::slice_from_raw_parts_mut(ptr.cast(), size);
        let memory = SendSyncPtr::new(NonNull::new(memory).unwrap());
        Mmap { memory, len: size }
    }

    #[cfg(feature = "std")]
    pub unsafe fn slice(&self, range: Range<usize>) -> &[u8] {
        assert!(range.start <= range.end);
        assert!(range.end <= self.len);
        core::slice::from_raw_parts(self.as_ptr().add(range.start), range.end - range.start)
    }

    pub unsafe fn slice_mut(&mut self, range: Range<usize>) -> &mut [u8] {
        assert!(range.start <= range.end);
        assert!(range.end <= self.len);
        core::slice::from_raw_parts_mut(self.as_mut_ptr().add(range.start), range.end - range.start)
    }

    pub fn make_accessible(&mut self, range: Range<usize>) {
        let ptr = self.memory.as_ptr().cast::<u8>();
        unsafe {
            mprotect(
                ptr.add(range.start).cast(),
                range.end - range.start,
                MprotectFlags::READ | MprotectFlags::WRITE,
            )
            .unwrap();
        }
    }

    #[inline]
    pub fn as_ptr(&self) -> *const u8 {
        self.memory.as_ptr() as *const u8
    }

    #[inline]
    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.memory.as_ptr() as *mut u8
    }

    pub unsafe fn make_executable(&self, range: Range<usize>, enable_branch_protection: bool) {
        let base = self.memory.as_ptr().cast::<u8>().add(range.start).cast();
        let len = range.end - range.start;

        let flags = MprotectFlags::READ | MprotectFlags::EXEC;
        let flags = if enable_branch_protection {
            #[cfg(all(target_arch = "aarch64", target_os = "linux"))]
            if std::arch::is_aarch64_feature_detected!("bti") {
                MprotectFlags::from_bits_retain(flags.bits() | /* PROT_BTI */ 0x10)
            } else {
                flags
            }

            #[cfg(not(all(target_arch = "aarch64", target_os = "linux")))]
            flags
        } else {
            flags
        };

        mprotect(base, len, flags).unwrap();
    }
}

impl Drop for Mmap {
    fn drop(&mut self) {
        unsafe {
            let ptr = self.memory.as_ptr().cast();
            let len = (*self.memory.as_ptr()).len();
            if len == 0 {
                return;
            }
            rustix::mm::munmap(ptr, len).expect("munmap failed");
        }
    }
}
