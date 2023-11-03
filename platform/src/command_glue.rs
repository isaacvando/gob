//  OUTPUT

#[derive(Clone, Debug, PartialEq)]
#[repr(C)]
pub struct Output {
    pub status: roc_std::RocResult<(), CommandErr>,
    pub stderr: roc_std::RocList<u8>,
    pub stdout: roc_std::RocList<u8>,
}

// COMMAND

#[derive(Clone, Default, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct Command {
    pub args: roc_std::RocList<roc_std::RocStr>,
    pub envs: roc_std::RocList<roc_std::RocStr>,
    pub program: roc_std::RocStr,
    pub clearEnvs: bool,
}

// COMMAND ERROR

#[derive(Clone, Copy, PartialEq)]
#[repr(u8)]
pub enum discriminant_CommandErr {
    ExitCode = 0,
    IOError = 1,
    KilledBySignal = 2,
}

impl core::fmt::Debug for discriminant_CommandErr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::ExitCode => f.write_str("discriminant_CommandErr::ExitCode"),
            Self::IOError => f.write_str("discriminant_CommandErr::IOError"),
            Self::KilledBySignal => f.write_str("discriminant_CommandErr::KilledBySignal"),
        }
    }
}

#[repr(C, align(8))]
pub union union_CommandErr {
    ExitCode: i32,
    IOError: core::mem::ManuallyDrop<roc_std::RocStr>,
    KilledBySignal: (),
}

const _SIZE_CHECK_union_CommandErr: () = assert!(core::mem::size_of::<union_CommandErr>() == 24);
const _ALIGN_CHECK_union_CommandErr: () = assert!(core::mem::align_of::<union_CommandErr>() == 8);

const _SIZE_CHECK_CommandErr: () = assert!(core::mem::size_of::<CommandErr>() == 32);
const _ALIGN_CHECK_CommandErr: () = assert!(core::mem::align_of::<CommandErr>() == 8);

impl CommandErr {
    /// Returns which variant this tag union holds. Note that this never includes a payload!
    pub fn discriminant(&self) -> discriminant_CommandErr {
        unsafe {
            let bytes = core::mem::transmute::<&Self, &[u8; core::mem::size_of::<Self>()]>(self);

            core::mem::transmute::<u8, discriminant_CommandErr>(*bytes.as_ptr().add(24))
        }
    }

    /// Internal helper
    fn set_discriminant(&mut self, discriminant: discriminant_CommandErr) {
        let discriminant_ptr: *mut discriminant_CommandErr = (self as *mut CommandErr).cast();

        unsafe {
            *(discriminant_ptr.add(24)) = discriminant;
        }
    }
}

#[repr(C)]
pub struct CommandErr {
    payload: union_CommandErr,
    discriminant: discriminant_CommandErr,
}

impl Clone for CommandErr {
    fn clone(&self) -> Self {
        use discriminant_CommandErr::*;

        let payload = unsafe {
            match self.discriminant {
                ExitCode => union_CommandErr {
                    ExitCode: self.payload.ExitCode.clone(),
                },
                IOError => union_CommandErr {
                    IOError: self.payload.IOError.clone(),
                },
                KilledBySignal => union_CommandErr {
                    KilledBySignal: self.payload.KilledBySignal.clone(),
                },
            }
        };

        Self {
            discriminant: self.discriminant,
            payload,
        }
    }
}

impl core::fmt::Debug for CommandErr {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use discriminant_CommandErr::*;

        unsafe {
            match self.discriminant {
                ExitCode => {
                    let field: &i32 = &self.payload.ExitCode;
                    f.debug_tuple("CommandErr::ExitCode").field(field).finish()
                }
                IOError => {
                    let field: &roc_std::RocStr = &self.payload.IOError;
                    f.debug_tuple("CommandErr::IOError").field(field).finish()
                }
                KilledBySignal => {
                    let field: &() = &self.payload.KilledBySignal;
                    f.debug_tuple("CommandErr::KilledBySignal")
                        .field(field)
                        .finish()
                }
            }
        }
    }
}

impl Eq for CommandErr {}

impl PartialEq for CommandErr {
    fn eq(&self, other: &Self) -> bool {
        use discriminant_CommandErr::*;

        if self.discriminant != other.discriminant {
            return false;
        }

        unsafe {
            match self.discriminant {
                ExitCode => self.payload.ExitCode == other.payload.ExitCode,
                IOError => self.payload.IOError == other.payload.IOError,
                KilledBySignal => self.payload.KilledBySignal == other.payload.KilledBySignal,
            }
        }
    }
}

impl core::hash::Hash for CommandErr {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        use discriminant_CommandErr::*;

        unsafe {
            match self.discriminant {
                ExitCode => self.payload.ExitCode.hash(state),
                IOError => self.payload.IOError.hash(state),
                KilledBySignal => self.payload.KilledBySignal.hash(state),
            }
        }
    }
}

impl CommandErr {
    pub fn unwrap_ExitCode(mut self) -> i32 {
        debug_assert_eq!(self.discriminant, discriminant_CommandErr::ExitCode);
        unsafe { self.payload.ExitCode }
    }

    pub fn is_ExitCode(&self) -> bool {
        matches!(self.discriminant, discriminant_CommandErr::ExitCode)
    }

    pub fn unwrap_IOError(mut self) -> roc_std::RocStr {
        debug_assert_eq!(self.discriminant, discriminant_CommandErr::IOError);
        unsafe { core::mem::ManuallyDrop::take(&mut self.payload.IOError) }
    }

    pub fn is_IOError(&self) -> bool {
        matches!(self.discriminant, discriminant_CommandErr::IOError)
    }

    pub fn is_KilledBySignal(&self) -> bool {
        matches!(self.discriminant, discriminant_CommandErr::KilledBySignal)
    }
}

impl CommandErr {
    pub fn ExitCode(payload: i32) -> Self {
        Self {
            discriminant: discriminant_CommandErr::ExitCode,
            payload: union_CommandErr { ExitCode: payload },
        }
    }

    pub fn IOError(payload: roc_std::RocStr) -> Self {
        Self {
            discriminant: discriminant_CommandErr::IOError,
            payload: union_CommandErr {
                IOError: core::mem::ManuallyDrop::new(payload),
            },
        }
    }

    pub fn KilledBySignal() -> Self {
        Self {
            discriminant: discriminant_CommandErr::KilledBySignal,
            payload: union_CommandErr { KilledBySignal: () },
        }
    }
}

impl Drop for CommandErr {
    fn drop(&mut self) {
        // Drop the payloads
        match self.discriminant() {
            discriminant_CommandErr::ExitCode => {}
            discriminant_CommandErr::IOError => unsafe {
                core::mem::ManuallyDrop::drop(&mut self.payload.IOError)
            },
            discriminant_CommandErr::KilledBySignal => {}
        }
    }
}
