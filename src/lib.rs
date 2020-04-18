//! AVR decoders implemented as part of the `yaxpeax` project. Implements traits
//! provided by `yaxpeax-arch`.
//!
//! Instruction set manual references are with respect to the document
//! [`Atmel-0856-AVR-Instruction-Set-Manual.pdf`](http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-0856-AVR-Instruction-Set-Manual.pdf)
//! as of 2020-04-13. `sha256: dbf578218c9f52f2fd22ccc538f53b9db4890320835725678c02b7b58f641981`
//!
//! References to the ATmega48A/PA/88A/PA/168A/PA/328/P datasheet are with
//! respect to the document [`ATmega48A-PA-88A-PA-168A-PA-328-P-DS-DS40002061A.pdf`](http://ww1.microchip.com/downloads/en/DeviceDoc/ATmega48A-PA-88A-PA-168A-PA-328-P-DS-DS40002061A.pdf)
//! as of 2020-04-13. `sha256: bf1c2e470f8ec7d4db340984f57556342557fed3eb9c457dd174b08db5993af6`
//!
//! Known "issues":
//! - The `brbc` and `brbs` instructions are displayed as their equivalent
//!   pseudo-instructions, based on the bit they test: `brbc 0, label` becomes
//!   `brsh label`. In this case specifically, `brcc` also exists (and is
//!   identical), however `brsh` will be displayed.
//! - Target specification is limited to enabling/disabling support for 16-bit
//!   `sts` and `lds` instructions (as they can collide with other instructions
//!   cores with support for them don't have). Valid instructions (even if they
//!   might be unsupported by a core) are never rejected. Bytes which don't
//!   resemble an instruction from _any_ instruction set subset are still
//!   invalid.
extern crate yaxpeax_arch;

use yaxpeax_arch::{Arch, Decoder, LengthedInstruction};

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SREGBit {
    C, Z, N, V, S, H, T, I
}

impl SREGBit {
    fn from_bit_index(idx: u8) -> SREGBit {
        use SREGBit::*;
        match idx {
            0 => C,
            1 => Z,
            2 => N,
            3 => V,
            4 => S,
            5 => H,
            6 => T,
            7 => I,
            _ => panic!("invalid sreg bit index"),
        }
    }
}

impl fmt::Display for SREGBit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SREGBit::*;
        write!(f, "{}", match self {
            C => 'c',
            Z => 'z',
            N => 'n',
            V => 'v',
            S => 's',
            H => 'h',
            T => 't',
            I => 'i',
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
    ADC,
    ADD,
    ADIW,
    AND,
    ANDI,
    ASR,
    BCLR(SREGBit),
    BLD,
    BRBC(SREGBit),
    BRBS(SREGBit),
    BREAK,
    BSET(SREGBit),
    BST,
    CALL,
    CBI,
    COM,
    CP,
    CPC,
    CPI,
    CPSE,
    DEC,
    DES,
    EICALL,
    EIJMP,
    ELPM,
    EOR,
    FMUL,
    FMULS,
    FMULSU,
    ICALL,
    IJMP,
    IN,
    INC,
    JMP,
    LAC,
    LAS,
    LAT,
    LD,
    LDD,
    LDI,
    LDS,
    LPM,
    LSR,
    MOV,
    MOVW,
    MUL,
    MULS,
    MULSU,
    NEG,
    NOP,
    OR,
    ORI,
    OUT,
    POP,
    PUSH,
    RCALL,
    RET,
    RETI,
    RJMP,
    ROR,
    SBC,
    SBCI,
    SBI,
    SBIC,
    SBIS,
    SBIW,
    SBRC,
    SBRS,
    SLEEP,
    SPM,
    ST,
    STD,
    STS,
    SUB,
    SUBI,
    SWAP,
    WDR,
    XCH,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Opcode::ADC => write!(f, "adc"),
            Opcode::ADD => write!(f, "add"),
            Opcode::ADIW => write!(f, "adiw"),
            Opcode::AND => write!(f, "and"),
            Opcode::ANDI => write!(f, "andi"),
            Opcode::ASR => write!(f, "asr"),
            Opcode::BCLR(bit) => write!(f, "cl{}", bit),
            Opcode::BLD => write!(f, "bld"),
            Opcode::BRBC(_) | Opcode::BRBS(_) => {
                let (bit, is_clear) = match self {
                    Opcode::BRBC(bit) => (bit, true),
                    Opcode::BRBS(bit) => (bit, false),
                    _ => unreachable!(),
                };

                let (suffix_clear, suffix_set) = match bit {
                    SREGBit::C => ("sh", "lo"),
                    SREGBit::Z => ("ne", "eq"),
                    SREGBit::N => ("pl", "mi"),
                    SREGBit::V => ("vc", "vs"),
                    SREGBit::S => ("ge", "lt"),
                    SREGBit::H => ("hc", "hs"),
                    SREGBit::T => ("tc", "ts"),
                    SREGBit::I => ("id", "ie"),
                };
                let suffix = if is_clear {
                    suffix_clear
                } else {
                    suffix_set
                };

                write!(f, "br{}", suffix)
            },
            Opcode::BREAK => write!(f, "break"),
            Opcode::BSET(bit) => write!(f, "se{}", bit),
            Opcode::BST => write!(f, "bst"),
            Opcode::CALL => write!(f, "call"),
            Opcode::CBI => write!(f, "cbi"),
            Opcode::COM => write!(f, "com"),
            Opcode::CP => write!(f, "cp"),
            Opcode::CPC => write!(f, "cpc"),
            Opcode::CPI => write!(f, "cpi"),
            Opcode::CPSE => write!(f, "cpse"),
            Opcode::DEC => write!(f, "dec"),
            Opcode::DES => write!(f, "des"),
            Opcode::EICALL => write!(f, "eicall"),
            Opcode::EIJMP => write!(f, "eijmp"),
            Opcode::ELPM => write!(f, "elpm"),
            Opcode::EOR => write!(f, "eor"),
            Opcode::FMUL => write!(f, "fmul"),
            Opcode::FMULS => write!(f, "fmuls"),
            Opcode::FMULSU => write!(f, "fmulsu"),
            Opcode::ICALL => write!(f, "icall"),
            Opcode::IJMP => write!(f, "ijmp"),
            Opcode::IN => write!(f, "in"),
            Opcode::INC => write!(f, "inc"),
            Opcode::JMP => write!(f, "jmp"),
            Opcode::LAC => write!(f, "lac"),
            Opcode::LAS => write!(f, "las"),
            Opcode::LAT => write!(f, "lat"),
            Opcode::LD => write!(f, "ld"),
            Opcode::LDD => write!(f, "ldd"),
            Opcode::LDI => write!(f, "ldi"),
            Opcode::LDS => write!(f, "lds"),
            Opcode::LPM => write!(f, "lpm"),
            Opcode::LSR => write!(f, "lsr"),
            Opcode::MOV => write!(f, "mov"),
            Opcode::MOVW => write!(f, "movw"),
            Opcode::MUL => write!(f, "mul"),
            Opcode::MULS => write!(f, "muls"),
            Opcode::MULSU => write!(f, "mulsu"),
            Opcode::NEG => write!(f, "neg"),
            Opcode::NOP => write!(f, "nop"),
            Opcode::OR => write!(f, "or"),
            Opcode::ORI => write!(f, "ori"),
            Opcode::OUT => write!(f, "out"),
            Opcode::POP => write!(f, "pop"),
            Opcode::PUSH => write!(f, "push"),
            Opcode::RCALL => write!(f, "rcall"),
            Opcode::RET => write!(f, "ret"),
            Opcode::RETI => write!(f, "reti"),
            Opcode::RJMP => write!(f, "rjmp"),
            Opcode::ROR => write!(f, "ror"),
            Opcode::SBC => write!(f, "sbc"),
            Opcode::SBCI => write!(f, "sbci"),
            Opcode::SBI => write!(f, "sbi"),
            Opcode::SBIC => write!(f, "sbic"),
            Opcode::SBIS => write!(f, "sbis"),
            Opcode::SBIW => write!(f, "sbiw"),
            Opcode::SBRC => write!(f, "sbrc"),
            Opcode::SBRS => write!(f, "sbrs"),
            Opcode::SLEEP => write!(f, "sleep"),
            Opcode::SPM => write!(f, "spm"),
            Opcode::ST => write!(f, "st"),
            Opcode::STD => write!(f, "std"),
            Opcode::STS => write!(f, "sts"),
            Opcode::SUB => write!(f, "sub"),
            Opcode::SUBI => write!(f, "subi"),
            Opcode::SWAP => write!(f, "swap"),
            Opcode::WDR => write!(f, "wdr"),
            Opcode::XCH => write!(f, "xch"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IndexRegister {
    X,
    Y,
    Z
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum IndexRegisterModifier {
    Unchanged,
    PostIncrement,
    PreDecrement,
    Displacement(u8),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    Register(u8),
    /// A register pair (Rn+1:Rn), where `low` stores `n >> 1`
    RegisterPair { low: u8 },
    IndexRegister(IndexRegister, IndexRegisterModifier)
}

#[derive(Debug, Copy, Clone)]
enum OperandSpec {
    Nothing,
    ImpliedRegister(Register),
    /// `.... ...d dddd ....`
    DestRegister,
    /// `.... ..r. .... rrrr`
    SrcRegister,
    /// `.... .... dddd ....`, where `dddd` is the register index minus 16
    DestRegister16_31,
    /// `.... .... .... rrrr`, where `rrrr` is the register index minus 16
    SrcRegister16_31,
    /// `.... .... ..dd ....`, where `dd` is an index into `[24, 26, 28, 30]`
    /// for the low register of the pair
    RegisterPair24_26_28_30,
    /// `.... .... .ddd ....` where `ddd` is the register index minus 16
    DestRegister16_23,
    /// `.... .... .... .rrr` where `rrr` is the register index minus 16
    SrcRegister16_23,
    /// `.... .... dddd ....` where `dddd` is `d >> 1` from `Rd+1:Rd`
    DestRegisterPair,
    /// `.... .... .... rrrr` where `rrrr` is `r >> 1` from `Rr+1:Rr`
    SrcRegisterPair,
    /// `.... .... .... .bbb` or `.... .... .... .sss`
    Imm3,
    /// `.... .... KKKK ....`
    Imm4,
    /// `.... .... KK.. KKKK`
    Imm6,
    /// `.... KKKK .... KKKK`
    Imm8,
    /// `..q. qq.. .... .qqq`
    RegDisp6(IndexRegister),
    /// `.... ..kk kkkk k...`
    BranchDisp7,
    /// `.... kkkk kkkk kkkk`
    RCALLJMPDisp12,
    /// `.... .... AAAA A...`
    Addr5,
    /// `.... .AA. .... AAAA`
    Addr6,
    /// `.... .kkk .... kkkk`
    Addr7,
    /// `.... .... .... ....` `kkkk kkkk kkkk kkkk` (two words)
    Addr16,
    /// `.... ...k kkkk ...k` `kkkk kkkk kkkk kkkk` (two words)
    Addr22,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operand {
    Nothing,
    Register(Register),
    ImmediateU8(u8),
    AddrPCRelative(i16),
    AddrAbsolute(u32),
    BitIndex(u8),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Nothing => panic!("can't display empty operand"),
            Operand::Register(r) => match r {
                Register::Register(i) => write!(f, "r{}", i),
                Register::RegisterPair { low } => {
                    let base = low * 2;
                    write!(f, "r{}:r{}", base + 1, base)
                },
                Register::IndexRegister(r, modifier) => {
                    let reg_name = match r {
                        IndexRegister::X => "x",
                        IndexRegister::Y => "y",
                        IndexRegister::Z => "z",
                    };

                    match modifier {
                        IndexRegisterModifier::Unchanged => write!(f, "{}", reg_name),
                        IndexRegisterModifier::PostIncrement => write!(f, "{}+", reg_name),
                        IndexRegisterModifier::PreDecrement => write!(f, "-{}", reg_name),
                        IndexRegisterModifier::Displacement(d) => write!(f, "{}+{}", reg_name, d),
                    }
                }
            }
            Operand::ImmediateU8(v) => write!(f, "${:02X}", v),
            Operand::AddrPCRelative(v) => if *v == 0 {
                write!(f, "pc")
            } else {
                write!(f, "pc{:+}", v)
            },
            Operand::AddrAbsolute(v) => write!(f, "${:X}", v),
            Operand::BitIndex(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: [Operand; 2],
    length: u8,
}

impl Default for Instruction {
    fn default() -> Self {
        Instruction {
            opcode: Opcode::NOP,
            operands: [Operand::Nothing, Operand::Nothing],
            length: 2,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.opcode)?;
        match &self.operands[0] {
            Operand::Nothing => return Ok(()),
            operand => write!(f, " {}", operand)?,
        }
        match &self.operands[1] {
            Operand::Nothing => return Ok(()),
            operand => write!(f, ", {}", operand)?,
        }

        Ok(())
    }
}

impl yaxpeax_arch::Instruction for Instruction {
    // TODO: this is wrong!!
    fn well_defined(&self) -> bool {
        true
    }
}

impl LengthedInstruction for Instruction {
    type Unit = <AVR as Arch>::Address;

    fn min_size() -> Self::Unit {
        2
    }

    fn len(&self) -> Self::Unit {
        self.length as Self::Unit
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum DecodeError {
    ExhaustedInput,
    InvalidOpcode,
    InvalidOperand,
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DecodeError::ExhaustedInput => write!(f, "exhausted input"),
            DecodeError::InvalidOpcode => write!(f, "invalid opcode"),
            DecodeError::InvalidOperand => write!(f, "invalid operand"),
        }
    }
}

impl yaxpeax_arch::DecodeError for DecodeError {
    fn data_exhausted(&self) -> bool {
        self == &DecodeError::ExhaustedInput
    }

    fn bad_opcode(&self) -> bool {
        self == &DecodeError::InvalidOpcode
    }

    fn bad_operand(&self) -> bool {
        self == &DecodeError::InvalidOperand
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Target {
    /// True if the target supports the 16-bit variants of the `lds` and `sts`
    /// instructions.
    pub has_16_bit_lds_sts: bool
}

fn decode_opcode(fullword: u16, target: &Target) -> Option<(Opcode, [OperandSpec; 2])> {
    // vvvv prefix4
    // xxxx xxxx xxxx xxxx
    // ^^^^ ^^^^ ^ prefix9
    let prefix4 = fullword >> (16 - 4);
    let prefix5 = fullword >> (16 - 5);
    let prefix6 = fullword >> (16 - 6);
    let prefix7 = fullword >> (16 - 7);
    let prefix8 = fullword >> (16 - 8);
    let prefix9 = fullword >> (16 - 9);

    //                vvvv suffix4
    // xxxx xxxx xxxx xxxx < suffix4_1
    //                ^^^
    //          suffix4_3
    let suffix4 = fullword & 0b1111;
    let suffix4_3 = suffix4 >> 1;
    let suffix4_1 = suffix4 & 0b1;

    match fullword {
        0b1001_0101_1001_1000 => return Some((Opcode::BREAK, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_0001_1001 => return Some((Opcode::EICALL, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0100_0001_1001 => return Some((Opcode::EIJMP, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_1101_1000 => return Some((Opcode::ELPM, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_0000_1001 => return Some((Opcode::ICALL, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0100_0000_1001 => return Some((Opcode::IJMP, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_1100_1000 => return Some((Opcode::LPM, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b0000_0000_0000_0000 => return Some((Opcode::NOP, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_0000_1000 => return Some((Opcode::RET, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_0001_1000 => return Some((Opcode::RETI, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_1000_1000 => return Some((Opcode::SLEEP, [OperandSpec::Nothing, OperandSpec::Nothing])),
        // In the instruction set reference ("116. SPM", pg. 169), this
        // instruction is shown with an implied operand of "Z+", but the
        // ATmega328 datasheet (a processor which has only the single SPM
        // instruction) shows that it's only a "Z" operand. This seems
        // consistent on every processor that only has a single SPM instruction.
        // Since it's consistently a "Z" operand, we'll leave it off to make the
        // output compatible with both (`avrasm2` rejects `spm z` on cores
        // without an `spm z+`)
        0b1001_0101_1110_1000 => return Some((Opcode::SPM, [OperandSpec::Nothing, OperandSpec::Nothing])),
        0b1001_0101_1111_1000 => return Some((Opcode::SPM, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PostIncrement)), OperandSpec::Nothing])),
        0b1001_0101_1010_1000 => return Some((Opcode::WDR, [OperandSpec::Nothing, OperandSpec::Nothing])),
        _ => {},
    }

    match prefix4 {
        0b0111 => return Some((Opcode::ANDI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        0b0011 => return Some((Opcode::CPI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        0b1110 => return Some((Opcode::LDI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        0b0110 => return Some((Opcode::ORI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        0b1101 => return Some((Opcode::RCALL, [OperandSpec::RCALLJMPDisp12, OperandSpec::Nothing])),
        0b1100 => return Some((Opcode::RJMP, [OperandSpec::RCALLJMPDisp12, OperandSpec::Nothing])),
        0b0100 => return Some((Opcode::SBCI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        0b0101 => return Some((Opcode::SUBI, [OperandSpec::DestRegister16_31, OperandSpec::Imm8])),
        _ => {},
    }

    match prefix5 {
        0b1011_0 => return Some((Opcode::IN, [OperandSpec::DestRegister, OperandSpec::Addr6])),
        0b1011_1 => return Some((Opcode::OUT, [OperandSpec::Addr6, OperandSpec::DestRegister])),
        _ => {},
    }

    match prefix6 {
        0b0001_11 => return Some((Opcode::ADC, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0000_11 => return Some((Opcode::ADD, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0010_00 => return Some((Opcode::AND, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b1111_01 | 0b1111_00 => {
            // This is basically an Imm3, but we've got to decode it now
            // to stuff it into the opcode itself, rather rather than as
            // an operand.
            let bit = SREGBit::from_bit_index((fullword & 0b111) as u8);
            return Some((
                if prefix6 & 1 != 0 { Opcode::BRBC(bit) } else { Opcode::BRBS(bit) },
                [OperandSpec::BranchDisp7, OperandSpec::Nothing],
            ))
        },
        0b0001_01 => return Some((Opcode::CP, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0000_01 => return Some((Opcode::CPC, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0001_00 => return Some((Opcode::CPSE, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0010_01 => return Some((Opcode::EOR, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0010_11 => return Some((Opcode::MOV, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b1001_11 => return Some((Opcode::MUL, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0010_10 => return Some((Opcode::OR, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0000_10 => return Some((Opcode::SBC, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        0b0001_10 => return Some((Opcode::SUB, [OperandSpec::DestRegister, OperandSpec::SrcRegister])),
        _ => {},
    }

    match (prefix7, suffix4_3, suffix4_1) {
        (0b1001_010, 0b010, 0b1) => return Some((Opcode::ASR, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1111_100, _, _) => return Some((Opcode::BLD, [OperandSpec::DestRegister, OperandSpec::Imm3])),
        (0b1111_101, _, _) => return Some((Opcode::BST, [OperandSpec::DestRegister, OperandSpec::Imm3])),
        (0b1001_010, 0b111, _) => return Some((Opcode::CALL, [OperandSpec::Addr22, OperandSpec::Nothing])),
        (0b1001_010, 0b000, 0b0) => return Some((Opcode::COM, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_010, 0b101, 0b0) => return Some((Opcode::DEC, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_000, 0b011, 0b0) => return Some((Opcode::ELPM, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged))])),
        (0b1001_000, 0b011, 0b1) => return Some((Opcode::ELPM, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PostIncrement))])),
        (0b1001_010, 0b001, 0b1) => return Some((Opcode::INC, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_010, 0b110, _) => return Some((Opcode::JMP, [OperandSpec::Addr22, OperandSpec::Nothing])),
        (0b1001_001, 0b011, 0b0) => return Some((Opcode::LAC, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_001, 0b010, 0b1) => return Some((Opcode::LAS, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_001, 0b011, 0b1) => return Some((Opcode::LAT, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_000, 0b110, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::Unchanged))])),
        (0b1001_000, 0b110, 0b1) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::PostIncrement))])),
        (0b1001_000, 0b111, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::PreDecrement))])),
        (0b1000_000, 0b100, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::Unchanged))])),
        (0b1001_000, 0b100, 0b1) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::PostIncrement))])),
        (0b1001_000, 0b101, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::PreDecrement))])),
        (0b1000_000, 0b000, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged))])),
        (0b1001_000, 0b000, 0b1) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PostIncrement))])),
        (0b1001_000, 0b001, 0b0) => return Some((Opcode::LD, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PreDecrement))])),
        (0b1001_000, 0b010, 0b0) => return Some((Opcode::LPM, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged))])),
        (0b1001_000, 0b010, 0b1) => return Some((Opcode::LPM, [OperandSpec::DestRegister, OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PostIncrement))])),
        // This is the only instruction in this form, so match the first half
        // then deal with the second half inside the block. This is fine, since
        // the prefix 0b1111_11x is only used by these two instructions anyway.
        (0b1111_110, r, _) => {
            if (r & 0b100) == 0b000 {
                return Some((Opcode::SBRC, [OperandSpec::DestRegister, OperandSpec::Imm3]));
            }
        },
        (0b1111_111, r, _) => {
            if (r & 0b100) == 0b000 {
                return Some((Opcode::SBRS, [OperandSpec::DestRegister, OperandSpec::Imm3]));
            }
        },
        (0b1001_001, 0b110, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_001, 0b110, 0b1) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::PostIncrement)), OperandSpec::DestRegister])),
        (0b1001_001, 0b111, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::X, IndexRegisterModifier::PreDecrement)), OperandSpec::DestRegister])),
        (0b1000_001, 0b100, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_001, 0b100, 0b1) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::PostIncrement)), OperandSpec::DestRegister])),
        (0b1001_001, 0b101, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Y, IndexRegisterModifier::PreDecrement)), OperandSpec::DestRegister])),
        (0b1000_001, 0b000, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        (0b1001_001, 0b000, 0b1) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PostIncrement)), OperandSpec::DestRegister])),
        (0b1001_001, 0b001, 0b0) => return Some((Opcode::ST, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::PreDecrement)), OperandSpec::DestRegister])),
        (0b1001_000, 0b000, 0b0) => return Some((Opcode::LDS, [OperandSpec::DestRegister, OperandSpec::Addr16])),
        (0b1001_010, 0b011, 0b0) => return Some((Opcode::LSR, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_010, 0b000, 0b1) => return Some((Opcode::NEG, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_000, 0b111, 0b1) => return Some((Opcode::POP, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_001, 0b111, 0b1) => return Some((Opcode::PUSH, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_010, 0b011, 0b1) => return Some((Opcode::ROR, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_001, 0b000, 0b0) => return Some((Opcode::STS, [OperandSpec::Addr16, OperandSpec::DestRegister])),
        (0b1001_010, 0b001, 0b0) => return Some((Opcode::SWAP, [OperandSpec::DestRegister, OperandSpec::Nothing])),
        (0b1001_001, 0b010, 0b0) => return Some((Opcode::XCH, [OperandSpec::ImpliedRegister(Register::IndexRegister(IndexRegister::Z, IndexRegisterModifier::Unchanged)), OperandSpec::DestRegister])),
        _ => {},
    };

    match (prefix8, suffix4) {
        (0b1001_0110, _) => return Some((Opcode::ADIW, [OperandSpec::RegisterPair24_26_28_30, OperandSpec::Imm6])),
        (0b1001_1000, _) => return Some((Opcode::CBI, [OperandSpec::Addr5, OperandSpec::Imm3])),
        (0b1001_0100, 0b1011) => return Some((Opcode::DES, [OperandSpec::Imm4, OperandSpec::Nothing])),
        (0b0000_0001, _) => return Some((Opcode::MOVW, [OperandSpec::DestRegisterPair, OperandSpec::SrcRegisterPair])),
        (0b0000_0010, _) => return Some((Opcode::MULS, [OperandSpec::DestRegister16_31, OperandSpec::SrcRegister16_31])),
        (0b1001_1010, _) => return Some((Opcode::SBI, [OperandSpec::Addr5, OperandSpec::Imm3])),
        (0b1001_1001, _) => return Some((Opcode::SBIC, [OperandSpec::Addr5, OperandSpec::Imm3])),
        (0b1001_1011, _) => return Some((Opcode::SBIS, [OperandSpec::Addr5, OperandSpec::Imm3])),
        (0b1001_0111, _) => return Some((Opcode::SBIW, [OperandSpec::RegisterPair24_26_28_30, OperandSpec::Imm6])),
        _ => {},
    }

    match (prefix9, suffix4) {
        (0b1001_0100_1, 0b1000) | (0b1001_0100_0, 0b1000) => return Some((
            {
                let bit = (fullword >> 4) & 0b111;
                let bit = SREGBit::from_bit_index(bit as u8);

                match prefix9 & 1 {
                    0 => Opcode::BSET(bit),
                    1 => Opcode::BCLR(bit),
                    _ => unreachable!(),
                }
            },
            [OperandSpec::Nothing, OperandSpec::Nothing]
        )),
        (0b0000_0011_0, _) | (0b0000_0011_1, _) => return Some((
            match (prefix9 & 1, suffix4 >> 3) {
                (0, 0) => Opcode::MULSU,
                (0, 1) => Opcode::FMUL,
                (1, 0) => Opcode::FMULS,
                (1, 1) => Opcode::FMULSU,
                _ => unreachable!(),
            },
            [OperandSpec::DestRegister16_23, OperandSpec::SrcRegister16_23]
        )),
        _ => {},
    }

    if target.has_16_bit_lds_sts {
        match prefix5 {
            0b1010_0 => return Some((Opcode::LDS, [OperandSpec::DestRegister16_31, OperandSpec::Addr7])),
            0b1010_1 => return Some((Opcode::STS, [OperandSpec::Addr7, OperandSpec::DestRegister16_31])),
            _ => {},
        }
    } else {
        match fullword & 0b1101_0010_0000_1000 {
            //1101 0010 0000 1000 - mask
            0b1000_0000_0000_1000 => return Some((Opcode::LDD, [OperandSpec::DestRegister, OperandSpec::RegDisp6(IndexRegister::Y)])),
            0b1000_0000_0000_0000 => return Some((Opcode::LDD, [OperandSpec::DestRegister, OperandSpec::RegDisp6(IndexRegister::Z)])),
            0b1000_0010_0000_1000 => return Some((Opcode::STD, [OperandSpec::RegDisp6(IndexRegister::Y), OperandSpec::DestRegister])),
            0b1000_0010_0000_0000 => return Some((Opcode::STD, [OperandSpec::RegDisp6(IndexRegister::Z), OperandSpec::DestRegister])),
            _ => {},
        }
    }

    None
}

#[derive(Debug)]
pub struct InstDecoder {
    target: Target,
}

impl InstDecoder {
    pub fn new(target: Target) -> InstDecoder {
        InstDecoder { target }
    }
}

impl Default for InstDecoder {
    fn default() -> Self {
        InstDecoder {
            // This is any core other than an AVRTiny, so it's basically 99% of
            // all AVRs
            target: Target { has_16_bit_lds_sts: false }
        }
    }
}

impl Decoder<Instruction> for InstDecoder {
    type Error = DecodeError;

    // Variable names in the interpretation match block try and copy the
    // instruction set manual, which leads to variable names like 'K' and 'A'.
    // We'll prefer matching the manual's conventions than pleasing rustc.
    #[allow(non_snake_case)]
    fn decode_into<T: IntoIterator<Item = u8>>(
        &self,
        instr: &mut Instruction,
        bytes: T,
    ) -> Result<(), Self::Error> {
        fn read_word<T: Iterator<Item = u8>>(bytes: &mut T) -> Result<u16, DecodeError> {
            let word: Vec<u8> = bytes.by_ref().take(2).collect();

            let (low, high) = match word[..] {
                [low, high] => (low, high),
                [] | [_] => {
                    return Err(DecodeError::ExhaustedInput);
                }
                _ => unreachable!(),
            };

            let fullword = ((high as u16) << 8) | (low as u16);
            Ok(fullword)
        }

        let mut bytes_iter = bytes.into_iter();
        let fullword = read_word(&mut bytes_iter)?;

        let (opcode, interpretation) = decode_opcode(fullword, &self.target).ok_or(DecodeError::InvalidOpcode)?;
        instr.opcode = opcode;

        for (interp, operand) in interpretation.iter().zip(instr.operands.iter_mut()) {
            *operand = match interp {
                OperandSpec::Nothing => { return Ok(()) },
                OperandSpec::ImpliedRegister(r) => Operand::Register(*r),
                OperandSpec::DestRegister => {
                    let d = (fullword >> 4) & 0b1_1111;
                    Operand::Register(Register::Register(d as u8))
                },
                OperandSpec::SrcRegister => {
                    let r = ((fullword >> 5) & 0b1_0000) | (fullword & 0b1111);
                    Operand::Register(Register::Register(r as u8))
                },
                OperandSpec::DestRegister16_31 => {
                    let d = (fullword >> 4) & 0b1111;
                    Operand::Register(Register::Register((d + 16) as u8))
                },
                OperandSpec::SrcRegister16_31 => {
                    let r = fullword & 0b1111;
                    Operand::Register(Register::Register((r + 16) as u8))
                },
                OperandSpec::RegisterPair24_26_28_30 => {
                    let d = (fullword >> 4) & 0b11;

                    // We're storing the base register's index shifted right
                    // one, so instead of [24, 26, 28, 30] as provided in the
                    // manual (for an example see "7. ADIW", pg. 33) we're using
                    // the values divided by 2.
                    Operand::Register(Register::RegisterPair { low: [12, 13, 14, 15][d as usize] })
                },
                OperandSpec::DestRegister16_23 => {
                    let d = (fullword >> 4) & 0b111;
                    Operand::Register(Register::Register((d + 16) as u8))
                },
                OperandSpec::SrcRegister16_23 => {
                    let r = fullword & 0b111;
                    Operand::Register(Register::Register((r + 16) as u8))
                },
                OperandSpec::DestRegisterPair => {
                    let d = (fullword >> 4) & 0b1111;
                    Operand::Register(Register::RegisterPair { low: d as u8 })
                },
                OperandSpec::SrcRegisterPair => {
                    let r = fullword & 0b1111;
                    Operand::Register(Register::RegisterPair { low: r as u8 })
                },
                OperandSpec::Imm3 => {
                    let K = fullword & 0b111;
                    Operand::BitIndex(K as u8)
                },
                OperandSpec::Imm4 => {
                    let K = (fullword >> 4) & 0b1111;
                    Operand::ImmediateU8(K as u8)
                },
                OperandSpec::Imm6 => {
                    let K = ((fullword >> 2) & 0b11_0000) | (fullword & 0b1111);
                    Operand::ImmediateU8(K as u8)
                },
                OperandSpec::Imm8 => {
                    let K = ((fullword >> 4) & 0b1111_0000) | (fullword & 0b1111);
                    Operand::ImmediateU8(K as u8)
                },
                OperandSpec::RegDisp6(r) => {
                    let q = ((fullword >> 8) & 0b10_0000) | ((fullword >> 7) & 0b1_1000) | (fullword & 0b111);
                    Operand::Register(Register::IndexRegister(*r, IndexRegisterModifier::Displacement(q as u8)))
                },
                OperandSpec::BranchDisp7 => {
                    // Operand is a signed 7-bit value, -64 <= k <= +63
                    // Need to sign extend - 1 more sign bit for an i8, which we
                    // can then "freely" (i.e. make Rust do it for us) sign
                    // extend to an i16
                    let k = (fullword >> 3) & 0b111_1111;
                    // Perform sign extension, and add once since final
                    // displacement range is (pc - 63) <= dest <= (pc + 64)
                    let k = ((k | ((k & 0b100_0000) << 1)) as i8) as i16 + 1;

                    Operand::AddrPCRelative(k as i16)
                },
                OperandSpec::RCALLJMPDisp12 => {
                    let k = fullword & 0b1111_1111_1111;
                    // Operand is a signed 12-bit value, -2048 <= k <= 2047
                    // Need to sign extend - 4 more sign bits for an i16
                    let signbit = fullword & 0b1000_0000_0000;
                    let signext = (signbit << 4) | (signbit << 3) | (signbit << 2) | (signbit << 1);
                    // Perform sign extension, and add one since final
                    // displacement range is (pc - 2047) <= dest <= (pc + 2048)
                    let k = (signext | k) as i16 + 1;

                    Operand::AddrPCRelative(k)
                },
                OperandSpec::Addr5 => {
                    let A = (fullword >> 3) & 0b1_1111;

                    Operand::AddrAbsolute(A as u32)
                },
                OperandSpec::Addr6 => {
                    let A = ((fullword >> 5) & 0b11_0000) | (fullword & 0b1111);

                    Operand::AddrAbsolute(A as u32)
                },
                OperandSpec::Addr7 => {
                    // This transformation is described in the instruction set
                    // manual ("75. LDS (16-bit)", pg. 117) as
                    // addr[7:0] = {
                    //     ~inst[8], inst[8], inst[10], inst[9],
                    //     inst[3], inst[2], inst[1], inst[0]
                    // }
                    let inst8 = (fullword >> 8) & 0b1;
                    let ninst8 = inst8 ^ 1;
                    let k = (ninst8 << 7) | (inst8 << 6) | ((fullword >> 5) & 0b11_0000) | (fullword & 0b1111);

                    Operand::AddrAbsolute(k as u32)
                },
                OperandSpec::Addr16 => {
                    let k = read_word(&mut bytes_iter)?;
                    instr.length += 2;

                    Operand::AddrAbsolute(k as u32)
                },
                OperandSpec::Addr22 => {
                    let A_high = ((fullword >> 3) & 0b11_1110) | (fullword & 0b1);
                    let A_low = read_word(&mut bytes_iter)?;
                    instr.length += 2;

                    let A = ((A_high as u32) << 16) | (A_low as u32);
                    // Word address (into program memory), convert to bytes
                    let A = A * 2;

                    Operand::AddrAbsolute(A)
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct AVR;

impl Arch for AVR {
    // TODO: Possibly distinguish between program memory and data memory
    // addresses
    type Address = u32;
    type Instruction = Instruction;
    type DecodeError = DecodeError;
    type Decoder = InstDecoder;
    type Operand = Operand;
}
