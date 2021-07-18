use yaxpeax_arch::{Arch, Decoder, StandardDecodeError, U8Reader};
use yaxpeax_avr::*;

type InstDecoder = <AVR as Arch>::Decoder;

const ALL_TARGETS: [Target; 2] = [
    Target { has_16_bit_lds_sts: false },
    Target { has_16_bit_lds_sts: true },
];

fn test_display(data: &[u8], expected: &'static str) {
    for target in &ALL_TARGETS {
        test_display_on(data, expected, *target);
    }
}

fn test_display_on(data: &[u8], expected: &'static str, target: Target) {
    let instr = InstDecoder::new(target)
        .decode(&mut U8Reader::new(data))
        .unwrap_or_else(|e| panic!("failed to decode {:x?} on {:?} ({})", data, target, e));

    assert_eq!(format!("{}", instr), expected);
}

fn test_fails(data: &[u8], expected: StandardDecodeError) {
    for target in &ALL_TARGETS {
        test_fails_on(data, expected, *target);
    }
}

fn test_fails_on(data: &[u8], expected: StandardDecodeError, target: Target) {
    let instr = InstDecoder::new(target)
        .decode(&mut U8Reader::new(data));

    match instr {
        Ok(instr) => panic!("successfully decoded {:x?} on {:?} when decode should fail (got {})", data, target, instr),
        Err(e) => assert_eq!(e, expected),
    }
}

#[test]
fn test_arith() {
    test_display(&[0x1E, 0x1D], "adc r17, r14");
    test_display(&[0x1E, 0x0D], "add r17, r14");
    test_display(&[0x91, 0x96], "adiw r27:r26, $21");
    test_display(&[0x10, 0x95], "com r17");
    test_display(&[0x1A, 0x95], "dec r17");
    test_display(&[0x1D, 0x03], "fmul r17, r21");
    test_display(&[0x95, 0x03], "fmuls r17, r21");
    test_display(&[0x9D, 0x03], "fmulsu r17, r21");
    test_display(&[0x13, 0x95], "inc r17");
    test_display(&[0x15, 0xE5], "ldi r17, $55");
    test_display(&[0x1E, 0x2D], "mov r17, r14");
    test_display(&[0xAD, 0x01], "movw r21:r20, r27:r26");
    test_display(&[0x1E, 0x9D], "mul r17, r14");
    test_display(&[0x15, 0x02], "muls r17, r21");
    test_display(&[0x15, 0x03], "mulsu r17, r21");
    test_display(&[0x11, 0x95], "neg r17");
    test_display(&[0x1E, 0x09], "sbc r17, r14");
    test_display(&[0x15, 0x45], "sbci r17, $55");
    test_display(&[0x9E, 0x97], "sbiw r27:r26, $2E");
    test_display(&[0x1E, 0x19], "sub r17, r14");
    test_display(&[0x15, 0x55], "subi r17, $55");
}

#[test]
fn test_bitwise() {
    test_display(&[0x1E, 0x21], "and r17, r14");
    test_display(&[0x15, 0x75], "andi r17, $55");
    test_display(&[0x15, 0x95], "asr r17");
    test_display(&[0xDB, 0x98], "cbi $1B, 3");
    test_display(&[0x1E, 0x25], "eor r17, r14");
    test_display(&[0x16, 0x95], "lsr r17");
    test_display(&[0x1E, 0x29], "or r17, r14");
    test_display(&[0x15, 0x65], "ori r17, $55");
    test_display(&[0x17, 0x95], "ror r17");
    test_display(&[0xDB, 0x9A], "sbi $1B, 3");
}

#[test]
fn test_mem_load_store() {
    test_display(&[0xD8, 0x95], "elpm");
    test_display(&[0x06, 0x90], "elpm r0, z");
    test_display(&[0x16, 0x91], "elpm r17, z");
    test_display(&[0x17, 0x91], "elpm r17, z+");
    test_display(&[0x1B, 0xB3], "in r17, $1B");
    test_display(&[0x16, 0x93], "lac z, r17");
    test_display(&[0x15, 0x93], "las z, r17");
    test_display(&[0x17, 0x93], "lat z, r17");
    test_display(&[0x1C, 0x91], "ld r17, x");
    test_display(&[0x1D, 0x91], "ld r17, x+");
    test_display(&[0x1E, 0x91], "ld r17, -x");
    test_display(&[0x18, 0x81], "ld r17, y");
    test_display(&[0x19, 0x91], "ld r17, y+");
    test_display(&[0x1A, 0x91], "ld r17, -y");
    test_display_on(&[0x1B, 0xA9], "ldd r17, y+51", Target { has_16_bit_lds_sts: false });
    test_display(&[0x10, 0x81], "ld r17, z");
    test_display(&[0x11, 0x91], "ld r17, z+");
    test_display(&[0x12, 0x91], "ld r17, -z");
    test_display_on(&[0x13, 0xA9], "ldd r17, z+51", Target { has_16_bit_lds_sts: false });
    test_display(&[0x10, 0x91, 0xAA, 0x55], "lds r17, $55AA");
    test_display_on(&[0x15, 0xA3], "lds r17, $55", Target { has_16_bit_lds_sts: true });
    test_display(&[0xC8, 0x95], "lpm");
    test_display(&[0x04, 0x90], "lpm r0, z");
    test_display(&[0x14, 0x91], "lpm r17, z");
    test_display(&[0x15, 0x91], "lpm r17, z+");
    test_display(&[0x1B, 0xBB], "out $1B, r17");
    test_display(&[0x1F, 0x91], "pop r17");
    test_display(&[0x1F, 0x93], "push r17");
    test_display(&[0x1C, 0x93], "st x, r17");
    test_display(&[0x1D, 0x93], "st x+, r17");
    test_display(&[0x1E, 0x93], "st -x, r17");
    test_display(&[0x18, 0x83], "st y, r17");
    test_display(&[0x19, 0x93], "st y+, r17");
    test_display(&[0x1A, 0x93], "st -y, r17");
    test_display_on(&[0x1B, 0xAB], "std y+51, r17", Target { has_16_bit_lds_sts: false });
    test_display(&[0x10, 0x83], "st z, r17");
    test_display(&[0x11, 0x93], "st z+, r17");
    test_display(&[0x12, 0x93], "st -z, r17");
    test_display_on(&[0x13, 0xAB], "std z+51, r17", Target { has_16_bit_lds_sts: false });
    test_display(&[0x10, 0x93, 0xAA, 0x55], "sts $55AA, r17");
    test_display_on(&[0x15, 0xAB], "sts $55, r17", Target { has_16_bit_lds_sts: true });
    test_display(&[0xE8, 0x95], "spm");
    test_display(&[0xF8, 0x95], "spm z+");
    test_display(&[0x14, 0x93], "xch z, r17");
}

#[test]
fn test_branch() {
    // Test the limits only for one (since they're all the same instruction
    // under the hood - bad practice, but this test would be pages long
    // otherwise)
    test_display(&[0x01, 0xF2], "breq pc-63");
    test_display(&[0xF1, 0xF3], "breq pc-1");
    test_display(&[0xF9, 0xF3], "breq pc");
    test_display(&[0x01, 0xF0], "breq pc+1");
    test_display(&[0x51, 0xF1], "breq pc+43");
    test_display(&[0xF9, 0xF1], "breq pc+64");
    test_display(&[0x54, 0xF5], "brge pc+43");
    test_display(&[0x55, 0xF5], "brhc pc+43");
    test_display(&[0x55, 0xF1], "brhs pc+43");
    test_display(&[0x57, 0xF5], "brid pc+43");
    test_display(&[0x57, 0xF1], "brie pc+43");
    test_display(&[0x50, 0xF1], "brlo pc+43");
    test_display(&[0x54, 0xF1], "brlt pc+43");
    test_display(&[0x52, 0xF1], "brmi pc+43");
    test_display(&[0x51, 0xF5], "brne pc+43");
    test_display(&[0x52, 0xF5], "brpl pc+43");
    test_display(&[0x50, 0xF5], "brsh pc+43");
    test_display(&[0x56, 0xF5], "brtc pc+43");
    test_display(&[0x56, 0xF1], "brts pc+43");
    test_display(&[0x53, 0xF5], "brvc pc+43");
    test_display(&[0x53, 0xF1], "brvs pc+43");
    test_display(&[0x1E, 0x94, 0x50, 0xAD], "call $55AA0");
    test_display(&[0x19, 0x95], "eicall");
    test_display(&[0x19, 0x94], "eijmp");
    test_display(&[0x09, 0x95], "icall");
    test_display(&[0x09, 0x94], "ijmp");
    test_display(&[0x1C, 0x94, 0x50, 0xAD], "jmp $55AA0");
    test_display(&[0x00, 0xD8], "rcall pc-2047");
    test_display(&[0xFE, 0xDF], "rcall pc-1");
    test_display(&[0xFF, 0xDF], "rcall pc");
    test_display(&[0x00, 0xD0], "rcall pc+1");
    test_display(&[0xFF, 0xD7], "rcall pc+2048");
    test_display(&[0x08, 0x95], "ret");
    test_display(&[0x18, 0x95], "reti");
    test_display(&[0x00, 0xC8], "rjmp pc-2047");
    test_display(&[0xFE, 0xCF], "rjmp pc-1");
    test_display(&[0xFF, 0xCF], "rjmp pc");
    test_display(&[0x00, 0xC0], "rjmp pc+1");
    test_display(&[0xFF, 0xC7], "rjmp pc+2048");
    test_display(&[0xDB, 0x99], "sbic $1B, 3");
    test_display(&[0xDB, 0x9B], "sbis $1B, 3");
    test_display(&[0x13, 0xFD], "sbrc r17, 3");
    test_display(&[0x13, 0xFF], "sbrs r17, 3");
}

#[test]
fn test_sreg() {
    test_display(&[0x13, 0xF9], "bld r17, 3");
    test_display(&[0x98, 0x95], "break");
    test_display(&[0x13, 0xFB], "bst r17, 3");
    test_display(&[0x88, 0x94], "clc");
    test_display(&[0xD8, 0x94], "clh");
    test_display(&[0xF8, 0x94], "cli");
    test_display(&[0xA8, 0x94], "cln");
    test_display(&[0xC8, 0x94], "cls");
    test_display(&[0xE8, 0x94], "clt");
    test_display(&[0xB8, 0x94], "clv");
    test_display(&[0x98, 0x94], "clz");
    test_display(&[0x08, 0x94], "sec");
    test_display(&[0x58, 0x94], "seh");
    test_display(&[0x78, 0x94], "sei");
    test_display(&[0x28, 0x94], "sen");
    test_display(&[0x48, 0x94], "ses");
    test_display(&[0x68, 0x94], "set");
    test_display(&[0x38, 0x94], "sev");
    test_display(&[0x18, 0x94], "sez");
}

#[test]
fn test_compare() {
    test_display(&[0x1E, 0x15], "cp r17, r14");
    test_display(&[0x1E, 0x05], "cpc r17, r14");
    test_display(&[0x15, 0x35], "cpi r17, $55");
    test_display(&[0x1E, 0x11], "cpse r17, r14");
}

#[test]
fn test_misc() {
    test_display(&[0xEB, 0x94], "des $0E");
    test_display(&[0x00, 0x00], "nop");
    test_display(&[0x88, 0x95], "sleep");
    test_display(&[0x12, 0x95], "swap r17");
    test_display(&[0xA8, 0x95], "wdr");
}

#[test]
fn test_invalid_opcode() {
    test_fails(&[0xFF, 0xFF], StandardDecodeError::InvalidOpcode);
    // 32-bit lds with missing word operand
    test_fails(&[0x10, 0x91], StandardDecodeError::ExhaustedInput);
    // Just not enough bytes to make an opcode
    test_fails(&[0xA8], StandardDecodeError::ExhaustedInput);
}
