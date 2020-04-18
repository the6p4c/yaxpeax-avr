# `yaxpeax-avr`
AVR decoders implemented as part of the `yaxpeax` project. Implements traits provided by `yaxpeax-arch`.

## Known "issues"
- The `brbc` and `brbs` instructions are displayed as their equivalent pseudo-instructions, based on the bit they test: `brbc 0, label` becomes `brsh label`. In this case specifically, `brcc` also exists (and is identical), however `brsh` will be displayed.
- Target specification is limited to enabling/disabling support for 16-bit `sts` and `lds` instructions (as they can collide with other instructions cores with support for them don't have). Valid instructions (even if they might be unsupported by a core) are never rejected. Bytes which don't resemble an instruction from _any_ instruction set subset are still invalid.
