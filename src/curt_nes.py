#!/usr/local/bin/python3

# http://www.6502.org/tutorials/6502opcodes.html
# https://www.nesdev.org/obelisk-6502-guide/reference.html

# TODO:
# * build tiny sample rom that runs in working emulator
#   * create .cfg file
#   * 
# * finish mapper (mirroring)
# * test mapper

# FUTURE OPTIMIZATIONS:
# * use lookups to set flags

import array

# Interrupt
INTERRUPT_OFFSET = 0xFFFA
NMI_OFFSET = 0xFFFA
RESET_OFFSET = 0xFFFC
IRQ_OFFSET = 0xFFFE

# Memory
RAM_ZERO_PAGE_OFFSET     = 0x0000
RAM_STACK_OFFSET         = 0x0100
RAM_OFFSET               = 0x0200
RAM_MIRRORS_OFFSET       = 0x0800
IO_REGISTERS_OFFSET      = 0x2000
IO_MIRRORS_OFFSET        = 0x2008
IO_MORE_REGISTERS_OFFSET = 0x4000
EXPANSION_ROM_OFFSET     = 0x4020
SRAM_OFFSET              = 0x6000
ROM_LOW_OFFSET           = 0x8000
ROM_HIGH_OFFSET          = 0xC000
TOTAL_MEMORY             = 0x10000

# Processor status
CARRY_FLAG = 0
ZERO_FLAG = 1
INTERRUPT_DISABLE_FLAG = 2
DECIMAL_MODE_FLAG = 3
BREAK_COMMAND_FLAG = 4
_UNUSED_FLAG = 5
OVERFLOW_FLAG = 6
NEGATIVE_FLAG = 7

class VMStop(Exception):
    pass

class Mapper(object):
    """
    This Mapper class is responsible for memory mapping done by the
    main bus (which doesn't change per cartridge but done here for efficiency)
    and the cartidge mapper chip.
    """
    mapper_num = 0
    # bank_registers = []

    def __init__(self, mem, num_prg_rom_banks, num_chr_rom_banks):
        self.mem = mem
        self.num_prg_rom_banks = num_prg_rom_banks
        self.num_chr_rom_banks = num_chr_rom_banks
        # Build address lookup (registers not included)
        addr_lookup = array.array('H', (i for i in range(0x10000)))
        prg_rom_length = num_prg_rom_banks * 0x4000
        for i in range(0x8000):  # connected/addressable 2 * 16 KB banks
            addr_lookup[ROM_LOW_OFFSET+i] = ROM_LOW_OFFSET + (i % prg_rom_length)
        self._addr_lookup = addr_lookup

    # def resolve_lookup(self):
    #     """
    #     Returns memory array and address lookup to support accessing memory
    #     without overhead of a method call.
    #     """
    #     return self._addr_lookup

    def resolve(self, addr16):
        return self._addr_lookup[addr16]

    def read(self, addr32):
        return self.mem[addr32]

    def write(self, addr32, value):
        self.mem[addr32] = value

mappers = {
    mapper.mapper_num: mapper
    for mapper in (Mapper,)
}

def signed8(value):
    return ((value&0xFF)^0x80) - 0x80

def play(mapper, registers=(0, 0, 0, 0, 0, 0), t=0):
    pc, sp, a, x, y, p = registers

    mem = mapper.mem  # for zero page r/w or resolved address *READS* ONLY!

    # Flag setting helpers
    def _set_p_c(p, v):
        return (p&~0x01) | (v&0x01)
    def _set_p_nz(p, v):
        return (p&~0x82) | (v&0x80) | (0x02 if v == 0 else 0)  # set N Z

    # Resolve address per mode
    def _resolve_indirect_addr(mem, pc):
        i = mapper.resolve(mem[pc+1]|(mem[pc+2]<<8))
        return mapper.resolve(mem[i]|(mem[i+1]<<8))
    def _resolve_relative_addr(mem, pc):
        return pc + signed8(mem[pc+1]) # no need to resolve??
    def _resolve_immediate(mem, pc):
        return pc + 1  # no need to resolve
    def _resolve_zero_page(mem, pc):
        return mem[pc+1]
    def _resolve_zero_page_indexed_x(mem, pc):
        return (mem[pc+1]+x)%0x100
    def _resolve_zero_page_indexed_y(mem, pc):
        return (mem[pc+1]+y)%0x100
    def _resolve_absolute(mem, pc):
        return mapper.resolve(((mem[pc+1]|(mem[pc+2]<<8))))
    def _resolve_absolute_indexed_x(mem, pc):
        return mapper.resolve(((mem[pc+1]|(mem[pc+2]<<8))+x)%0x10000)
    def _resolve_absolute_indexed_y(mem, pc):
        return mapper.resolve(((mem[pc+1]|(mem[pc+2]<<8))+y)%0x10000)
    def _resolve_indexed_indirect(mem, pc):
        i = (mem[pc+1]+x) % 0x100
        return mapper.resolve(mem[i]|(mem[i+1]<<8))
    def _resolve_indirect_indexed(mem, pc):
        i = (mem[pc+1])
        return mapper.resolve(((mem[i]|(mem[i+1]<<8))+y)%0x10000)

    # Read helpers
    def _read_indirect_addr(mem, pc):
        return mem[_resolve_indirect_addr(mem, pc)]
    def _read_relative_addr(mem, pc):
        return mem[_resolve_relative_addr(mem, pc)]
    def _read_immediate(mem, pc):
        return mem[_resolve_immediate(mem, pc)]
    def _read_zero_page(mem, pc):
        return mem[_resolve_zero_page(mem, pc)]
    def _read_zero_page_indexed_x(mem, pc):
        return mem[_resolve_zero_page_indexed_x(mem, pc)]
    def _read_zero_page_indexed_y(mem, pc):
        return mem[_resolve_zero_page_indexed_y(mem, pc)]
    def _read_absolute(mem, pc):
        return mem[_resolve_absolute(mem, pc)]
    def _read_absolute_indexed_x(mem, pc):
        return mem[_resolve_absolute_indexed_x(mem, pc)]
    def _read_absolute_indexed_y(mem, pc):
        return mem[_resolve_absolute_indexed_y(mem, pc)]
    def _read_indexed_indirect(mem, pc):
        return mem[_resolve_indexed_indirect(mem, pc)]
    def _read_indirect_indexed(mem, pc):
        return mem[_resolve_indirect_indexed(mem, pc)]

    # Write helpers
    def _write_indirect_addr(mem, pc, value):
        mapper.write(_resolve_indirect_addr(mem, pc), value)
    def _write_relative_addr(mem, pc, value):
        mapper.write(_resolve_relative_addr(mem, pc), value)
    def _write_immediate(mem, pc, value):
        mapper.write(_resolve_immediate(mem, pc), value)
    def _write_zero_page(mem, pc, value):
        mapper.write(_resolve_zero_page(mem, pc), value)
    def _write_zero_page_indexed_x(mem, pc, value):
        mapper.write(_resolve_zero_page_indexed_x(mem, pc), value)
    def _write_zero_page_indexed_y(mem, pc, value):
        mapper.write(_resolve_zero_page_indexed_y(mem, pc), value)
    def _write_absolute(mem, pc, value):
        mapper.write(_resolve_absolute(mem, pc), value)
    def _write_absolute_indexed_x(mem, pc, value):
        mapper.write(_resolve_absolute_indexed_x(mem, pc), value)
    def _write_absolute_indexed_y(mem, pc, value):
        mapper.write(_resolve_absolute_indexed_y(mem, pc), value)
    def _write_indexed_indirect(mem, pc, value):
        mapper.write(_resolve_indexed_indirect(mem, pc), value)
    def _write_indirect_indexed(mem, pc, value):
        mapper.write(_resolve_indirect_indexed(mem, pc), value)

    def _build_undefined_op(opcode):
        def undefined_op(pc):
            raise ValueError('Undefined opcode {}'.format(opcode))
        return undefined_op

    ops = [_build_undefined_op(opcode) for opcode in range(0x100)]

    # Placeholder (should be BRK)
    def _stop(pc):
        raise VMStop()
    ops[0x00] = _stop

    # NOP (No OPeration)
    # Affects Flags: none
    def _nop(pc):
        # MODE           SYNTAX       HEX LEN TIM
        # Implied       NOP           $EA  1   2
        return pc + 1
    ops[0xEA] = _nop

    # LDA (LoaD Accumulator)
    # Affects Flags: N Z
    def _lda_immediate(pc):
        # MODE           SYNTAX       HEX LEN TIM
        # Immediate     LDA #$44      $A9  2   2
        nonlocal a
        nonlocal p
        a = _read_immediate(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 2
    def _lda_zero_page(pc):
        # Zero Page     LDA $44       $A5  2   3
        nonlocal a
        nonlocal p
        a = _read_zero_page(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 2
    def _lda_zero_page_indexed_x(pc):
        # Zero Page,X   LDA $44,X     $B5  2   4
        nonlocal a
        nonlocal p
        a = _read_zero_page_indexed_x(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 2
    def _lda_absolute(pc):
        # Absolute      LDA $4400     $AD  3   4
        nonlocal a
        nonlocal p
        a = _read_absolute(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 3
    def _lda_absolute_indexed_x(pc):
        # Absolute,X    LDA $4400,X   $BD  3   4+
        nonlocal a
        nonlocal p
        a = _read_absolute_indexed_x(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 3
    def _lda_absolute_indexed_y(pc):
        # Absolute,Y    LDA $4400,Y   $B9  3   4+
        nonlocal a
        nonlocal p
        a = _read_absolute_indexed_y(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 3
    def _lda_indexed_indirect(pc):
        # Indirect,X    LDA ($44,X)   $A1  2   6
        nonlocal a
        nonlocal p
        a = _read_indexed_indirect(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 2
    def _lda_indirect_indexed(pc):
        # Indirect,Y    LDA ($44),Y   $B1  2   5+
        nonlocal a
        nonlocal p
        a = _read_indirect_indexed(mem, pc)
        p = _set_p_nz(p, a)
        return pc + 2
    ops[0xA9] = _lda_immediate
    ops[0xA5] = _lda_zero_page
    ops[0xB5] = _lda_zero_page_indexed_x
    ops[0xAD] = _lda_absolute
    ops[0xBD] = _lda_absolute_indexed_x
    ops[0xB9] = _lda_absolute_indexed_y
    ops[0xA1] = _lda_indexed_indirect
    ops[0xB1] = _lda_indirect_indexed

    # LDX (LoaD X register)
    # Affects Flags: N Z
    def _ldx_immediate(pc):
        # MODE           SYNTAX       HEX LEN TIM
        # Immediate     LDX #$44      $A2  2   2
        nonlocal x
        nonlocal p
        x = _read_immediate(mem, pc)
        p = _set_p_nz(p, x)
        return pc + 2
    def _ldx_zero_page(pc):
        # Zero Page     LDX $44       $A6  2   3
        nonlocal x
        nonlocal p
        x = _read_zero_page(mem, pc)
        p = _set_p_nz(p, x)
        return pc + 2
    def _ldx_zero_page_indexed_y(pc):
        # Zero Page,Y   LDX $44,Y     $B6  2   4
        nonlocal x
        nonlocal p
        x = _read_zero_page_indexed_y(mem, pc)
        p = _set_p_nz(p, x)
        return pc + 2
    def _ldx_absolute(pc):
        # Absolute      LDX $4400     $AE  3   4
        nonlocal x
        nonlocal p
        x = _read_absolute(mem, pc)
        p = _set_p_nz(p, x)
        return pc + 3
    def _ldx_absolute_indexed_y(pc):
        # Absolute,Y    LDX $4400,Y   $BE  3   4+
        nonlocal x
        nonlocal p
        x = _read_absolute_indexed_y(mem, pc)
        p = _set_p_nz(p, x)
        return pc + 3
    ops[0xA2] = _ldx_immediate
    ops[0xA6] = _ldx_zero_page
    ops[0xB6] = _ldx_zero_page_indexed_y
    ops[0xAE] = _ldx_absolute
    ops[0xBE] = _ldx_absolute_indexed_y

    # LDY (LoaD Y register)
    # Affects Flags: N Z
    def _ldy_immediate(pc):
        # MODE           SYNTAX       HEX LEN TIM
        # Immediate     LDY #$44      $A0  2   2
        nonlocal y
        nonlocal p
        y = _read_immediate(mem, pc)
        p = _set_p_nz(p, y)
        return pc + 2
    def _ldy_zero_page(pc):
        # Zero Page     LDY $44       $A4  2   3
        nonlocal y
        nonlocal p
        y = _read_zero_page(mem, pc)
        p = _set_p_nz(p, y)
        return pc + 2
    def _ldy_zero_page_indexed_x(pc):
        # Zero Page,X   LDY $44,X     $B4  2   4
        nonlocal y
        nonlocal p
        y = _read_zero_page_indexed_x(mem, pc)
        p = _set_p_nz(p, y)
        return pc + 2
    def _ldy_absolute(pc):
        # Absolute      LDY $4400     $AC  3   4
        nonlocal y
        nonlocal p
        y = _read_absolute(mem, pc)
        p = _set_p_nz(p, y)
        return pc + 3
    def _ldy_absolute_indexed_x(pc):
        # Absolute,X    LDY $4400,X   $BC  3   4+
        nonlocal y
        nonlocal p
        y = _read_absolute_indexed_x(mem, pc)
        p = _set_p_nz(p, y)
        return pc + 3
    ops[0xA0] = _ldy_immediate
    ops[0xA4] = _ldy_zero_page
    ops[0xB4] = _ldy_zero_page_indexed_x
    ops[0xAC] = _ldy_absolute
    ops[0xBC] = _ldy_absolute_indexed_x

    # LSR (Logical Shift Right)
    # Affects Flags: N Z C
    # LSR shifts all bits right one position. 0 is shifted into bit 7 and the original bit 0 is shifted into the Carry.
    def _lsr_accumulator(pc):
        # MODE           SYNTAX       HEX LEN TIM
        # Accumulator   LSR A         $4A  1   2
        nonlocal a
        nonlocal p
        p = _set_p_c(p, a)
        a >>= 1
        p = _set_p_nz(p, a)
        return pc + 1
    def _lsr_zero_page(pc):
        # Zero Page     LSR $44       $46  2   5
        nonlocal p
        v = _read_zero_page(mem, pc)
        p = _set_p_c(p, v)
        v >>= 1
        _write_zero_page(mem, pc, v)
        p = _set_p_nz(p, v)
        return pc + 2
    def _lsr_zero_page_indexed_x(pc):
        # Zero Page,X   LSR $44,X     $56  2   6
        nonlocal p
        v = _read_zero_page_indexed_x(mem, pc)
        p = _set_p_c(p, v)
        v >>= 1
        _write_zero_page_indexed_x(mem, pc, v)
        p = _set_p_nz(p, v)
        return pc + 2
    # Absolute      LSR $4400     $4E  3   6
    # Absolute,X    LSR $4400,X   $5E  3   7
    ops[0x4A] = _lsr_accumulator
    ops[0x46] = _lsr_zero_page

    try:
        while True:
            pc = ops[mem[pc]](pc)
    except VMStop:
        pass  # clean exit

    return (pc, sp, a, x, y, p), t

class Cart(object):
    def __init__(
        self,
        prg_rom_banks,
        chr_rom_banks,
        mapper_cls=None,
        trainer=None,
        has_non_volatile_memory=None,
        hard_wired_nametable_mirroring_type=None,
        hard_wired_four_screen_mode=None,
        has_ines_2_0_identifier=None,
        console_type=None,
        num_ram_banks=None,
        timing_mode=None,
        more_console_type_info=None,
        num_misc_roms=None,
        default_expansion_device=None
    ):
        self.prg_rom_banks = prg_rom_banks
        self.chr_rom_banks = chr_rom_banks
        self.mapper_cls = mapper_cls or Mapper
        self.trainer = trainer
        self.hard_wired_nametable_mirroring_type = hard_wired_nametable_mirroring_type
        self.has_non_volatile_memory = has_non_volatile_memory
        self.hard_wired_four_screen_mode = hard_wired_four_screen_mode
        self.has_ines_2_0_identifier = has_ines_2_0_identifier
        self.console_type = console_type
        self.num_ram_banks = num_ram_banks
        self.timing_mode = timing_mode
        self.more_console_type_info = more_console_type_info
        self.num_misc_roms = num_misc_roms
        self.default_expansion_device = default_expansion_device
        
    @staticmethod
    def from_ines(ines_file):
        # See: https://www.nesdev.org/wiki/NES_2.0
        header = ines_file.read(0x10)

        # Extract header bytes upfront, clearing unused bits (for sanity)
        identification_string    = header[0x0:0x4]
        num_prg_rom_banks_low    = header[0x4]
        num_chr_rom_banks_low    = header[0x5]
        control_byte_low         = header[0x6]
        control_byte_high        = header[0x7]
        submapper                = header[0x8]
        num_rom_banks_high       = header[0x9]
        prg_ram_size             = header[0xA]
        chr_ram_size             = header[0xB]
        timing_mode              = header[0xC] & 0x3
        more_console_type_info   = header[0xD]
        num_misc_roms            = header[0xE] & 0x3
        default_expansion_device = header[0xF] & 0x3F

        # Validate file format is iNES
        assert(identification_string == b'NES\x1A')

        # Map header bytes to actual full-fledged values
        num_prg_rom_banks = num_prg_rom_banks_low | ((num_rom_banks_high&0xF)<<8)
        num_chr_rom_banks = num_chr_rom_banks_low | ((num_rom_banks_high&0xF0)<<4)
        hard_wired_nametable_mirroring_type = control_byte_low & 1
        has_non_volatile_memory = bool((control_byte_low>>1)&1)
        has_trainer = (control_byte_low>>2) & 1
        hard_wired_four_screen_mode = (control_byte_low>>3) & 1
        mapper_num = ((control_byte_low>>4)&0xF) | (control_byte_high&0xF0)
        has_ines_2_0_identifier = bool(((control_byte_high>>2)&3)==0x10)
        console_type = control_byte_high & 3
        num_ram_banks = prg_ram_size or 1

        # Extract trainer
        if has_trainer:
            trainer = ines_file.read(0x200)
        else:
            trainer = None

        # Extract 16 KB PRG-ROM banks
        prg_rom_banks = []
        for _ in range(num_prg_rom_banks):
            prg_rom_banks.append(ines_file.read(0x4000))

        # Extract  8 KB CHR-ROM banks
        chr_rom_banks = []
        for _ in range(num_chr_rom_banks):
            chr_rom_banks.append(ines_file.read(0x2000))

        # Instantiate mapper
        try:
            mapper_cls = mappers[mapper_num]
        except IndexError:
            raise ValueError('Memory mapper {} not supported'.format(mapper_num))

        return Cart(
            prg_rom_banks,
            chr_rom_banks,
            mapper_cls=mapper_cls,
            trainer=trainer,
            has_non_volatile_memory=has_non_volatile_memory,
            hard_wired_nametable_mirroring_type=hard_wired_nametable_mirroring_type,
            hard_wired_four_screen_mode=hard_wired_four_screen_mode,
            has_ines_2_0_identifier=has_ines_2_0_identifier,
            console_type=console_type,
            num_ram_banks=num_ram_banks,
            timing_mode=timing_mode,
            more_console_type_info=more_console_type_info,
            num_misc_roms=num_misc_roms,
            default_expansion_device=default_expansion_device
        )

    def print_config(self):
        mirroring_types = ['Horizontal or mapper-controlled', 'Vertical']
        mapper_names = {0: 'NROM, no mapper', 1: 'Nintendo MMC1', 2: 'UNROM switch', 3: 'CNROM switch', 4: 'Nintendo MMC3', 5: 'Nintendo MMC5', 6: 'FFE F4xxx', 7: 'AOROM switch', 8: 'FFE F3xxx', 9: 'Nintendo MMC2', 10: 'Nintendo MMC4', 11: 'ColorDreams chip', 12: 'FFE F6xxx', 15: '100-in-1 switch', 16: 'Bandai chip', 17: 'FFE F8xxx', 18: 'Jaleco SS8806 chip', 19: 'Namcot 106 chip', 20: 'Nintendo Disk System', 21: 'Konami VRC4a', 22: 'Konami VRC2a', 23: 'Konami VRC2a', 24: 'Konami VRC6', 25: 'Konami VRC4b', 32: 'Irem G-101 chip', 33: 'Taito TC0190/TC0350', 34: '32 KB ROM switch', 64: 'Tengen RAMBO-1 chip', 65: 'Irem H-3001 chip', 66: 'GNROM switch', 67: 'SunSoft3 chip', 68: 'SunSoft4 chip', 69: 'SunSoft5 FME-7 chip', 71: 'Camerica chip', 78: 'Irem 74HC161/32-based', 91: 'Pirate HK-SF3 chip'}
        console_types = ['Nintendo Entertainment System/Family Computer', 'Nintendo Vs. System', 'Nintendo Playchoice 10', 'Extended Console Type']
        timing_modes = ['RP2C02 ("NTSC NES")', 'RP2C07 ("Licensed PAL NES")', 'Multiple-region', 'UMC 6527P ("Dendy")']
        print(f'Number of 16 KB PRG-ROM banks: {len(self.prg_rom_banks)}')
        print(f'Number of 8 KB CHR-ROM banks: {len(self.chr_rom_banks)}')
        print(f'"Battery" and other non-volatile memory: {self.has_non_volatile_memory}')
        print(f'512-byte trainer: {self.trainer is not None}')
        print(f'Hard-wired nametable mirroring type: {mirroring_types[self.hard_wired_nametable_mirroring_type]}')
        print(f'Hard-wired four-screen mode: {self.hard_wired_four_screen_mode}')
        print(f'Memory mapper: {mapper_names[self.mapper_cls.mapper_num]} ({self.mapper_cls.mapper_num})')
        print(f'iNES 2.0 identifier: {self.has_ines_2_0_identifier}')
        print(f'Console type: {console_types[self.console_type]}')
        print(f'Number of 8 KB RAM banks: {self.num_ram_banks}')
        print(f'CPU/PPU Timing: {timing_modes[self.timing_mode]}')
        print(f'More console type info: {self.more_console_type_info}')
        print(f'Number of miscellaneous ROMs: {self.num_misc_roms}')
        print(f'Default expansion device: {self.default_expansion_device}')

class NES(object):
    def __init__(self, cart, registers=(0, 0, 0, 0, 0, 0), t=0):
        self.cart = cart
        self.registers = registers
        self.t = t
        self.mem = array.array('B', (0 for _ in range(0x8000+len(cart.prg_rom_banks)*0x4000)))
        self.mapper = cart.mapper_cls(self.mem, len(cart.prg_rom_banks), len(cart.chr_rom_banks))
        # Append PRG-ROM then CHR-ROM to memory starting at 0x8000
        for bank_i, bank in enumerate(cart.prg_rom_banks):
            for i in range(0x4000):
                self.mem[ROM_LOW_OFFSET+bank_i*0x4000] = bank[i]

    def play_cart(self):
        self.registers, self.t = \
            play(self.cart.mapper, self.registers, self.t)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Curt NES v0.0.0')
    parser.add_argument('rom')
    parser.add_argument('--print-cart-config', action='store_true')
    args = parser.parse_args()

    cart = Cart.from_ines(open(args.rom, 'rb'))

    if args.print_cart_config:
        cart.print_config()
        exit(0)

    #play(open(args.rom).read())
