#!/usr/local/bin/python3

# NOTE: closures are used heavily vs conventional/Pythonic object-oriented ways,
#       to improve performance; from a sloppy benchmark, using local vars in a 
#       closure appeared to be nearly twice as fast as using class attributes

# TODO:
# * finish rendering
#   * test ppu functionality: (1) bg eval (2) sp eval (3) pixel rendering
#   * handle sprite-0 hit flag
#   * handle rendering disabled
#   * review docs for flags, glitches, exceptions, etc.
# * tick / figure out initialization/syncing w/ cpu
# * add pygame and draw to screen
# * refactor ppu reads/writes (so they all go through single calls and make sense w/ sp eval)
# * handle controllers
# * remove unused constants etc
# * try out test ROMs!
# * rename: crazyNES, coolNES, nneess, HI-NES (crown icon)
# * default status to unused = 1 and removed redundant sets
# * lots of cleanup / refactoring
#   * break instructions into single cycles
# * test interrupts
# * build tiny sample rom that runs in working emulator
#   * (1) create .cfg file (2) compile do nothing program (3) compile program that loops 
# FUTURE TO DOS:
# * check if carry is supposed to only be set/reset per adc, sbc (not both within each op)
# * use "massive" lookups to set flags?
# * or at least, reduce flags setting as much as possible (ex. ((r>>8)&X) == (r>>8)) OR p ^= (p ^ a) & MASK etc)

import array
import os

# Memory addressing
ZERO_PAGE_OFFSET         = 0x0000
STACK_OFFSET             = 0x0100
STACK_SIZE               = 0x0100
TOP_OF_STACK_OFFSET      = 0x01FF
RAM_OFFSET               = 0x0200
RAM_SIZE                 = 0x0800
RAM_MIRRORS_OFFSET       = 0x0800
RAM_MIRRORS_SIZE         = 0x1800
PPU_REGS_OFFSET          = 0x2000
PPU_REGS_SIZE            = 0x0008
PPU_REG_MIRRORS_OFFSET   = 0x2008
PPU_REG_MIRRORS_SIZE     = 0x1FF8
APU_REGS_OFFSET          = 0x4000
APU_REGS_SIZE            = 0x0020  # end 8 bytes are for "APU and I/O functionality that is normally disabled"
EXPANSION_ROM_OFFSET     = 0x4020
SRAM_OFFSET              = 0x6000
ROM_LOWER_BANK_OFFSET    = 0x8000
ROM_BANK_SIZE            = 0x4000
ROM_UPPER_BANK_OFFSET    = 0xC000
TOTAL_ADDRESS_SPACE      = 0x10000

# Interrupt types (internal to this emulator)
NMI = 0
RESET = 1
IRQ = 2

# Processor statuses
N = 1 << 7  # negative
V = 1 << 6  # overflow
U = 1 << 5  # unused
B = 1 << 4  # break command
D = 1 << 3  # decimal mode
I = 1 << 2  # interrupt disable
Z = 1 << 1  # zero
C = 1 << 0  # carry

MASK_V    = ~(V)
MASK_B    = ~(B)
MASK_D    = ~(D)
MASK_I    = ~(I)
MASK_C    = ~(C)
MASK_NZ   = ~(N|Z)
MASK_NVZ  = ~(N|V|Z)
MASK_NVZC = ~(N|V|Z|C)
MASK_NZC  = ~(N|Z|C)

# For debugging purposes
INSTRUCTION_LABELS = [
    'BRK', 'ORA', '   ', '   ', '   ', 'ORA', 'ASL', '   ', 'PHP', 'ORA', 'ASL', '   ', '   ', 'ORA', 'ASL', '   ',
    'BPL', 'ORA', '   ', '   ', '   ', 'ORA', 'ASL', '   ', 'CLC', 'ORA', '   ', '   ', '   ', 'ORA', 'ASL', '   ',
    'JSR', 'AND', '   ', '   ', 'BIT', 'AND', 'ROL', '   ', 'PLP', 'AND', 'ROL', '   ', 'BIT', 'AND', 'ROL', '   ',
    'BMI', 'AND', '   ', '   ', '   ', 'AND', 'ROL', '   ', 'SEC', 'AND', '   ', '   ', '   ', 'AND', 'ROL', '   ',
    'RTI', 'EOR', '   ', '   ', '   ', 'EOR', 'LSR', '   ', 'PHA', 'EOR', 'LSR', '   ', 'JMP', 'EOR', 'LSR', '   ',
    'BVC', 'EOR', '   ', '   ', '   ', 'EOR', 'LSR', '   ', 'CLI', 'EOR', '   ', '   ', '   ', 'EOR', 'LSR', '   ',
    'RTS', 'ADC', '   ', '   ', '   ', 'ADC', 'ROR', '   ', 'PLA', 'ADC', 'ROR', '   ', 'JMP', 'ADC', 'ROR', '   ',
    'BVS', 'ADC', '   ', '   ', '   ', 'ADC', 'ROR', '   ', 'SEI', 'ADC', '   ', '   ', '   ', 'ADC', 'ROR', '   ',
    '   ', 'STA', '   ', '   ', 'STY', 'STA', 'STX', '   ', 'DEY', '   ', 'TXA', '   ', 'STY', 'STA', 'STX', '   ',
    'BCC', 'STA', '   ', '   ', 'STY', 'STA', 'STX', '   ', 'TYA', 'STA', 'TXS', '   ', '   ', 'STA', '   ', '   ',
    'LDY', 'LDA', 'LDX', '   ', 'LDY', 'LDA', 'LDX', '   ', 'TAY', 'LDA', 'TAX', '   ', 'LDY', 'LDA', 'LDX', '   ',
    'BCS', 'LDA', '   ', '   ', 'LDY', 'LDA', 'LDX', '   ', 'CLV', 'LDA', 'TSX', '   ', 'LDY', 'LDA', 'LDX', '   ',
    'CPY', 'CMP', '   ', '   ', 'CPY', 'CMP', 'DEC', '   ', 'INY', 'CMP', 'DEX', '   ', 'CPY', 'CMP', 'DEC', '   ',
    'BNE', 'CMP', '   ', '   ', '   ', 'CMP', 'DEC', '   ', 'CLD', 'CMP', '   ', '   ', '   ', 'CMP', 'DEC', '   ',
    'CPX', 'SBC', '   ', '   ', 'CPX', 'SBC', 'INC', '   ', 'INX', 'SBC', 'NOP', '   ', 'CPX', 'SBC', 'INC', '   ',
    'BEQ', 'SBC', '   ', '   ', '   ', 'SBC', 'INC', '   ', 'SED', 'SBC', '   ', '   ', '   ', 'SBC', 'INC', '   '
]

INSTRUCTION_BYTES = [
    1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 0, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0,
    3, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0,
    1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0,
    1, 2, 0, 0, 0, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0,
    0, 2, 0, 0, 2, 2, 2, 0, 1, 0, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 0, 3, 0, 0,
    2, 2, 2, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 0, 2, 2, 0, 1, 3, 0, 0, 0, 3, 3, 0
]

# Two tables below use follow constants for address modes:
# IMPLIED = 0
# IMMEDIATE = 1
# ACCUMULATOR = 2
# ZERO_PAGE = 3
# ZERO_PAGE_INDEXED_X = 4
# ZERO_PAGE_INDEXED_Y = 5
# ABSOLUTE = 6
# ABSOLUTE_INDEXED_X = 7
# ABSOLUTE_INDEXED_Y = 8
# INDEXED_INDIRECT = 9
# INDIRECT_INDEXED = 10
# INDIRECT = 11
# RELATIVE = 12

INSTRUCTION_ADDR_MODES = [
    0x0, 0x9, 0x0, 0x0, 0x0, 0x3, 0x3, 0x0, 0x0, 0x1, 0x2, 0x0, 0x0, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0,
    0x6, 0x9, 0x0, 0x0, 0x3, 0x3, 0x3, 0x0, 0x0, 0x1, 0x2, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0,
    0x0, 0x9, 0x0, 0x0, 0x0, 0x3, 0x3, 0x0, 0x0, 0x1, 0x2, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0,
    0x0, 0x9, 0x0, 0x0, 0x0, 0x3, 0x3, 0x0, 0x0, 0x1, 0x2, 0x0, 0xB, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0,
    0x0, 0x9, 0x0, 0x0, 0x3, 0x3, 0x3, 0x0, 0x0, 0x0, 0x0, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x4, 0x4, 0x5, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x0, 0x0,
    0x1, 0x9, 0x1, 0x0, 0x3, 0x3, 0x3, 0x0, 0x0, 0x1, 0x0, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x4, 0x4, 0x5, 0x0, 0x0, 0x8, 0x0, 0x0, 0x7, 0x7, 0x8, 0x0,
    0x1, 0x9, 0x0, 0x0, 0x3, 0x3, 0x3, 0x0, 0x0, 0x1, 0x0, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0,
    0x1, 0x9, 0x0, 0x0, 0x3, 0x3, 0x3, 0x0, 0x0, 0x1, 0x0, 0x0, 0x6, 0x6, 0x6, 0x0,
    0xC, 0xA, 0x0, 0x0, 0x0, 0x4, 0x4, 0x0, 0x0, 0x8, 0x0, 0x0, 0x0, 0x7, 0x7, 0x0
]

ADDR_MODE_FORMATS = [
    lambda cpu_read, pc, operands: '',
    lambda cpu_read, pc, operands: ' #${:02X}'   .format(operands[1]),
    lambda cpu_read, pc, operands: ' A',
    lambda cpu_read, pc, operands: ' ${:02X} = {:02X}'.format(operands[1], cpu_read(operands[1])),
    lambda cpu_read, pc, operands: ' ${:02X},X'  .format(operands[1]),
    lambda cpu_read, pc, operands: ' ${:02X},Y'  .format(operands[1]),
    lambda cpu_read, pc, operands: ' ${:04X}'    .format(operands[1]|(operands[2]<<8)),
    lambda cpu_read, pc, operands: ' ${:04X},X'  .format(operands[1]|(operands[2]<<8)),
    lambda cpu_read, pc, operands: ' ${:04X},Y'  .format(operands[1]|(operands[2]<<8)),
    lambda cpu_read, pc, operands: ' (${:02X},X)'.format(operands[1]),
    lambda cpu_read, pc, operands: ' (${:02X}),Y'.format(operands[1]),
    lambda cpu_read, pc, operands: ' (${:04X})'  .format(operands[1]|(operands[2]<<8)),
    lambda cpu_read, pc, operands: ' ${:02X}'    .format(pc+2+signed8(operands[1])),
]

# PPU memory constants
PPUCTRL	  = 0x2000 # VPHB SINN	NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select (NN)
PPUMASK   = 0x2001 # BGRs bMmG	color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
PPUSTATUS = 0x2002 # VSO- ----	vblank (V), sprite 0 hit (S), sprite overflow (O); read resets write pair for $2005/$2006
OAMADDR   = 0x2003 # aaaa aaaa	OAM read/write address
OAMDATA   = 0x2004 # dddd dddd	OAM data read/write
# VRAM reading and writing shares the same internal address register that rendering uses
PPUSCROLL = 0x2005 # xxxx xxxx	fine scroll position (two writes: X scroll, Y scroll)
PPUADDR   = 0x2006 # aaaa aaaa	PPU read/write address (two writes: most significant byte, least significant byte)
PPUDATA   = 0x2007 # dddd dddd	PPU data read/write
OAMDMA    = 0x4014 # aaaa aaaa	OAM DMA high address

# If the PPU is currently in vertical blank, and the PPUSTATUS ($2002) vblank flag is still set (1), changing the NMI flag in bit 7 of $2000 from 0 to 1 will immediately generate an NMI. This can result in graphical errors (most likely a misplaced scroll) if the NMI routine is executed too late in the blanking period to finish on time. To avoid this problem it is prudent to read $2002 immediately before writing $2000 to clear the vblank flag.
# After power/reset, writes to this register are ignored for about 30,000 cycles.
PPUCTRL_V = 0x80 # Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
PPUCTRL_P = 0x40 # PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins)
PPUCTRL_H = 0x20 # Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
PPUCTRL_B = 0x10 # Background pattern table address (0: $0000; 1: $1000)
PPUCTRL_S = 0x08 # Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
PPUCTRL_I = 0x04 # VRAM address increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down)
PPUCTRL_N = 0x03 # Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)

PPU_ADDR_INCREMENTS = [1, 0, 0, 0, 32]

PPUMASK_B = 0x80 # Emphasize blue
PPUMASK_G = 0x40 # Emphasize green (red on PAL/Dendy)
PPUMASK_R = 0x20 # Emphasize red (green on PAL/Dendy)
PPUMASK_s = 0x10 # 1: Show sprites
PPUMASK_b = 0x08 # 1: Show background
PPUMASK_M = 0x04 # 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
PPUMASK_m = 0x02 # 1: Show background in leftmost 8 pixels of screen, 0: Hide
PPUMASK_g = 0x01 # Greyscale (0: normal color, 1: produce a greyscale display)

PPUSTATUS_V = 0x80
PPUSTATUS_S = 0x40
PPUSTATUS_O = 0x20

NT_MIRRORING_HORIZONTAL = 0
NT_MIRRORING_VERTICAL = 1
NT_MIRRORING_SINGLE_SCREEN = 3
NT_MIRRORING_FOUR_SCREEN = 4

CONSOLE_FLAVOR_NES = 0
CONSOLE_FLAVOR_VS_SYSTEM = 1
CONSOLE_FLAVOR_PLAYCHOICE_10 = 2
CONSOLE_FLAVOR_OTHER = 3

NTSC_PALETTE = 'RRR\x01\x1aQ\x0f\x0fe#\x06c6\x03K@\x04&?\t\x042\x13\x00\x1f \x00\x0b*\x00\x00/\x00\x00.\n\x00&-\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xa0\xa0\x1eJ\x9d87\xbcX(\xb8u!\x94\x84#\\\x82.$o?\x00QR\x001c\x00\x1ak\x05\x0ei.\x10\\h\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xffi\x9e\xfc\x89\x87\xff\xaev\xff\xcem\xf1\xe0p\xb2\xde|p\xc8\x91>\xa6\xa7%\x81\xba(c\xc4FT\xc1}V\xb3\xc0<<<\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xbe\xd6\xfd\xcc\xcc\xff\xdd\xc4\xff\xea\xc0\xf9\xf2\xc1\xdf\xf1\xc7\xc2\xe8\xd0\xaa\xd9\xda\x9d\xc9\xe2\x9e\xbc\xe6\xae\xb4\xe5\xc7\xb5\xdf\xe4\xa9\xa9\xa9\x00\x00\x00\x00\x00\x00'

class VMStop(Exception):
    pass

def create_default_mapper_funcs(ram, vram, pals, prg_rom_banks, prg_ram, chr_rom_banks, chr_ram, cpu_transfer_page_to_oam, ppu_read_reg, ppu_write_reg, apu_read_reg, apu_write_reg, nt_mirroring=NT_MIRRORING_VERTICAL):
    """
    The mapper is represented as a tuple of functions:
    (
        cpu_read,
        cpu_write,
        ppu_read,
        ppu_write
    )
    """
    prg_rom_bank_0 = prg_rom_banks[0]
    prg_rom_bank_1 = prg_rom_banks[1%len(prg_rom_banks)]
    chr_rom_bank   = chr_rom_banks and chr_rom_banks[0]

    def create_cpu_accessors():
        # Address range	Size	Device
        # $0000–$07FF	$0800	2 KB internal RAM
        # $0800–$0FFF	$0800	Mirrors of $0000–$07FF
        # $1000–$17FF	$0800
        # $1800–$1FFF	$0800
        # $2000–$2007	$0008	NES PPU regs
        # $2008–$3FFF	$1FF8	Mirrors of $2000–$2007 (repeats every 8 bytes)
        # $4000–$4017	$0018	NES APU and I/O registers
        # $4018–$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
        # $4020–$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (see note)

        def write_ram_0(addr, value):
            ram[addr] = value
        def write_ram_1(addr, value):
            ram[addr-0x0800] = value
        def write_ram_2(addr, value):
            ram[addr-0x1000] = value
        def write_ram_3(addr, value):
            ram[addr-0x1800] = value
        def write_prg_ram(addr, value):
            prg_ram[addr-0x6000] = value
        def write_prg_rom_bank_0(addr, value):
            pass # prg_rom_bank_0[addr-0x8000] = value
        def write_prg_rom_bank_1(addr, value):
            pass # prg_rom_bank_1[addr-0xC000] = value
        def write_oamdma(_, page_num):
            cpu_transfer_page_to_oam(page_num)

        return (
            # cpu_readers
            (
                # ram
                [lambda addr: ram[addr]] * 0x0800 +
                [lambda addr: ram[addr-0x0800]] * 0x0800 + # mirror
                [lambda addr: ram[addr-0x1000]] * 0x0800 + # mirror
                [lambda addr: ram[addr-0x1800]] * 0x0800 + # mirror
                # ppu
                [lambda addr: ppu_read_reg((addr-0x2000))] * 8 +
                [lambda addr: ppu_read_reg((addr-0x2000)&7)] * (0x2000-8) + # mirrors
                # apu
                [lambda addr: apu_read_reg((addr-0x4000))] * 0x0020 +
                # cart
                [lambda addr: 0] * 0x1FE0 + # expansion rom?
                [lambda addr: prg_ram[addr-0x6000]] * 0x2000 + # prg ram
                [lambda addr: prg_rom_bank_0[addr-0x8000]] * 0x4000 + # prg rom bank 0
                [lambda addr: prg_rom_bank_1[addr-0xC000]] * 0x4000   # prg rom bank 1
            ),
            # cpu_writers
            (
                # ram
                [write_ram_0] * 0x0800 +
                [write_ram_1] * 0x0800 +
                [write_ram_2] * 0x0800 +
                [write_ram_3] * 0x0800 +
                # ppu / 0x2000
                [lambda addr, value: ppu_write_reg((addr-0x2000)&7, value)] * 0x2000 +
                # apu / 0x4000
                [lambda addr, value: apu_write_reg((addr-0x4000), value)] * 0x0014 +
                # dma / 0x4014
                [write_oamdma] +
                # apu / 0x4015
                [lambda addr, value: apu_write_reg((addr-0x4000), value)] * 0x000B +
                # cart
                [lambda addr, value: None] * 0x1FE0 + # expansion rom?
                [write_prg_ram] * 0x2000 + # prg ram
                [write_prg_rom_bank_0] * 0x4000 + # prg rom bank 0
                [write_prg_rom_bank_1] * 0x4000   # prg rom bank 1
            )
        )

    def create_ppu_accessors():
        # Address range	Size	Description
        # $0000-$0FFF	$1000	Pattern table 0
        # $1000-$1FFF	$1000	Pattern table 1
        # $2000-$23FF	$0400	Nametable 0
        # $2400-$27FF	$0400	Nametable 1
        # $2800-$2BFF	$0400	Nametable 2
        # $2C00-$2FFF	$0400	Nametable 3
        # $3000-$3EFF	$0F00	Mirrors of $2000-$2EFF
        # $3F00-$3F1F	$0020	Palette RAM indexes
        # $3F20-$3FFF	$00E0	Mirrors of $3F00-$3F1F

        def r_pattern_tables_from_chr_rom(addr):
            return chr_rom_bank[addr]
        def r_pattern_tables_from_chr_ram(addr):
            return chr_ram[addr]
        def r_palette_ram_indexes(addr):
            return pals[(addr-(0x3F00))&0x1F]
        def r_nametable_0_0(addr):
            return vram[(addr-(0x2000-0x0000))&0x0FFF]
        def r_nametable_0_1(addr):
            return vram[(addr-(0x2000-0x0400))&0x0FFF]
        def r_nametable_0_2(addr):
            return vram[(addr-(0x2000-0x0800))&0x0FFF]
        def r_nametable_0_3(addr):
            return vram[(addr-(0x2000-0x0C00))&0x0FFF]
        def r_nametable_1_0(addr):
            return vram[(addr-(0x2400-0x0000))&0x0FFF]
        def r_nametable_1_1(addr):
            return vram[(addr-(0x2400-0x0400))&0x0FFF]
        def r_nametable_1_2(addr):
            return vram[(addr-(0x2400-0x0800))&0x0FFF]
        def r_nametable_1_3(addr):
            return vram[(addr-(0x2400-0x0C00))&0x0FFF]
        def r_nametable_2_0(addr):
            return vram[(addr-(0x2800-0x0000))&0x0FFF]
        def r_nametable_2_1(addr):
            return vram[(addr-(0x2800-0x0400))&0x0FFF]
        def r_nametable_2_2(addr):
            return vram[(addr-(0x2800-0x0800))&0x0FFF]
        def r_nametable_2_3(addr):
            return vram[(addr-(0x2800-0x0C00))&0x0FFF]
        def r_nametable_3_0(addr):
            return vram[(addr-(0x2C00-0x0000))&0x0FFF]
        def r_nametable_3_1(addr):
            return vram[(addr-(0x2C00-0x0400))&0x0FFF]
        def r_nametable_3_2(addr):
            return vram[(addr-(0x2C00-0x0800))&0x0FFF]
        def r_nametable_3_3(addr):
            return vram[(addr-(0x2C00-0x0C00))&0x0FFF]
            
        def w_pattern_tables_from_chr_rom(addr, value):
            chr_rom_bank[addr] = value
        def w_pattern_tables_from_chr_ram(addr, value):
            chr_ram[addr] = value
        def w_palette_ram_indexes(addr, value):
            pals[(addr-(0x3F00))&0x1F] = value
        def w_nametable_0_0(addr, value):
            vram[(addr-(0x2000-0x0000))&0x0FFF] = value
        def w_nametable_0_1(addr, value):
            vram[(addr-(0x2000-0x0400))&0x0FFF] = value
        def w_nametable_0_2(addr, value):
            vram[(addr-(0x2000-0x0800))&0x0FFF] = value
        def w_nametable_0_3(addr, value):
            vram[(addr-(0x2000-0x0C00))&0x0FFF] = value
        def w_nametable_1_0(addr, value):
            vram[(addr-(0x2400-0x0000))&0x0FFF] = value
        def w_nametable_1_1(addr, value):
            vram[(addr-(0x2400-0x0400))&0x0FFF] = value
        def w_nametable_1_2(addr, value):
            vram[(addr-(0x2400-0x0800))&0x0FFF] = value
        def w_nametable_1_3(addr, value):
            vram[(addr-(0x2400-0x0C00))&0x0FFF] = value
        def w_nametable_2_0(addr, value):
            vram[(addr-(0x2800-0x0000))&0x0FFF] = value
        def w_nametable_2_1(addr, value):
            vram[(addr-(0x2800-0x0400))&0x0FFF] = value
        def w_nametable_2_2(addr, value):
            vram[(addr-(0x2800-0x0800))&0x0FFF] = value
        def w_nametable_2_3(addr, value):
            vram[(addr-(0x2800-0x0C00))&0x0FFF] = value
        def w_nametable_3_0(addr, value):
            vram[(addr-(0x2C00-0x0000))&0x0FFF] = value
        def w_nametable_3_1(addr, value):
            vram[(addr-(0x2C00-0x0400))&0x0FFF] = value
        def w_nametable_3_2(addr, value):
            vram[(addr-(0x2C00-0x0800))&0x0FFF] = value
        def w_nametable_3_3(addr, value):
            vram[(addr-(0x2C00-0x0C00))&0x0FFF] = value

        if nt_mirroring == NT_MIRRORING_VERTICAL:
            r_nametable_0 = r_nametable_0_0
            r_nametable_1 = r_nametable_1_1
            r_nametable_2 = r_nametable_2_0
            r_nametable_3 = r_nametable_3_1
            w_nametable_0 = w_nametable_0_0
            w_nametable_1 = w_nametable_1_1
            w_nametable_2 = w_nametable_2_0
            w_nametable_3 = w_nametable_3_1
        elif nt_mirroring == NT_MIRRORING_HORIZONTAL:
            r_nametable_0 = r_nametable_0_0
            r_nametable_1 = r_nametable_1_0
            r_nametable_2 = r_nametable_2_2
            r_nametable_3 = r_nametable_3_2
            w_nametable_0 = w_nametable_0_0
            w_nametable_1 = w_nametable_1_0
            w_nametable_2 = w_nametable_2_2
            w_nametable_3 = w_nametable_3_2
        elif nt_mirroring == NT_MIRRORING_SINGLE_SCREEN:
            r_nametable_0 = r_nametable_0_0
            r_nametable_1 = r_nametable_1_0
            r_nametable_2 = r_nametable_2_0
            r_nametable_3 = r_nametable_3_0
            w_nametable_0 = w_nametable_0_0
            w_nametable_1 = w_nametable_1_0
            w_nametable_2 = w_nametable_2_0
            w_nametable_3 = w_nametable_3_0
        elif nt_mirroring == NT_MIRRORING_FOUR_SCREEN:
            r_nametable_0 = r_nametable_0_0
            r_nametable_1 = r_nametable_1_1
            r_nametable_2 = r_nametable_2_2
            r_nametable_3 = r_nametable_3_3
            w_nametable_0 = w_nametable_0_0
            w_nametable_1 = w_nametable_1_1
            w_nametable_2 = w_nametable_2_2
            w_nametable_3 = w_nametable_3_3
        else:
            raise ValueError(f'Nametable mirroring {nt_mirroring} not supported')

        if chr_ram and chr_rom_bank:
            raise ValueError('CHR-ROM/RAM configuration not supported by default mapper (both are present)')

        if chr_ram:
            r_pattern_table_0 = r_pattern_tables_from_chr_ram
            r_pattern_table_1 = r_pattern_tables_from_chr_ram
            w_pattern_table_0 = w_pattern_tables_from_chr_ram
            w_pattern_table_1 = w_pattern_tables_from_chr_ram
        else:
            r_pattern_table_0 = r_pattern_tables_from_chr_rom
            r_pattern_table_1 = r_pattern_tables_from_chr_rom
            w_pattern_table_0 = w_pattern_tables_from_chr_rom
            w_pattern_table_1 = w_pattern_tables_from_chr_rom

        return (
            (
                [r_pattern_table_0]      * 0x1000 +
                [r_pattern_table_1]      * 0x1000 +
                [r_nametable_0]          * 0x0400 +
                [r_nametable_1]          * 0x0400 +
                [r_nametable_2]          * 0x0400 +
                [r_nametable_3]          * 0x0400 +
                [r_nametable_0]          * 0x0400 +
                [r_nametable_1]          * 0x0400 +
                [r_nametable_2]          * 0x0400 +
                [r_nametable_3]          * 0x0300 +
                [r_palette_ram_indexes]  * 0x100
            ),
            (
                [w_pattern_table_0]      * 0x1000 +
                [w_pattern_table_1]      * 0x1000 +
                [w_nametable_0]          * 0x0400 +
                [w_nametable_1]          * 0x0400 +
                [w_nametable_2]          * 0x0400 +
                [w_nametable_3]          * 0x0400 +
                [w_nametable_0]          * 0x0400 +
                [w_nametable_1]          * 0x0400 +
                [w_nametable_2]          * 0x0400 +
                [w_nametable_3]          * 0x0300 +
                [w_palette_ram_indexes]  * 0x100
            )
        )

    cpu_readers, cpu_writers = create_cpu_accessors()
    cpu_read = lambda addr: cpu_readers[addr](addr)
    def cpu_write(addr, value):
        cpu_writers[addr](addr, value)

    ppu_readers, ppu_writers = create_ppu_accessors()
    ppu_read = lambda addr: ppu_readers[addr](addr)
    def ppu_write(addr, value):
        ppu_writers[addr](addr, value)

    return (
        cpu_read,
        cpu_write,
        ppu_read,
        ppu_write
    )

mappers = [create_default_mapper_funcs]

def signed8(value):
    return ((value&0xFF)^0x80) - 0x80

def load_pal_from_file(pal_filepath='ntscpalette.pal'):
    # thanks to https://bisqwit.iki.fi/utils/nespalette.php
    with open(pal_filepath, 'rb') as pal_file:
        pal_bytes = pal_file.read()
        return list(zip(pal_bytes[0::3], pal_bytes[1::3], pal_bytes[2::3]))

def create_ppu_funcs(
    screen,
    reg_io_value=0x00,
    reg_io_write_state=0,
    ppu_status=0x00,
    ppu_ctrl=0x00,
    ppu_mask=0x00,
    ppu_addr=0x0000,
    tmp_addr=0x0000,
    oam_addr=0x00,
    ppu_data=0x00, # buffer for last ppudata read
    fine_x_scroll=0x0,
    t=0
):
    """
    The PPU is represented as a tuple of functions:
    (
        read_reg,
        write_reg,
        write_oam,
        pals,
        connect,
        inspect_regs
    )
    """
    fetch        = None
    store        = None
    trigger_nmi  = None
    oam          = array.array('B', (0 for _ in range(0x100)))
    pals         = array.array('B', (0 for _ in range(0x20)))
    pt_addr      = 0
    frame_num    = 0
    scanline_num = 0
    scanline_t   = 0
    scanline_oam_addr = 0

    # rendering registers/latches/memory
    # background
    tile_0       = 0x0000 # AABB
    tile_1       = 0x0000 # AABB
    next_0       = 0x0000 # CC (next tile lo-bits to be loaded into BB)
    next_1       = 0x0000 # CC (next tile hi-bits to be loaded into BB)
    attr         = 0x0    # 2 bit attr (not bothering to implement this literally)
    attr_n       = 0x0    # next 2 bit attr

    # sprites
    scanline_oam = array.array('B', (0 for _ in range(0x20)))
    sp_tiles_0   = array.array('B', (0 for _ in range(8))) # sprite pattern/tile slice (lo)
    sp_tiles_1   = array.array('B', (0 for _ in range(8))) # sprite pattern/tile slice (hi)
    sp_attrs     = array.array('B', (0 for _ in range(8))) # sprite attribute bytes
    sp_x_pos     = array.array('B', (0 for _ in range(8))) # sprite x positions
    sp_pt_addr   = 0x00  # made up temporary storage for loading sprite pattern into regs

    def _flip_bits(b):
        # Thanks https://graphics.stanford.edu/~seander/bithacks.html
        return ((((b * 0x0802 & 0x22110) | (b * 0x8020 & 0x88440)) * 0x10101) >> 16) & 0xFF
    _flipped_bits_lookup = [_flip_bits(b) for b in range(0x100)]
    def flip_bits(b):
        return _flipped_bits_lookup[b]

    def idle():
        pass

    def fetch_bg_0_addr_only():
        pass  # (((ppu_ctrl&0x10)<<8)|pt_addr|0|(ppu_addr>>12))  # no reason to actually do this

    def fetch_nt():
        nonlocal pt_addr
        # _yyy NNYY YYYX XXXX => _000 NNYY YYYX XXXX => ____ RRRR CCCC ____ (__0H RRRR CCCC 0TTT)
        pt_addr = fetch(0x2000|(ppu_addr&0x0FFF)) << 4

    def fetch_at():
        nonlocal attr_n
        # _yyy NNYY YYYX XXXX => _000 NN00 00YY YXXX
        attr_quad = fetch(0x23C0|(ppu_addr&0x0C00)|(((ppu_addr&0x0380)>>4)|((ppu_addr&0x001C)>>2)))
        # _yyy NNYY YYYX XXXX => _000 0000 0Y00 00X0 => _000 0000 0000 00YX
        attr_n = ((attr_quad>>(((ppu_addr&0x0040)>>4)|(ppu_addr&0x0002)))&3) << 2

    def fetch_bg_0():
        nonlocal next_0
        # __0H RRRR CCCC 0TTT
        next_0 = fetch(((ppu_ctrl&0x10)<<8)|pt_addr|0|(ppu_addr>>12))

    def fetch_bg_1():
        nonlocal next_1
        # __0H RRRR CCCC 1TTT
        next_1 = fetch(((ppu_ctrl&0x10)<<8)|pt_addr|8|(ppu_addr>>12))

    def inc_x():
        nonlocal ppu_addr, tile_0, tile_1, attr
        # _yyy NNYY YYYX XXXX => _yyy NnYY YYYx xxxx (x=X+1, n=carry)
        x = (ppu_addr&0x001F) + 1
        ppu_addr ^= (ppu_addr^((x<<5)|x)) & 0x041F
        # load shifters with next tile
        tile_0 |= next_0
        tile_1 |= next_1
        attr = attr_n

    def inc_xy():
        nonlocal ppu_addr, tile_0, tile_1, attr
        # _yyy NNYY YYYX XXXX => _yyy nnyy yyyx xxxx
        fine_y_x = (ppu_addr&0x701F) + 0x1001   # add 1 to fine y and corase x, at the same time
        y = (ppu_addr&0x03E0) + (fine_y_x>>10)  # add fine y to coarse y (w/ garbage coarse x bits)
        ppu_addr  = ((y&0x0400)<<1) | (y&0x03E0) | (((fine_y_x<<5)|fine_y_x)&0x741F)
        # load shifters with next tile
        tile_0 |= next_0
        tile_1 |= next_1
        attr = attr_n

    def reset_x():
        nonlocal ppu_addr, tile_0, tile_1, attr
        ppu_addr ^= (ppu_addr^tmp_addr) & 0x001F
        # load shifters with next tile
        tile_0 |= next_0
        tile_1 |= next_1
        attr = attr_n

    def reset_y():
        nonlocal ppu_addr
        ppu_addr ^= (ppu_addr^tmp_addr) & 0x03E0
        # this probably loads shifters? shouldn't need to emulate it, though

    def set_vblank_flag():
        nonlocal ppu_status
        ppu_status |= 0x80
        if ppu_ctrl & 0x80:
            trigger_nmi()

    def clear_flags():
        nonlocal ppu_status
        ppu_status &= 0x1F

    def fetch_sp_0():
        # __0H RRRR CCCC 0TTT
        sp_tiles_0[scanline_oam_addr>>2] = flip_bits(fetch(sp_pt_addr|0)) if sp_attrs[scanline_oam_addr>>2]&0x40 else fetch(sp_pt_addr|0)

    def fetch_sp_1():
        # __0H RRRR CCCC 1TTT
        sp_tiles_1[scanline_oam_addr>>2] = flip_bits(fetch(sp_pt_addr|8)) if sp_attrs[scanline_oam_addr>>2]&0x40 else fetch(sp_pt_addr|8)

    fetch_next_tile_funcs = [
        idle,             fetch_nt, # this one's different to allow easy overriding on pre-render line
        fetch_at,         idle,
        fetch_bg_0,       idle,
        fetch_bg_1,       inc_x
    ]
    
    fetch_next_sprites_funcs = [
        idle,             fetch_nt, # the two nt fetches done here (under sprite) don't serve any purpose
        fetch_nt,         idle,     #
        fetch_sp_0,       idle,
        fetch_sp_1,       idle,     # timing of fetch_sp_1 aligns with incrementing scanline_oam_addr below
    ]

    visible_scanline_funcs = (
        # Cycle 0: First cycle is always an idle
        [idle] +
        # Cycles 1 - 256: Fetch data for 32 tiles (the last 2 don't matter)
        fetch_next_tile_funcs * 32 +
        # Cycles 257 - 320: Fetch data for 8 sprites on the next scanline
        fetch_next_sprites_funcs * 8 +
        # Cycles 321 - 336: Fetch first two tiles for the next scanline
        fetch_next_tile_funcs * 2 +
        # Cycles 337 - 340: Fetch nametable byte twice for unknown reason
        [idle, fetch_nt] * 2
    )
    visible_scanline_funcs[257] = reset_x

    pre_render_scanline_funcs = list(visible_scanline_funcs)
    pre_render_scanline_funcs[1] = clear_flags
    pre_render_scanline_funcs[304] = reset_y # this is supposed to be 280 - 304, but doesn't seem like leading calls matter

    vblank_scanline_funcs = [idle] * 341

    first_vblank_scanline_funcs = list(vblank_scanline_funcs)
    first_vblank_scanline_funcs[1] = set_vblank_flag

    post_render_scanline_funcs = vblank_scanline_funcs
    
    bg_tick_funcs = [
        [visible_scanline_funcs] * 240 +
        [post_render_scanline_funcs] +
        [first_vblank_scanline_funcs] +
        [vblank_scanline_funcs] * (261-242) +
        [pre_render_scanline_funcs]
    ] * 2

    oam_bytes_to_copy = 0
    oam_byte = 0
    disable_writes = False
    check_overflow = False

    def fetch_ff_from_oam():
        nonlocal oam_byte # TODO: make this happen through read_oam_data() / reg_io_value
        oam_byte = 0xFF

    def store_ff_in_scanline_oam():
        nonlocal scanline_oam_addr
        scanline_oam[scanline_oam_addr] = oam_byte
        # NOTE: the next set of sprite operations counts on this wrapping perfectly back to 0x00
        scanline_oam_addr = (scanline_oam_addr+1) & 0x1F

    def fetch_oam():
        nonlocal oam_byte # TODO: make this happen through read_oam_data() / reg_io_value
        oam_byte = oam[oam_addr]

    def store_scanline_oam():
        nonlocal ppu_status, oam_addr, scanline_oam_addr, disable_writes, check_overflow, oam_bytes_to_copy

        # This is probably pretty far from how the actual PPU operates,
        # but it's hard to guess going only by what info there is in the wiki.
        # An important effect of this implementation is that upon completion
        # scanline_oam_addr leaves being assigned 0x00, so it's ready for the next operation!

        # Perform read instead of write if writing is disabled
        if disable_writes:
            scanline_oam[scanline_oam_addr]
        else:
            scanline_oam[scanline_oam_addr] = oam_byte  # oam_byte set by fetch_oam() in previous cycle

        # Switch on "mode" (this can probably be refactored once it's proved to do what it's suppoed to!)
        if oam_bytes_to_copy: # copy bytes
            # Copy byte (1-3) of sprite that's already been identified as "on this scanline"
            scanline_oam_addr += 1
            oam_addr += 1
            oam_bytes_to_copy -= 1
        elif disable_writes and check_overflow:
            # All 8 scanline sprites found, check overflow
            if oam_byte <= scanline_num < (oam_byte+8+((ppu_ctrl&0x20)>>2)):
                ppu_status |= 0x20 # overflow found, set status flag!
                check_overflow = False
                oam_addr = (oam_addr + 4) & 0x03 # increment oam_addr and re-align, correcting "diagonal" bug
            else:
                oam_addr = (oam_addr + 5) # overflow incrementing "diagonal" bug (when checking overflow, but none found yet)
        elif disable_writes:
            # All 64 sprites checked OR 8 found and overflow found
            oam_addr += 4 # nothing to do / skip sprite
        else:
            # Normal operation, check for sprites on scanline
            if oam_byte <= scanline_num < (oam_byte+8+((ppu_ctrl&0x20)>>2)): # sprite is on this scanline?
                scanline_oam_addr += 1 # start copying
                oam_addr += 1
                oam_bytes_to_copy = 3
            else:
                oam_addr += 4 # skip sprite

        if scanline_oam_addr > 0x1F:
            # All 8 scanline sprites have been found
            check_overflow = True
            disable_writes = True
            scanline_oam_addr = 0x00

        if oam_addr > 0xFF:
            # All 64 sprites have been checked
            disable_writes = True
            scanline_oam_addr = 0x00
            oam_addr &= 0xFF

    def load_sp_y():
        nonlocal sp_pt_addr
        # This is the same calc for 8x8 and 8x16 sprites,
        # since second pattern immediately follows first when 8x16.
        # For 8x8 the value range will be 0 - 7 and 8x16 it'll be 0 - 15.
        fine_y = scanline_num - scanline_oam[scanline_oam_addr+0]
        if sp_attrs[scanline_oam_addr>>2]&0x80: # flip y
            fine_y = (8<<((ppu_ctrl&0x20)>>5)) - fine_y
        # Put upper bit of fine y (that's currently sitting in "plane" bit of a pattern address)
        # into lowest "column" bit to indicate the NEXT tile
        sp_pt_addr = ((fine_y<<1)&0x10) | (fine_y&0x7)

    def load_sp_tile_num():
        nonlocal sp_pt_addr
        tile_num = scanline_oam[scanline_oam_addr+1]
        if ppu_ctrl&0x20:
            # For 8x16 sprites: the first bit indicates bank (=> __0C RRRR CCCT PTTT)
            sp_pt_addr |= ((tile_num<<12)|(tile_num<<4)) & 0x1FE0
        else:
            # For 8x 8 sprites: use bank specified in ppu_ctrl like nametables does (=> __0H RRRR CCCC PTTT)
            sp_pt_addr |= ((ppu_ctrl&0x08)<<9) | (tile_num<<4)

    def load_sp_attr():
        sp_attrs[scanline_oam_addr>>2] = scanline_oam[scanline_oam_addr+2]

    def load_sp_x():
        sp_x_pos[scanline_oam_addr>>2] = scanline_oam[scanline_oam_addr+3]

    def fetch_sp_x():
        nonlocal scanline_oam_addr
        scanline_oam[scanline_oam_addr|0x03]
        # IMPORTANT: incrementing in this function wouldn't work if sprite tick happened
        # before background tick, as background tick is copying byte 4 of sprite pattern data.
        # It's also important that it wraps at the end of sprites
        scanline_oam_addr = (scanline_oam_addr+1) & 0x1F

    def fetch_sp_y():
        scanline_oam[scanline_oam_addr]
    
    # Evaluate sprites on visible scanlines
    sp_tick_funcs_for_visible_scanline = (
        # Cycle 0 : Idle
        [idle] +
        # Cycles 1 - 64: Clear 8 scanline sprites (4 bytes each), alternating read/write
        [fetch_ff_from_oam, store_ff_in_scanline_oam] * (8*4) +
        # Cycles 65 - 256: Copy active sprites to scanline sprite list, alternating read/write
        [fetch_oam, store_scanline_oam] * (192//2) +
        # Cycles 257 - 320: Store sprite data in registers for rendering, for all 8 sprites
        [load_sp_y, load_sp_tile_num, load_sp_attr, load_sp_x, fetch_sp_x, fetch_sp_x, fetch_sp_x, fetch_sp_x] * 8 +
        # Cycles 321 - 340: Fetch zero sprite y repeatedly, while first two background tiles are loaded
        # NOTE: this isn't explicitly fetching zero sprite, but addr should wrap around so it is always 0
        [fetch_sp_y] * 20
    )
    
    # For scanlines that aren't visible, no sprite work is done
    idle_scanline = [idle] * 341

    sp_tick_funcs = [
        [sp_tick_funcs_for_visible_scanline] * 240 +
        [idle_scanline] * (262-240)
    ] * 2

    def next_pixel():
        nonlocal tile_0, tile_1
        
        # Get background pixel
        pixel = (((tile_0<<fine_x_scroll>>1)&0x4000)|((tile_1<<fine_x_scroll)&0x8000)) >> 14
        if pixel != 0:
            pixel |= attr

        # Get first active sprite pixel, if it's non-zero, and either:
        # background pixel is zero or priority is not set to background
        sp_idx = 0
        while sp_idx < 8:
            if sp_x_pos[sp_idx]:
                continue # skip sprites that are ahead of current position on scanline
            sp_pixel = ((sp_tiles_0[sp_idx]&0x40)|(sp_tiles_1[sp_idx]&0x80)) >> 6
            if sp_pixel != 0 and ((pixel&0x03) == 0 or not (sp_attrs[sp_idx]&0x20)):
                pixel = ((sp_attrs[sp_idx]&0x03)<<2) | sp_pixel
                break
            sp_idx += 1

        # Shift background registers
        tile_0 <<= 1 # this leaves garbage in upper bits but who cares
        tile_1 <<= 1 # 

        # Decrement sprite x counters, shift sprite registers
        # NOTE: wiki says that x position is decremented before rendering,
        # but then sprites would be offset by 1 pixel and it just seems wrong?
        # Is this a broader misinterpretation by me (ex. like "first active sprite pixel"
        # not exactly what's happening -- maybe the loop decrements back from 8th sprite to 0th?)?
        sp_idx = 0
        while sp_idx < 8:
            if sp_x_pos[sp_idx]:
                sp_x_pos[sp_idx] -= 1
            else:
                sp_tiles_0[sp_idx] <<= 1
                sp_tiles_1[sp_idx] <<= 1
            sp_idx += 1

        return pixel

    def render_pixel():
        nonlocal scanline_t
        screen[(scanline_num<<8)+scanline_t] = pals[next_pixel()]
        scanline_t  += 1

    def inc_scanline_t():
        nonlocal scanline_t
        scanline_t  += 1

    def inc_scanline():
        nonlocal frame_num, scanline_num, scanline_t
        scanline_num += 1
        frame_num    += scanline_num / 262
        scanline_num %= 262
        scanline_t    = 0

    render_scanline_funcs = (           # first 240 scanlines are rendered like this; even/odd are the same
        [render_pixel]   * (256)     +  # render 256 pixels
        [inc_scanline_t] * (340-256) +  # do nothing up until the last cycle of the scanline
        [inc_scanline]                  # on the last cycle, increment scanline
    )

    do_not_render_scanline_funcs = [inc_scanline_t] * 340 + [inc_scanline] # 341 total cycles
    
    render_funcs = [
        [render_scanline_funcs] * 240 +
        [do_not_render_scanline_funcs] * (262-240)
    ] * 2

    # REMOVE ME: once this is validated!
    for frame_funcs_to_check_i, frame_funcs_to_check in enumerate((sp_tick_funcs, bg_tick_funcs, render_funcs)):
        assert len(frame_funcs_to_check) == 2
        for frame_num_to_check, scanline_funcs_to_check in enumerate(frame_funcs_to_check):
            assert len(scanline_funcs_to_check) == 262
            for cycle_funcs_to_check in scanline_funcs_to_check:
                assert len(cycle_funcs_to_check) == (341 if frame_num_to_check == 0 else 341)  # both the same for now

    def tick():
        bg_tick_funcs[frame_num&1][scanline_num][scanline_t]()
        sp_tick_funcs[frame_num&1][scanline_num][scanline_t]()
        render_funcs [frame_num&1][scanline_num][scanline_t]()

    def read_nothing():
        pass
    def read_ppu_status():
        nonlocal reg_io_value, ppu_status
        # VSO.....
        reg_io_value ^= (reg_io_value^ppu_status) & 0xE0
        ppu_status &= 0x7F  # clear vblank flag
    def read_oam_data():
        nonlocal reg_io_value
        reg_io_value = oam[oam_addr] # TODO: return 0xFF if in cycles 1 - 64 of scanline
        # NOTE: it seems that oam_addr doesn't get incremented
    def read_ppu_data():
        nonlocal reg_io_value, ppu_data, ppu_addr
        if ppu_addr >= 0x3F00:  # TODO: factor out this IF ... ELSE :(
            reg_io_value = fetch(ppu_addr)
            ppu_data = fetch(ppu_addr-0x1000) # TODO: move to mapper?
        else:
            reg_io_value = ppu_data
            ppu_data = fetch(ppu_addr)
        ppu_addr = (ppu_addr + PPU_ADDR_INCREMENTS[ppu_ctrl&0x04]) & 0x3FFF

    def write_nothing():
        pass
    def write_ppu_ctrl():
        nonlocal ppu_ctrl, tmp_addr
        if t < 30000:
            return  # TODO: figure out reg_io_value / reg_io_write_state
        if ppu_status & reg_io_value & ~ppu_ctrl & 0x80:
            trigger_nmi()
        ppu_ctrl = reg_io_value
        tmp_addr ^= (tmp_addr ^ (reg_io_value<<10)) & 0x0C00
    def write_ppu_mask():
        nonlocal ppu_mask
        ppu_mask = reg_io_value
    def write_oam_addr():
        nonlocal oam_addr
        oam_addr = reg_io_value
    def write_oam_data():
        nonlocal oam_addr
        # TODO: ignore write during rendering (+pre-rendering) but increment addr by 4
        # TODO: if OAMADDR is not less than eight when rendering starts, the eight bytes starting at OAMADDR & 0xF8 are copied to the first eight bytes of OAM
        oam[oam_addr] = reg_io_value
        oam_addr = (oam_addr+1) & 0xFF
    def write_ppu_scroll_0():
        nonlocal fine_x_scroll, tmp_addr
        # ........ ...XXXXX (course X)
        fine_x_scroll = reg_io_value & 0x07
        tmp_addr ^= (tmp_addr^(reg_io_value>>3)) & 0x001F
    def write_ppu_scroll_1():
        nonlocal tmp_addr
        # .yyy..YY YYY..... (fine y, course Y)
        tmp_addr ^= (tmp_addr^((reg_io_value<<12)|(reg_io_value<<2))) & 0x73E0
    def write_ppu_addr_0():
        nonlocal tmp_addr
        # .0AAAAAA ........ (address high 6 bits + clear 14th bit)
        tmp_addr ^= (tmp_addr^((reg_io_value&0x3F)<<8)) & 0x7F00
    def write_ppu_addr_1():
        nonlocal tmp_addr, ppu_addr
        # ........ AAAAAAAA (address low 8 bits)
        tmp_addr ^= (tmp_addr^reg_io_value) & 0x00FF
        ppu_addr = tmp_addr
    def write_ppu_data():
        nonlocal ppu_addr
        store(ppu_addr, reg_io_value)
        ppu_addr = (ppu_addr + PPU_ADDR_INCREMENTS[ppu_ctrl&0x04]) & 0x3FFF

    reg_readers = [
        read_nothing,
        read_nothing,
        read_ppu_status,
        read_nothing,
        read_oam_data,
        read_nothing,
        read_nothing,
        read_ppu_data
    ]

    reg_writers = [
        # first write
        write_ppu_ctrl,
        write_ppu_mask,
        write_nothing,
        write_oam_addr,
        write_oam_data,
        write_ppu_scroll_0,
        write_ppu_addr_0,
        write_ppu_data,
        # second write
        write_ppu_ctrl,
        write_ppu_mask,
        write_nothing,
        write_oam_addr,
        write_oam_data,
        write_ppu_scroll_1,
        write_ppu_addr_1,
        write_ppu_data
    ]

    def read_reg(reg_idx):
        nonlocal reg_io_write_state
        reg_readers[reg_idx]()
        reg_io_write_state = 0
        return reg_io_value

    def write_reg(reg_idx, value):
        nonlocal reg_io_value, reg_io_write_state
        reg_io_value = value
        reg_writers[reg_idx+reg_io_write_state]()
        reg_io_write_state ^= 8

    def write_oam(value):
        nonlocal reg_io_value
        reg_io_value = value # NOTE: I think this is how the value gets to write_oam_data?
        write_oam_data()

    def connect(ppu_read, ppu_write, cpu_trigger_nmi):
        nonlocal fetch, store, trigger_nmi
        fetch = ppu_read
        store = ppu_write
        trigger_nmi = cpu_trigger_nmi

    return (
        tick,
        read_reg,
        write_reg,
        write_oam,
        lambda: pals,  # return internal pals storage,
        connect,
        lambda: dict(  # return internal regs / mem for testing purposes
            oam=oam,
            reg_io_value=reg_io_value,
            reg_io_write_state=reg_io_write_state,
            ppu_status=ppu_status,
            ppu_ctrl=ppu_ctrl,
            ppu_mask=ppu_mask,
            ppu_addr=ppu_addr,
            tmp_addr=tmp_addr,
            oam_addr=oam_addr,
            ppu_data=ppu_data,
            fine_x_scroll=fine_x_scroll,
            frame_num=frame_num,
            scanline_num=scanline_num,
            scanline_t=scanline_t,
            t=t
        )
    )

def create_cpu_funcs(regs=None, t=0, stop_on_brk=False):
    """
    The CPU is represented as a tuple of functions:
    (
        tick,
        trigger_nmi,
        trigger_reset,
        trigger_irq,
        transfer_page_to_oam,
        connect,
        inspect_regs
    )
    """

    fetch = store = write_oam = None
    
    pc, s, a, x, y, p = regs or (0x0000, 0xFF, 0x00, 0x00, 0x00, 0x34)
    
    # NOTE: consider all flag letter names reserved, even if not currently used: c, z, i, d, b, v, n
    
    # Resolve address per mode

    def resolve_immediate(pc):
        return pc + 1
    def resolve_zero_page(pc):
        return fetch(pc+1)
    def resolve_zero_page_indexed_x(pc):
        return (fetch(pc+1)+x) & 0xFF
    def resolve_zero_page_indexed_y(pc):
        return (fetch(pc+1)+y) & 0xFF
    def resolve_absolute(pc):
        return (fetch(pc+1)|(fetch(pc+2)<<8))
    def resolve_absolute_indexed_x(pc):
        nonlocal t
        addr = fetch(pc+1) + x
        t += addr>>8
        return (addr+(fetch(pc+2)<<8)) & 0xFFFF
    def resolve_absolute_indexed_y(pc):
        nonlocal t
        addr = fetch(pc+1) + y
        t += addr>>8
        return (addr+(fetch(pc+2)<<8)) & 0xFFFF
    def resolve_absolute_indexed_x_no_extra_cycle(pc):
        return ((fetch(pc+1)|(fetch(pc+2)<<8))+x) & 0xFFFF
    def resolve_absolute_indexed_y_no_extra_cycle(pc):
        return ((fetch(pc+1)|(fetch(pc+2)<<8))+y) & 0xFFFF
    def resolve_indexed_indirect(pc):
        i = fetch(pc+1) + x
        return fetch(i&0xFF)|(fetch((i+1)&0xFF)<<8)
    def resolve_indirect_indexed(pc):
        nonlocal t
        i = fetch(pc+1)
        addr = fetch(i) + y
        t += addr>>8
        return (addr+(fetch((i+1)&0xFF)<<8)) & 0xFFFF
    def resolve_indirect_indexed_no_extra_cycle(pc):
        i = fetch(pc+1)
        return ((fetch(i)|(fetch((i+1)&0xFF)<<8))+y) & 0xFFFF
    def resolve_indirect(pc):
        addr = fetch(pc+1) | (fetch(pc+2)<<8)
        return fetch(addr) | (fetch((addr&0xFF00)|((addr+1)&0xFF))<<8)  # NOTE: byte two cannot cross page
    def resolve_relative(pc):
        nonlocal t
        rel_addr = signed8(fetch(pc+1))
        pc += 2
        t += (((pc&0xFF)+rel_addr)>>8) & 1
        return pc + rel_addr

    # Instructions

    # ADC (ADd with Carry)
    # Writes flags: N V Z C
    def adc_69_immediate(pc):
        nonlocal t, a, p
        m = fetch(resolve_immediate(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def adc_65_zero_page(pc):
        nonlocal t, a, p
        m = fetch(resolve_zero_page(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def adc_75_zero_page_indexed_x(pc):
        nonlocal t, a, p
        m = fetch(resolve_zero_page_indexed_x(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def adc_6d_absolute(pc):
        nonlocal t, a, p
        m = fetch(resolve_absolute(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def adc_7d_absolute_indexed_x(pc):
        nonlocal t, a, p
        m = fetch(resolve_absolute_indexed_x(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def adc_79_absolute_indexed_y(pc):
        nonlocal t, a, p
        m = fetch(resolve_absolute_indexed_y(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def adc_61_indexed_indirect(pc):
        nonlocal t, a, p
        m = fetch(resolve_indexed_indirect(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def adc_71_indirect_indexed(pc):
        nonlocal t, a, p
        m = fetch(resolve_indirect_indexed(pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # AND (bitwise AND with accumulator)
    # Writes flags: N Z
    def and_29_immediate(pc):
        nonlocal t, a, p
        a &= fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def and_25_zero_page(pc):
        nonlocal t, a, p
        a &= fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def and_35_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a &= fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def and_2d_absolute(pc):
        nonlocal t, a, p
        a &= fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def and_3d_absolute_indexed_x(pc):
        nonlocal t, a, p
        a &= fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def and_39_absolute_indexed_y(pc):
        nonlocal t, a, p
        a &= fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def and_21_indexed_indirect(pc):
        nonlocal t, a, p
        a &= fetch(resolve_indexed_indirect(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def and_31_indirect_indexed(pc):
        nonlocal t, a, p
        a &= fetch(resolve_indirect_indexed(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # ASL (Arithmetic Shift Left)
    # Writes flags: N Z C
    def asl_0a_accumulator(pc):
        nonlocal t, a, p
        r = a << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def asl_06_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 5
        return pc + 2
    def asl_16_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 2
    def asl_0e_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 3
    def asl_1e_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        m = fetch(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # BIT (test BITs)
    # Writes flags: N V Z
    def bit_24_zero_page(pc):
        nonlocal t, p
        m = fetch(resolve_zero_page(pc))
        p = (p&MASK_NVZ) | (m & N) | (m & V) | (0x00 if (m&a) else Z)
        t += 3
        return pc + 2
    def bit_2c_absolute(pc):
        nonlocal t, p
        m = fetch(resolve_absolute(pc))
        p = (p&MASK_NVZ) | (m & N) | (m & V) | (0x00 if (m&a) else Z)
        t += 4
        return pc + 3
    
    # Branch Instructions
    # BPL (Branch on PLus)
    def bpl_10(pc):
        nonlocal t
        if p & N:
            t += 2
            return pc + 2
        else:
            t += 3
            return resolve_relative(pc)
    # BMI (Branch on MInus)
    def bmi_30(pc):
        nonlocal t
        if p & N:
            t += 3
            return resolve_relative(pc)
        else:
            t += 2
            return pc + 2
    # BVC (Branch on oVerflow Clear)
    def bvc_50(pc):
        nonlocal t
        if p & V:
            t += 2
            return pc + 2
        else:
            t += 3
            return resolve_relative(pc)
    # BVS (Branch on oVerflow Set)
    def bvs_70(pc):
        nonlocal t
        if p & V:
            t += 3
            return resolve_relative(pc)
        else:
            t += 2
            return pc + 2
    # BCC (Branch on Carry Clear)
    def bcc_90(pc):
        nonlocal t
        if p & C:
            t += 2
            return pc + 2
        else:
            t += 3
            return resolve_relative(pc)
    # BCS (Branch on Carry Set)
    def bcs_b0(pc):
        nonlocal t
        if p & C:
            t += 3
            return resolve_relative(pc)
        else:
            t += 2
            return pc + 2
    # BNE (Branch on Not Equal)
    def bne_d0(pc):
        nonlocal t
        if p & Z:
            t += 2
            return pc + 2
        else:
            t += 3
            return resolve_relative(pc)
    # BEQ (Branch on EQual)
    def beq_f0(pc):
        nonlocal t
        if p & Z:
            t += 3
            return resolve_relative(pc)
        else:
            t += 2
            return pc + 2
    
    # BRK (BReaK)
    # Writes flags: B
    def brk_00_implied(_):
        nonlocal p
        p |= B  # NOTE: B flag won't be reset after nested interrupt (via PHP) -- is this okay?
        trigger_irq()
        return pc
    def brk_00_implied_stop_execution(_):  # for unit testing / debugging
        raise VMStop()
    
    # CMP (CoMPare accumulator)
    # Writes flags: N Z C
    def cmp_c9_immediate(pc):
        nonlocal t, p
        r = a - fetch(resolve_immediate(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def cmp_c5_zero_page(pc):
        nonlocal t, p
        r = a - fetch(resolve_zero_page(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def cmp_d5_zero_page_indexed_x(pc):
        nonlocal t, p
        r = a - fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 2
    def cmp_cd_absolute(pc):
        nonlocal t, p
        r = a - fetch(resolve_absolute(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def cmp_dd_absolute_indexed_x(pc):
        nonlocal t, p
        r = a - fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def cmp_d9_absolute_indexed_y(pc):
        nonlocal t, p
        r = a - fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def cmp_c1_indexed_indirect(pc):
        nonlocal t, p
        r = a - fetch(resolve_indexed_indirect(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 6
        return pc + 2
    def cmp_d1_indirect_indexed(pc):
        nonlocal t, p
        r = a - fetch(resolve_indirect_indexed(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 5
        return pc + 2
    
    # CPX (ComPare X register)
    # Writes flags: N Z C
    def cpx_e0_immediate(pc):
        nonlocal t, p
        r = x - fetch(resolve_immediate(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def cpx_e4_zero_page(pc):
        nonlocal t, p
        r = x - fetch(resolve_zero_page(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def cpx_ec_absolute(pc):
        nonlocal t, p
        r = x - fetch(resolve_absolute(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    
    # CPY (ComPare Y register)
    # Writes flags: N Z C
    def cpy_c0_immediate(pc):
        nonlocal t, p
        r = y - fetch(resolve_immediate(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def cpy_c4_zero_page(pc):
        nonlocal t, p
        r = y - fetch(resolve_zero_page(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def cpy_cc_absolute(pc):
        nonlocal t, p
        r = y - fetch(resolve_absolute(pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    
    # DEC (DECrement memory)
    # Writes flags: N Z
    def dec_c6_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 5
        return pc + 2
    def dec_d6_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 6
        return pc + 2
    def dec_ce_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 6
        return pc + 3
    def dec_de_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        m = fetch(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # EOR (bitwise Exclusive OR)
    # Writes flags: N Z
    def eor_49_immediate(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def eor_45_zero_page(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def eor_55_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def eor_4d_absolute(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def eor_5d_absolute_indexed_x(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def eor_59_absolute_indexed_y(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def eor_41_indexed_indirect(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_indexed_indirect(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def eor_51_indirect_indexed(pc):
        nonlocal t, a, p
        a ^= fetch(resolve_indirect_indexed(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # Flag (Processor Status) Instructions
    # CLC (CLear Carry)
    def clc_18(pc):
        nonlocal t, p
        p &= MASK_C
        t += 2
        return pc + 1
    # SEC (SEt Carry)
    def sec_38(pc):
        nonlocal t, p
        p |= C
        t += 2
        return pc + 1
    # CLI (CLear Interrupt)
    def cli_58(pc):
        nonlocal t, p
        p &= MASK_I
        t += 2
        return pc + 1
    # SEI (SEt Interrupt)
    def sei_78(pc):
        nonlocal t, p
        p |= I
        t += 2
        return pc + 1
    # CLV (CLear oVerflow)
    def clv_b8(pc):
        nonlocal t, p
        p &= MASK_V
        t += 2
        return pc + 1
    # CLD (CLear Decimal)
    def cld_d8(pc):
        nonlocal t, p
        p &= MASK_D
        t += 2
        return pc + 1
    # SED (SEt Decimal)
    def sed_f8(pc):
        nonlocal t, p
        p |= D
        t += 2
        return pc + 1
    
    # INC (INCrement memory)
    # Writes flags: N Z
    def inc_e6_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 5
        return pc + 2
    def inc_f6_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 6
        return pc + 2
    def inc_ee_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 6
        return pc + 3
    def inc_fe_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        m = fetch(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        store(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # JMP (JuMP)
    # Writes flags: none
    def jmp_4c_absolute(pc):
        nonlocal t
        t += 3
        return resolve_absolute(pc)
    def jmp_6c_indirect(pc):
        nonlocal t
        t += 5
        return resolve_indirect(pc)
    
    # JSR (Jump to SubRoutine)
    # Writes flags: none
    def jsr_20_absolute(pc):
        nonlocal t, s
        to = resolve_absolute(pc)
        pc += 2 # 3 - 1 (offset by one before storing on stack)
        store(STACK_OFFSET+s, pc>>8)
        s = (s-1) & 0xFF
        store(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        t += 6
        return to
    
    # LDA (LoaD Accumulator)
    # Writes flags: N Z
    def lda_a9_immediate(pc):
        nonlocal t, a, p
        a = fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def lda_a5_zero_page(pc):
        nonlocal t, a, p
        a = fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def lda_b5_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a = fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def lda_ad_absolute(pc):
        nonlocal t, a, p
        a = fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def lda_bd_absolute_indexed_x(pc):
        nonlocal t, a, p
        a = fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def lda_b9_absolute_indexed_y(pc):
        nonlocal t, a, p
        a = fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def lda_a1_indexed_indirect(pc):
        nonlocal t, a, p
        a = fetch(resolve_indexed_indirect(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def lda_b1_indirect_indexed(pc):
        nonlocal t, a, p
        a = fetch(resolve_indirect_indexed(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # LDX (LoaD X register)
    # Writes flags: N Z
    def ldx_a2_immediate(pc):
        nonlocal t, x, p
        x = fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 2
        return pc + 2
    def ldx_a6_zero_page(pc):
        nonlocal t, x, p
        x = fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 3
        return pc + 2
    def ldx_b6_zero_page_indexed_y(pc):
        nonlocal t, x, p
        x = fetch(resolve_zero_page_indexed_y(pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 2
    def ldx_ae_absolute(pc):
        nonlocal t, x, p
        x = fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 3
    def ldx_be_absolute_indexed_y(pc):
        nonlocal t, x, p
        x = fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 3
    
    # LDY (LoaD Y register)
    # Writes flags: N Z
    def ldy_a0_immediate(pc):
        nonlocal t, y, p
        y = fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 2
        return pc + 2
    def ldy_a4_zero_page(pc):
        nonlocal t, y, p
        y = fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 3
        return pc + 2
    def ldy_b4_zero_page_indexed_x(pc):
        nonlocal t, y, p
        y = fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 2
    def ldy_ac_absolute(pc):
        nonlocal t, y, p
        y = fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 3
    def ldy_bc_absolute_indexed_x(pc):
        nonlocal t, y, p
        y = fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 3
    
    # LSR (Logical Shift Right)
    # Writes flags: N Z C
    def lsr_4a_accumulator(pc):
        nonlocal t, a, p
        r = a >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (a&C)
        a = r
        t += 2
        return pc + 1
    def lsr_46_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r)
        t += 5
        return pc + 2
    def lsr_56_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r)
        t += 6
        return pc + 2
    def lsr_4e_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r)
        t += 6
        return pc + 3
    def lsr_5e_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        m = fetch(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r)
        t += 7
        return pc + 3
    
    # NOP (No OPeration)
    # Writes flags: none
    def nop_ea_implied(pc):
        nonlocal t
        t += 2
        return pc + 1
    
    # ORA (bitwise OR with Accumulator)
    # Writes flags: N Z
    def ora_09_immediate(pc):
        nonlocal t, a, p
        a |= fetch(resolve_immediate(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def ora_05_zero_page(pc):
        nonlocal t, a, p
        a |= fetch(resolve_zero_page(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def ora_15_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a |= fetch(resolve_zero_page_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def ora_0d_absolute(pc):
        nonlocal t, a, p
        a |= fetch(resolve_absolute(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def ora_1d_absolute_indexed_x(pc):
        nonlocal t, a, p
        a |= fetch(resolve_absolute_indexed_x(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def ora_19_absolute_indexed_y(pc):
        nonlocal t, a, p
        a |= fetch(resolve_absolute_indexed_y(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def ora_01_indexed_indirect(pc):
        nonlocal t, a, p
        a |= fetch(resolve_indexed_indirect(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def ora_11_indirect_indexed(pc):
        nonlocal t, a, p
        a |= fetch(resolve_indirect_indexed(pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # Register Instructions
    # Writes flags: N Z
    # TAX (Transfer A to X)
    def tax_aa(pc):
        nonlocal t, x, p
        x = a
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # TXA (Transfer X to A)
    def txa_8a(pc):
        nonlocal t, a, p
        a = x
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 1
    # DEX (DEcrement X)
    def dex_ca(pc):
        nonlocal t, x, p
        x = (x - 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # INX (INcrement X)
    def inx_e8(pc):
        nonlocal t, x, p
        x = (x + 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # TAY (Transfer A to Y)
    def tay_a8(pc):
        nonlocal t, y, p
        y = a
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    # TYA (Transfer Y to A)
    def tya_98(pc):
        nonlocal t, a, p
        a = y
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 1
    # DEY (DEcrement Y)
    def dey_88(pc):
        nonlocal t, y, p
        y = (y - 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    # INY (INcrement Y)
    def iny_c8(pc):
        nonlocal t, y, p
        y = (y + 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    
    # ROL (ROtate Left)
    # Writes flags: N Z C
    def rol_2a_accumulator(pc):
        nonlocal t, a, p
        r = (a<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def rol_26_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        r = (fetch(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 5
        return pc + 2
    def rol_36_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        r = (fetch(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 2
    def rol_2e_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        r = (fetch(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 3
    def rol_3e_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        r = (fetch(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        store(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # ROR (ROtate Right)
    # Writes flags: N Z C
    def ror_6a_accumulator(pc):
        nonlocal t, a, p
        r = (a>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (a&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def ror_66_zero_page(pc):
        nonlocal t, p
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r&0xFF)
        t += 5
        return pc + 2
    def ror_76_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 2
    def ror_6e_absolute(pc):
        nonlocal t, p
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r&0xFF)
        t += 6
        return pc + 3
    def ror_7e_absolute_indexed_x(pc):
        nonlocal t, p
        addr = resolve_absolute_indexed_x_no_extra_cycle(pc)
        m = fetch(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        store(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # RTI (ReTurn from Interrupt)
    # Writes flags: all
    def rti_40_implied(pc):
        nonlocal t, p, s
        s = (s+1) & 0xFF
        p = fetch(STACK_OFFSET+s) & MASK_B | U
        s = (s+1) & 0xFF
        pc = fetch(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        pc |= fetch(STACK_OFFSET+s) << 8
        t += 6
        return pc
    
    # RTS (ReTurn from Subroutine)
    # Writes flags: none
    def rts_60_implied(pc):
        nonlocal t, s
        s = (s+1) & 0xFF
        pc = fetch(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        pc |= fetch(STACK_OFFSET+s) << 8
        t += 6
        return pc + 1
    
    # SBC (SuBtract with Carry)
    # Writes flags: N V Z C
    def sbc_e9_immediate(pc):
        nonlocal t, p, a
        m = fetch(resolve_immediate(pc))
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def sbc_e5_zero_page(pc):
        nonlocal t, p, a
        addr = resolve_zero_page(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def sbc_f5_zero_page_indexed_x(pc):
        nonlocal t, p, a
        addr = resolve_zero_page_indexed_x(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def sbc_ed_absolute(pc):
        nonlocal t, p, a
        addr = resolve_absolute(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def sbc_fd_absolute_indexed_x(pc):
        nonlocal t, p, a
        addr = resolve_absolute_indexed_x(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def sbc_f9_absolute_indexed_y(pc):
        nonlocal t, p, a
        addr = resolve_absolute_indexed_y(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def sbc_e1_indexed_indirect(pc):
        nonlocal t, p, a
        addr = resolve_indexed_indirect(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def sbc_f1_indirect_indexed(pc):
        nonlocal t, p, a
        addr = resolve_indirect_indexed(pc)
        m = fetch(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # STA (STore Accumulator)
    # Writes flags: none
    def sta_85_zero_page(pc):
        nonlocal t
        store(resolve_zero_page(pc), a)
        t += 3
        return pc + 2
    def sta_95_zero_page_indexed_x(pc):
        nonlocal t
        store(resolve_zero_page_indexed_x(pc), a)
        t += 4
        return pc + 2
    def sta_8d_absolute(pc):
        nonlocal t
        store(resolve_absolute(pc), a)
        t += 4
        return pc + 3
    def sta_9d_absolute_indexed_x(pc):
        nonlocal t
        store(resolve_absolute_indexed_x_no_extra_cycle(pc), a)
        t += 5
        return pc + 3
    def sta_99_absolute_indexed_y(pc):
        nonlocal t
        store(resolve_absolute_indexed_y_no_extra_cycle(pc), a)
        t += 5
        return pc + 3
    def sta_81_indexed_indirect(pc):
        nonlocal t
        store(resolve_indexed_indirect(pc), a)
        t += 6
        return pc + 2
    def sta_91_indirect_indexed(pc):
        nonlocal t
        store(resolve_indirect_indexed_no_extra_cycle(pc), a)
        t += 6
        return pc + 2
    
    # Stack Instructions
    # TXS (Transfer X to Stack ptr)
    def txs_9a(pc):
        nonlocal t, s
        s = x
        t += 2
        return pc + 1
    # TSX (Transfer Stack ptr to X)
    def tsx_ba(pc):
        nonlocal t, x, p
        x = s
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # PHA (PusH Accumulator)
    def pha_48(pc):
        nonlocal t, s
        store(STACK_OFFSET+s, a)
        s = (s-1) & 0xFF
        t += 3
        return pc + 1
    # PLA (PuLl Accumulator)
    def pla_68(pc):
        nonlocal t, s, a, p
        s = (s+1) & 0xFF
        a = fetch(STACK_OFFSET+s)
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 1
    # PHP (PusH Processor status)
    def php_08(pc):
        nonlocal t, s
        store(STACK_OFFSET+s, p|B|U)
        s = (s-1) & 0xFF
        t += 3
        return pc + 1
    # PLP (PuLl Processor status)
    def plp_28(pc):
        nonlocal t, s, p
        s = (s+1) & 0xFF
        p = fetch(STACK_OFFSET+s) & MASK_B | U  # always clear break bit, set unused bit 
        t += 4
        return pc + 1
    
    # STX (STore X register)
    # Writes flags: none
    def stx_86_zero_page(pc):
        nonlocal t
        store(resolve_zero_page(pc), x)
        t += 3
        return pc + 2
    def stx_96_zero_page_indexed_y(pc):
        nonlocal t
        store(resolve_zero_page_indexed_y(pc), x)
        t += 4
        return pc + 2
    def stx_8e_absolute(pc):
        nonlocal t
        store(resolve_absolute(pc), x)
        t += 4
        return pc + 3
    
    # STY (STore Y register)
    # Writes flags: none
    def sty_84_zero_page(pc):
        nonlocal t
        store(resolve_zero_page(pc), y)
        t += 3
        return pc + 2
    def sty_94_zero_page_indexed_x(pc):
        nonlocal t
        store(resolve_zero_page_indexed_x(pc), y)
        t += 4
        return pc + 2
    def sty_8c_absolute(pc):
        nonlocal t
        store(resolve_absolute(pc), y)
        t += 4
        return pc + 3

    def build_undefined_op(opcode):
        def undefined_op(pc):
            raise ValueError('Undefined opcode {}'.format(opcode))
        return undefined_op

    ops = [build_undefined_op(opcode) for opcode in range(0x100)]
    ops[0x69] = adc_69_immediate
    ops[0x65] = adc_65_zero_page
    ops[0x75] = adc_75_zero_page_indexed_x
    ops[0x6d] = adc_6d_absolute
    ops[0x7d] = adc_7d_absolute_indexed_x
    ops[0x79] = adc_79_absolute_indexed_y
    ops[0x61] = adc_61_indexed_indirect
    ops[0x71] = adc_71_indirect_indexed
    ops[0x29] = and_29_immediate
    ops[0x25] = and_25_zero_page
    ops[0x35] = and_35_zero_page_indexed_x
    ops[0x2d] = and_2d_absolute
    ops[0x3d] = and_3d_absolute_indexed_x
    ops[0x39] = and_39_absolute_indexed_y
    ops[0x21] = and_21_indexed_indirect
    ops[0x31] = and_31_indirect_indexed
    ops[0x0a] = asl_0a_accumulator
    ops[0x06] = asl_06_zero_page
    ops[0x16] = asl_16_zero_page_indexed_x
    ops[0x0e] = asl_0e_absolute
    ops[0x1e] = asl_1e_absolute_indexed_x
    ops[0x24] = bit_24_zero_page
    ops[0x2c] = bit_2c_absolute
    ops[0x10] = bpl_10
    ops[0x30] = bmi_30
    ops[0x50] = bvc_50
    ops[0x70] = bvs_70
    ops[0x90] = bcc_90
    ops[0xb0] = bcs_b0
    ops[0xd0] = bne_d0
    ops[0xf0] = beq_f0
    ops[0x00] = brk_00_implied_stop_execution if stop_on_brk else brk_00_implied
    ops[0xc9] = cmp_c9_immediate
    ops[0xc5] = cmp_c5_zero_page
    ops[0xd5] = cmp_d5_zero_page_indexed_x
    ops[0xcd] = cmp_cd_absolute
    ops[0xdd] = cmp_dd_absolute_indexed_x
    ops[0xd9] = cmp_d9_absolute_indexed_y
    ops[0xc1] = cmp_c1_indexed_indirect
    ops[0xd1] = cmp_d1_indirect_indexed
    ops[0xe0] = cpx_e0_immediate
    ops[0xe4] = cpx_e4_zero_page
    ops[0xec] = cpx_ec_absolute
    ops[0xc0] = cpy_c0_immediate
    ops[0xc4] = cpy_c4_zero_page
    ops[0xcc] = cpy_cc_absolute
    ops[0xc6] = dec_c6_zero_page
    ops[0xd6] = dec_d6_zero_page_indexed_x
    ops[0xce] = dec_ce_absolute
    ops[0xde] = dec_de_absolute_indexed_x
    ops[0x49] = eor_49_immediate
    ops[0x45] = eor_45_zero_page
    ops[0x55] = eor_55_zero_page_indexed_x
    ops[0x4d] = eor_4d_absolute
    ops[0x5d] = eor_5d_absolute_indexed_x
    ops[0x59] = eor_59_absolute_indexed_y
    ops[0x41] = eor_41_indexed_indirect
    ops[0x51] = eor_51_indirect_indexed
    ops[0x18] = clc_18
    ops[0x38] = sec_38
    ops[0x58] = cli_58
    ops[0x78] = sei_78
    ops[0xb8] = clv_b8
    ops[0xd8] = cld_d8
    ops[0xf8] = sed_f8
    ops[0xe6] = inc_e6_zero_page
    ops[0xf6] = inc_f6_zero_page_indexed_x
    ops[0xee] = inc_ee_absolute
    ops[0xfe] = inc_fe_absolute_indexed_x
    ops[0x4c] = jmp_4c_absolute
    ops[0x6c] = jmp_6c_indirect
    ops[0x20] = jsr_20_absolute
    ops[0xa9] = lda_a9_immediate
    ops[0xa5] = lda_a5_zero_page
    ops[0xb5] = lda_b5_zero_page_indexed_x
    ops[0xad] = lda_ad_absolute
    ops[0xbd] = lda_bd_absolute_indexed_x
    ops[0xb9] = lda_b9_absolute_indexed_y
    ops[0xa1] = lda_a1_indexed_indirect
    ops[0xb1] = lda_b1_indirect_indexed
    ops[0xa2] = ldx_a2_immediate
    ops[0xa6] = ldx_a6_zero_page
    ops[0xb6] = ldx_b6_zero_page_indexed_y
    ops[0xae] = ldx_ae_absolute
    ops[0xbe] = ldx_be_absolute_indexed_y
    ops[0xa0] = ldy_a0_immediate
    ops[0xa4] = ldy_a4_zero_page
    ops[0xb4] = ldy_b4_zero_page_indexed_x
    ops[0xac] = ldy_ac_absolute
    ops[0xbc] = ldy_bc_absolute_indexed_x
    ops[0x4a] = lsr_4a_accumulator
    ops[0x46] = lsr_46_zero_page
    ops[0x56] = lsr_56_zero_page_indexed_x
    ops[0x4e] = lsr_4e_absolute
    ops[0x5e] = lsr_5e_absolute_indexed_x
    ops[0xea] = nop_ea_implied
    ops[0x09] = ora_09_immediate
    ops[0x05] = ora_05_zero_page
    ops[0x15] = ora_15_zero_page_indexed_x
    ops[0x0d] = ora_0d_absolute
    ops[0x1d] = ora_1d_absolute_indexed_x
    ops[0x19] = ora_19_absolute_indexed_y
    ops[0x01] = ora_01_indexed_indirect
    ops[0x11] = ora_11_indirect_indexed
    ops[0xaa] = tax_aa
    ops[0x8a] = txa_8a
    ops[0xca] = dex_ca
    ops[0xe8] = inx_e8
    ops[0xa8] = tay_a8
    ops[0x98] = tya_98
    ops[0x88] = dey_88
    ops[0xc8] = iny_c8
    ops[0x2a] = rol_2a_accumulator
    ops[0x26] = rol_26_zero_page
    ops[0x36] = rol_36_zero_page_indexed_x
    ops[0x2e] = rol_2e_absolute
    ops[0x3e] = rol_3e_absolute_indexed_x
    ops[0x6a] = ror_6a_accumulator
    ops[0x66] = ror_66_zero_page
    ops[0x76] = ror_76_zero_page_indexed_x
    ops[0x6e] = ror_6e_absolute
    ops[0x7e] = ror_7e_absolute_indexed_x
    ops[0x40] = rti_40_implied
    ops[0x60] = rts_60_implied
    ops[0xe9] = sbc_e9_immediate
    ops[0xe5] = sbc_e5_zero_page
    ops[0xf5] = sbc_f5_zero_page_indexed_x
    ops[0xed] = sbc_ed_absolute
    ops[0xfd] = sbc_fd_absolute_indexed_x
    ops[0xf9] = sbc_f9_absolute_indexed_y
    ops[0xe1] = sbc_e1_indexed_indirect
    ops[0xf1] = sbc_f1_indirect_indexed
    ops[0x85] = sta_85_zero_page
    ops[0x95] = sta_95_zero_page_indexed_x
    ops[0x8d] = sta_8d_absolute
    ops[0x9d] = sta_9d_absolute_indexed_x
    ops[0x99] = sta_99_absolute_indexed_y
    ops[0x81] = sta_81_indexed_indirect
    ops[0x91] = sta_91_indirect_indexed
    ops[0x9a] = txs_9a
    ops[0xba] = tsx_ba
    ops[0x48] = pha_48
    ops[0x68] = pla_68
    ops[0x08] = php_08
    ops[0x28] = plp_28
    ops[0x86] = stx_86_zero_page
    ops[0x96] = stx_96_zero_page_indexed_y
    ops[0x8e] = stx_8e_absolute
    ops[0x84] = sty_84_zero_page
    ops[0x94] = sty_94_zero_page_indexed_x
    ops[0x8c] = sty_8c_absolute

    def tick():
        nonlocal pc
        pc = ops[fetch(pc)](pc)
        return t

    def trigger_interrupt(new_pc):
        # note that this is the only place pc is used outside of the main loop,
        # as interrupts are the only exception to normal execution
        nonlocal pc
        nonlocal t, s
        store(STACK_OFFSET+s, pc<<8)
        s = (s-1) & 0xFF
        store(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        store(STACK_OFFSET+s, p|U)
        s = (s-1) & 0xFF
        t += 7
        pc = new_pc

    def trigger_nmi():
        trigger_interrupt(fetch(0xFFFA)|(fetch(0xFFFB)<<8))

    def trigger_reset():
        trigger_interrupt(fetch(0xFFFC)|(fetch(0xFFFD)<<8))

    def trigger_irq():
        if p&I:
            return pc  # interrupt disabled
        trigger_interrupt(fetch(0xFFFE)|(fetch(0xFFFF)<<8))

    def transfer_page_to_oam(page_num):
        nonlocal t
        addr = page_num << 8
        i = 0
        while i < 0x100:
            write_oam(fetch(addr+i))
            i += 1
        t = (t+514) & ~1 # 1 wait state cycle while waiting for writes to complete, +1 if on an odd CPU cycle, then 256 alternating read/write cycles
    
    def connect(cpu_read, cpu_write, ppu_write_oam):
        nonlocal fetch, store, write_oam
        fetch = cpu_read
        store = cpu_write
        write_oam = ppu_write_oam

    return (
        tick,
        trigger_nmi,
        trigger_reset,
        trigger_irq,
        transfer_page_to_oam,
        connect,
        lambda: (pc, s, a, x, y, p)
    )

class Cart(object):
    def __init__(
        self,
        prg_rom_banks,
        chr_rom_banks,
        prg_ram_size,
        chr_ram_size,
        trainer,
        mapper_num=0,
        nt_mirroring=NT_MIRRORING_HORIZONTAL,
        has_non_volatile_memory=False,
        console_flavor=CONSOLE_FLAVOR_NES,
        cart_filename=None
    ):
        self.prg_rom_banks = prg_rom_banks
        self.chr_rom_banks = chr_rom_banks
        self.prg_ram = array.array('B', (0 for _ in range(prg_ram_size)))
        self.chr_ram = array.array('B', (0 for _ in range(chr_ram_size)))
        self.trainer = trainer
        self.mapper_num = mapper_num
        self.nt_mirroring = nt_mirroring
        self.has_non_volatile_memory = has_non_volatile_memory  # redundant to prg-nvram once it's adddeds
        self.console_flavor = console_flavor
        self.cart_filename = cart_filename
        
    @staticmethod
    def from_file(cart_filepath):
        with open(cart_filepath, 'rb') as cart_file:
            return Cart.from_nes2_or_ines_file(
                cart_file,
                cart_options={'cart_filename': os.path.basename(cart_filepath)}
            )

    @staticmethod
    def from_nes2_or_ines_file(nes2_file, cart_options={}):
        header = nes2_file.read(0x10)
        
        # byte 0 - 3
        assert(header[0x0:0x4] == b'NES\x1A')
        # byte 4 - 5
        num_prg_rom_banks           = header[0x4]
        num_chr_rom_banks           = header[0x5]
        # byte 6
        mapper_num                  = header[0x6] >> 4
        use_four_screen_mirroring   = header[0x6] & 0x08 == 0x08 # provide four-screen VRAM (precedence over v/h mirroring)
        has_trainer                 = header[0x6] & 0x04 == 0x04
        has_non_volatile_memory     = header[0x6] & 0x02 == 0x02 # battery-backed PRG RAM ($6000-7FFF) or other persistent memory
        use_vertical_mirroring      = header[0x6] & 0x01 == 0x01 # otherwise uses horizontal
        # byte 7
        mapper_num                 |= header[0x7] & 0xF0
        is_nes2_format              = header[0x7] & 0x0C == 0x08
        console_flavor              = header[0x7] & 0x03

        if mapper_num >= len(mappers):
            raise ValueError('Mapper {} not supported'.format(mapper_num))

        if use_four_screen_mirroring:
            nt_mirroring = NT_MIRRORING_FOUR_SCREEN
        elif use_vertical_mirroring:
            nt_mirroring = NT_MIRRORING_VERTICAL
        else:
            nt_mirroring = NT_MIRRORING_HORIZONTAL 

        cart_options = dict(
            cart_options,
            nt_mirroring=nt_mirroring,
            has_non_volatile_memory=has_non_volatile_memory,
            console_flavor=console_flavor,
            mapper_num=mapper_num
        )

        if is_nes2_format:
            return Cart.from_nes2_file(nes2_file, header, num_prg_rom_banks, num_chr_rom_banks, has_trainer, mapper_num, cart_options)
        else:
            return Cart.from_ines_file(nes2_file, header, num_prg_rom_banks, num_chr_rom_banks, has_trainer, mapper_num, cart_options)

    @staticmethod
    def from_nes2_file(nes2_file, header, num_prg_rom_banks, num_chr_rom_banks, has_trainer, mapper_num, cart_options):
        # See: https://www.nesdev.org/wiki/NES_2.0
        raise NotImplementedError

    @staticmethod
    def from_ines_file(ines_file, header, num_prg_rom_banks, num_chr_rom_banks, has_trainer, mapper_num, cart_options):
        # See: https://www.nesdev.org/wiki/INES
        # Extract trainer
        trainer = ines_file.read(0x200) if has_trainer else None

        # Extract 16 KB PRG-ROM banks
        prg_rom_banks = [ines_file.read(0x4000) for _ in range(num_prg_rom_banks)]

        # Extract  8 KB CHR-ROM banks
        chr_rom_banks = [ines_file.read(0x2000) for _ in range(num_chr_rom_banks)]

        return Cart(
            prg_rom_banks,
            chr_rom_banks,
            0x2000,
            0 if chr_rom_banks else 0x2000,
            trainer,
            **cart_options
        )

    def print_config(self):
        nt_mirroring_descs = ['Horizontal or mapper-controlled', 'Vertical', 'Four Screen']
        mapper_names = {0: 'NROM, no mapper', 1: 'Nintendo MMC1', 2: 'UNROM switch', 3: 'CNROM switch', 4: 'Nintendo MMC3', 5: 'Nintendo MMC5', 6: 'FFE F4xxx', 7: 'AOROM switch', 8: 'FFE F3xxx', 9: 'Nintendo MMC2', 10: 'Nintendo MMC4', 11: 'ColorDreams chip', 12: 'FFE F6xxx', 15: '100-in-1 switch', 16: 'Bandai chip', 17: 'FFE F8xxx', 18: 'Jaleco SS8806 chip', 19: 'Namcot 106 chip', 20: 'Nintendo Disk System', 21: 'Konami VRC4a', 22: 'Konami VRC2a', 23: 'Konami VRC2a', 24: 'Konami VRC6', 25: 'Konami VRC4b', 32: 'Irem G-101 chip', 33: 'Taito TC0190/TC0350', 34: '32 KB ROM switch', 64: 'Tengen RAMBO-1 chip', 65: 'Irem H-3001 chip', 66: 'GNROM switch', 67: 'SunSoft3 chip', 68: 'SunSoft4 chip', 69: 'SunSoft5 FME-7 chip', 71: 'Camerica chip', 78: 'Irem 74HC161/32-based', 91: 'Pirate HK-SF3 chip'}
        console_flavors = ['Nintendo Entertainment System/Family Computer', 'Nintendo Vs. System', 'Nintendo Playchoice 10', 'Extended Console Type']
        print(f'Number of 16 KB PRG-ROM banks: {len(self.prg_rom_banks)}')
        print(f'Number of 8 KB CHR-ROM banks: {len(self.chr_rom_banks)}')
        print(f'Number PRG-RAM bytes: {len(self.prg_ram)}')
        print(f'Number CHR-RAM bytes: {len(self.chr_ram)}')
        print(f'Memory mapper: {mapper_names[self.mapper_num]} ({self.mapper_num})')
        print(f'Has "battery" or other non-volatile memory: {self.has_non_volatile_memory}')
        print(f'Has 512-byte trainer: {self.trainer is not None}')
        print(f'Nametable mirroring: {nt_mirroring_descs[self.nt_mirroring]}')
        print(f'Console flavor: {console_flavors[self.console_flavor]}')
        
    def create_mapper_funcs(self, ram, vram, pals, cpu_transfer_page_to_oam, ppu_read_reg, ppu_write_reg, apu_read_reg, apu_write_reg):
        return mappers[self.mapper_num](
            ram,
            vram,
            pals,
            self.prg_rom_banks,
            self.prg_ram,
            self.chr_rom_banks,
            self.chr_ram,
            cpu_transfer_page_to_oam,
            ppu_read_reg,
            ppu_write_reg,
            apu_read_reg,
            apu_write_reg,
            self.nt_mirroring
        )

class NES(object):
    def __init__(self, screen, cart, cpu_regs=None, t=0, print_cpu_log=False, pal=None):
        self.cart = cart
        self.pal = pal or NTSC_PALETTE
        self.cpu_regs = cpu_regs
        self.initial_t = t
        self.ram = array.array('B', (0 for _ in range(0x800)))
        self.vram = array.array('B', (0 for _ in range(0x1000 if cart.nt_mirroring == NT_MIRRORING_FOUR_SCREEN else 0x800)))
        self.print_cpu_log = print_cpu_log
        self.ppu_tick,\
        self.ppu_read_reg,\
        self.ppu_write_reg,\
        self.ppu_write_oam,\
        self.ppu_pals,\
        self.ppu_connect,\
        self.ppu_inspect_regs = \
            create_ppu_funcs(screen)
        self.cpu_tick,\
        self.cpu_trigger_nmi,\
        self.cpu_trigger_reset,\
        self.cpu_trigger_irq,\
        self.cpu_transfer_page_to_oam,\
        self.cpu_connect,\
        self.cpu_inspect_regs = \
            create_cpu_funcs(
                regs=cpu_regs,
                t=t,
                stop_on_brk=False)
        def apu_read_reg(addr):
            return 0
        def apu_write_reg(addr, value):
            pass
        self.cpu_read,\
        self.cpu_write,\
        self.ppu_read,\
        self.ppu_write = cart.create_mapper_funcs(
            self.ram,
            self.vram,
            self.ppu_pals(),
            self.cpu_transfer_page_to_oam,
            self.ppu_read_reg,
            self.ppu_write_reg,
            apu_read_reg,
            apu_write_reg)
        self.cpu_connect(self.cpu_read, self.cpu_write, self.ppu_write_oam)
        self.ppu_connect(self.ppu_read, self.ppu_write, self.cpu_trigger_nmi)

    def play(self):
        t = self.initial_t
        try:
            if self.print_cpu_log:
                while True:
                    pc, s, a, x, y, p = self.cpu_inspect_regs()
                    op = self.cpu_read(pc)
                    num_operands = INSTRUCTION_BYTES[op]
                    operands = [(self.cpu_read(pc+operand_i) if operand_i < num_operands else None) for operand_i in range(3)]
                    operands_text = ' '.join(['  ' if operand is None else f'{operand:02X}' for operand in operands])
                    addr_mode = INSTRUCTION_ADDR_MODES[op]
                    addr_mode_format = ADDR_MODE_FORMATS[addr_mode]
                    log_line = f'{pc:04X}  {operands_text}  {INSTRUCTION_LABELS[op]}{addr_mode_format(self.cpu_read, pc, operands)}'
                    log_line += ' ' * (48-len(log_line))
                    log_line += f'A:{a:02X} X:{x:02X} Y:{y:02X} P:{p:02X} SP:{s:02X} CYC:{t}'
                    print(log_line)
                    t = self.cpu_tick()
            else:
                self.cpu_trigger_reset()
                while True:
                    t = self.cpu_tick()
        except VMStop:
            return t  # clean exit

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Curt NES v0.0.0')
    parser.add_argument('rom')
    parser.add_argument('--print-cart-config', action='store_true')
    parser.add_argument('--print-cpu-log', action='store_true')
    parser.add_argument('--pal-filepath', '--pal')
    args = parser.parse_args()

    cart = Cart.from_file(args.rom)

    if args.print_cart_config:
        cart.print_config()
        exit(0)

    if args.pal_filepath:
        pal = load_pal_from_file(args.pal_filepath)

    # Currently overriding registers + time for nestest.nes -- not sure why it doesn't align with documented initial values?
    screen = array.array('B', (0 for _ in range(256*240)))
    nes = NES(
        screen,
        cart,
        cpu_regs=(0xC000, 0xFD, 0x00, 0x00, 0x00, 0x24),
        t=7,
        print_cpu_log=args.print_cpu_log,
        pal=pal
    )
    nes.play()
