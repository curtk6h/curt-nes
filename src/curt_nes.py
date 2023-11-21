#!/usr/local/bin/python3

# NOTE: closures are used heavily vs conventional/Pythonic object-oriented ways,
#       to improve performance; from a sloppy benchmark, using local vars in a 
#       closure appeared to be nearly twice as fast as using class attributes

# TODO:
# * tick / figure out initialization/syncing w/ cpu
# * add pygame and draw to screen
# * try out test ROMs!
# * handle controllers
# * review docs for flags, glitches, exceptions, etc.
# * add unit tests for sprite rendering (may have to extract code to test it, or expose individuals rendering funcs)
# * refactor ppu reads/writes (so they all go through single calls and make sense w/ sp eval)
# * remove unused constants etc
# * default status to unused = 1 and removed redundant sets
# * lots of cleanup / refactoring
#   * break instructions into single cycles
# * test interrupts
# * build tiny sample rom that runs in working emulator
#   * (1) create .cfg file (2) compile do nothing program (3) compile program that loops 
# FUTURE TO DOS:
# * color emphasis settings from PPUMASK
# * show sprite/background in leftmost 8 pixels settings from PPUMASK
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

NTSC_PALETTE = bytearray(b'RRR\x01\x1aQ\x0f\x0fe#\x06c6\x03K@\x04&?\t\x042\x13\x00\x1f \x00\x0b*\x00\x00/\x00\x00.\n\x00&-\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\xa0\xa0\x1eJ\x9d87\xbcX(\xb8u!\x94\x84#\\\x82.$o?\x00QR\x001c\x00\x1ak\x05\x0ei.\x10\\h\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\xff\xffi\x9e\xfc\x89\x87\xff\xaev\xff\xcem\xf1\xe0p\xb2\xde|p\xc8\x91>\xa6\xa7%\x81\xba(c\xc4FT\xc1}V\xb3\xc0<<<\x00\x00\x00\x00\x00\x00\xfe\xff\xff\xbe\xd6\xfd\xcc\xcc\xff\xdd\xc4\xff\xea\xc0\xf9\xf2\xc1\xdf\xf1\xc7\xc2\xe8\xd0\xaa\xd9\xda\x9d\xc9\xe2\x9e\xbc\xe6\xae\xb4\xe5\xc7\xb5\xdf\xe4\xa9\xa9\xa9\x00\x00\x00\x00\x00\x00')

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
        write_oamdma = lambda _, page_num: cpu_transfer_page_to_oam(page_num)

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
        def r_palette_ram(addr):
            return pals[(addr-0x3F00)&0x1F]
        def r_palette_ram_sp_mirror(addr):
            return pals[(addr-0x3F10)&0x1F]
        def r_nametable_0_0(addr):
            return vram[(addr-0x2000+0x0000)&0x07FF]
        def r_nametable_0_1(addr):
            return vram[(addr-0x2000+0x0400)&0x07FF]
        def r_nametable_0_2(addr):
            return vram[(addr-0x2000+0x0800)&0x07FF]
        def r_nametable_0_3(addr):
            return vram[(addr-0x2000+0x0C00)&0x07FF]
        def r_nametable_1_0(addr):
            return vram[(addr-0x2400+0x0000)&0x07FF]
        def r_nametable_1_1(addr):
            return vram[(addr-0x2400+0x0400)&0x07FF]
        def r_nametable_1_2(addr):
            return vram[(addr-0x2400+0x0800)&0x07FF]
        def r_nametable_1_3(addr):
            return vram[(addr-0x2400+0x0C00)&0x07FF]
        def r_nametable_2_0(addr):
            return vram[(addr-0x2800+0x0000)&0x07FF]
        def r_nametable_2_1(addr):
            return vram[(addr-0x2800+0x0400)&0x07FF]
        def r_nametable_2_2(addr):
            return vram[(addr-0x2800+0x0800)&0x07FF]
        def r_nametable_2_3(addr):
            return vram[(addr-0x2800+0x0C00)&0x07FF]
        def r_nametable_3_0(addr):
            return vram[(addr-0x2C00+0x0000)&0x07FF]
        def r_nametable_3_1(addr):
            return vram[(addr-0x2C00+0x0400)&0x07FF]
        def r_nametable_3_2(addr):
            return vram[(addr-0x2C00+0x0800)&0x07FF]
        def r_nametable_3_3(addr):
            return vram[(addr-0x2C00+0x0C00)&0x07FF]
            
        def w_pattern_tables_from_chr_rom(addr, value):
            chr_rom_bank[addr] = value
        def w_pattern_tables_from_chr_ram(addr, value):
            chr_ram[addr] = value
        def w_palette_ram(addr, value):
            pals[(addr-0x3F00)&0x1F] = value
        def w_palette_ram_sp_mirror(addr, value):
            pals[(addr-0x3F10)&0x1F] = value
        def w_nametable_0_0(addr, value):
            vram[(addr-0x2000+0x0000)&0x07FF] = value
        def w_nametable_0_1(addr, value):
            vram[(addr-0x2000+0x0400)&0x07FF] = value
        def w_nametable_0_2(addr, value):
            vram[(addr-0x2000+0x0800)&0x07FF] = value
        def w_nametable_0_3(addr, value):
            vram[(addr-0x2000+0x0C00)&0x07FF] = value
        def w_nametable_1_0(addr, value):
            vram[(addr-0x2400+0x0000)&0x07FF] = value
        def w_nametable_1_1(addr, value):
            vram[(addr-0x2400+0x0400)&0x07FF] = value
        def w_nametable_1_2(addr, value):
            vram[(addr-0x2400+0x0800)&0x07FF] = value
        def w_nametable_1_3(addr, value):
            vram[(addr-0x2400+0x0C00)&0x07FF] = value
        def w_nametable_2_0(addr, value):
            vram[(addr-0x2800+0x0000)&0x07FF] = value
        def w_nametable_2_1(addr, value):
            vram[(addr-0x2800+0x0400)&0x07FF] = value
        def w_nametable_2_2(addr, value):
            vram[(addr-0x2800+0x0800)&0x07FF] = value
        def w_nametable_2_3(addr, value):
            vram[(addr-0x2800+0x0C00)&0x07FF] = value
        def w_nametable_3_0(addr, value):
            vram[(addr-0x2C00+0x0000)&0x07FF] = value
        def w_nametable_3_1(addr, value):
            vram[(addr-0x2C00+0x0400)&0x07FF] = value
        def w_nametable_3_2(addr, value):
            vram[(addr-0x2C00+0x0800)&0x07FF] = value
        def w_nametable_3_3(addr, value):
            vram[(addr-0x2C00+0x0C00)&0x07FF] = value

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
                (
                    [r_palette_ram] * 4 * 4 +
                    [r_palette_ram_sp_mirror, r_palette_ram, r_palette_ram, r_palette_ram] * 4
                ) * 8
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
                (
                    [w_palette_ram] * 4 * 4 +
                    [w_palette_ram_sp_mirror, w_palette_ram, w_palette_ram, w_palette_ram] * 4
                ) * 8
            )
        )

    cpu_readers, cpu_writers = create_cpu_accessors()
    cpu_read  = lambda addr: cpu_readers[addr](addr)
    cpu_write = lambda addr, value: cpu_writers[addr](addr, value)

    ppu_readers, ppu_writers = create_ppu_accessors()
    ppu_read  = lambda addr: ppu_readers[addr](addr)
    ppu_write = lambda addr, value: ppu_writers[addr](addr, value)

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
    out_pixels,
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
    scanline_num=0,
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
    scanline_t   = 0
    scanline_oam_addr = 0
    zero_sprite_on_scanline = 0x00
    zero_sprite_on_next_scanline = 0x00

    # rendering registers/latches/memory
    # background
    tile_0       = 0x0000 # AABB
    tile_1       = 0x0000 # AABB
    next_0       = 0x0000 # CC (next tile lo-bits to be loaded into BB)
    next_1       = 0x0000 # CC (next tile hi-bits to be loaded into BB)
    attr         = 0x0    # 2 bit attr (not bothering to implement this literally)
    attr_n       = 0x0    # next 2 bit attr
    attr_tmp     = 0x0    # incoming attr

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

    def fetch_nt():
        nonlocal pt_addr
        # _yyy NN YYYYY XXXXX
        #  ||| || ||||| +++++-- coarse X scroll
        #  ||| || +++++-------- coarse Y scroll
        #  ||| ++-------------- nametable select
        #  +++----------------- fine Y scroll
        # _yyy NNYY YYYX XXXX => _000 NNYY YYYX XXXX => ____ RRRR CCCC ____ (__0H RRRR CCCC 0TTT)
        pt_addr = fetch(0x2000|(ppu_addr&0x0FFF)) << 4

    def fetch_at():
        nonlocal attr_tmp
        # _yyy NNYY YYYX XXXX => _000 NN00 00YY YXXX
        attr_quad = fetch(0x23C0|(ppu_addr&0x0C00)|(((ppu_addr&0x0380)>>4)|((ppu_addr&0x001C)>>2)))
        # _yyy NNYY YYYX XXXX => _000 0000 0Y00 00X0 => _000 0000 0000 0YX0
        attr_tmp = ((attr_quad>>(((ppu_addr&0x0040)>>4)|(ppu_addr&0x0002)))&3) << 2

    def fetch_bg_0():
        nonlocal next_0
        # __0H RRRR CCCC 0TTT
        next_0 = fetch(((ppu_ctrl&0x10)<<8)|pt_addr|0|(ppu_addr>>12))

    def fetch_bg_1():
        nonlocal next_1
        # __0H RRRR CCCC 1TTT
        next_1 = fetch(((ppu_ctrl&0x10)<<8)|pt_addr|8|(ppu_addr>>12))

    def inc_x():
        nonlocal ppu_addr
        # _yyy NNYY YYYX XXXX => _yyy NnYY YYYx xxxx (x=X+1, n=carry)
        x = (ppu_addr&0x001F) + 1
        ppu_addr ^= (ppu_addr^((x<<5)|x)) & 0x041F

    def inc_xy():
        nonlocal ppu_addr
        # _yyy NNYY YYYX XXXX => _yyy nnyy yyyx xxxx
        fine_y_x = (ppu_addr&0x701F) + 0x1001  # add 1 to fine y and coarse x, at the same time
        y = (ppu_addr&0x03E0) + ((fine_y_x&0x8000)>>10)
        n = (ppu_addr&0x0C00) ^ ((fine_y_x&0x0020)<<5) # wrap nt on x
        if y == 0x3C0: # y == 30
            y = 0x0000
            n ^= 0x0800 # wrap nt on y
        ppu_addr  = (fine_y_x&0x701F) | n | y

    def reset_x():
        nonlocal ppu_addr
        ppu_addr ^= (ppu_addr^tmp_addr) & 0x041F

    def reset_y():
        nonlocal ppu_addr
        ppu_addr ^= (ppu_addr^tmp_addr) & 0x7BE0

    def set_vblank_flag():
        nonlocal ppu_status
        print(f"calling nmi at {t}")
        ppu_status |= 0x80
        if ppu_ctrl & 0x80:
            trigger_nmi()

    def clear_flags():
        nonlocal ppu_status
        print(f"clearing ppu flags at {t}")
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
        idle,             fetch_nt, # the two nt fetches done here don't serve any purpose
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
    visible_scanline_funcs[256] = inc_xy
    visible_scanline_funcs[257] = reset_x

    pre_render_scanline_funcs = list(visible_scanline_funcs)
    pre_render_scanline_funcs[1] = clear_flags
    for i in range(280, 305):
        pre_render_scanline_funcs[i] = reset_y

    vblank_scanline_funcs = [idle] * 341

    first_vblank_scanline_funcs = list(vblank_scanline_funcs)
    first_vblank_scanline_funcs[1] = set_vblank_flag

    pre_render_scanline_funcs_no_render = list(vblank_scanline_funcs)
    pre_render_scanline_funcs_no_render[1] = clear_flags

    post_render_scanline_funcs = vblank_scanline_funcs
    
    bg_tick_funcs = [
        [visible_scanline_funcs] * 240 +
        [post_render_scanline_funcs] +
        [first_vblank_scanline_funcs] +
        [vblank_scanline_funcs] * (261-242) +
        [pre_render_scanline_funcs]
    ] * 2

    no_tick_funcs = [
        [vblank_scanline_funcs] * 240 +
        [post_render_scanline_funcs] +
        [first_vblank_scanline_funcs] +
        [vblank_scanline_funcs] * (261-242) +
        [pre_render_scanline_funcs_no_render]
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
        nonlocal ppu_status, oam_addr, scanline_oam_addr, disable_writes, check_overflow, oam_bytes_to_copy, zero_sprite_on_next_scanline

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
                if scanline_oam_addr == 0x00:
                    zero_sprite_on_next_scanline = 0x40 if oam_addr == 0x00 else 0x00
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

    def reset_sprite_eval():
        nonlocal oam_bytes_to_copy, oam_byte, disable_writes, check_overflow, oam_addr
        oam_bytes_to_copy = 0
        oam_byte = 0
        disable_writes = False
        check_overflow = False
        oam_addr = 0x00

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
        reset_sprite_eval()

    def load_sp_tile_num():
        nonlocal sp_pt_addr
        tile_num = scanline_oam[scanline_oam_addr+1]
        if ppu_ctrl&0x20:
            # For 8x16 sprites: the first bit indicates bank (=> __0C RRRR CCCT PTTT)
            sp_pt_addr |= ((tile_num<<12)|(tile_num<<4)) & 0x1FE0
        else:
            # For 8x 8 sprites: use bank specified in ppu_ctrl like nametables does (=> __0H RRRR CCCC PTTT)
            sp_pt_addr |= ((ppu_ctrl&0x08)<<9) | (tile_num<<4)
        reset_sprite_eval()
        
    def load_sp_attr():
        sp_attrs[scanline_oam_addr>>2] = scanline_oam[scanline_oam_addr+2]
        reset_sprite_eval()

    def load_sp_x():
        sp_x_pos[scanline_oam_addr>>2] = scanline_oam[scanline_oam_addr+3]
        reset_sprite_eval()

    def fetch_sp_x():
        nonlocal scanline_oam_addr
        scanline_oam[scanline_oam_addr|0x03]
        # IMPORTANT: incrementing in this function wouldn't work if sprite tick happened
        # before background tick, as background tick is copying byte 4 of sprite pattern data.
        # It's also important that it wraps at the end of sprites
        scanline_oam_addr = (scanline_oam_addr+1) & 0x1F
        reset_sprite_eval()

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
        nonlocal tile_0, tile_1, attr, attr_n, ppu_status, zero_sprite_on_scanline
        
        # Get background pixel
        if (ppu_mask&0x08) == 0 or ((ppu_mask&0x02) == 0 and scanline_t < 8):
            # Rendering is OFF entirely or for the left-most column
            pixel = 0x00
        else:
            # Rendering is ON
            pixel = (((tile_0<<fine_x_scroll>>1)&0x4000)|((tile_1<<fine_x_scroll)&0x8000)) >> 14
            if pixel != 0:
                pixel |= attr

            # Shift background registers
            if scanline_t & 7:
                tile_0 <<= 1
                tile_1 <<= 1
            else:
                tile_0 = ((tile_0<<1)&0xFFFF) | next_0
                tile_1 = ((tile_1<<1)&0xFFFF) | next_1
                attr   = attr_n
                attr_n = attr_tmp

        # Get first active sprite pixel, if it's non-zero, and either:
        # background pixel is zero or priority is not set to background
        if (ppu_mask&0x10) == 0 or ((ppu_mask&0x04) == 0 and scanline_t <= 8) or scanline_num < 1 or scanline_t >= 256:
            # Rendering is OFF entirely or for the left-most column
            pass
        else:
            # Rendering is ON
            sp_idx = 0
            while sp_idx < 8:
                if sp_x_pos[sp_idx] == 0:  # skip sprites that are ahead of current position on scanline
                    sp_pixel = ((sp_tiles_0[sp_idx]&0x80)>>7) | ((sp_tiles_1[sp_idx]&0x80)>>6)
                    if sp_pixel != 0 and ((pixel&0x03) == 0 or (sp_attrs[sp_idx]&0x20) == 0):
                        if sp_idx == 0:
                            if (pixel&0x03) != 0: # there must be background pixel for a hit to occur
                                ppu_status |= zero_sprite_on_scanline  # sprite zero hit!
                            zero_sprite_on_scanline = zero_sprite_on_next_scanline # copy next scanline flag either way
                        pixel = 0x10 | ((sp_attrs[sp_idx]&0x03)<<2) | sp_pixel
                        break
                sp_idx += 1

            # Decrement sprite x counters, shift sprite registers
            # NOTE: wiki says that x position is decremented before rendering,
            # but then sprites would be offset by 1 pixel and it just seems wrong?
            # Is this a broader misinterpretation by me (ex. like "first active sprite pixel"
            # not exactly what's happening -- maybe the loop decrements back from 8th sprite to 0th?)?
            sp_idx = 0
            while sp_idx < 8:
                if sp_x_pos[sp_idx] > 0:
                    sp_x_pos[sp_idx] -= 1
                else:
                    sp_tiles_0[sp_idx] = (sp_tiles_0[sp_idx]<<1) & 0xFF
                    sp_tiles_1[sp_idx] = (sp_tiles_1[sp_idx]<<1) & 0xFF
                sp_idx += 1

        return pixel

    def render_pixel():
        nonlocal frame_num, scanline_num, scanline_t
        out_pixels[scanline_num*341+scanline_t] = pals[next_pixel()] & (0x30 if ppu_mask&1 else -1) # TODO: use pal memory accessor
        scanline_t   += 1
        scanline_num += scanline_t // 341
        frame_num    += scanline_num // 262
        scanline_num %= 262
        scanline_t   %= 341

    def skip_pixel():
        nonlocal frame_num, scanline_num, scanline_t
        scanline_t   += 1
        scanline_num += scanline_t // 341
        frame_num    += scanline_num // 262
        scanline_num %= 262
        scanline_t   %= 341

    def next_frame():
        nonlocal frame_num, scanline_num, scanline_t
        frame_num    += 1
        scanline_num  = 0
        scanline_t    = 0

    render_scanline_funcs = (
        [skip_pixel]               +  # idle
        [render_pixel] * (257-1)   +  # visible pixels
        [render_pixel] * (337-257) +  # hblank, junk + next line tiles
        [skip_pixel]   * (341-337)    # hblank, unknown
    )

    last_render_scanline_odd_frame_funcs = render_scanline_funcs[:-2] + [next_frame]
    
    render_funcs = [
        [render_scanline_funcs] * (240)     +  # visible scanlines
        [render_scanline_funcs] * (262-240)    # vblank
        ,
        [render_scanline_funcs] * (240)     +  # visible scanlines
        [render_scanline_funcs] * (262-239) +  # vblank
        [last_render_scanline_odd_frame_funcs] # skip last cycle (TODO: skip first cycle of first visible scanline instead?)
    ]

    def tick():
        nonlocal t
        if (ppu_mask&0x18) == 0:
            # TODO: lots more cleanup to do here
            no_tick_funcs[frame_num&1][scanline_num][scanline_t]() # TODO: look at this more
            skip_pixel()
        else:
            bg_tick_funcs[frame_num&1][scanline_num][scanline_t]()
            sp_tick_funcs[frame_num&1][scanline_num][scanline_t]()
            render_funcs [frame_num&1][scanline_num][scanline_t]()
        t += 1

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
        if ppu_addr >= 0x3F00:
            reg_io_value = fetch(ppu_addr) & (0x30 if ppu_mask&1 else -1)
            ppu_data = fetch(ppu_addr-0x1000) # TODO: move to mapper?
        else:
            reg_io_value = ppu_data
            ppu_data = fetch(ppu_addr)
        ppu_addr = (ppu_addr + PPU_ADDR_INCREMENTS[ppu_ctrl&0x04]) & 0x3FFF

    def write_nothing():
        pass
    def write_ppu_ctrl():
        nonlocal ppu_ctrl, tmp_addr
        if t < 29658:
            return  # TODO: figure out reg_io_value / reg_io_write_state
        if ppu_status & reg_io_value & ~ppu_ctrl & 0x80:
            trigger_nmi()
        ppu_ctrl = reg_io_value
        tmp_addr ^= (tmp_addr ^ (reg_io_value<<10)) & 0x0C00 # base nametable address
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
            t=t,
            tile_0=tile_0,
            tile_1=tile_1,
            next_0=next_0,
            next_1=next_1,
            attr=attr,
            attr_n=attr_n,
        )
    )

def create_cpu_funcs(regs=None, stop_on_brk=False, t=0):
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
    
    # NOTE: the pc register is one that's not split into literal
    #       pch/pcl, due to how often it gets incremented
    pc, s, a, x, y, p = regs or (0x0000, 0xFF, 0x00, 0x00, 0x00, 0x34)

    # Internal registers that support addressing
    adh  = 0x00
    adl  = 0x00
    bah  = 0x00
    bal  = 0x00
    iah  = 0x00
    ial  = 0x00
    off  = 0x00

    # For read/modify/store operations
    addr = 0x0000 # full 16 bit addess of operand
    data = 0x00   # operand data
    
    # NOTE: also consider all flag single letter names reserved,
    #       even if not currently used: c, z, i, d, b, v, n
    
    # # Resolve address per mode
    # def resolve_immediate(pc):
    #     return pc
    # def resolve_zero_page(pc):
    #     return fetch(pc)
    # def resolve_zero_page_indexed_x(pc):
    #     return (addr+x) & 0xFF
    # def resolve_zero_page_indexed_y(pc):
    #     return (addr+y) & 0xFF
    # def resolve_absolute_0(pc):
    #     return fetch(pc)
    # def resolve_absolute_1(pc):
    #     return addr | (fetch(pc)<<8)
    # def resolve_absolute_indexed_x_0(pc):
    #     return fetch(pc) + x
    # def resolve_absolute_indexed_x_1(pc):
    #     return addr
    #     t += addr>>8
    #     return (addr+(fetch(pc+1)<<8)) & 0xFFFF
    # def resolve_absolute_indexed_x_2(pc):
    #     nonlocal t
    #     addr = fetch(pc) + x
    #     t += addr>>8
    #     return (addr+(fetch(pc+1)<<8)) & 0xFFFF
    # def resolve_absolute_indexed_y(pc):
    #     nonlocal t
    #     addr = fetch(pc) + y
    #     t += addr>>8
    #     return (addr+(fetch(pc+1)<<8)) & 0xFFFF
    # def resolve_absolute_indexed_x_no_extra_cycle(pc):
    #     return ((fetch(pc)|(fetch(pc+1)<<8))+x) & 0xFFFF
    # def resolve_absolute_indexed_y_no_extra_cycle(pc):
    #     return ((fetch(pc)|(fetch(pc+1)<<8))+y) & 0xFFFF
    # def resolve_indexed_indirect(pc):
    #     i = fetch(pc) + x
    #     return fetch(i&0xFF)|(fetch((i+1)&0xFF)<<8)
    # def resolve_indirect_indexed(pc):
    #     nonlocal t
    #     i = fetch(pc)
    #     addr = fetch(i) + y
    #     t += addr>>8
    #     return (addr+(fetch((i+1)&0xFF)<<8)) & 0xFFFF
    # def resolve_indirect_indexed_no_extra_cycle(pc):
    #     i = fetch(pc)
    #     return ((fetch(i)|(fetch((i+1)&0xFF)<<8))+y) & 0xFFFF
    # def resolve_indirect(pc):
    #     addr = fetch(pc) | (fetch(pc+1)<<8)
    #     return fetch(addr) | (fetch((addr&0xFF00)|((addr+1)&0xFF))<<8)  # NOTE: byte two cannot cross page
    # def resolve_relative(pc):
    #     nonlocal t
    #     rel_addr = signed8(fetch(pc))
    #     pc += 1
    #     t += (((pc&0xFF)+rel_addr)>>8) & 1
    #     return pc + rel_addr

    # Addressing modes

    def next_op():
        nonlocal pc
        opcode = fetch(pc); pc += 1
        return ops[opcode]
    
    def immediate(f):
        def immediate_0():
            nonlocal pc
            pc += 1
            return f(pc-1)
        return immediate_0
    
    def zero_page(f):
        def zero_page_0():
            nonlocal pc, adl
            adl = fetch(pc); pc += 1
            return zero_page_1
        def zero_page_1():
            return f(adl)
        return zero_page_0
    
    def zero_page_indexed_x(f):
        def zero_page_indexed_x_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return zero_page_indexed_x_1
        def zero_page_indexed_x_1():
            nonlocal adl
            adl = (bal+x) & 0xFF
            return zero_page_indexed_x_2
        def zero_page_indexed_x_2():
            return f(adl)
        return zero_page_indexed_x_0
    
    def zero_page_indexed_y(f):
        def zero_page_indexed_y_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return zero_page_indexed_y_1
        def zero_page_indexed_y_1():
            nonlocal adl
            adl = (bal+y) & 0xFF
            return zero_page_indexed_y_2
        def zero_page_indexed_y_2():
            return f(adl)
        return zero_page_indexed_y_0

    def absolute(f):
        def absolute_0():
            nonlocal pc, adl
            adl = fetch(pc); pc += 1
            return absolute_1
        def absolute_1():
            nonlocal pc, adh
            adh = fetch(pc); pc += 1
            return absolute_2
        def absolute_2():
            return f((adh<<8)|adl)
        return absolute_0

    def absolute_indexed_x(f):
        def absolute_indexed_x_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return absolute_indexed_x_1
        def absolute_indexed_x_1():
            nonlocal pc, bah
            bah = fetch(pc); pc += 1
            return absolute_indexed_x_2
        def absolute_indexed_x_2():
            nonlocal adl, adh
            adl = bal + x
            adh = (bah+(adl>>8)) & 0xFF
            if adl & 0x100: # extra cycle for page crossing (carry on low byte addition)
                adl &= 0xFF
                return absolute_indexed_x_3
            return f((adh<<8)|adl)
        def absolute_indexed_x_3():
            return f((adh<<8)|adl)
        return absolute_indexed_x_0
    
    def absolute_indexed_x_always_extra_cycle(f):
        def absolute_indexed_x_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return absolute_indexed_x_1
        def absolute_indexed_x_1():
            nonlocal pc, bah
            bah = fetch(pc); pc += 1
            return absolute_indexed_x_2
        def absolute_indexed_x_2():
            nonlocal adl, adh
            adl = bal + x
            adh = (bah+(adl>>8)) & 0xFF
            adl &= 0xFF
            return absolute_indexed_x_3
        def absolute_indexed_x_3():
            return f((adh<<8)|adl)
        return absolute_indexed_x_0

    def absolute_indexed_y(f):
        def absolute_indexed_y_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return absolute_indexed_y_1
        def absolute_indexed_y_1():
            nonlocal pc, bah
            bah = fetch(pc); pc += 1
            return absolute_indexed_y_2
        def absolute_indexed_y_2():
            nonlocal adl, adh
            adl = bal + y
            adh = (bah+(adl>>8)) & 0xFF
            if adl & 0x100: # extra cycle for page crossing (carry on low byte addition)
                adl &= 0xFF
                return absolute_indexed_y_3
            return f((adh<<8)|adl)
        def absolute_indexed_y_3():
            return f((adh<<8)|adl)
        return absolute_indexed_y_0

    def absolute_indexed_y_always_extra_cycle(f):
        def absolute_indexed_y_0():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return absolute_indexed_y_1
        def absolute_indexed_y_1():
            nonlocal pc, bah
            bah = fetch(pc); pc += 1
            return absolute_indexed_y_2
        def absolute_indexed_y_2():
            nonlocal adl, adh
            adl = bal + y
            adh = (bah+(adl>>8)) & 0xFF
            adl &= 0xFF
            return absolute_indexed_y_3
        def absolute_indexed_y_3():
            return f((adh<<8)|adl)
        return absolute_indexed_y_0

    def indexed_indirect(f):
        def indexed_indirect():
            nonlocal pc, bal
            bal = fetch(pc); pc += 1
            return indexed_indirect_1
        def indexed_indirect_1():
            return indexed_indirect_2
        def indexed_indirect_2():
            nonlocal adl
            adl = fetch((bal+x)&0xFF)
            return indexed_indirect_3
        def indexed_indirect_3():
            nonlocal adh
            adh = fetch((bal+x+1)&0xFF)
            return indexed_indirect_4
        def indexed_indirect_4():
            return f((adh<<8)|adl)
        return indexed_indirect

    def indirect_indexed(f):
        def indirect_indexed_0():
            nonlocal pc, ial
            ial = fetch(pc); pc += 1
            return indirect_indexed_1
        def indirect_indexed_1():
            nonlocal bal
            bal = fetch(ial)
            return indirect_indexed_2
        def indirect_indexed_2():
            nonlocal bah
            bah = fetch((ial+1)&0xFF)
            return indirect_indexed_3
        def indirect_indexed_3():
            nonlocal adl, adh
            adl = bal + y
            adh = (bah+(adl>>8)) & 0xFF
            if adl & 0x100: # extra cycle for page crossing (carry on low byte addition)
                adl &= 0xFF
                return indirect_indexed_4
            return f((adh<<8)|adl)
        def indirect_indexed_4():
            return f((adh<<8)|adl)
        return indirect_indexed_0

    def indirect_indexed_always_extra_cycle(f):
        def indirect_indexed_0():
            nonlocal pc, ial
            ial = fetch(pc); pc += 1
            return indirect_indexed_1
        def indirect_indexed_1():
            nonlocal bal
            bal = fetch(ial)
            return indirect_indexed_2
        def indirect_indexed_2():
            nonlocal bah
            bah = fetch((ial+1)&0xFF)
            return indirect_indexed_3
        def indirect_indexed_3():
            nonlocal adl, adh
            adl = bal + y
            adh = (bah+(adl>>8)) & 0xFF
            adl &= 0xFF
            return indirect_indexed_4
        def indirect_indexed_4():
            return f((adh<<8)|adl)
        return indirect_indexed_0

    def relative_branch(f):   
        def relative_branch_0():
            nonlocal pc, off
            off = signed8(fetch(pc)); pc += 1
            return next_op if f() else relative_branch_1
        def relative_branch_1():
            nonlocal pc, off
            off += pc & 0xFF # remember this is signed!
            pc ^= (pc^off) & 0xFF
            return relative_branch_2 if off & 0x100 else next_op
        def relative_branch_2():
            nonlocal pc
            pc += off & ~0xFF # add carry (may be negative)
            return next_op
        return relative_branch_0

    def fetch_op(f):
        def fetch_op_0(addr):
            f(fetch(addr))
            return next_op
        return fetch_op_0

    def store_op(f):
        def store_op_0(addr):
            return store(addr, f()) or next_op
        return store_op_0

    def fetch_modify_store_op(f):
        def fetch_modify_store_op_0(_addr):
            nonlocal addr, data
            addr = _addr
            data = fetch(addr)
            return fetch_modify_store_op_1
        def fetch_modify_store_op_1():
            nonlocal data
            data = f(data)
            return fetch_modify_store_op_2
        def fetch_modify_store_op_2():
            return store(addr, data) or next_op
        return fetch_modify_store_op_0
    
    def modify_a_op(f):
        def modify_a_op_0():
            nonlocal a
            a = f(a)
            return next_op
        return modify_a_op_0

    # Instructions

    # ADC (ADd with Carry)
    @fetch_op
    def adc(data):
        nonlocal a, p
        r = data + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(data^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF

    # AND (bitwise AND with accumulator)
    @fetch_op
    def and_(data):
        nonlocal a, p
        a &= data
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)

    # ASL (Arithmetic Shift Left)
    def asl(data):
        nonlocal p
        r = data << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        return r & 0xFF
    asl_accumulator = modify_a_op(asl)
    asl = fetch_modify_store_op(asl)

    # BIT (test BITs)
    @fetch_op
    def bit(data):
        nonlocal p
        p = (p&MASK_NVZ) | (data & N) | (data & V) | (0x00 if (data&a) else Z)

    # Branch Instructions
    # BPL (Branch on PLus)
    bpl_implied = relative_branch(lambda: p & N)
    # BMI (Branch on MInus)
    bmi_implied = relative_branch(lambda:~p & N)
    # BVS (Branch on oVerflow Set)
    bvs_implied = relative_branch(lambda:~p & V)
    # BVC (Branch on oVerflow Clear)
    bvc_implied = relative_branch(lambda: p & V)
    # BCS (Branch on Carry Set)
    bcs_implied = relative_branch(lambda:~p & C)
    # BCC (Branch on Carry Clear)
    bcc_implied = relative_branch(lambda: p & C)
    # BEQ (Branch on EQual)    
    beq_implied = relative_branch(lambda:~p & Z)
    # BNE (Branch on Not Equal)
    bne_implied = relative_branch(lambda: p & Z)

    # BRK (BReaK)
    def brk_implied():
        nonlocal p, ial, iah
        p |= B  # NOTE: B flag won't be reset after nested interrupt (via PHP) -- is this okay?
        if p & I:
            return next_op
        ial = 0xFE
        iah = 0xFF
        return brk_implied_1
    def brk_implied_1():
        fetch(pc) # discard
        return brk_implied_2
    def brk_implied_2():
        nonlocal s
        store(STACK_OFFSET+s, pc>>8)
        s = (s-1) & 0xFF
        return brk_implied_3
    def brk_implied_3():
        nonlocal s
        store(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        return brk_implied_4
    def brk_implied_4():
        nonlocal s
        store(STACK_OFFSET+s, p|U)
        s = (s-1) & 0xFF
        return brk_implied_5
    def brk_implied_5():
        nonlocal adl
        adl = fetch(0xFF00|ial)
        return brk_implied_6
    def brk_implied_6():
        nonlocal adh, pc
        adh = fetch(0xFF00|iah)
        pc = (adh<<8) | adl
        return next_op
    def brk_implied_stop_execution():  # for unit testing / debugging
        nonlocal pc
        pc -= 1 # REMOVE ME: this is to avoid updating tests after breaking ops into cycles
        raise VMStop()
    
    # CMP (CoMPare accumulator)
    @fetch_op
    def cmp(data):
        nonlocal p
        r = a - data
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)

    # CPX (ComPare X register)
    @fetch_op
    def cpx(data):
        nonlocal p
        r = x - data
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)

    # CPY (ComPare Y register)
    @fetch_op
    def cpy(data):
        nonlocal p
        r = y - data
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)

    # DEC (DECrement memory)
    @fetch_modify_store_op
    def dec(data):
        nonlocal p
        r = data - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        return r & 0xFF

    # EOR (bitwise Exclusive OR)
    @fetch_op
    def eor(data):
        nonlocal a, p
        a ^= data
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)

    # Flag (Processor Status) Instructions
    # CLC (CLear Carry)
    def clc_implied():
        nonlocal p
        p &= MASK_C
        return next_op
    # SEC (SEt Carry)
    def sec_implied():
        nonlocal p
        p |= C
        return next_op
    # CLI (CLear Interrupt)
    def cli_implied():
        nonlocal p
        p &= MASK_I
        return next_op
    # SEI (SEt Interrupt)
    def sei_implied():
        nonlocal p
        p |= I
        return next_op
    # CLV (CLear oVerflow)
    def clv_implied():
        nonlocal p
        p &= MASK_V
        return next_op
    # CLD (CLear Decimal)
    def cld_implied():
        nonlocal p
        p &= MASK_D
        return next_op
    # SED (SEt Decimal)
    def sed_implied():
        nonlocal p
        p |= D
        return next_op
    
    # INC (INCrement memory)
    @fetch_modify_store_op
    def inc(data):
        nonlocal p
        r = data + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        return r & 0xFF

    # JMP (JuMP)
    def jmp_absolute():
        nonlocal pc, adl
        adl = fetch(pc); pc += 1
        return jmp_absolute_1
    def jmp_absolute_1():
        nonlocal pc, adh
        adh = fetch(pc); pc += 1
        pc = (adh<<8) | adl
        return next_op
    def jmp_indirect():
        nonlocal pc, ial
        ial = fetch(pc); pc += 1
        return jmp_indirect_1
    def jmp_indirect_1():
        nonlocal pc, iah
        iah = fetch(pc); pc += 1
        return jmp_indirect_2
    def jmp_indirect_2():
        nonlocal adl
        adl = fetch((iah<<8)|ial)
        return jmp_indirect_3
    def jmp_indirect_3():
        nonlocal adh, pc
        adh = fetch((iah<<8)|((ial+1)&0xFF)) # do not cross page
        pc = (adh<<8) | adl
        return next_op
    
    # JSR (Jump to SubRoutine)
    def jsr_absolute():
        nonlocal pc, adl
        adl = fetch(pc); pc += 1
        return jsr_absolute_1
    def jsr_absolute_1():
        return jsr_absolute_2
    def jsr_absolute_2():
        nonlocal s
        store(STACK_OFFSET+s, pc>>8)
        s = (s-1) & 0xFF
        return jsr_absolute_3
    def jsr_absolute_3():
        nonlocal s
        store(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        return jsr_absolute_4
    jsr_absolute_4 = jmp_absolute_1

    # LDA (LoaD Accumulator)
    @fetch_op
    def lda(data):
        nonlocal a, p
        a = data
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)

    # LDX (LoaD X register)
    @fetch_op
    def ldx(data):
        nonlocal x, p
        x = data
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)

    # LDY (LoaD Y register)
    @fetch_op
    def ldy(data):
        nonlocal y, p
        y = data
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)

    # LSR (Logical Shift Right)
    def lsr(data):
        nonlocal p
        r = data >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (data&C)
        return r
    lsr_accumulator = modify_a_op(lsr)
    lsr = fetch_modify_store_op(lsr)

    # NOP (No OPeration)
    def nop_implied():
        return next_op

    # ORA (bitwise OR with Accumulator)
    @fetch_op
    def ora(data):
        nonlocal a, p
        a |= data
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)

    # Register Instructions
    # TAX (Transfer A to X)
    def tax_implied():
        nonlocal x, p
        x = a
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        return next_op
    # TXA (Transfer X to A)
    def txa_implied():
        nonlocal a, p
        a = x
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        return next_op
    # DEX (DEcrement X)
    def dex_implied():
        nonlocal x, p
        x = (x - 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        return next_op
    # INX (INcrement X)
    def inx_implied():
        nonlocal x, p
        x = (x + 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        return next_op
    # TAY (Transfer A to Y)
    def tay_implied():
        nonlocal y, p
        y = a
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        return next_op
    # TYA (Transfer Y to A)
    def tya_implied():
        nonlocal a, p
        a = y
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        return next_op
    # DEY (DEcrement Y)
    def dey_implied():
        nonlocal y, p
        y = (y - 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        return next_op
    # INY (INcrement Y)
    def iny_implied():
        nonlocal y, p
        y = (y + 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        return next_op

    # ROL (ROtate Left)
    def rol(data):
        nonlocal p
        r = (data<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        return r & 0xFF
    rol_accumulator = modify_a_op(rol)
    rol = fetch_modify_store_op(rol)

    # ROR (ROtate Right)
    def ror(data):
        nonlocal p
        r = (data>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (data&C)
        return r & 0xFF
    ror_accumulator = modify_a_op(ror)
    ror = fetch_modify_store_op(ror)

    # RTI (ReTurn from Interrupt)
    def rti_implied():
        nonlocal pc
        fetch(pc); pc += 1 # discard
        return rti_implied_1
    def rti_implied_1():
        nonlocal s
        fetch(STACK_OFFSET+s) # discard
        s = (s+1) & 0xFF
        return rti_implied_2
    def rti_implied_2():
        nonlocal p, s
        p = fetch(STACK_OFFSET+s) & MASK_B | U
        s = (s+1) & 0xFF
        return rti_implied_3
    def rti_implied_3():
        nonlocal pc, s
        pc = fetch(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        return rti_implied_4
    def rti_implied_4():
        nonlocal pc, s
        pc |= fetch(STACK_OFFSET+s) << 8
        return next_op

    # RTS (ReTurn from Subroutine)
    def rts_implied():
        nonlocal pc
        fetch(pc); pc += 1 # discard
        return rts_implied_1
    def rts_implied_1():
        nonlocal s
        fetch(STACK_OFFSET+s) # discard
        s = (s+1) & 0xFF
        return rts_implied_2
    def rts_implied_2():
        nonlocal pc, s
        pc = fetch(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        return rts_implied_3
    def rts_implied_3():
        nonlocal pc
        pc |= fetch(STACK_OFFSET+s) << 8
        return rts_implied_4
    def rts_implied_4():
        nonlocal pc
        fetch(pc); pc += 1 # discard
        return next_op

    # SBC (SuBtract with Carry)
    @fetch_op
    def sbc(data):
        nonlocal a, p
        r = a - data - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(data^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF

    # STA (STore Accumulator)
    @store_op
    def sta():
        return a
    
    # Stack Instructions
    # TXS (Transfer X to Stack ptr)
    def txs_implied():
        nonlocal s
        s = x
        return next_op
    # TSX (Transfer Stack ptr to X)
    def tsx_implied():
        nonlocal x, p
        x = s
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        return next_op
    # PHA (PusH Accumulator)
    def pha_implied():
        return pha_implied_1
    def pha_implied_1():
        nonlocal s
        store(STACK_OFFSET+s, a)
        s = (s-1) & 0xFF
        return next_op
    # PLA (PuLl Accumulator)
    def pla_implied():
        return pla_implied_1
    def pla_implied_1():
        return pla_implied_2
    def pla_implied_2():
        nonlocal s, a, p
        s = (s+1) & 0xFF
        a = fetch(STACK_OFFSET+s)
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        return next_op
    # PHP (PusH Processor status)
    def php_implied():
        return php_implied_1
    def php_implied_1():
        nonlocal s
        store(STACK_OFFSET+s, p|B|U)
        s = (s-1) & 0xFF
        return next_op
    # PLP (PuLl Processor status)
    def plp_implied():
        return plp_implied_1
    def plp_implied_1():
        return plp_implied_2
    def plp_implied_2():
        nonlocal s, p
        s = (s+1) & 0xFF
        p = fetch(STACK_OFFSET+s) & MASK_B | U  # always clear break bit, set unused bit 
        return next_op

    # STX (STore X register)
    @store_op
    def stx():
        return x

    # STY (STore Y register)
    @store_op
    def sty():
        return y

    def build_undefined_op(opcode):
        def undefined_op(pc):
            raise ValueError('Undefined opcode {}'.format(opcode))
        return undefined_op

    ops = [build_undefined_op(opcode) for opcode in range(0x100)]
    ops[0x69] = immediate(adc)
    ops[0x65] = zero_page(adc)
    ops[0x75] = zero_page_indexed_x(adc)
    ops[0x6d] = absolute(adc)
    ops[0x7d] = absolute_indexed_x(adc)
    ops[0x79] = absolute_indexed_y(adc)
    ops[0x61] = indexed_indirect(adc)
    ops[0x71] = indirect_indexed(adc)
    ops[0x29] = immediate(and_)
    ops[0x25] = zero_page(and_)
    ops[0x35] = zero_page_indexed_x(and_)
    ops[0x2d] = absolute(and_)
    ops[0x3d] = absolute_indexed_x(and_)
    ops[0x39] = absolute_indexed_y(and_)
    ops[0x21] = indexed_indirect(and_)
    ops[0x31] = indirect_indexed(and_)
    ops[0x0a] = asl_accumulator
    ops[0x06] = zero_page(asl)
    ops[0x16] = zero_page_indexed_x(asl)
    ops[0x0e] = absolute(asl)
    ops[0x1e] = absolute_indexed_x_always_extra_cycle(asl)
    ops[0x24] = zero_page(bit)
    ops[0x2c] = absolute(bit)
    ops[0x10] = bpl_implied
    ops[0x30] = bmi_implied
    ops[0x50] = bvc_implied
    ops[0x70] = bvs_implied
    ops[0x90] = bcc_implied
    ops[0xb0] = bcs_implied
    ops[0xd0] = bne_implied
    ops[0xf0] = beq_implied
    ops[0x00] = brk_implied_stop_execution if stop_on_brk else brk_implied
    ops[0xc9] = immediate(cmp)
    ops[0xc5] = zero_page(cmp)
    ops[0xd5] = zero_page_indexed_x(cmp)
    ops[0xcd] = absolute(cmp)
    ops[0xdd] = absolute_indexed_x(cmp)
    ops[0xd9] = absolute_indexed_y(cmp)
    ops[0xc1] = indexed_indirect(cmp)
    ops[0xd1] = indirect_indexed(cmp)
    ops[0xe0] = immediate(cpx)
    ops[0xe4] = zero_page(cpx)
    ops[0xec] = absolute(cpx)
    ops[0xc0] = immediate(cpy)
    ops[0xc4] = zero_page(cpy)
    ops[0xcc] = absolute(cpy)
    ops[0xc6] = zero_page(dec)
    ops[0xd6] = zero_page_indexed_x(dec)
    ops[0xce] = absolute(dec)
    ops[0xde] = absolute_indexed_x_always_extra_cycle(dec)
    ops[0x49] = immediate(eor)
    ops[0x45] = zero_page(eor)
    ops[0x55] = zero_page_indexed_x(eor)
    ops[0x4d] = absolute(eor)
    ops[0x5d] = absolute_indexed_x(eor)
    ops[0x59] = absolute_indexed_y(eor)
    ops[0x41] = indexed_indirect(eor)
    ops[0x51] = indirect_indexed(eor)
    ops[0x18] = clc_implied
    ops[0x38] = sec_implied
    ops[0x58] = cli_implied
    ops[0x78] = sei_implied
    ops[0xb8] = clv_implied
    ops[0xd8] = cld_implied
    ops[0xf8] = sed_implied
    ops[0xe6] = zero_page(inc)
    ops[0xf6] = zero_page_indexed_x(inc)
    ops[0xee] = absolute(inc)
    ops[0xfe] = absolute_indexed_x_always_extra_cycle(inc)
    ops[0x4c] = jmp_absolute
    ops[0x6c] = jmp_indirect
    ops[0x20] = jsr_absolute
    ops[0xa9] = immediate(lda)
    ops[0xa5] = zero_page(lda)
    ops[0xb5] = zero_page_indexed_x(lda)
    ops[0xad] = absolute(lda)
    ops[0xbd] = absolute_indexed_x(lda)
    ops[0xb9] = absolute_indexed_y(lda)
    ops[0xa1] = indexed_indirect(lda)
    ops[0xb1] = indirect_indexed(lda)
    ops[0xa2] = immediate(ldx)
    ops[0xa6] = zero_page(ldx)
    ops[0xb6] = zero_page_indexed_y(ldx)
    ops[0xae] = absolute(ldx)
    ops[0xbe] = absolute_indexed_y(ldx)
    ops[0xa0] = immediate(ldy)
    ops[0xa4] = zero_page(ldy)
    ops[0xb4] = zero_page_indexed_x(ldy)
    ops[0xac] = absolute(ldy)
    ops[0xbc] = absolute_indexed_x(ldy)
    ops[0x4a] = lsr_accumulator
    ops[0x46] = zero_page(lsr)
    ops[0x56] = zero_page_indexed_x(lsr)
    ops[0x4e] = absolute(lsr)
    ops[0x5e] = absolute_indexed_x_always_extra_cycle(lsr)
    ops[0xea] = nop_implied
    ops[0x09] = immediate(ora)
    ops[0x05] = zero_page(ora)
    ops[0x15] = zero_page_indexed_x(ora)
    ops[0x0d] = absolute(ora)
    ops[0x1d] = absolute_indexed_x(ora)
    ops[0x19] = absolute_indexed_y(ora)
    ops[0x01] = indexed_indirect(ora)
    ops[0x11] = indirect_indexed(ora)
    ops[0xaa] = tax_implied
    ops[0x8a] = txa_implied
    ops[0xca] = dex_implied
    ops[0xe8] = inx_implied
    ops[0xa8] = tay_implied
    ops[0x98] = tya_implied
    ops[0x88] = dey_implied
    ops[0xc8] = iny_implied
    ops[0x2a] = rol_accumulator
    ops[0x26] = zero_page(rol)
    ops[0x36] = zero_page_indexed_x(rol)
    ops[0x2e] = absolute(rol)
    ops[0x3e] = absolute_indexed_x_always_extra_cycle(rol)
    ops[0x6a] = ror_accumulator
    ops[0x66] = zero_page(ror)
    ops[0x76] = zero_page_indexed_x(ror)
    ops[0x6e] = absolute(ror)
    ops[0x7e] = absolute_indexed_x_always_extra_cycle(ror)
    ops[0x40] = rti_implied
    ops[0x60] = rts_implied
    ops[0xe9] = immediate(sbc)
    ops[0xe5] = zero_page(sbc)
    ops[0xf5] = zero_page_indexed_x(sbc)
    ops[0xed] = absolute(sbc)
    ops[0xfd] = absolute_indexed_x(sbc)
    ops[0xf9] = absolute_indexed_y(sbc)
    ops[0xe1] = indexed_indirect(sbc)
    ops[0xf1] = indirect_indexed(sbc)
    ops[0x85] = zero_page(sta)
    ops[0x95] = zero_page_indexed_x(sta)
    ops[0x8d] = absolute(sta)
    ops[0x9d] = absolute_indexed_x_always_extra_cycle(sta)
    ops[0x99] = absolute_indexed_y_always_extra_cycle(sta)
    ops[0x81] = indexed_indirect(sta)
    ops[0x91] = indirect_indexed_always_extra_cycle(sta)
    ops[0x9a] = txs_implied
    ops[0xba] = tsx_implied
    ops[0x48] = pha_implied
    ops[0x68] = pla_implied
    ops[0x08] = php_implied
    ops[0x28] = plp_implied
    ops[0x86] = zero_page(stx)
    ops[0x96] = zero_page_indexed_y(stx)
    ops[0x8e] = absolute(stx)
    ops[0x84] = zero_page(sty)
    ops[0x94] = zero_page_indexed_x(sty)
    ops[0x8c] = absolute(sty)

    _next_op = next_op
    def tick():
        nonlocal t, _next_op
        _next_op = _next_op()
        t += 1
    def debug_tick():  # REMOVE ME
        nonlocal t, _next_op
        start_of_op = _next_op is next_op
        _next_op = _next_op()
        t += 1
        return start_of_op

    def trigger_interrupt(new_pcl, new_pch):
        nonlocal ial, iah
        nonlocal _next_op
        ial = new_pcl
        iah = new_pch
        _next_op = trigger_interrupt_0
    def trigger_interrupt_0():
        fetch(pc) # discard
        return brk_implied_1

    def trigger_nmi():
        trigger_interrupt(0xFA, 0xFB)

    def trigger_reset():
        trigger_interrupt(0xFC, 0xFD)

    def trigger_irq():
        if p & I:
            return
        trigger_interrupt(0xFE, 0xFF)

    def transfer_page_to_oam(page_num):
        nonlocal adh, adl
        adh = page_num
        adl = 0x00
        return transfer_page_to_oam_wait_for_writes_to_complete
    def transfer_page_to_oam_wait_for_writes_to_complete(): # I don't really get what "writes" this is referring to
        if t & 1: # if current cycle is odd, next cycle will be even / read
            return transfer_page_to_oam_read
        return transfer_page_to_oam_sync
    def transfer_page_to_oam_sync():
        return transfer_page_to_oam_read
    def transfer_page_to_oam_read():
        nonlocal adl, data
        data = fetch((adh<<8)|adl); adl += 1
        return transfer_page_to_oam_write
    def transfer_page_to_oam_write():
        nonlocal adl
        write_oam(data)
        if adl <= 0xFF:
            return transfer_page_to_oam_read
        adl &= 0xFF
        return next_op
    
    def connect(cpu_read, cpu_write, ppu_write_oam):
        nonlocal fetch, store, write_oam
        fetch = cpu_read
        store = cpu_write
        write_oam = ppu_write_oam

    return (
        debug_tick,
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
        prg_rom_banks = [bytearray(ines_file.read(0x4000)) for _ in range(num_prg_rom_banks)]

        # Extract  8 KB CHR-ROM banks
        chr_rom_banks = [bytearray(ines_file.read(0x2000)) for _ in range(num_chr_rom_banks)]

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
    def __init__(self, cart, cpu_regs=None, t=0, print_cpu_log=False, pal=None):
        self.out_pixels = array.array('B', (0 for _ in range(341*262)))
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
            create_ppu_funcs(self.out_pixels)
        self.cpu_tick,\
        self.cpu_trigger_nmi,\
        self.cpu_trigger_reset,\
        self.cpu_trigger_irq,\
        self.cpu_transfer_page_to_oam,\
        self.cpu_connect,\
        self.cpu_inspect_regs = \
            create_cpu_funcs(
                regs=cpu_regs,
                stop_on_brk=False,
                t=t)
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
        import pygame
        pygame.display.init()
        screen = pygame.display.set_mode(size=(341, 262)) # , flags=0, depth=0, display=0, vsync=0)
        rgbs = [
            (
                self.pal[i+0],
                self.pal[i+1],
                self.pal[i+2]
            )
            for i in range(0, 192, 3)
        ]

        t = ppu_t = self.initial_t
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
                    if self.cpu_tick():
                        print(log_line)
                    t += 1
            else:
                frame_duration = int(113.667*262)
                frame_start = t
                self.cpu_trigger_reset()
                pygame.event.clear()
                while not pygame.event.get(eventtype=pygame.QUIT):
                    if t <= 29658:
                        self.cpu_tick(); t += 1
                        ppu_t += 3
                    else: # if True:
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
                        
                        self.ppu_tick(); ppu_t += 1
                        self.cpu_tick(); t += 1
                        self.ppu_tick(); ppu_t += 1
                        self.ppu_tick(); ppu_t += 1

                        if (t - frame_start) >= frame_duration:
                            with pygame.PixelArray(screen) as screen_pixels:
                                for y in range(262):
                                    for x in range(341):
                                        screen_pixels[x,y] = rgbs[self.out_pixels[x+y*341]]
                            pygame.display.flip()
                            print(f"flip t = {t}")
                            frame_start = t
        except VMStop:
            pygame.display.quit()
        return t  # clean exit
        
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Curt NES v0.0.0')
    parser.add_argument('rom')
    parser.add_argument('--print-cart-config', action='store_true')
    parser.add_argument('--print-cpu-log', action='store_true')
    parser.add_argument('--pal-filepath', '--pal')
    parser.add_argument('--dump-chr-rom', action='store_true')
    args = parser.parse_args()

    cart = Cart.from_file(args.rom)

    if args.print_cart_config:
        cart.print_config()
        exit(0)

    if args.dump_chr_rom:
        import sys
        sys.stdout.buffer.write(cart.chr_rom_banks[0])
        exit(0)

    if args.pal_filepath:
        pal = load_pal_from_file(args.pal_filepath)
    else:
        pal = None

    # Currently overriding registers + time for nestest.nes -- not sure why it doesn't align with documented initial values?
    nes = NES(
        cart,
        #cpu_regs=(0xC000, 0xFD, 0x00, 0x00, 0x00, 0x24),
        #t=7,
        print_cpu_log=args.print_cpu_log,
        pal=pal
    )
    nes.play()
