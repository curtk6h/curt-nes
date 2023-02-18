#!/usr/local/bin/python3

# TODO:
# * add tests for ppu funcs
# * connect ppu to cpu/mapper
# * implement dma
# * implement rendering
# * implement palette ram
# * tick / figure out initialization/syncing w/ cpu
# * remove unused constants etc
# * add pygame and draw to screen
# * rename: crazyNES, coolNES, nneess, HI-NES (crown icon)
# * default status to unused = 1 and removed redundant sets
# * lots of cleanup / refactoring
#   * break instructions into single cycles
#   * consider exposing registers to inspection (ie. use vector instead of individual vars)
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
    lambda mapper, pc, operands: '',
    lambda mapper, pc, operands: ' #${:02X}'   .format(operands[1]),
    lambda mapper, pc, operands: ' A',
    lambda mapper, pc, operands: ' ${:02X} = {:02X}'.format(operands[1], mapper.cpu_read(operands[1])),
    lambda mapper, pc, operands: ' ${:02X},X'  .format(operands[1]),
    lambda mapper, pc, operands: ' ${:02X},Y'  .format(operands[1]),
    lambda mapper, pc, operands: ' ${:04X}'    .format(operands[1]|(operands[2]<<8)),
    lambda mapper, pc, operands: ' ${:04X},X'  .format(operands[1]|(operands[2]<<8)),
    lambda mapper, pc, operands: ' ${:04X},Y'  .format(operands[1]|(operands[2]<<8)),
    lambda mapper, pc, operands: ' (${:02X},X)'.format(operands[1]),
    lambda mapper, pc, operands: ' (${:02X}),Y'.format(operands[1]),
    lambda mapper, pc, operands: ' (${:04X})'  .format(operands[1]|(operands[2]<<8)),
    lambda mapper, pc, operands: ' ${:02X}'    .format(pc+2+signed8(operands[1])),
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

class VMStop(Exception):
    pass

class Mapper(object):
    mapper_num = 0

    def __init__(self, ram, vram, prg_rom_banks, prg_ram, chr_rom_banks, chr_ram, ppu_read, ppu_write, apu_read, apu_write, nt_mirroring=NT_MIRRORING_VERTICAL):
        self.ram            = ram
        self.vram           = vram
        self.prg_rom_banks  = prg_rom_banks
        self.prg_rom_bank_0 = prg_rom_banks[0]
        self.prg_rom_bank_1 = prg_rom_banks[1]
        self.prg_ram        = prg_ram
        self.chr_rom_banks  = chr_rom_banks
        self.chr_rom_bank   = chr_rom_banks and chr_rom_banks[0]
        self.chr_ram        = chr_ram
        self._init_cpu_io(ppu_read, ppu_write, apu_read, apu_write)
        self._init_ppu_io(nt_mirroring)

    def cpu_read(self, addr):
        return self.reg_readers[addr](addr)

    def cpu_write(self, addr, value):
        self.reg_writers[addr](addr, value)

    def ppu_read(self, addr):
        return self.ppu_readers[addr](addr)

    def ppu_write(self, addr, value):
        self.ppu_writers[addr](addr, value)
    
    def set_nametable_mirroring(self, nt_mirroring):
        self._init_ppu_io(nt_mirroring)

    def _init_cpu_io(self, ppu_read, ppu_write, apu_read, apu_write):
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

        ram = self.ram
        prg_ram = self.prg_ram
        self.reg_readers = (
            # ram
            [lambda addr: ram[addr]] * 0x0800 +
            [lambda addr: ram[addr-0x0800]] * 0x0800 + # mirror
            [lambda addr: ram[addr-0x1000]] * 0x0800 + # mirror
            [lambda addr: ram[addr-0x1800]] * 0x0800 + # mirror
            # ppu
            [lambda addr: ppu_read((addr-0x2000))] * 8 +
            [lambda addr: ppu_read((addr-0x2000)&7)] * (0x2000-8) + # mirrors
            # apu
            [lambda addr: apu_read((addr-0x4000))] * 0x0020 +
            # cart
            [lambda addr: 0] * 0x1FE0 + # expansion rom?
            [lambda addr: prg_ram[addr-0x6000]] * 0x2000 + # prg ram
            [lambda addr: self.prg_rom_bank_0[addr-0x8000]] * 0x4000 + # prg rom bank 0
            [lambda addr: self.prg_rom_bank_1[addr-0xC000]] * 0x4000   # prg rom bank 1
        )
        assert len(self.reg_readers) == 0x10000

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
            pass # self.prg_rom_bank_0[addr-0x8000] = value
        def write_prg_rom_bank_1(addr, value):
            pass # self.prg_rom_bank_1[addr-0xC000] = value
        self.reg_writers = (
            # ram
            [write_ram_0] * 0x0800 +
            [write_ram_1] * 0x0800 +
            [write_ram_2] * 0x0800 +
            [write_ram_3] * 0x0800 +
            # ppu
            [lambda addr, value: ppu_write((addr-0x2000)&7, value)] * 0x2000 + # TODO: expand this for mirrors to factor out modulus
            # apu
            [lambda addr, value: apu_write((addr-0x4000), value)] * 0x0020 +
            # cart
            [lambda addr, value: None] * 0x1FE0 + # expansion rom?
            [write_prg_ram] * 0x2000 + # prg ram
            [write_prg_rom_bank_0] * 0x4000 + # prg rom bank 0
            [write_prg_rom_bank_1] * 0x4000   # prg rom bank 1
        )
        assert len(self.reg_writers) == 0x10000

    def _init_ppu_io(self, nt_mirroring):
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

        def read_pattern_tables_from_chr_rom(addr):
            return self.chr_rom_bank[addr]
        def read_pattern_tables_from_chr_ram(addr):
            return self.chr_ram[addr]
        def read_palette_ram_indexes(addr):
            idx = (addr-0x3F00) & 0x1F  # TODO: implement this
        def read_nametable_0_0(addr):
            return self.vram[(addr-(0x2000-0x0000))&0x0FFF]
        def read_nametable_0_1(addr):
            return self.vram[(addr-(0x2000-0x0400))&0x0FFF]
        def read_nametable_0_2(addr):
            return self.vram[(addr-(0x2000-0x0800))&0x0FFF]
        def read_nametable_0_3(addr):
            return self.vram[(addr-(0x2000-0x0C00))&0x0FFF]
        def read_nametable_1_0(addr):
            return self.vram[(addr-(0x2400-0x0000))&0x0FFF]
        def read_nametable_1_1(addr):
            return self.vram[(addr-(0x2400-0x0400))&0x0FFF]
        def read_nametable_1_2(addr):
            return self.vram[(addr-(0x2400-0x0800))&0x0FFF]
        def read_nametable_1_3(addr):
            return self.vram[(addr-(0x2400-0x0C00))&0x0FFF]
        def read_nametable_2_0(addr):
            return self.vram[(addr-(0x2800-0x0000))&0x0FFF]
        def read_nametable_2_1(addr):
            return self.vram[(addr-(0x2800-0x0400))&0x0FFF]
        def read_nametable_2_2(addr):
            return self.vram[(addr-(0x2800-0x0800))&0x0FFF]
        def read_nametable_2_3(addr):
            return self.vram[(addr-(0x2800-0x0C00))&0x0FFF]
        def read_nametable_3_0(addr):
            return self.vram[(addr-(0x2C00-0x0000))&0x0FFF]
        def read_nametable_3_1(addr):
            return self.vram[(addr-(0x2C00-0x0400))&0x0FFF]
        def read_nametable_3_2(addr):
            return self.vram[(addr-(0x2C00-0x0800))&0x0FFF]
        def read_nametable_3_3(addr):
            return self.vram[(addr-(0x2C00-0x0C00))&0x0FFF]

        if nt_mirroring == NT_MIRRORING_VERTICAL:
            read_nametable_0 = read_nametable_0_0
            read_nametable_1 = read_nametable_1_1
            read_nametable_2 = read_nametable_2_0
            read_nametable_3 = read_nametable_3_1
        elif nt_mirroring == NT_MIRRORING_HORIZONTAL:
            read_nametable_0 = read_nametable_0_0
            read_nametable_1 = read_nametable_1_0
            read_nametable_2 = read_nametable_2_2
            read_nametable_3 = read_nametable_3_2
        elif nt_mirroring == NT_MIRRORING_SINGLE_SCREEN:
            read_nametable_0 = read_nametable_0_0
            read_nametable_1 = read_nametable_1_0
            read_nametable_2 = read_nametable_2_0
            read_nametable_3 = read_nametable_3_0
        elif nt_mirroring == NT_MIRRORING_FOUR_SCREEN:
            read_nametable_0 = read_nametable_0_0
            read_nametable_1 = read_nametable_1_1
            read_nametable_2 = read_nametable_2_2
            read_nametable_3 = read_nametable_3_3
        else:
            raise ValueError(f'Nametable mirroring {nt_mirroring} not supported')

        # Not sure what the default rules for "no mapper" should be (TODO: figure it out!);
        # this assumes that either one of CHR-ROM or CHR-RAM is present at a time 
        # and other configurations require a custom mapper
        if self.chr_ram and self.chr_rom_bank:
            raise ValueError('CHR-ROM/RAM configuration not supported by default mapper (both are present)')

        if self.chr_ram:
            read_pattern_table_0 = read_pattern_tables_from_chr_ram
            read_pattern_table_1 = read_pattern_tables_from_chr_ram
        else:
            read_pattern_table_0 = read_pattern_tables_from_chr_rom
            read_pattern_table_1 = read_pattern_tables_from_chr_rom

        self.ppu_readers = (
            [read_pattern_table_0] * (0x1000//0x100) +
            [read_pattern_table_1] * (0x1000//0x100) +
            [read_nametable_0]     * (0x0400//0x100) +
            [read_nametable_1]     * (0x0400//0x100) +
            [read_nametable_2]     * (0x0400//0x100) +
            [read_nametable_3]     * (0x0400//0x100) +
            [read_nametable_0]     * (0x0400//0x100) +
            [read_nametable_1]     * (0x0400//0x100) +
            [read_nametable_2]     * (0x0400//0x100) +
            [read_nametable_3]     * (0x0300//0x100) +
            [read_palette_ram_indexes]
        )
        
        assert(len(self.ppu_readers)==0x4000/0x100)

mappers = {
    mapper.mapper_num: mapper
    for mapper in (Mapper,)
}

def signed8(value):
    return ((value&0xFF)^0x80) - 0x80

class PPU(object):
    def __init__(self):
        self.oam = array.array('B', (0 for _ in range(0x100)))
        self.reg_io_value = 0x00
        self.reg_io_write_state = 0
        self.ppu_status = 0x00
        self.ppu_ctrl = 0x00
        self.ppu_mask = 0x00
        self.ppu_addr = 0x0000  # 15 bits
        self.tmp_addr = 0x0000
        self.oam_addr = 0x00
        self.fine_x_scroll = 0x0  # 3 bits
        self.frame_num = 0
        self.scanline_idx = 0
        self.t = 0
        self._init_reg_io()

    def _init_reg_io(self):
        def read_nothing():
            pass
        def read_ppu_status():
            # VSO.....
            self.reg_io_value ^= (self.reg_io_value^self.ppu_status) & 0xE0
            self.ppu_status &= 0x7F  # clear vblank flag
        def read_oam_data():
            self.reg_io_value ^= (self.reg_io_value^self.oam[self.oam_addr]) & 0x00FF
            self.oam_addr = (self.oam_addr+1) & 0xFF  # TODO: reads during v/forced blanking do not increment oamaddr
        def read_ppu_data():
            self.reg_io_value = self.mapper.ppu_read(self.ppu_addr)
            self.ppu_addr = (self.ppu_addr + PPU_ADDR_INCREMENTS[self.ppu_ctrl&0x04]) & 0xFFFF

        def write_nothing():
            pass
        def write_ppu_ctrl():
            if self.t < 30000:
                return  # TODO: figure out reg_io_value / reg_io_write_state
            gen_nmi_immediately = self.ppu_status & self.reg_io_value & ~self.ppu_ctrl & 0x80
            self.ppu_ctrl = self.reg_io_value & 0xFF
            # TODO: figure out if this really happens here or on copy to ppu_addr?
            self.tmp_addr ^= (self.tmp_addr ^ (self.reg_io_value<<10)) & 0x0C00
            if gen_nmi_immediately:
                raise ValueError("TODO: generate NMI!")  # TODO: immediately generate an NMI! still set ppuctrl value?
        def write_ppu_mask():
            self.ppu_mask = self.reg_io_value & 0xFF
        def write_oam_addr():
            self.oam_addr = self.reg_io_value & 0xFF
        def write_oam_data():
            # TODO: ignore during rendering
            # TODO: if OAMADDR is not less than eight when rendering starts, the eight bytes starting at OAMADDR & 0xF8 are copied to the first eight bytes of OAM
            self.oam[self.oam_addr] = self.reg_io_value & 0xFF
            self.oam_addr = (self.oam_addr + 1) & 0xFF
        def write_ppu_scroll_0():
            # ........ ...XXXXX (course X)
            self.fine_x_scroll = self.reg_io_value & 0x07
            self.tmp_addr ^= (self.tmp_addr^(self.reg_io_value>>3)) & 0x001F
        def write_ppu_scroll_1():
            # .yyy..YY YYY..... (fine y, course Y)
            self.tmp_addr ^= (self.tmp_addr^((self.reg_io_value<<12)|(self.reg_io_value<<2))) & 0x73E0
        def write_ppu_addr_0():
            # .0AAAAAA ........ (address high 6 bits + clear 14th bit)
            self.tmp_addr ^= (self.tmp_addr^((self.reg_io_value&0x3F)<<8)) & 0x7F00
        def write_ppu_addr_1():
            # ........ AAAAAAAA (address low 8 bits)
            self.tmp_addr ^= (self.tmp_addr^self.reg_io_value) & 0x00FF
            self.ppu_addr = self.tmp_addr
        def write_ppu_data():
            self.mapper.ppu_write(self.ppu_addr, self.reg_io_value&0xFF)
            self.ppu_addr = (self.ppu_addr + PPU_ADDR_INCREMENTS[self.ppu_ctrl&0x04]) & 0xFFFF

        self.reg_readers = [
            read_nothing,
            read_nothing,
            read_ppu_status,
            read_nothing,
            read_oam_data,
            read_nothing,
            read_nothing,
            read_ppu_data
        ]

        self.reg_writers = [
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

    def set_mapper(self, mapper):
        self.mapper = mapper

    def read_reg(self, reg_idx):
        self.reg_readers[reg_idx]()
        self.reg_io_write_state = 0
        return self.reg_io_value & 0xFF

    def write_reg(self, reg_idx, value):
        self.reg_io_value = value
        self.reg_writers[reg_idx+self.reg_io_write_state]()
        self.reg_io_write_state ^= 8

    def transfer_oam_via_dma(self, page_num): # 0x4014
        # CPU is suspended during the transfer, which will take 513 or 514 cycles after the $4014 write tick.
        # (1 wait state cycle while waiting for writes to complete, +1 if on an odd CPU cycle, then 256 alternating read/write cycles.)
        page = self.mapper.cpu_read_page(page_num)  # TODO: add cpu_read_page()
        i = 0
        while i < 0x100:
            self.oam[(self.oam_addr+i)&0xFF] = page[i]
            i += 1

    def render(self, t_elapsed):
        pixel_idx = 0
        screen = array.array('B', (0 for _ in range(256*240)))
        # background
        tile_0 = 0x0000 # AABB
        tile_1 = 0x0000 # AABB
        attr_0 = 0x00   # aa
        attr_1 = 0x00   # aa
        attr_n = 0x00   # AA
        # sprites
        primary_oam = self.oam
        secondary_oam = array.array('B', (0 for _ in range(8*4))) # is this real? 8 sprites x 4 bytes
        spr_tiles_0 = array.array('B', (0 for _ in range(8))) # 8 pairs of 8-bit shift registers 
        spr_tiles_1 = array.array('B', (0 for _ in range(8))) # 
        spr_attrs = array.array('B', (0 for _ in range(8))) # 8 latches - These contain the attribute bytes for up to 8 sprites
        spr_x_pos = array.array('B', (0 for _ in range(8))) # 8 counters - These contain the X positions for up to 8 sprites.

        def render_pixel():
            screen[pixel_idx] = ((tile_0>>self.fine_x_scroll)|(tile_1>>self.fine_x_scroll<<1)) & 0x03  # is this actually 8 - fine_x_scroll ?
            tile_0 >>= 1
            tile_1 >>= 1
            pixel_idx += 1

        def render_scanline():
            # 1 cycle
            t += 1 # idle cycle
            # 256 cycles
            pixel_end = pixel_idx + 256
            while pixel_idx < pixel_end:
                render_pixel()
                render_pixel()
                render_pixel()
                render_pixel()
                render_pixel()
                render_pixel()
                render_pixel()
                render_pixel()
                attr_0 = attr_n # TODO: fill 0 correctly
                attr_1 = attr_n # TODO: fill 1 correctly
                attr_n = 0x00   # TODO: lookup next attr, increment t by 2
                next_tile_0 = 0x00 # TODO: lookup next tile in map, increment t by 2
                next_tile_1 = 0x00 # TODO: lookup next tile in map, increment t by 2
                tile_0 |= next_tile_0 << 8
                tile_1 |= next_tile_1 << 8

                #if spr_x_pos[0]:
                #    spr_x_pos[0] -= 1
                #else:
                #    pass # active
                
            # 64 cycles
            # fetch tile data for the sprites on the next scanline
            # 16 cycles
            # fetch first two tiles for the next scanline
            # 16 cycles
            # fetch first two tiles for the next scanline
            # 2 cycles
            # fetch bytes here are the same nametable byte that will be fetched at the beginning of the next scanline

        # Visible scanlines 0 - 239
        line_num = 0
        while line_num < 240:
            render_scanline()

        # Post-render scanlines 240 - 260
        # The PPU makes no memory accesses during these scanlines, so PPU memory can be freely accessed by the program
        
        # Scanline 240:
        # Idle

        # Scanline 241:
        # The VBlank flag of the PPU is set at tick 1 (the second tick) of scanline 241, where the VBlank NMI also occurs.

        # Scanlines 242 - 260:
        # TODO: do something?

        # Pre-render scanline (261)
        # TODO: do it! remember to skip last cycle on odd frames (check self.frame_num)

        self.frame_num += 1

    def build_scanline_funcs(self):
        def idle():
            pass
        def fetch_bg_0_addr_only():
            pass
        def fetch_nt():
            pass
        def fetch_at():
            pass
        def fetch_bg_0():
            pass
        def fetch_bg_1():
            pass
        def inc_x():
            pass
        def inc_xy():
            pass
        def reset_x():
            pass
        # TODO: this lookup can be easily flattened
        visible_scanline_funcs = [None] * 340
        idle_scanline_funcs = [None] * 340
        vblank_scanline_funcs = [None] * 340
        pre_render_scanline_funcs = [None] * 340
        self.scanline_funcs = [visible_scanline_funcs] * 240 + [idle_scanline_funcs] * (262-240)
        self.scanline_funcs[241] = vblank_scanline_funcs
        self.scanline_funcs[261] = pre_render_scanline_funcs

    def tick(self):
        self.scanline_funcs[self.scanline_idx][self.t]()

def play(mapper, regs=None, t=0, do_other_things=None, stop_on_brk=False, print_cpu_log=False):
    pc, s, a, x, y, p = regs or (0x0000, 0xFF, 0x00, 0x00, 0x00, 0x34)

    # NOTE: consider all flag letter names reserved, even if not currently used: c, z, i, d, b, v, n

    def _do_no_other_things(_):
        return 0xFFFF  # arbitrarily large number of cycles
    do_other_things = do_other_things or _do_no_other_things

    # Resolve address per mode

    def _resolve_immediate(mapper, pc):
        return pc + 1
    def _resolve_zero_page(mapper, pc):
        return mapper.cpu_read(pc+1)
    def _resolve_zero_page_indexed_x(mapper, pc):
        return (mapper.cpu_read(pc+1)+x) & 0xFF
    def _resolve_zero_page_indexed_y(mapper, pc):
        return (mapper.cpu_read(pc+1)+y) & 0xFF
    def _resolve_absolute(mapper, pc):
        return (mapper.cpu_read(pc+1)|(mapper.cpu_read(pc+2)<<8))
    def _resolve_absolute_indexed_x(mapper, pc):
        nonlocal t
        addr = mapper.cpu_read(pc+1) + x
        t += addr>>8
        return (addr+(mapper.cpu_read(pc+2)<<8)) & 0xFFFF
    def _resolve_absolute_indexed_y(mapper, pc):
        nonlocal t
        addr = mapper.cpu_read(pc+1) + y
        t += addr>>8
        return (addr+(mapper.cpu_read(pc+2)<<8)) & 0xFFFF
    def _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc):
        return ((mapper.cpu_read(pc+1)|(mapper.cpu_read(pc+2)<<8))+x) & 0xFFFF
    def _resolve_absolute_indexed_y_no_extra_cycle(mapper, pc):
        return ((mapper.cpu_read(pc+1)|(mapper.cpu_read(pc+2)<<8))+y) & 0xFFFF
    def _resolve_indexed_indirect(mapper, pc):
        i = mapper.cpu_read(pc+1) + x
        return mapper.cpu_read(i&0xFF)|(mapper.cpu_read((i+1)&0xFF)<<8)
    def _resolve_indirect_indexed(mapper, pc):
        nonlocal t
        i = mapper.cpu_read(pc+1)
        addr = mapper.cpu_read(i) + y
        t += addr>>8
        return (addr+(mapper.cpu_read((i+1)&0xFF)<<8)) & 0xFFFF
    def _resolve_indirect_indexed_no_extra_cycle(mapper, pc):
        i = mapper.cpu_read(pc+1)
        return ((mapper.cpu_read(i)|(mapper.cpu_read((i+1)&0xFF)<<8))+y) & 0xFFFF
    def _resolve_indirect(mapper, pc):
        addr = mapper.cpu_read(pc+1) | (mapper.cpu_read(pc+2)<<8)
        return mapper.cpu_read(addr) | (mapper.cpu_read((addr&0xFF00)|((addr+1)&0xFF))<<8)  # NOTE: byte two cannot cross page
    def _resolve_relative(mapper, pc):
        nonlocal t
        rel_addr = signed8(mapper.cpu_read(pc+1))
        pc += 2
        t += (((pc&0xFF)+rel_addr)>>8) & 1
        return pc + rel_addr

    # Interrupts

    def _trigger_interrupt(new_pc):
        # note that this is the only place pc is used outside of the main loop,
        # as interrupts are the only exception to normal execution
        nonlocal pc
        nonlocal t, s
        mapper.cpu_write(STACK_OFFSET+s, pc<<8)
        s = (s-1) & 0xFF
        mapper.cpu_write(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        mapper.cpu_write(STACK_OFFSET+s, p|U)
        s = (s-1) & 0xFF
        t += 7
        pc = new_pc

    def _nmi():
        _trigger_interrupt(mapper.cpu_read(0xFFFA)|(mapper.cpu_read(0xFFFB)<<8))

    def _reset():
        _trigger_interrupt(mapper.cpu_read(0xFFFC)|(mapper.cpu_read(0xFFFD)<<8))

    def _irq():
        if p&I:
            return pc  # interrupt disabled
        _trigger_interrupt(mapper.cpu_read(0xFFFE)|(mapper.cpu_read(0xFFFF)<<8))

    # Instructions

    interrupts = (_nmi, _reset, _irq)

    def _build_undefined_op(opcode):
        def undefined_op(pc):
            raise ValueError('Undefined opcode {}'.format(opcode))
        return undefined_op

    ops = [_build_undefined_op(opcode) for opcode in range(0x100)]

    # ADC (ADd with Carry)
    # Writes flags: N V Z C
    def _69_adc_immediate(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_immediate(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def _65_adc_zero_page(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_zero_page(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def _75_adc_zero_page_indexed_x(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def _6d_adc_absolute(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_absolute(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _7d_adc_absolute_indexed_x(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _79_adc_absolute_indexed_y(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _61_adc_indexed_indirect(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def _71_adc_indirect_indexed(pc):
        nonlocal t, a, p
        m = mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # AND (bitwise AND with accumulator)
    # Writes flags: N Z
    def _29_and_immediate(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _25_and_zero_page(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _35_and_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _2d_and_absolute(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _3d_and_absolute_indexed_x(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _39_and_absolute_indexed_y(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _21_and_indexed_indirect(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _31_and_indirect_indexed(pc):
        nonlocal t, a, p
        a &= mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # ASL (Arithmetic Shift Left)
    # Writes flags: N Z C
    def _0a_asl_accumulator(pc):
        nonlocal t, a, p
        r = a << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def _06_asl_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 5
        return pc + 2
    def _16_asl_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 2
    def _0e_asl_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 3
    def _1e_asl_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # BIT (test BITs)
    # Writes flags: N V Z
    def _24_bit_zero_page(pc):
        nonlocal t, p
        m = mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NVZ) | (m & N) | (m & V) | (0x00 if (m&a) else Z)
        t += 3
        return pc + 2
    def _2c_bit_absolute(pc):
        nonlocal t, p
        m = mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NVZ) | (m & N) | (m & V) | (0x00 if (m&a) else Z)
        t += 4
        return pc + 3
    
    # Branch Instructions
    # BPL (Branch on PLus)
    def _10_bpl(pc):
        nonlocal t
        if p & N:
            t += 2
            return pc + 2
        else:
            t += 3
            return _resolve_relative(mapper, pc)
    # BMI (Branch on MInus)
    def _30_bmi(pc):
        nonlocal t
        if p & N:
            t += 3
            return _resolve_relative(mapper, pc)
        else:
            t += 2
            return pc + 2
    # BVC (Branch on oVerflow Clear)
    def _50_bvc(pc):
        nonlocal t
        if p & V:
            t += 2
            return pc + 2
        else:
            t += 3
            return _resolve_relative(mapper, pc)
    # BVS (Branch on oVerflow Set)
    def _70_bvs(pc):
        nonlocal t
        if p & V:
            t += 3
            return _resolve_relative(mapper, pc)
        else:
            t += 2
            return pc + 2
    # BCC (Branch on Carry Clear)
    def _90_bcc(pc):
        nonlocal t
        if p & C:
            t += 2
            return pc + 2
        else:
            t += 3
            return _resolve_relative(mapper, pc)
    # BCS (Branch on Carry Set)
    def _b0_bcs(pc):
        nonlocal t
        if p & C:
            t += 3
            return _resolve_relative(mapper, pc)
        else:
            t += 2
            return pc + 2
    # BNE (Branch on Not Equal)
    def _d0_bne(pc):
        nonlocal t
        if p & Z:
            t += 2
            return pc + 2
        else:
            t += 3
            return _resolve_relative(mapper, pc)
    # BEQ (Branch on EQual)
    def _f0_beq(pc):
        nonlocal t
        if p & Z:
            t += 3
            return _resolve_relative(mapper, pc)
        else:
            t += 2
            return pc + 2
    
    # BRK (BReaK)
    # Writes flags: B
    def _00_brk_implied(_):
        nonlocal p
        p |= B  # NOTE: B flag won't be reset after nested interrupt (via PHP) -- is this okay?
        _irq()
        return pc
    def _00_brk_implied_stop_execution(_):  # for unit testing / debugging
        raise VMStop()
    
    # CMP (CoMPare accumulator)
    # Writes flags: N Z C
    def _c9_cmp_immediate(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def _c5_cmp_zero_page(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def _d5_cmp_zero_page_indexed_x(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 2
    def _cd_cmp_absolute(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _dd_cmp_absolute_indexed_x(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _d9_cmp_absolute_indexed_y(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _c1_cmp_indexed_indirect(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 6
        return pc + 2
    def _d1_cmp_indirect_indexed(pc):
        nonlocal t, p
        r = a - mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 5
        return pc + 2
    
    # CPX (ComPare X register)
    # Writes flags: N Z C
    def _e0_cpx_immediate(pc):
        nonlocal t, p
        r = x - mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def _e4_cpx_zero_page(pc):
        nonlocal t, p
        r = x - mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def _ec_cpx_absolute(pc):
        nonlocal t, p
        r = x - mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    
    # CPY (ComPare Y register)
    # Writes flags: N Z C
    def _c0_cpy_immediate(pc):
        nonlocal t, p
        r = y - mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def _c4_cpy_zero_page(pc):
        nonlocal t, p
        r = y - mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def _cc_cpy_absolute(pc):
        nonlocal t, p
        r = y - mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    
    # DEC (DECrement memory)
    # Writes flags: N Z
    def _c6_dec_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 5
        return pc + 2
    def _d6_dec_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 2
    def _ce_dec_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 3
    def _de_dec_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m - 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # EOR (bitwise Exclusive OR)
    # Writes flags: N Z
    def _49_eor_immediate(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _45_eor_zero_page(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _55_eor_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _4d_eor_absolute(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _5d_eor_absolute_indexed_x(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _59_eor_absolute_indexed_y(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _41_eor_indexed_indirect(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _51_eor_indirect_indexed(pc):
        nonlocal t, a, p
        a ^= mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # Flag (Processor Status) Instructions
    # CLC (CLear Carry)
    def _18_clc(pc):
        nonlocal t, p
        p &= MASK_C
        t += 2
        return pc + 1
    # SEC (SEt Carry)
    def _38_sec(pc):
        nonlocal t, p
        p |= C
        t += 2
        return pc + 1
    # CLI (CLear Interrupt)
    def _58_cli(pc):
        nonlocal t, p
        p &= MASK_I
        t += 2
        return pc + 1
    # SEI (SEt Interrupt)
    def _78_sei(pc):
        nonlocal t, p
        p |= I
        t += 2
        return pc + 1
    # CLV (CLear oVerflow)
    def _b8_clv(pc):
        nonlocal t, p
        p &= MASK_V
        t += 2
        return pc + 1
    # CLD (CLear Decimal)
    def _d8_cld(pc):
        nonlocal t, p
        p &= MASK_D
        t += 2
        return pc + 1
    # SED (SEt Decimal)
    def _f8_sed(pc):
        nonlocal t, p
        p |= D
        t += 2
        return pc + 1
    
    # INC (INCrement memory)
    # Writes flags: N Z
    def _e6_inc_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 5
        return pc + 2
    def _f6_inc_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 2
    def _ee_inc_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 3
    def _fe_inc_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m + 1
        p = (p&MASK_NZ) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.cpu_write(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # JMP (JuMP)
    # Writes flags: none
    def _4c_jmp_absolute(pc):
        nonlocal t
        t += 3
        return _resolve_absolute(mapper, pc)
    def _6c_jmp_indirect(pc):
        nonlocal t
        t += 5
        return _resolve_indirect(mapper, pc)
    
    # JSR (Jump to SubRoutine)
    # Writes flags: none
    def _20_jsr_absolute(pc):
        nonlocal t, s
        to = _resolve_absolute(mapper, pc)
        pc += 2 # 3 - 1 (offset by one before storing on stack)
        mapper.cpu_write(STACK_OFFSET+s, pc>>8)
        s = (s-1) & 0xFF
        mapper.cpu_write(STACK_OFFSET+s, pc&0xFF)
        s = (s-1) & 0xFF
        t += 6
        return to
    
    # LDA (LoaD Accumulator)
    # Writes flags: N Z
    def _a9_lda_immediate(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _a5_lda_zero_page(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _b5_lda_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _ad_lda_absolute(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _bd_lda_absolute_indexed_x(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _b9_lda_absolute_indexed_y(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _a1_lda_indexed_indirect(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _b1_lda_indirect_indexed(pc):
        nonlocal t, a, p
        a = mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # LDX (LoaD X register)
    # Writes flags: N Z
    def _a2_ldx_immediate(pc):
        nonlocal t, x, p
        x = mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 2
        return pc + 2
    def _a6_ldx_zero_page(pc):
        nonlocal t, x, p
        x = mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 3
        return pc + 2
    def _b6_ldx_zero_page_indexed_y(pc):
        nonlocal t, x, p
        x = mapper.cpu_read(_resolve_zero_page_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 2
    def _ae_ldx_absolute(pc):
        nonlocal t, x, p
        x = mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 3
    def _be_ldx_absolute_indexed_y(pc):
        nonlocal t, x, p
        x = mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if x else Z) | (x&N)
        t += 4
        return pc + 3
    
    # LDY (LoaD Y register)
    # Writes flags: N Z
    def _a0_ldy_immediate(pc):
        nonlocal t, y, p
        y = mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 2
        return pc + 2
    def _a4_ldy_zero_page(pc):
        nonlocal t, y, p
        y = mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 3
        return pc + 2
    def _b4_ldy_zero_page_indexed_x(pc):
        nonlocal t, y, p
        y = mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 2
    def _ac_ldy_absolute(pc):
        nonlocal t, y, p
        y = mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 3
    def _bc_ldy_absolute_indexed_x(pc):
        nonlocal t, y, p
        y = mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (0x00 if y else Z) | (y&N)
        t += 4
        return pc + 3
    
    # LSR (Logical Shift Right)
    # Writes flags: N Z C
    def _4a_lsr_accumulator(pc):
        nonlocal t, a, p
        r = a >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (a&C)
        a = r
        t += 2
        return pc + 1
    def _46_lsr_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r)
        t += 5
        return pc + 2
    def _56_lsr_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r)
        t += 6
        return pc + 2
    def _4e_lsr_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r)
        t += 6
        return pc + 3
    def _5e_lsr_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        m = mapper.cpu_read(addr)
        r = m >> 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r)
        t += 7
        return pc + 3
    
    # NOP (No OPeration)
    # Writes flags: none
    def _ea_nop_implied(pc):
        nonlocal t
        t += 2
        return pc + 1
    
    # ORA (bitwise OR with Accumulator)
    # Writes flags: N Z
    def _09_ora_immediate(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_immediate(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _05_ora_zero_page(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_zero_page(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _15_ora_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_zero_page_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _0d_ora_absolute(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_absolute(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _1d_ora_absolute_indexed_x(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_absolute_indexed_x(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _19_ora_absolute_indexed_y(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_absolute_indexed_y(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _01_ora_indexed_indirect(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_indexed_indirect(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _11_ora_indirect_indexed(pc):
        nonlocal t, a, p
        a |= mapper.cpu_read(_resolve_indirect_indexed(mapper, pc))
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # Register Instructions
    # Writes flags: N Z
    # TAX (Transfer A to X)
    def _aa_tax(pc):
        nonlocal t, x, p
        x = a
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # TXA (Transfer X to A)
    def _8a_txa(pc):
        nonlocal t, a, p
        a = x
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 1
    # DEX (DEcrement X)
    def _ca_dex(pc):
        nonlocal t, x, p
        x = (x - 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # INX (INcrement X)
    def _e8_inx(pc):
        nonlocal t, x, p
        x = (x + 1) & 0xFF
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # TAY (Transfer A to Y)
    def _a8_tay(pc):
        nonlocal t, y, p
        y = a
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    # TYA (Transfer Y to A)
    def _98_tya(pc):
        nonlocal t, a, p
        a = y
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 2
        return pc + 1
    # DEY (DEcrement Y)
    def _88_dey(pc):
        nonlocal t, y, p
        y = (y - 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    # INY (INcrement Y)
    def _c8_iny(pc):
        nonlocal t, y, p
        y = (y + 1) & 0xFF
        p = (p&MASK_NZ) | (y&N) | (0x00 if y else Z)
        t += 2
        return pc + 1
    
    # ROL (ROtate Left)
    # Writes flags: N Z C
    def _2a_rol_accumulator(pc):
        nonlocal t, a, p
        r = (a<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def _26_rol_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        r = (mapper.cpu_read(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 5
        return pc + 2
    def _36_rol_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        r = (mapper.cpu_read(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 2
    def _2e_rol_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        r = (mapper.cpu_read(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 3
    def _3e_rol_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        r = (mapper.cpu_read(addr)<<1) | (p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # ROR (ROtate Right)
    # Writes flags: N Z C
    def _6a_ror_accumulator(pc):
        nonlocal t, a, p
        r = (a>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (a&C)
        a = r & 0xFF
        t += 2
        return pc + 1
    def _66_ror_zero_page(pc):
        nonlocal t, p
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 5
        return pc + 2
    def _76_ror_zero_page_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 2
    def _6e_ror_absolute(pc):
        nonlocal t, p
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 6
        return pc + 3
    def _7e_ror_absolute_indexed_x(pc):
        nonlocal t, p
        addr = _resolve_absolute_indexed_x_no_extra_cycle(mapper, pc)
        m = mapper.cpu_read(addr)
        r = (m>>1) | ((p&C)<<7)
        p = (p&MASK_NZC) | (r&N) | (0x00 if r else Z) | (m&C)
        mapper.cpu_write(addr, r&0xFF)
        t += 7
        return pc + 3
    
    # RTI (ReTurn from Interrupt)
    # Writes flags: all
    def _40_rti_implied(pc):
        nonlocal t, p, s
        s = (s+1) & 0xFF
        p = mapper.cpu_read(STACK_OFFSET+s) & MASK_B | U
        s = (s+1) & 0xFF
        pc = mapper.cpu_read(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        pc |= mapper.cpu_read(STACK_OFFSET+s) << 8
        t += 6
        return pc
    
    # RTS (ReTurn from Subroutine)
    # Writes flags: none
    def _60_rts_implied(pc):
        nonlocal t, s
        s = (s+1) & 0xFF
        pc = mapper.cpu_read(STACK_OFFSET+s)
        s = (s+1) & 0xFF
        pc |= mapper.cpu_read(STACK_OFFSET+s) << 8
        t += 6
        return pc + 1
    
    # SBC (SuBtract with Carry)
    # Writes flags: N V Z C
    def _e9_sbc_immediate(pc):
        nonlocal t, p, a
        m = mapper.cpu_read(_resolve_immediate(mapper, pc))
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def _e5_sbc_zero_page(pc):
        nonlocal t, p, a
        addr = _resolve_zero_page(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def _f5_sbc_zero_page_indexed_x(pc):
        nonlocal t, p, a
        addr = _resolve_zero_page_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def _ed_sbc_absolute(pc):
        nonlocal t, p, a
        addr = _resolve_absolute(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _fd_sbc_absolute_indexed_x(pc):
        nonlocal t, p, a
        addr = _resolve_absolute_indexed_x(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _f9_sbc_absolute_indexed_y(pc):
        nonlocal t, p, a
        addr = _resolve_absolute_indexed_y(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _e1_sbc_indexed_indirect(pc):
        nonlocal t, p, a
        addr = _resolve_indexed_indirect(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def _f1_sbc_indirect_indexed(pc):
        nonlocal t, p, a
        addr = _resolve_indirect_indexed(mapper, pc)
        m = mapper.cpu_read(addr)
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)^(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # STA (STore Accumulator)
    # Writes flags: none
    def _85_sta_zero_page(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page(mapper, pc), a)
        t += 3
        return pc + 2
    def _95_sta_zero_page_indexed_x(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page_indexed_x(mapper, pc), a)
        t += 4
        return pc + 2
    def _8d_sta_absolute(pc):
        nonlocal t
        mapper.cpu_write(_resolve_absolute(mapper, pc), a)
        t += 4
        return pc + 3
    def _9d_sta_absolute_indexed_x(pc):
        nonlocal t
        mapper.cpu_write(_resolve_absolute_indexed_x_no_extra_cycle(mapper, pc), a)
        t += 5
        return pc + 3
    def _99_sta_absolute_indexed_y(pc):
        nonlocal t
        mapper.cpu_write(_resolve_absolute_indexed_y_no_extra_cycle(mapper, pc), a)
        t += 5
        return pc + 3
    def _81_sta_indexed_indirect(pc):
        nonlocal t
        mapper.cpu_write(_resolve_indexed_indirect(mapper, pc), a)
        t += 6
        return pc + 2
    def _91_sta_indirect_indexed(pc):
        nonlocal t
        mapper.cpu_write(_resolve_indirect_indexed_no_extra_cycle(mapper, pc), a)
        t += 6
        return pc + 2
    
    # Stack Instructions
    # TXS (Transfer X to Stack ptr)
    def _9a_txs(pc):
        nonlocal t, s
        s = x
        t += 2
        return pc + 1
    # TSX (Transfer Stack ptr to X)
    def _ba_tsx(pc):
        nonlocal t, x, p
        x = s
        p = (p&MASK_NZ) | (x&N) | (0x00 if x else Z)
        t += 2
        return pc + 1
    # PHA (PusH Accumulator)
    def _48_pha(pc):
        nonlocal t, s
        mapper.cpu_write(STACK_OFFSET+s, a)
        s = (s-1) & 0xFF
        t += 3
        return pc + 1
    # PLA (PuLl Accumulator)
    def _68_pla(pc):
        nonlocal t, s, a, p
        s = (s+1) & 0xFF
        a = mapper.cpu_read(STACK_OFFSET+s)
        p = (p&MASK_NZ) | (a&N) | (0x00 if a else Z)
        t += 4
        return pc + 1
    # PHP (PusH Processor status)
    def _08_php(pc):
        nonlocal t, s
        mapper.cpu_write(STACK_OFFSET+s, p|B|U)
        s = (s-1) & 0xFF
        t += 3
        return pc + 1
    # PLP (PuLl Processor status)
    def _28_plp(pc):
        nonlocal t, s, p
        s = (s+1) & 0xFF
        p = mapper.cpu_read(STACK_OFFSET+s) & MASK_B | U  # always clear break bit, set unused bit 
        t += 4
        return pc + 1
    
    # STX (STore X register)
    # Writes flags: none
    def _86_stx_zero_page(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page(mapper, pc), x)
        t += 3
        return pc + 2
    def _96_stx_zero_page_indexed_y(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page_indexed_y(mapper, pc), x)
        t += 4
        return pc + 2
    def _8e_stx_absolute(pc):
        nonlocal t
        mapper.cpu_write(_resolve_absolute(mapper, pc), x)
        t += 4
        return pc + 3
    
    # STY (STore Y register)
    # Writes flags: none
    def _84_sty_zero_page(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page(mapper, pc), y)
        t += 3
        return pc + 2
    def _94_sty_zero_page_indexed_x(pc):
        nonlocal t
        mapper.cpu_write(_resolve_zero_page_indexed_x(mapper, pc), y)
        t += 4
        return pc + 2
    def _8c_sty_absolute(pc):
        nonlocal t
        mapper.cpu_write(_resolve_absolute(mapper, pc), y)
        t += 4
        return pc + 3
    
    ops[0x69] = _69_adc_immediate
    ops[0x65] = _65_adc_zero_page
    ops[0x75] = _75_adc_zero_page_indexed_x
    ops[0x6d] = _6d_adc_absolute
    ops[0x7d] = _7d_adc_absolute_indexed_x
    ops[0x79] = _79_adc_absolute_indexed_y
    ops[0x61] = _61_adc_indexed_indirect
    ops[0x71] = _71_adc_indirect_indexed
    ops[0x29] = _29_and_immediate
    ops[0x25] = _25_and_zero_page
    ops[0x35] = _35_and_zero_page_indexed_x
    ops[0x2d] = _2d_and_absolute
    ops[0x3d] = _3d_and_absolute_indexed_x
    ops[0x39] = _39_and_absolute_indexed_y
    ops[0x21] = _21_and_indexed_indirect
    ops[0x31] = _31_and_indirect_indexed
    ops[0x0a] = _0a_asl_accumulator
    ops[0x06] = _06_asl_zero_page
    ops[0x16] = _16_asl_zero_page_indexed_x
    ops[0x0e] = _0e_asl_absolute
    ops[0x1e] = _1e_asl_absolute_indexed_x
    ops[0x24] = _24_bit_zero_page
    ops[0x2c] = _2c_bit_absolute
    ops[0x10] = _10_bpl
    ops[0x30] = _30_bmi
    ops[0x50] = _50_bvc
    ops[0x70] = _70_bvs
    ops[0x90] = _90_bcc
    ops[0xb0] = _b0_bcs
    ops[0xd0] = _d0_bne
    ops[0xf0] = _f0_beq
    ops[0x00] = _00_brk_implied_stop_execution if stop_on_brk else _00_brk_implied
    ops[0xc9] = _c9_cmp_immediate
    ops[0xc5] = _c5_cmp_zero_page
    ops[0xd5] = _d5_cmp_zero_page_indexed_x
    ops[0xcd] = _cd_cmp_absolute
    ops[0xdd] = _dd_cmp_absolute_indexed_x
    ops[0xd9] = _d9_cmp_absolute_indexed_y
    ops[0xc1] = _c1_cmp_indexed_indirect
    ops[0xd1] = _d1_cmp_indirect_indexed
    ops[0xe0] = _e0_cpx_immediate
    ops[0xe4] = _e4_cpx_zero_page
    ops[0xec] = _ec_cpx_absolute
    ops[0xc0] = _c0_cpy_immediate
    ops[0xc4] = _c4_cpy_zero_page
    ops[0xcc] = _cc_cpy_absolute
    ops[0xc6] = _c6_dec_zero_page
    ops[0xd6] = _d6_dec_zero_page_indexed_x
    ops[0xce] = _ce_dec_absolute
    ops[0xde] = _de_dec_absolute_indexed_x
    ops[0x49] = _49_eor_immediate
    ops[0x45] = _45_eor_zero_page
    ops[0x55] = _55_eor_zero_page_indexed_x
    ops[0x4d] = _4d_eor_absolute
    ops[0x5d] = _5d_eor_absolute_indexed_x
    ops[0x59] = _59_eor_absolute_indexed_y
    ops[0x41] = _41_eor_indexed_indirect
    ops[0x51] = _51_eor_indirect_indexed
    ops[0x18] = _18_clc
    ops[0x38] = _38_sec
    ops[0x58] = _58_cli
    ops[0x78] = _78_sei
    ops[0xb8] = _b8_clv
    ops[0xd8] = _d8_cld
    ops[0xf8] = _f8_sed
    ops[0xe6] = _e6_inc_zero_page
    ops[0xf6] = _f6_inc_zero_page_indexed_x
    ops[0xee] = _ee_inc_absolute
    ops[0xfe] = _fe_inc_absolute_indexed_x
    ops[0x4c] = _4c_jmp_absolute
    ops[0x6c] = _6c_jmp_indirect
    ops[0x20] = _20_jsr_absolute
    ops[0xa9] = _a9_lda_immediate
    ops[0xa5] = _a5_lda_zero_page
    ops[0xb5] = _b5_lda_zero_page_indexed_x
    ops[0xad] = _ad_lda_absolute
    ops[0xbd] = _bd_lda_absolute_indexed_x
    ops[0xb9] = _b9_lda_absolute_indexed_y
    ops[0xa1] = _a1_lda_indexed_indirect
    ops[0xb1] = _b1_lda_indirect_indexed
    ops[0xa2] = _a2_ldx_immediate
    ops[0xa6] = _a6_ldx_zero_page
    ops[0xb6] = _b6_ldx_zero_page_indexed_y
    ops[0xae] = _ae_ldx_absolute
    ops[0xbe] = _be_ldx_absolute_indexed_y
    ops[0xa0] = _a0_ldy_immediate
    ops[0xa4] = _a4_ldy_zero_page
    ops[0xb4] = _b4_ldy_zero_page_indexed_x
    ops[0xac] = _ac_ldy_absolute
    ops[0xbc] = _bc_ldy_absolute_indexed_x
    ops[0x4a] = _4a_lsr_accumulator
    ops[0x46] = _46_lsr_zero_page
    ops[0x56] = _56_lsr_zero_page_indexed_x
    ops[0x4e] = _4e_lsr_absolute
    ops[0x5e] = _5e_lsr_absolute_indexed_x
    ops[0xea] = _ea_nop_implied
    ops[0x09] = _09_ora_immediate
    ops[0x05] = _05_ora_zero_page
    ops[0x15] = _15_ora_zero_page_indexed_x
    ops[0x0d] = _0d_ora_absolute
    ops[0x1d] = _1d_ora_absolute_indexed_x
    ops[0x19] = _19_ora_absolute_indexed_y
    ops[0x01] = _01_ora_indexed_indirect
    ops[0x11] = _11_ora_indirect_indexed
    ops[0xaa] = _aa_tax
    ops[0x8a] = _8a_txa
    ops[0xca] = _ca_dex
    ops[0xe8] = _e8_inx
    ops[0xa8] = _a8_tay
    ops[0x98] = _98_tya
    ops[0x88] = _88_dey
    ops[0xc8] = _c8_iny
    ops[0x2a] = _2a_rol_accumulator
    ops[0x26] = _26_rol_zero_page
    ops[0x36] = _36_rol_zero_page_indexed_x
    ops[0x2e] = _2e_rol_absolute
    ops[0x3e] = _3e_rol_absolute_indexed_x
    ops[0x6a] = _6a_ror_accumulator
    ops[0x66] = _66_ror_zero_page
    ops[0x76] = _76_ror_zero_page_indexed_x
    ops[0x6e] = _6e_ror_absolute
    ops[0x7e] = _7e_ror_absolute_indexed_x
    ops[0x40] = _40_rti_implied
    ops[0x60] = _60_rts_implied
    ops[0xe9] = _e9_sbc_immediate
    ops[0xe5] = _e5_sbc_zero_page
    ops[0xf5] = _f5_sbc_zero_page_indexed_x
    ops[0xed] = _ed_sbc_absolute
    ops[0xfd] = _fd_sbc_absolute_indexed_x
    ops[0xf9] = _f9_sbc_absolute_indexed_y
    ops[0xe1] = _e1_sbc_indexed_indirect
    ops[0xf1] = _f1_sbc_indirect_indexed
    ops[0x85] = _85_sta_zero_page
    ops[0x95] = _95_sta_zero_page_indexed_x
    ops[0x8d] = _8d_sta_absolute
    ops[0x9d] = _9d_sta_absolute_indexed_x
    ops[0x99] = _99_sta_absolute_indexed_y
    ops[0x81] = _81_sta_indexed_indirect
    ops[0x91] = _91_sta_indirect_indexed
    ops[0x9a] = _9a_txs
    ops[0xba] = _ba_tsx
    ops[0x48] = _48_pha
    ops[0x68] = _68_pla
    ops[0x08] = _08_php
    ops[0x28] = _28_plp
    ops[0x86] = _86_stx_zero_page
    ops[0x96] = _96_stx_zero_page_indexed_y
    ops[0x8e] = _8e_stx_absolute
    ops[0x84] = _84_sty_zero_page
    ops[0x94] = _94_sty_zero_page_indexed_x
    ops[0x8c] = _8c_sty_absolute

    # During normal execution, reset interrupt is called at power-on
    if regs is None:
        _reset()

    try:
        while True:
            next_t = t + do_other_things(interrupts)  # such as drawing a pixel, scanline, frame (if not directly on screen, in a memory buffer)
            while t < next_t:  # catch up with external system(s)
                if print_cpu_log:
                    op = mapper.cpu_read(pc)
                    num_operands = INSTRUCTION_BYTES[op]
                    operands = [(mapper.cpu_read(pc+operand_i) if operand_i < num_operands else None) for operand_i in range(3)]
                    operands_text = ' '.join(['  ' if operand is None else f'{operand:02X}' for operand in operands])
                    addr_mode = INSTRUCTION_ADDR_MODES[op]
                    addr_mode_format = ADDR_MODE_FORMATS[addr_mode]
                    log_line = f'{pc:04X}  {operands_text}  {INSTRUCTION_LABELS[op]}{addr_mode_format(mapper, pc, operands)}'
                    log_line += ' ' * (48-len(log_line))
                    log_line += f'A:{a:02X} X:{x:02X} Y:{y:02X} P:{p:02X} SP:{s:02X} CYC:{t}'
                    print(log_line)
                pc = ops[mapper.cpu_read(pc)](pc)
    except VMStop:
        pass  # clean exit

    return (pc, s, a, x, y, p), t

class Cart(object):
    def __init__(
        self,
        prg_rom_banks,
        chr_rom_banks,
        mapper_cls=None,
        prg_ram_size=0,
        chr_ram_size=0,
        trainer=None,
        has_non_volatile_memory=None,
        hard_wired_nametable_mirroring_type=None,
        hard_wired_four_screen_mode=None,
        has_ines_2_0_identifier=None,
        console_type=None,
        timing_mode=None,
        more_console_type_info=None,
        num_misc_roms=None,
        default_expansion_device=None,
        cart_filename=None
    ):
        self.prg_rom_banks = prg_rom_banks
        self.chr_rom_banks = chr_rom_banks
        self.prg_ram = array.array('B', (0 for _ in range(prg_ram_size)))
        self.chr_ram = array.array('B', (0 for _ in range(chr_ram_size)))
        self.mapper_cls = mapper_cls or Mapper
        self.trainer = trainer
        self.hard_wired_nametable_mirroring_type = hard_wired_nametable_mirroring_type
        self.has_non_volatile_memory = has_non_volatile_memory  # redundant to prg-nvram once it's adddeds
        self.hard_wired_four_screen_mode = hard_wired_four_screen_mode
        self.has_ines_2_0_identifier = has_ines_2_0_identifier
        self.console_type = console_type
        self.prg_ram_size = prg_ram_size
        self.timing_mode = timing_mode
        self.more_console_type_info = more_console_type_info
        self.num_misc_roms = num_misc_roms
        self.default_expansion_device = default_expansion_device
        self.cart_filename = cart_filename
        
    @staticmethod
    def from_ines(ines_filepath):
        # See: https://www.nesdev.org/wiki/NES_2.0
        ines_file = open(ines_filepath, 'rb')
        header = ines_file.read(0x10)

        # Extract header bytes upfront, clearing unused bits (for sanity)
        identification_string    = header[0x0:0x4]
        num_prg_rom_banks_low    = header[0x4]
        num_chr_rom_banks_low    = header[0x5]
        control_byte_low         = header[0x6]
        control_byte_high        = header[0x7]
        submapper                = header[0x8]
        num_rom_banks_high       = header[0x9]
        prg_ram_shift            = header[0xA] & 0xF # TODO: PRG-NVRAM
        chr_ram_shift            = header[0xB] & 0xF # TODO: CHR-NVRAM
        timing_mode              = header[0xC] & 0x3
        more_console_type_info   = header[0xD]
        num_misc_roms            = header[0xE] & 0x3
        default_expansion_device = header[0xF] & 0x3F

        # Validate file format is iNES
        assert(identification_string == b'NES\x1A')
        assert((num_rom_banks_high&0xF) != 0xF)    # TODO: handle exponent-multiplier notation
        assert((num_rom_banks_high&0xF0) != 0xF0)  # TODO: handle exponent-multiplier notation

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
        prg_ram_size = 64 << prg_ram_shift
        chr_ram_size = 64 << chr_ram_shift

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
            prg_ram_size=prg_ram_size,
            chr_ram_size=chr_ram_size,
            trainer=trainer,
            has_non_volatile_memory=has_non_volatile_memory,
            hard_wired_nametable_mirroring_type=hard_wired_nametable_mirroring_type,
            hard_wired_four_screen_mode=hard_wired_four_screen_mode,
            has_ines_2_0_identifier=has_ines_2_0_identifier,
            console_type=console_type,
            timing_mode=timing_mode,
            more_console_type_info=more_console_type_info,
            num_misc_roms=num_misc_roms,
            default_expansion_device=default_expansion_device,
            cart_filename=os.path.basename(ines_filepath)
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
        print(f'PRG-RAM size: {self.prg_ram_size}')
        print(f'CHR-RAM size: {self.chr_ram_size}')
        print(f'CPU/PPU Timing: {timing_modes[self.timing_mode]}')
        print(f'More console type info: {self.more_console_type_info}')
        print(f'Number of miscellaneous ROMs: {self.num_misc_roms}')
        print(f'Default expansion device: {self.default_expansion_device}')

    def connect(self, ram, vram, ppu_read, ppu_write):
        def apu_read(addr):
            return 0
        def apu_write(addr, value):
            pass
        self.mapper = self.mapper_cls(
            ram,
            vram,
            self.prg_rom_banks,
            self.prg_ram,
            self.chr_rom_banks,
            self.chr_ram,
            ppu_read,
            ppu_write,
            apu_read,
            apu_write,
            [
                NT_MIRRORING_HORIZONTAL,
                NT_MIRRORING_VERTICAL
            ][self.hard_wired_nametable_mirroring_type])

class NES(object):
    def __init__(self, cart, regs=None, t=0, print_cpu_log=False):
        self.cart = cart
        self.regs = regs
        self.t = t
        self.ram = array.array('B', (0 for _ in range(0x800)))
        self.vram = array.array('B', (0 for _ in range(0x800)))
        self.ppu = PPU()
        self.mapper = cart.connect(self.ram, self.vram, self.ppu.cpu_read, self.ppu.cpu_read)
        self.print_cpu_log = print_cpu_log

    def play_cart(self):
        self.regs, self.t = play(self.mapper, self.regs, self.t, print_cpu_log=self.print_cpu_log)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Curt NES v0.0.0')
    parser.add_argument('rom')
    parser.add_argument('--print-cart-config', action='store_true')
    parser.add_argument('--print-cpu-log', action='store_true')
    args = parser.parse_args()

    cart = Cart.from_ines(args.rom)

    if args.print_cart_config:
        cart.print_config()
        exit(0)

    # Currently overriding registers, time for nestest.nes!
    nes = NES(cart, regs=(0xC000, 0xFD, 0x00, 0x00, 0x00, 0x24), t=7, print_cpu_log=args.print_cpu_log)
    nes.play_cart()
