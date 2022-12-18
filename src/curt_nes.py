#!/usr/local/bin/python3

# http://www.6502.org/tutorials/6502opcodes.html
# https://www.nesdev.org/obelisk-6502-guide/reference.html

# TODO:
# * build tiny sample rom that runs in working emulator
#   * create .cfg file
# FUTURE TO DOS:
# * check if carry is supposed to only be set/reset per adc, sbc (not both within each op)
# * use "massive" lookups to set flags?
# * or at least, reduce flags setting as much as possible (ex. ((r>>8)&X) == (r>>8))

import array

# Memory addressing
ZERO_PAGE_OFFSET         = 0x0000
STACK_OFFSET             = 0x0100
STACK_SIZE               = 0x0100
TOP_OF_STACK_OFFSET      = 0x0200
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

# Interrupt addresses
INTERRUPT_OFFSET         = 0xFFFA
NMI_OFFSET               = 0xFFFA
RESET_OFFSET             = 0xFFFC
IRQ_OFFSET               = 0xFFFE

# Processor statuses
C = 1 << 0  # carry
Z = 1 << 1  # zero
I = 1 << 2  # interrupt disable
D = 1 << 3  # decimal mode
B = 1 << 4  # break command
_ = 1 << 5  # unused
V = 1 << 6  # overflow
N = 1 << 7  # negative

MASK_NZ   = ~(N|Z)
MASK_NVZ  = ~(N|V|Z)
MASK_NVZC = ~(N|V|Z|C)
MASK_NZC  = ~(N|Z|C)

class VMStop(Exception):
    pass

class Mapper(object):
    """
    This Mapper class is responsible for memory mapping done by the
    main bus (which doesn't change per cartridge but done here for efficiency)
    and the cartidge mapper chip.
    """
    mapper_num = 0

    def __init__(self, mem, num_prg_rom_banks, num_chr_rom_banks):
        self.mem = mem
        self.num_prg_rom_banks = num_prg_rom_banks
        self.num_chr_rom_banks = num_chr_rom_banks
        
        # Build address lookup (registers not included)
        addr_lookup = array.array('H', (i for i in range(TOTAL_ADDRESS_SPACE)))
        # first, the rom space
        prg_rom_length = num_prg_rom_banks * ROM_BANK_SIZE
        for i in range(0x8000):  # connected/addressable 2 * 16 KB banks
            addr_lookup[ROM_LOWER_BANK_OFFSET+i] = ROM_LOWER_BANK_OFFSET + (i % prg_rom_length)
        # then RAM mirrors
        for i in range(RAM_MIRRORS_SIZE):
            addr_lookup[RAM_MIRRORS_OFFSET+i] = i % RAM_SIZE
        # then PPU mirrors
        for i in range(PPU_REG_MIRRORS_SIZE):
            addr_lookup[PPU_REG_MIRRORS_OFFSET+i] = PPU_REGS_OFFSET + (i%PPU_REGS_SIZE)

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

    # NOTE: consider flag names reserved: c, z, i, d, b, v, n

    mem = mapper.mem  # for zero page r/w or resolved address *READS* ONLY!

    # Resolve address per mode
    def _resolve_indirect_addr(mem, pc):
        i = mapper.resolve(mem[pc+1]|(mem[pc+2]<<8))
        return mapper.resolve(mem[i]|(mem[i+1]<<8))
    def _resolve_relative_addr(mem, pc):
        nonlocal t
        rel_addr = signed8(mem[pc+1])
        pc += 2
        t += (((pc&0xFF)+rel_addr)>>8) & 1
        return pc + rel_addr
    def _resolve_immediate(mem, pc):
        return pc + 1
    def _resolve_zero_page(mem, pc):
        return mem[pc+1]
    def _resolve_zero_page_indexed_x(mem, pc):
        return (mem[pc+1]+x)%0x100
    def _resolve_zero_page_indexed_y(mem, pc):
        return (mem[pc+1]+y)%0x100
    def _resolve_absolute(mem, pc):
        return mapper.resolve(((mem[pc+1]|(mem[pc+2]<<8))))
    def _resolve_absolute_indexed_x(mem, pc):
        nonlocal t
        addr = mem[pc+1] + x
        t += addr>>8
        return mapper.resolve((addr+(mem[pc+2]<<8))%0x10000)
    def _resolve_absolute_indexed_y(mem, pc):
        nonlocal t
        addr = mem[pc+1] + y
        t += addr>>8
        return mapper.resolve((addr+(mem[pc+2]<<8))%0x10000)
    def _resolve_indexed_indirect(mem, pc):
        i = (mem[pc+1]+x) % 0x100
        return mapper.resolve(mem[i]|(mem[i+1]<<8))
    def _resolve_indirect_indexed(mem, pc):
        nonlocal t
        i = (mem[pc+1])
        addr = mem[i] + y
        t += addr>>8
        return mapper.resolve((addr+(mem[i+1]<<8))%0x10000)

    def _build_undefined_op(opcode):
        def undefined_op(pc):
            raise ValueError('Undefined opcode {}'.format(opcode))
        return undefined_op

    ops = [_build_undefined_op(opcode) for opcode in range(0x100)]

    # ADC (ADd with Carry)
    # Writes flags: N V Z C
    def _69_adc_immediate(pc):
        nonlocal t, a, p
        m = mem[_resolve_immediate(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def _65_adc_zero_page(pc):
        nonlocal t, a, p
        m = mem[_resolve_zero_page(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def _75_adc_zero_page_indexed_x(pc):
        nonlocal t, a, p
        m = mem[_resolve_zero_page_indexed_x(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def _6d_adc_absolute(pc):
        nonlocal t, a, p
        m = mem[_resolve_absolute(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _7d_adc_absolute_indexed_x(pc):
        nonlocal t, a, p
        m = mem[_resolve_absolute_indexed_x(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _79_adc_absolute_indexed_y(pc):
        nonlocal t, a, p
        m = mem[_resolve_absolute_indexed_y(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _61_adc_indexed_indirect(pc):
        nonlocal t, a, p
        m = mem[_resolve_indexed_indirect(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def _71_adc_indirect_indexed(pc):
        nonlocal t, a, p
        m = mem[_resolve_indirect_indexed(mem, pc)]
        r = m + a + (p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # AND (bitwise AND with accumulator)
    # Writes flags: N Z
    def _29_and_immediate(pc):
        nonlocal t, a, p
        a &= mem[_resolve_immediate(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _25_and_zero_page(pc):
        nonlocal t, a, p
        a &= mem[_resolve_zero_page(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _35_and_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a &= mem[_resolve_zero_page_indexed_x(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _2d_and_absolute(pc):
        nonlocal t, a, p
        a &= mem[_resolve_absolute(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _3d_and_absolute_indexed_x(pc):
        nonlocal t, a, p
        a &= mem[_resolve_absolute_indexed_x(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _39_and_absolute_indexed_y(pc):
        nonlocal t, a, p
        a &= mem[_resolve_absolute_indexed_y(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _21_and_indexed_indirect(pc):
        nonlocal t, a, p
        a &= mem[_resolve_indexed_indirect(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _31_and_indirect_indexed(pc):
        nonlocal t, a, p
        a &= mem[_resolve_indirect_indexed(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
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
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    def _16_asl_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _0e_asl_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    def _1e_asl_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m << 1
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 7
        return pc + 3
    
    # BIT (test BITs)
    # Writes flags: N V Z
    def _24_bit_zero_page(pc):
        nonlocal t, p
        m = mem[_resolve_zero_page(mem, pc)]
        p = (p&MASK_NVZ) | (m & N) | (m & V) | (0x00 if (m&a) else Z)
        t += 3
        return pc + 2
    def _2c_bit_absolute(pc):
        nonlocal t, p
        m = mem[_resolve_absolute(mem, pc)]
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
            return _resolve_relative_addr(mem, pc)
    # BMI (Branch on MInus)
    def _30_bmi(pc):
        nonlocal t
        if p & N:
            t += 3
            return _resolve_relative_addr(mem, pc)
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
            return _resolve_relative_addr(mem, pc)
    # BVS (Branch on oVerflow Set)
    def _70_bvs(pc):
        nonlocal t
        if p & V:
            t += 3
            return _resolve_relative_addr(mem, pc)
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
            return _resolve_relative_addr(mem, pc)
    # BCS (Branch on Carry Set)
    def _b0_bcs(pc):
        nonlocal t
        if p & C:
            t += 3
            return _resolve_relative_addr(mem, pc)
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
            return _resolve_relative_addr(mem, pc)
    # BEQ (Branch on EQual)
    def _f0_beq(pc):
        nonlocal t
        if p & Z:
            t += 3
            return _resolve_relative_addr(mem, pc)
        else:
            t += 2
            return pc + 2
    
    # BRK (BReaK)
    # Writes flags: B
    def _00_brk_implied(pc):
        # nonlocal t, p
        # b = p & B
        # t += 7
        # return pc + 1
        raise VMStop()
    
    # CMP (CoMPare accumulator)
    # Writes flags: N Z C
    # Compare sets flags as if a subtraction had been carried out. If the value in the accumulator is equal or greater than the compared value, the Carry will be set. The equal (Z) and negative (N) flags will be set based on equality or lack thereof and the sign (i.e. A>=$80) of the accumulator.
    def _c9_cmp_immediate(pc):
        nonlocal t, p
        r = a - mem[_resolve_immediate(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 2
        return pc + 2
    def _c5_cmp_zero_page(pc):
        nonlocal t, p
        r = a - mem[_resolve_zero_page(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 3
        return pc + 2
    def _d5_cmp_zero_page_indexed_x(pc):
        nonlocal t, p
        r = a - mem[_resolve_zero_page_indexed_x(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 2
    def _cd_cmp_absolute(pc):
        nonlocal t, p
        r = a - mem[_resolve_absolute(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _dd_cmp_absolute_indexed_x(pc):
        nonlocal t, p
        r = a - mem[_resolve_absolute_indexed_x(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _d9_cmp_absolute_indexed_y(pc):
        nonlocal t, p
        r = a - mem[_resolve_absolute_indexed_y(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 4
        return pc + 3
    def _c1_cmp_indexed_indirect(pc):
        nonlocal t, p
        r = a - mem[_resolve_indexed_indirect(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 6
        return pc + 2
    def _d1_cmp_indirect_indexed(pc):
        nonlocal t, p
        r = a - mem[_resolve_indirect_indexed(mem, pc)] - (~p&C)
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        t += 5
        return pc + 2
    
    # CPX (ComPare X register)
    # Writes flags: N Z C
    def _e0_cpx_immediate(pc):
        nonlocal t, p
        m = mem[_resolve_immediate(mem, pc)]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 2
    def _e4_cpx_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _ec_cpx_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    
    # CPY (ComPare Y register)
    # Writes flags: N Z C
    def _c0_cpy_immediate(pc):
        nonlocal t, p
        m = mem[_resolve_immediate(mem, pc)]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 2
    def _c4_cpy_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _cc_cpy_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    
    # DEC (DECrement memory)
    # Writes flags: N Z
    def _c6_dec_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    def _d6_dec_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _ce_dec_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    def _de_dec_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 7
        return pc + 3
    
    # EOR (bitwise Exclusive OR)
    # Writes flags: N Z
    def _49_eor_immediate(pc):
        nonlocal t, a, p
        m = mem[_resolve_immediate(mem, pc)]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 2
    def _45_eor_zero_page(pc):
        nonlocal t, a, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _55_eor_zero_page_indexed_x(pc):
        nonlocal t, a, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 2
    def _4d_eor_absolute(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _5d_eor_absolute_indexed_x(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _59_eor_absolute_indexed_y(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute_indexed_y(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _41_eor_indexed_indirect(pc):
        nonlocal t, a, p
        addr32 = _resolve_indexed_indirect(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _51_eor_indirect_indexed(pc):
        nonlocal t, a, p
        addr32 = _resolve_indirect_indexed(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    
    # Flag (Processor Status) Instructions
    # Writes flags: as noted
    # These instructions are implied mode, have a length of one byte and require two machine cycles.
    # 
    # Notes:
    #   The Interrupt flag is used to prevent (SEI) or enable (CLI) maskable interrupts (aka IRQ's). It does not signal the presence or absence of an interrupt condition. The 6502 will set this flag automatically in response to an interrupt and restore it to its prior status on completion of the interrupt service routine. If you want your interrupt service routine to permit other maskable interrupts, you must clear the I flag in your code.
    # 
    # The Decimal flag controls how the 6502 adds and subtracts. If set, arithmetic is carried out in packed binary coded decimal. This flag is unchanged by interrupts and is unknown on power-up. The implication is that a CLD should be included in boot or interrupt coding.
    # 
    # The Overflow flag is generally misunderstood and therefore under-utilised. After an ADC or SBC instruction, the overflow flag will be set if the twos complement result is less than -128 or greater than +127, and it will cleared otherwise. In twos complement, $80 through $FF represents -128 through -1, and $00 through $7F represents 0 through +127. Thus, after:
    # 
    # CLC
    #   LDA #$7F ;   +127
    #   ADC #$01 ; +   +1
    # the overflow flag is 1 (+127 + +1 = +128), and after:
    #   CLC
    #   LDA #$81 ;   -127
    #   ADC #$FF ; +   -1
    # the overflow flag is 0 (-127 + -1 = -128). The overflow flag is not affected by increments, decrements, shifts and logical operations i.e. only ADC, BIT, CLV, PLP, RTI and SBC affect it. There is no op code to set the overflow but a BIT test on an RTS instruction will do the trick.
    # CLC (CLear Carry)
    def _18_clc(pc):
        nonlocal t, p
    
        return pc + None
    # SEC (SEt Carry)
    def _38_sec(pc):
        nonlocal t, p
    
        return pc + None
    # CLI (CLear Interrupt)
    def _58_cli(pc):
        nonlocal t, p
    
        return pc + None
    # SEI (SEt Interrupt)
    def _78_sei(pc):
        nonlocal t, p
    
        return pc + None
    # CLV (CLear oVerflow)
    def _b8_clv(pc):
        nonlocal t, p
    
        return pc + None
    # CLD (CLear Decimal)
    def _d8_cld(pc):
        nonlocal t, p
    
        return pc + None
    # SED (SEt Decimal)
    def _f8_sed(pc):
        nonlocal t, p
    
        return pc + None
    
    # INC (INCrement memory)
    # Writes flags: N Z
    def _e6_inc_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    def _f6_inc_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _ee_inc_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    def _fe_inc_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 7
        return pc + 3
    
    # JMP (JuMP)
    # Writes flags: none
    def _4c_jmp_absolute(pc):
        nonlocal t
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 3
    def _6c_jmp_indirect(pc):
        nonlocal t
        addr32 = _resolve_indirect(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 3
    
    # JSR (Jump to SubRoutine)
    # Writes flags: none
    def _20_jsr_absolute(pc):
        nonlocal t
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    
    # LDA (LoaD Accumulator)
    # Writes flags: N Z
    def _a9_lda_immediate(pc):
        nonlocal t, a, p
        a = mem[_resolve_immediate(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 2
        return pc + 2
    def _a5_lda_zero_page(pc):
        nonlocal t, a, p
        a = mem[_resolve_zero_page(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 3
        return pc + 2
    def _b5_lda_zero_page_indexed_x(pc):
        nonlocal t, a, p
        a = mem[_resolve_zero_page_indexed_x(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 2
    def _ad_lda_absolute(pc):
        nonlocal t, a, p
        a = mem[_resolve_absolute(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _bd_lda_absolute_indexed_x(pc):
        nonlocal t, a, p
        a = mem[_resolve_absolute_indexed_x(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _b9_lda_absolute_indexed_y(pc):
        nonlocal t, a, p
        a = mem[_resolve_absolute_indexed_y(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 4
        return pc + 3
    def _a1_lda_indexed_indirect(pc):
        nonlocal t, a, p
        a = mem[_resolve_indexed_indirect(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 6
        return pc + 2
    def _b1_lda_indirect_indexed(pc):
        nonlocal t, a, p
        a = mem[_resolve_indirect_indexed(mem, pc)]
        p = (p&MASK_NZ) | (a & N) | (0x00 if a else Z)
        t += 5
        return pc + 2
    
    # LDX (LoaD X register)
    # Writes flags: N Z
    def _a2_ldx_immediate(pc):
        nonlocal t, x, p
        x = mem[_resolve_immediate(mem, pc)]
        p = (0x00 if x else Z) | (x & N)
        t += 2
        return pc + 2
    def _a6_ldx_zero_page(pc):
        nonlocal t, x, p
        x = mem[_resolve_zero_page(mem, pc)]
        p = (0x00 if x else Z) | (x & N)
        t += 3
        return pc + 2
    def _b6_ldx_zero_page_indexed_y(pc):
        nonlocal t, x, p
        x = mem[_resolve_zero_page_indexed_y(mem, pc)]
        p = (0x00 if x else Z) | (x & N)
        t += 4
        return pc + 2
    def _ae_ldx_absolute(pc):
        nonlocal t, x, p
        x = mem[_resolve_absolute(mem, pc)]
        p = (0x00 if x else Z) | (x & N)
        t += 4
        return pc + 3
    def _be_ldx_absolute_indexed_y(pc):
        nonlocal t, x, p
        x = mem[_resolve_absolute_indexed_y(mem, pc)]
        p = (0x00 if x else Z) | (x & N)
        t += 4
        return pc + 3
    
    # LDY (LoaD Y register)
    # Writes flags: N Z
    def _a0_ldy_immediate(pc):
        nonlocal t, y, p
        y = mem[_resolve_immediate(mem, pc)]
        p = (0x00 if y else Z) | (y & N)
        t += 2
        return pc + 2
    def _a4_ldy_zero_page(pc):
        nonlocal t, y, p
        y = mem[_resolve_zero_page(mem, pc)]
        p = (0x00 if y else Z) | (y & N)
        t += 3
        return pc + 2
    def _b4_ldy_zero_page_indexed_x(pc):
        nonlocal t, y, p
        y = mem[_resolve_zero_page_indexed_x(mem, pc)]
        p = (0x00 if y else Z) | (y & N)
        t += 4
        return pc + 2
    def _ac_ldy_absolute(pc):
        nonlocal t, y, p
        y = mem[_resolve_absolute(mem, pc)]
        p = (0x00 if y else Z) | (y & N)
        t += 4
        return pc + 3
    def _bc_ldy_absolute_indexed_x(pc):
        nonlocal t, y, p
        y = mem[_resolve_absolute_indexed_x(mem, pc)]
        p = (0x00 if y else Z) | (y & N)
        t += 4
        return pc + 3
    
    # LSR (Logical Shift Right)
    # Writes flags: N Z C
    def _4a_lsr_accumulator(pc):
        nonlocal t, a, p
        r = a >> 1
        p = (a & C) | (0x00 if (r&0xFF) else Z) | (r&N)
        a = r
        t += 2
        return pc + 1
    def _46_lsr_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m >> 1
        p = (m & C) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r)
        t += 5
        return pc + 2
    def _56_lsr_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m >> 1
        p = (m & C) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r)
        t += 6
        return pc + 2
    def _4e_lsr_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m >> 1
        p = (m & C) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r)
        t += 6
        return pc + 3
    def _5e_lsr_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m >> 1
        p = (m & C) | (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r)
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
        m = mem[_resolve_immediate(mem, pc)]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 2
    def _05_ora_zero_page(pc):
        nonlocal t, a, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _15_ora_zero_page_indexed_x(pc):
        nonlocal t, a, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 2
    def _0d_ora_absolute(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _1d_ora_absolute_indexed_x(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _19_ora_absolute_indexed_y(pc):
        nonlocal t, a, p
        addr32 = _resolve_absolute_indexed_y(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _01_ora_indexed_indirect(pc):
        nonlocal t, a, p
        addr32 = _resolve_indexed_indirect(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _11_ora_indirect_indexed(pc):
        nonlocal t, a, p
        addr32 = _resolve_indirect_indexed(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (0x00 if (r&0xFF) else Z) | (r&N)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    
    # Register Instructions
    # Writes flags: N Z
    # These instructions are implied mode, have a length of one byte and require two machine cycles.
    # TAX (Transfer A to X)
    def _aa_tax(pc):
        nonlocal t, x, p
    
        return pc + None
    # TXA (Transfer X to A)
    def _8a_txa(pc):
        nonlocal t, a, p
    
        return pc + None
    # DEX (DEcrement X)
    def _ca_dex(pc):
        nonlocal t, x, p
    
        return pc + None
    # INX (INcrement X)
    def _e8_inx(pc):
        nonlocal t, x, p
    
        return pc + None
    # TAY (Transfer A to Y)
    def _a8_tay(pc):
        nonlocal t, y, p
    
        return pc + None
    # TYA (Transfer Y to A)
    def _98_tya(pc):
        nonlocal t, a, p
    
        return pc + None
    # DEY (DEcrement Y)
    def _88_dey(pc):
        nonlocal t, y, p
    
        return pc + None
    # INY (INcrement Y)
    def _c8_iny(pc):
        nonlocal t, y, p
    
        return pc + None
    
    # ROL (ROtate Left)
    # Writes flags: N Z C
    def _2a_rol_accumulator(pc):
        nonlocal t, a, p
        addr32 = _resolve_accumulator(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 1
    def _26_rol_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    def _36_rol_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _2e_rol_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    def _3e_rol_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 7
        return pc + 3
    
    # ROR (ROtate Right)
    # Writes flags: N Z C
    def _6a_ror_accumulator(pc):
        nonlocal t, a, p
        addr32 = _resolve_accumulator(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 2
        return pc + 1
    def _66_ror_zero_page(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 2
    def _76_ror_zero_page_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _6e_ror_absolute(pc):
        nonlocal t, p
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 3
    def _7e_ror_absolute_indexed_x(pc):
        nonlocal t, p
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NZC) | (r&N) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 7
        return pc + 3
    
    # RTI (ReTurn from Interrupt)
    # Writes flags: all
    def _40_rti_implied(pc):
        nonlocal t, p
        addr32 = _resolve_implied(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | ((r>>8)&C)
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 1
    
    # RTS (ReTurn from Subroutine)
    # Writes flags: none
    def _60_rts_implied(pc):
        nonlocal t
        addr32 = _resolve_implied(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 1
    
    # SBC (SuBtract with Carry)
    # Writes flags: N V Z C
    def _e9_sbc_immediate(pc):
        nonlocal t, p, a
        m = mem[_resolve_immediate(mem, pc)]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 2
        return pc + 2
    def _e5_sbc_zero_page(pc):
        nonlocal t, p, a
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 3
        return pc + 2
    def _f5_sbc_zero_page_indexed_x(pc):
        nonlocal t, p, a
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 2
    def _ed_sbc_absolute(pc):
        nonlocal t, p, a
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _fd_sbc_absolute_indexed_x(pc):
        nonlocal t, p, a
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _f9_sbc_absolute_indexed_y(pc):
        nonlocal t, p, a
        addr32 = _resolve_absolute_indexed_y(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 4
        return pc + 3
    def _e1_sbc_indexed_indirect(pc):
        nonlocal t, p, a
        addr32 = _resolve_indexed_indirect(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 6
        return pc + 2
    def _f1_sbc_indirect_indexed(pc):
        nonlocal t, p, a
        addr32 = _resolve_indirect_indexed(mem, pc)
        m = mem[addr32]
        r = a - m - (~p&C)
        p = (p&MASK_NVZC) | (r&N) | ((((a^r)&(m^r))>>1)&V) | (0x00 if (r&0xFF) else Z) | (~(r>>8)&C)
        a = r & 0xFF
        t += 5
        return pc + 2
    
    # STA (STore Accumulator)
    # Writes flags: none
    def _85_sta_zero_page(pc):
        nonlocal t
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _95_sta_zero_page_indexed_x(pc):
        nonlocal t
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 2
    def _8d_sta_absolute(pc):
        nonlocal t
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    def _9d_sta_absolute_indexed_x(pc):
        nonlocal t
        addr32 = _resolve_absolute_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 3
    def _99_sta_absolute_indexed_y(pc):
        nonlocal t
        addr32 = _resolve_absolute_indexed_y(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 5
        return pc + 3
    def _81_sta_indexed_indirect(pc):
        nonlocal t
        addr32 = _resolve_indexed_indirect(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    def _91_sta_indirect_indexed(pc):
        nonlocal t
        addr32 = _resolve_indirect_indexed(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 6
        return pc + 2
    
    # Stack Instructions
    # 
    # These instructions are implied mode, have a length of one byte and require machine cycles as indicated. The "PuLl" operations are known as "POP" on most other microprocessors. With the 6502, the stack is always on page one ($100-$1FF) and works top down.
    # TXS (Transfer X to Stack ptr)
    def _9a_txs(pc):
        nonlocal t, sp
    
        return pc + None
    # TSX (Transfer Stack ptr to X)
    def _ba_tsx(pc):
        nonlocal t, x
    
        return pc + None
    # PHA (PusH Accumulator)
    def _48_pha(pc):
        nonlocal t, sp
    
        return pc + None
    # PLA (PuLl Accumulator)
    def _68_pla(pc):
        nonlocal t, a
    
        return pc + None
    # PHP (PusH Processor status)
    def _08_php(pc):
        nonlocal t, sp
    
        return pc + None
    # PLP (PuLl Processor status)
    def _28_plp(pc):
        nonlocal t, p
    
        return pc + None
    
    # STX (STore X register)
    # Writes flags: none
    def _86_stx_zero_page(pc):
        nonlocal t
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _96_stx_zero_page_indexed_y(pc):
        nonlocal t
        addr32 = _resolve_zero_page_indexed_y(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 2
    def _8e_stx_absolute(pc):
        nonlocal t
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 3
    
    # STY (STore Y register)
    # Writes flags: none
    def _84_sty_zero_page(pc):
        nonlocal t
        addr32 = _resolve_zero_page(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 3
        return pc + 2
    def _94_sty_zero_page_indexed_x(pc):
        nonlocal t
        addr32 = _resolve_zero_page_indexed_x(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
        t += 4
        return pc + 2
    def _8c_sty_absolute(pc):
        nonlocal t
        addr32 = _resolve_absolute(mem, pc)
        m = mem[addr32]
        r = m  # do something here
        mapper.write(addr32, r&0xFF)
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
    ops[0x00] = _00_brk_implied
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
                self.mem[ROM_LOWER_BANK_OFFSET+bank_i*0x4000] = bank[i]

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
