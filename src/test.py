import unittest
import array

from curt_nes import play, Mapper

# TODO:
# - test how page crossing impacts timing

class TestMapper(unittest.TestCase):
    empty_mem = array.array('B', (0 for _ in range(0x10000)))

    def test_ram_mappings(self):
        mapper = Mapper(self.empty_mem, 1, 0)
        # $0000-$07FF	$0800	2KB internal RAM
        for i in range(0x0000, 0x0800):
            self.assertEqual(mapper.resolve(i), i)
        # $0800-$0FFF	$0800	Mirrors of $0000-$07FF
        for i in range(0x0800, 0x1000):
            self.assertEqual(mapper.resolve(i), i-0x0800)
        # $1000-$17FF	$0800
        for i in range(0x1000, 0x1800):
            self.assertEqual(mapper.resolve(i), i-0x1000)
        # $1800-$1FFF	$0800
        for i in range(0x1800, 0x2000):
            self.assertEqual(mapper.resolve(i), i-0x1800)

    def test_ppu_mappings(self):
        mapper = Mapper(self.empty_mem, 1, 0)
        # $2000-$2007	$0008	NES PPU registers
        for i in range(0x2000, 0x2008):
            self.assertEqual(mapper.resolve(i), i)
        # $2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
        for i in range(0x2008, 0x4000):
            self.assertEqual(mapper.resolve(i), 0x2000+(i%8))

    def test_apu_mappings(self):
        mapper = Mapper(self.empty_mem, 1, 0)
        # $4000-$4017	$0018	NES APU and I/O registers
        for i in range(0x4000, 0x4018):
            self.assertEqual(mapper.resolve(i), i)
        # $4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
        for i in range(0x4018, 0x4020):
            self.assertEqual(mapper.resolve(i), i)

    def test_misc_cart_mappings(self):
        mapper = Mapper(self.empty_mem, 1, 0)
        for i in range(0x4018, 0x8000):
            self.assertEqual(mapper.resolve(i), i)

    def test_rom_one_bank_mappings(self):
        mapper = Mapper(self.empty_mem, 1, 0)
        # $4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
        for i in range(0x8000, 0xC000):
            self.assertEqual(mapper.resolve(i), i)
        for i in range(0xC000, 0x10000):
            self.assertEqual(mapper.resolve(i), 0x8000+(i-0xC000))

    def test_rom_two_bank_mappings(self):
        mapper = Mapper(self.empty_mem, 2, 0)
        # $4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
        for i in range(0x8000, 0x10000):
            self.assertEqual(mapper.resolve(i), i)

class TestOperations(unittest.TestCase):
    @staticmethod
    def _build_mapper(prg_rom, chr_rom=b'', mapper_cls=Mapper):
        mem = array.array('B', (0 for _ in range(0x10000)))
        for i, value in enumerate(prg_rom):
            mem[0x8000+i] = value
        return mapper_cls(mem, 2, 0)

    def _test_play(self, prg_rom, expected_registers, expected_t=None, pc=0x8000, s=0x00, a=0x00, x=0x00, y=0x00, p=0x00, mem_patches=[], expected_mem_patches=[]):
        mapper = TestOperations._build_mapper(prg_rom)
        for i, values in mem_patches:
            for j, value in enumerate(values):
                mapper.mem[i+j] = value

        expected_mem = bytearray(mapper.mem)

        registers, t = play(mapper, (pc, s, a, x, y, p))

        self.assertTupleEqual(registers, expected_registers)

        for addr, expected_mem_bytes in expected_mem_patches:
            start = addr
            end = addr + len(expected_mem_bytes)
            expected_mem[start:end] = mapper.mem[start:end]
            self.assertEqual(
                bytearray(mapper.mem[start:end]),
                expected_mem_bytes)

        # Ensure memory other than what was patched, wasn't touched
        self.assertEqual(mapper.mem, expected_mem)

        if expected_t is not None:
            self.assertEqual(t, expected_t)

    # Test NOP!

    def test_nop(self):
        self._test_play(b'\xEA', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00))
    
    # Test ALL read operations (via LDA/LDX)

    def test_lda_immediate(self):
        self._test_play(b'\xA9\x37', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 2)
        self._test_play(b'\xA9\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2)
        self._test_play(b'\xA9\x80', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80), 2)
        self._test_play(b'\xA9\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2)

    def test_lda_zero_page(self):
        self._test_play(b'\xA5\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xA5\x80', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, mem_patches=[(0x80, b'\x37')])
        self._test_play(b'\xA5\xFF', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, mem_patches=[(0xFF, b'\x37')])
        self._test_play(b'\xA5\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, mem_patches=[(0x12, b'\x37')])
        self._test_play(b'\xA5\x13', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 3, mem_patches=[(0x13, b'\x00')])
        self._test_play(b'\xA5\x14', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80), 3, mem_patches=[(0x14, b'\x80')])
        self._test_play(b'\xA5\x15', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 3, mem_patches=[(0x15, b'\xFF')])

    def test_lda_zero_page_indexed_x(self):
        self._test_play(b'\xB5\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x80', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x8F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\xFF', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x0E, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x12', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x21, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x13', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x02), 4, mem_patches=[(0x22, b'\x00')], x=0x0F)
        self._test_play(b'\xB5\x14', (0x8002, 0x00, 0x80, 0x0F, 0x00, 0x80), 4, mem_patches=[(0x23, b'\x80')], x=0x0F)
        self._test_play(b'\xB5\x15', (0x8002, 0x00, 0xFF, 0x0F, 0x00, 0x80), 4, mem_patches=[(0x24, b'\xFF')], x=0x0F)
        
    def test_ldx_zero_page_indexed_y(self):
        self._test_play(b'\xB6\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, mem_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x80', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, mem_patches=[(0x8F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\xFF', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, mem_patches=[(0x0E, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x12', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, mem_patches=[(0x21, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x13', (0x8002, 0x00, 0x00, 0x00, 0x0F, 0x02), 4, mem_patches=[(0x22, b'\x00')], y=0x0F)
        self._test_play(b'\xB6\x14', (0x8002, 0x00, 0x00, 0x80, 0x0F, 0x80), 4, mem_patches=[(0x23, b'\x80')], y=0x0F)
        self._test_play(b'\xB6\x15', (0x8002, 0x00, 0x00, 0xFF, 0x0F, 0x80), 4, mem_patches=[(0x24, b'\xFF')], y=0x0F)

    def test_lda_absolute(self):
        self._test_play(b'\xAD\x00\x80', (0x8003, 0x00, 0xAD, 0x00, 0x00, 0x80), 4)
        self._test_play(b'\xAD\x00\xC0', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, mem_patches=[(0xC000, b'\x37')])
        self._test_play(b'\xAD\xFF\xFF', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, mem_patches=[(0xFFFF, b'\x37')])
        self._test_play(b'\xAD\x12\x80', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, mem_patches=[(0x8012, b'\x37')])
        self._test_play(b'\xAD\x13\x80', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x02), 4, mem_patches=[(0x8013, b'\x00')])
        self._test_play(b'\xAD\x14\x80', (0x8003, 0x00, 0x80, 0x00, 0x00, 0x80), 4, mem_patches=[(0x8014, b'\x80')])
        self._test_play(b'\xAD\x15\x80', (0x8003, 0x00, 0xFF, 0x00, 0x00, 0x80), 4, mem_patches=[(0x8015, b'\xFF')])

    def test_lda_absolute_indexed_x(self):
        self._test_play(b'\xBD\x00\x80', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x800F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x00\xC0', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0xC00F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\xFF\xFF', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 5, mem_patches=[(0x000E, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x12\x80', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, mem_patches=[(0x8021, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x13\x80', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x02), 4, mem_patches=[(0x8022, b'\x00')], x=0x0F)
        self._test_play(b'\xBD\x14\x80', (0x8003, 0x00, 0x80, 0x0F, 0x00, 0x80), 4, mem_patches=[(0x8023, b'\x80')], x=0x0F)
        self._test_play(b'\xBD\x15\x80', (0x8003, 0x00, 0xFF, 0x0F, 0x00, 0x80), 4, mem_patches=[(0x8024, b'\xFF')], x=0x0F)

    def test_lda_absolute_indexed_y(self):
        self._test_play(b'\xB9\x00\x80', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), 4, x=0x0F, y=0x01)
        self._test_play(b'\xB9\x00\xC0', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 4, mem_patches=[(0xC001, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\xFF\xFF', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 5, mem_patches=[(0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x12\x80', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 4, mem_patches=[(0x8013, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x13\x80', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), 4, mem_patches=[(0x8014, b'\x00')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x14\x80', (0x8003, 0x00, 0x80, 0x0F, 0x01, 0x80), 4, mem_patches=[(0x8015, b'\x80')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x15\x80', (0x8003, 0x00, 0xFF, 0x0F, 0x01, 0x80), 4, mem_patches=[(0x8016, b'\xFF')], x=0x0F, y=0x01)

    def test_lda_indexed_indirect(self):
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, mem_patches=[(0x000F, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, mem_patches=[(0x000F, b'\xFF\xFF'), (0xFFFF, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xC0', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, mem_patches=[(0x00CF, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xFF', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, mem_patches=[(0x000E, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)

    def test_lda_indirect_indexed(self):
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, mem_patches=[(0x0000, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 6, mem_patches=[(0x0000, b'\xFF\xFF'), (0x000E, b'\x37')], x=0x01, y=0x0F) # page crossing
        self._test_play(b'\xB1\xC0', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, mem_patches=[(0x00C0, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\xFF', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, mem_patches=[(0x00FF, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)

    # Test all operations (besides LDA which is tested thoroughly above)

    def test_ldx(self):
        self._test_play(b'\xA2\x37', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00))
        self._test_play(b'\xA6\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00), mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB6\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xAE\x00\x80', (0x8003, 0x00, 0x00, 0xAE, 0x00, 0x80))
        self._test_play(b'\xBE\x00\x80', (0x8003, 0x00, 0x00, 0x00, 0x01, 0x02), x=0x0F, y=0x01)

    def test_ldy(self):
        self._test_play(b'\xA0\x37', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00))
        self._test_play(b'\xA4\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00), mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB4\x00', (0x8002, 0x00, 0x00, 0x0F, 0x37, 0x00), mem_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xAC\x00\x80', (0x8003, 0x00, 0x00, 0x00, 0xAC, 0x80))
        self._test_play(b'\xBC\x00\x80', (0x8003, 0x00, 0x00, 0x01, 0x00, 0x02), x=0x01, y=0x0F)

    def test_lsr(self):
        self._test_play(b'\x4A', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x6F)
        self._test_play(b'\x4A', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x00), a=0x6E)
        self._test_play(b'\x46\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x01), mem_patches=[(0x00, b'\x6F')], expected_mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\x56\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x01), mem_patches=[(0x0F, b'\x6F')], x=0x0F, expected_mem_patches=[(0x0F, b'\x37')])
        self._test_play(b'\x4E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x01), mem_patches=[(0x0200, b'\x6F')], expected_mem_patches=[(0x0200, b'\x37')])
        self._test_play(b'\x5E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x01), mem_patches=[(0x020F, b'\x6F')], x=0x0F, expected_mem_patches=[(0x020F, b'\x37')])

    def test_adc(self):
        self._test_play(b'\x69\x12', (0x8002, 0x00, 0x49, 0x00, 0x00, 0x00), a=0x37)
        self._test_play(b'\x69\x12', (0x8002, 0x00, 0x4A, 0x00, 0x00, 0x00), a=0x37, p=0x01) # borrow
        self._test_play(b'\x69\x02', (0x8002, 0x00, 0x01, 0x00, 0x00, 0x01), a=0xFF) # carry
        self._test_play(b'\x69\x7E', (0x8002, 0x00, 0xFC, 0x00, 0x00, 0xC0), a=0x7E) # overflow, negative
        self._test_play(b'\x69\xFE', (0x8002, 0x00, 0x7E, 0x00, 0x00, 0x41), a=0x80) # overflow, carry
        self._test_play(b'\x69\xFE', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x03), a=0x02) # zero
        self._test_play(b'\x65\x00', (0x8002, 0x00, 0x49, 0x00, 0x00, 0x00), a=0x37, mem_patches=[(0x00, b'\x12')])
        self._test_play(b'\x75\x00', (0x8002, 0x00, 0x49, 0x0F, 0x00, 0x00), a=0x37, mem_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\x6D\x00\x02', (0x8003, 0x00, 0x49, 0x00, 0x00, 0x00), a=0x37, mem_patches=[(0x0200, b'\x12')])
        self._test_play(b'\x7D\x00\x02', (0x8003, 0x00, 0x49, 0x0F, 0x00, 0x00), a=0x37, mem_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\x79\x00\x02', (0x8003, 0x00, 0x49, 0x01, 0x0F, 0x00), a=0x37, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x12')])
        self._test_play(b'\x61\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), a=0x01, mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x36')], x=0x0F, y=0x01)
        self._test_play(b'\x71\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), a=0x01, mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x36')], x=0x01, y=0x0F)

    def test_and(self):
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), a=0xFF)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), a=0x00)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), a=0xAA)
        self._test_play(b'\x29\xAA', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), a=0xFF)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0x55, 0x00, 0x00, 0x00), a=0x55)
        self._test_play(b'\x29\xAA', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), a=0x55)
        self._test_play(b'\x25\x00', (0x8002, 0x00, 0x0A, 0x00, 0x00, 0x00), a=0xAA, mem_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x35\x00', (0x8002, 0x00, 0x0A, 0x0F, 0x00, 0x00), a=0xAA, mem_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x2D\x00\x02', (0x8003, 0x00, 0x0A, 0x00, 0x00, 0x00), a=0xAA, mem_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x3D\x00\x02', (0x8003, 0x00, 0x0A, 0x0F, 0x00, 0x00), a=0xAA, mem_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x39\x00\x02', (0x8003, 0x00, 0x0A, 0x01, 0x0F, 0x00), a=0xAA, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x21\x00', (0x8002, 0x00, 0x0A, 0x0F, 0x01, 0x00), a=0xAA, mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x31\x00', (0x8002, 0x00, 0x0A, 0x01, 0x0F, 0x00), a=0xAA, mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

    def test_asl(self):
        self._test_play(b'\x0A', (0x8001, 0x00, 0x0A, 0x00, 0x00, 0x00), a=0x05)
        self._test_play(b'\x0A', (0x8001, 0x00, 0xAA, 0x00, 0x00, 0x80), a=0x55) # negative
        self._test_play(b'\x0A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x03), a=0x80) # zero, carry
        self._test_play(b'\x0A', (0x8001, 0x00, 0x40, 0x00, 0x00, 0x01), a=0xA0) # carry
        self._test_play(b'\x0A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), a=0x00) # zero
        self._test_play(b'\x06\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), mem_patches=[(0x00, b'\x55')], expected_mem_patches=[(0x00, b'\xAA')])
        self._test_play(b'\x16\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x80), mem_patches=[(0x0F, b'\x55')], x=0x0F, expected_mem_patches=[(0x0F, b'\xAA')])
        self._test_play(b'\x0E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x80), mem_patches=[(0x0200, b'\x55')], expected_mem_patches=[(0x0200, b'\xAA')])
        self._test_play(b'\x1E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x80), mem_patches=[(0x020F, b'\x55')], x=0x0F, expected_mem_patches=[(0x020F, b'\xAA')])

    def test_bit(self):
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0x55, 0x00, 0x00, 0x00), a=0x55, mem_patches=[(0x00, b'\x01')])
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x02), a=0xAA, mem_patches=[(0x00, b'\x05')]) # zero
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), a=0xAA, mem_patches=[(0x00, b'\xA0')]) # negative
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x40), a=0xAA, mem_patches=[(0x00, b'\x42')]) # overflow
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0xC0), a=0xAA, mem_patches=[(0x00, b'\xF0')]) # negative, overflow
        self._test_play(b'\x2C\x00\x02', (0x8003, 0x00, 0x55, 0x00, 0x00, 0x00), a=0x55, mem_patches=[(0x0200, b'\x01')])

    def test_sbc(self):
        self._test_play(b'\xE9\x12', (0x8002, 0x00, 0x25, 0x00, 0x00, 0x01), a=0x37, p=0x01)
        self._test_play(b'\xE9\x12', (0x8002, 0x00, 0x24, 0x00, 0x00, 0x01), a=0x37, p=0x00) # borrow-ed
        self._test_play(b'\xE9\x0F', (0x8002, 0x00, 0xF9, 0x00, 0x00, 0xC0), a=0x08, p=0x01) # borrow, negative, overflow
        self._test_play(b'\xE9\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x03), a=0x02, p=0x01) # zero
        self._test_play(b'\xE5\x00', (0x8002, 0x00, 0x25, 0x00, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x00, b'\x12')])
        self._test_play(b'\xF5\x00', (0x8002, 0x00, 0x25, 0x0F, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\xED\x00\x02', (0x8003, 0x00, 0x25, 0x00, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x0200, b'\x12')])
        self._test_play(b'\xFD\x00\x02', (0x8003, 0x00, 0x25, 0x0F, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\xF9\x00\x02', (0x8003, 0x00, 0x25, 0x01, 0x0F, 0x01), a=0x37, p=0x01, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x12')])
        self._test_play(b'\xE1\x00', (0x8002, 0x00, 0x25, 0x0F, 0x01, 0x01), a=0x37, p=0x01, mem_patches=[(0x000F, b'\x00\x02'), (0x0200, b'\x12')], x=0x0F, y=0x01)
        self._test_play(b'\xF1\x00', (0x8002, 0x00, 0x25, 0x01, 0x0F, 0x01), a=0x37, p=0x01, mem_patches=[(0x0000, b'\x00\x02'), (0x020F, b'\x12')], x=0x01, y=0x0F)

    def test_bpl(self):
        self._test_play(b'\x10\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 2, p=0x80) # no branch
        self._test_play(b'\x10\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00) # branch
        self._test_play(b'', (0x8100, 0x00, 0x00, 0x00, 0x00, 0x00), 4, pc=0x807F, p=0x00, mem_patches=[(0x807F, b'\x10\x7F')]) # branch / page crossed
        self._test_play(b'\x10\x02\x00\x00\xEA', (0x8005, 0x00, 0x00, 0x00, 0x00, 0x00), 5, p=0x00) # branch to nop (and execute that instruction too)
        self._test_play(b'\x00\x10\xFD', (0x8000, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00, pc=0x8001) # branch negative address
        self._test_play(b'\x10\xFD', (0x7FFF, 0x00, 0x00, 0x00, 0x00, 0x00), 4, p=0x00) # branch negative address / page crossed

    def test_bmi(self):
        self._test_play(b'\x30\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x00) # no branch
        self._test_play(b'\x30\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x80), 3, p=0x80) # branch

    def test_bvc(self):
        self._test_play(b'\x50\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x40), 2, p=0x40) # no branch
        self._test_play(b'\x50\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00) # branch

    def test_bvs(self):
        self._test_play(b'\x70\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x00) # no branch
        self._test_play(b'\x70\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x40), 3, p=0x40) # branch

    def test_bcc(self):
        self._test_play(b'\x90\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x01), 2, p=0x01) # no branch
        self._test_play(b'\x90\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00) # branch

    def test_bcs(self):
        self._test_play(b'\xB0\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x00) # no branch
        self._test_play(b'\xB0\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x01), 3, p=0x01) # branch

    def test_bne(self):
        self._test_play(b'\xD0\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, p=0x02) # no branch
        self._test_play(b'\xD0\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00) # branch

    def test_beq(self):
        self._test_play(b'\xF0\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x00) # no branch
        self._test_play(b'\xF0\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x02), 3, p=0x02) # branch

    def test_cmp(self):
        self._test_play(b'\xC9\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xC9\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xC9\x37', (0x8002, 0x00, 0x12, 0x00, 0x00, 0x80), a=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xC9\x02', (0x8002, 0x00, 0x02, 0x00, 0x00, 0x03), a=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xC5\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x00, b'\x12')])
        self._test_play(b'\xD5\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\xCD\x00\x02', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x0200, b'\x12')])
        self._test_play(b'\xDD\x00\x02', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x01), a=0x37, p=0x01, mem_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\xD9\x00\x02', (0x8003, 0x00, 0x37, 0x01, 0x0F, 0x01), a=0x37, p=0x01, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x12')])
        self._test_play(b'\xC1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x01), a=0x37, p=0x01, mem_patches=[(0x000F, b'\x00\x02'), (0x0200, b'\x12')], x=0x0F, y=0x01)
        self._test_play(b'\xD1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x01), a=0x37, p=0x01, mem_patches=[(0x0000, b'\x00\x02'), (0x020F, b'\x12')], x=0x01, y=0x0F)

    def test_cpx(self):
        self._test_play(b'\xE0\x12', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), x=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xE0\x12', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), x=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xE0\x37', (0x8002, 0x00, 0x00, 0x12, 0x00, 0x80), x=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xE0\x02', (0x8002, 0x00, 0x00, 0x02, 0x00, 0x03), x=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xE4\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), x=0x37, p=0x01, mem_patches=[(0x00, b'\x12')])
        self._test_play(b'\xEC\x00\x02', (0x8003, 0x00, 0x00, 0x37, 0x00, 0x01), x=0x37, p=0x01, mem_patches=[(0x0200, b'\x12')])

    def test_cpy(self):
        self._test_play(b'\xC0\x12', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), y=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xC0\x12', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), y=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xC0\x37', (0x8002, 0x00, 0x00, 0x00, 0x12, 0x80), y=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xC0\x02', (0x8002, 0x00, 0x00, 0x00, 0x02, 0x03), y=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xC4\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), y=0x37, p=0x01, mem_patches=[(0x00, b'\x12')])
        self._test_play(b'\xCC\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x37, 0x01), y=0x37, p=0x01, mem_patches=[(0x0200, b'\x12')])
    
    def test_dec(self):
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 5, mem_patches=[(0x00, b'\x12')], expected_mem_patches=[(0x00, b'\x11')])
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 5, mem_patches=[(0x00, b'\x01')], expected_mem_patches=[(0x00, b'\x00')]) # zero
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 5, mem_patches=[(0x00, b'\x00')], expected_mem_patches=[(0x00, b'\xFF')]) # negative
        self._test_play(b'\xD6\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x00), 6, mem_patches=[(0x0F, b'\x12')], x=0x0F, expected_mem_patches=[(0x0F, b'\x11')])
        self._test_play(b'\xCE\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x00), 6, mem_patches=[(0x0200, b'\x12')], expected_mem_patches=[(0x0200, b'\x11')])
        self._test_play(b'\xDE\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x00), 7, mem_patches=[(0x020F, b'\x12')], x=0x0F, expected_mem_patches=[(0x020F, b'\x11')])

    def test_eor(self):
        self._test_play(b'\x49\xFF', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0xFF)
        self._test_play(b'\x49\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00)
        self._test_play(b'\x49\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0x00)
        self._test_play(b'\x49\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x49\x55', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xAA)
        self._test_play(b'\x45\x00', (0x8002, 0x00, 0xF0, 0x00, 0x00, 0x80), 3, a=0xAA, mem_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x55\x00', (0x8002, 0x00, 0xF0, 0x0F, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x4D\x00\x02', (0x8003, 0x00, 0xF0, 0x00, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x5D\x00\x02', (0x8003, 0x00, 0xF0, 0x0F, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x59\x00\x02', (0x8003, 0x00, 0xF0, 0x01, 0x0F, 0x80), 4, a=0xAA, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x41\x00', (0x8002, 0x00, 0xF0, 0x0F, 0x01, 0x80), 6, a=0xAA, mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x51\x00', (0x8002, 0x00, 0xF0, 0x01, 0x0F, 0x80), 5, a=0xAA, mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

    def test_clc(self):
        self._test_play(b'\x18', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xFE), 2, p=0xFF)
        self._test_play(b'\x18', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x01)

    def test_sec(self):
        self._test_play(b'\x38', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xFF), 2, p=0xFE)
        self._test_play(b'\x38', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x01), 2, p=0x00)

    def test_cli(self):
        self._test_play(b'\x58', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xFB), 2, p=0xFF)
        self._test_play(b'\x58', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x04)

    def test_sei(self):
        self._test_play(b'\x78', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xFF), 2, p=0xFB)
        self._test_play(b'\x78', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x04), 2, p=0x00)

    def test_clv(self):
        self._test_play(b'\xB8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xBF), 2, p=0xFF)
        self._test_play(b'\xB8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x40)

    def test_cld(self):
        self._test_play(b'\xD8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xF7), 2, p=0xFF)
        self._test_play(b'\xD8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00), 2, p=0x08)

    def test_sed(self):
        self._test_play(b'\xF8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0xFF), 2, p=0xF7)
        self._test_play(b'\xF8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x08), 2, p=0x00)

    def test_inc(self):
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 5, mem_patches=[(0x00, b'\x12')], expected_mem_patches=[(0x00, b'\x13')])
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 5, mem_patches=[(0x00, b'\xFF')], expected_mem_patches=[(0x00, b'\x00')]) # zero
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 5, mem_patches=[(0x00, b'\x80')], expected_mem_patches=[(0x00, b'\x81')]) # negative
        self._test_play(b'\xF6\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x00), 6, mem_patches=[(0x0F, b'\x12')], x=0x0F, expected_mem_patches=[(0x0F, b'\x13')])
        self._test_play(b'\xEE\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x00), 6, mem_patches=[(0x0200, b'\x12')], expected_mem_patches=[(0x0200, b'\x13')])
        self._test_play(b'\xFE\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x00), 7, mem_patches=[(0x020F, b'\x12')], x=0x0F, expected_mem_patches=[(0x020F, b'\x13')])

    def test_jmp(self):
        self._test_play(b'\x4C\x00\x02', (0x0200, 0x00, 0x00, 0x00, 0x00, 0x00), 3)
        self._test_play(b'\x6C\x00\x02', (0xC000, 0x00, 0x00, 0x00, 0x00, 0x00), 5, mem_patches=[(0x0200, b'\x00\xC0')])

    def test_jsr(self):
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFD, 0x00, 0x00, 0x00, 0x00), 6, s=0xFF, expected_mem_patches=[(0x01FE, b'\x02\x80')])
        self._test_play(b'\x20\x00\x02', (0x0200, 0x00, 0x00, 0x00, 0x00, 0x00), 6, s=0x02, expected_mem_patches=[(0x0101, b'\x02\x80')])
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFE, 0x00, 0x00, 0x00, 0x00), 6, s=0x00, expected_mem_patches=[(0x0100, b'\x80'), (0x01FF, b'\x02')]) # stack wrap-around mid address
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFF, 0x00, 0x00, 0x00, 0x00), 6, s=0x01, expected_mem_patches=[(0x0100, b'\x02\x80')]) # stack wrap-around after address

    def test_ora(self):
        self._test_play(b'\x09\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00)
        self._test_play(b'\x09\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0x00)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x55', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xAA)
        self._test_play(b'\x05\x00', (0x8002, 0x00, 0xFA, 0x00, 0x00, 0x80), 3, a=0xAA, mem_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x15\x00', (0x8002, 0x00, 0xFA, 0x0F, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x0D\x00\x02', (0x8003, 0x00, 0xFA, 0x00, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x1D\x00\x02', (0x8003, 0x00, 0xFA, 0x0F, 0x00, 0x80), 4, a=0xAA, mem_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x19\x00\x02', (0x8003, 0x00, 0xFA, 0x01, 0x0F, 0x80), 4, a=0xAA, x=0x01, y=0x0F, mem_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x01\x00', (0x8002, 0x00, 0xFA, 0x0F, 0x01, 0x80), 6, a=0xAA, mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x11\x00', (0x8002, 0x00, 0xFA, 0x01, 0x0F, 0x80), 5, a=0xAA, mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

    def test_tax(self):
        self._test_play(b'\xAA', (0x8001, 0x00, 0x37, 0x37, 0x00, 0x00), 2, a=0x37)
        self._test_play(b'\xAA', (0x8001, 0x00, 0xFF, 0xFF, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\xAA', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00, x=0xFF)

    def test_txa(self):
        self._test_play(b'\x8A', (0x8001, 0x00, 0x37, 0x37, 0x00, 0x00), 2, x=0x37)
        self._test_play(b'\x8A', (0x8001, 0x00, 0xFF, 0xFF, 0x00, 0x80), 2, x=0xFF)
        self._test_play(b'\x8A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, x=0x00, a=0xFF)

    def test_tay(self):
        self._test_play(b'\xA8', (0x8001, 0x00, 0x37, 0x00, 0x37, 0x00), 2, a=0x37)
        self._test_play(b'\xA8', (0x8001, 0x00, 0xFF, 0x00, 0xFF, 0x80), 2, a=0xFF)
        self._test_play(b'\xA8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00, y=0xFF)

    def test_tya(self):
        self._test_play(b'\x98', (0x8001, 0x00, 0x37, 0x00, 0x37, 0x00), 2, y=0x37)
        self._test_play(b'\x98', (0x8001, 0x00, 0xFF, 0x00, 0xFF, 0x80), 2, y=0xFF)
        self._test_play(b'\x98', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, y=0x00, a=0xFF)

    def test_dex(self):
        self._test_play(b'\xCA', (0x8001, 0x00, 0x00, 0x36, 0x00, 0x00), 2, x=0x37)
        self._test_play(b'\xCA', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, x=0x01)
        self._test_play(b'\xCA', (0x8001, 0x00, 0x00, 0xFE, 0x00, 0x80), 2, x=0xFF)
        self._test_play(b'\xCA', (0x8001, 0x00, 0x00, 0xFF, 0x00, 0x80), 2, x=0x00)

    def test_dey(self):
        self._test_play(b'\x88', (0x8001, 0x00, 0x00, 0x00, 0x36, 0x00), 2, y=0x37)
        self._test_play(b'\x88', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, y=0x01)
        self._test_play(b'\x88', (0x8001, 0x00, 0x00, 0x00, 0xFE, 0x80), 2, y=0xFF)
        self._test_play(b'\x88', (0x8001, 0x00, 0x00, 0x00, 0xFF, 0x80), 2, y=0x00)

    def test_inx(self):
        self._test_play(b'\xE8', (0x8001, 0x00, 0x00, 0x38, 0x00, 0x00), 2, x=0x37)
        self._test_play(b'\xE8', (0x8001, 0x00, 0x00, 0xFF, 0x00, 0x80), 2, x=0xFE)
        self._test_play(b'\xE8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, x=0xFF)
        self._test_play(b'\xE8', (0x8001, 0x00, 0x00, 0x01, 0x00, 0x00), 2, x=0x00)

    def test_iny(self):
        self._test_play(b'\xC8', (0x8001, 0x00, 0x00, 0x00, 0x38, 0x00), 2, y=0x37)
        self._test_play(b'\xC8', (0x8001, 0x00, 0x00, 0x00, 0xFF, 0x80), 2, y=0xFE)
        self._test_play(b'\xC8', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, y=0xFF)
        self._test_play(b'\xC8', (0x8001, 0x00, 0x00, 0x00, 0x01, 0x00), 2, y=0x00)

    def test_rol(self):
        self._test_play(b'\x2A', (0x8001, 0x00, 0x0A, 0x00, 0x00, 0x00), 2, a=0x05)
        self._test_play(b'\x2A', (0x8001, 0x00, 0x0B, 0x00, 0x00, 0x00), 2, a=0x05, p=0x01) # rotate in carry
        self._test_play(b'\x2A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x80) # rotate out carry, zero
        self._test_play(b'\x2A', (0x8001, 0x00, 0x01, 0x00, 0x00, 0x01), 2, a=0x80, p=0x01) # rotate in/out carry
        self._test_play(b'\x26\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x81), 5, p=0x01, mem_patches=[(0x00, b'\xD5')], expected_mem_patches=[(0x00, b'\xAB')])
        self._test_play(b'\x36\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x81), 6, p=0x01, mem_patches=[(0x0F, b'\xD5')], x=0x0F, expected_mem_patches=[(0x0F, b'\xAB')])
        self._test_play(b'\x2E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x81), 6, p=0x01, mem_patches=[(0x0200, b'\xD5')], expected_mem_patches=[(0x0200, b'\xAB')])
        self._test_play(b'\x3E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x81), 7, p=0x01, mem_patches=[(0x020F, b'\xD5')], x=0x0F, expected_mem_patches=[(0x020F, b'\xAB')])

    def test_ror(self):
        self._test_play(b'\x6A', (0x8001, 0x00, 0x01, 0x00, 0x00, 0x00), 2, a=0x02)
        self._test_play(b'\x6A', (0x8001, 0x00, 0x02, 0x00, 0x00, 0x01), 2, a=0x05) # rotate out carry
        self._test_play(b'\x6A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x01) # rotate out carry, zero
        self._test_play(b'\x6A', (0x8001, 0x00, 0xC0, 0x00, 0x00, 0x81), 2, a=0x81, p=0x01) # rotate in/out carry
        self._test_play(b'\x6A', (0x8001, 0x00, 0x80, 0x00, 0x00, 0x80), 2, a=0x00, p=0x01) # rotate in
        self._test_play(b'\x66\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x81), 5, p=0x01, mem_patches=[(0x00, b'\xD5')], expected_mem_patches=[(0x00, b'\xEA')])
        self._test_play(b'\x76\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x81), 6, p=0x01, mem_patches=[(0x0F, b'\xD5')], x=0x0F, expected_mem_patches=[(0x0F, b'\xEA')])
        self._test_play(b'\x6E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x81), 6, p=0x01, mem_patches=[(0x0200, b'\xD5')], expected_mem_patches=[(0x0200, b'\xEA')])
        self._test_play(b'\x7E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x81), 7, p=0x01, mem_patches=[(0x020F, b'\xD5')], x=0x0F, expected_mem_patches=[(0x020F, b'\xEA')])

    def test_rti(self):
        self._test_play(b'\x00', (0x8000, 0xFF, 0x00, 0x00, 0x00, 0xAA), 6, pc=0xC000, s=0xFC, mem_patches=[(0x1FD, b'\xAA\x00\x80'), (0xC000, b'\x40')])

    def test_rts(self):
        self._test_play(b'\x00', (0x8000, 0xFF, 0x00, 0x00, 0x00, 0x00), 6, pc=0xC000, s=0xFD, mem_patches=[(0x1FE, b'\xFF\x7F'), (0xC000, b'\x60')])

    def test_sta(self):
        pass

    def test_txs(self):
        pass

    def test_tsx(self):
        pass

    def test_pha(self):
        pass

    def test_pla(self):
        pass

    def test_php(self):
        pass

    def test_plp(self):
        pass

    def test_stx(self):
        pass

    def test_sty(self):
        pass
