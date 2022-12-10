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

    def _test_play(self, prg_rom, expected_registers, mem_patches=[], a=0x00, x=0x00, y=0x00, expected_mem_patches=[]):
        mapper = TestOperations._build_mapper(prg_rom)
        for i, values in mem_patches:
            for j, value in enumerate(values):
                mapper.mem[i+j] = value

        expected_mem = bytearray(mapper.mem)

        registers, t = play(mapper, (0x8000, 0x00, a, x, y, 0x00))

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

    # Test NOP!

    def test_nop(self):
        self._test_play(b'\xEA\x00', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00))
    
    # Test ALL read operations (via LDA/LDX)

    def test_lda_immediate(self):
        self._test_play(b'\xA9\x37\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00))
        self._test_play(b'\xA9\x00\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02))
        self._test_play(b'\xA9\x80\x00', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80))
        self._test_play(b'\xA9\xFF\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80))

    def test_lda_zero_page(self):
        self._test_play(b'\xA5\x00\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xA5\x80\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0x80, b'\x37')])
        self._test_play(b'\xA5\xFF\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0xFF, b'\x37')])
        self._test_play(b'\xA5\x12\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0x12, b'\x37')])
        self._test_play(b'\xA5\x13\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), mem_patches=[(0x13, b'\x00')])
        self._test_play(b'\xA5\x14\x00', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80), mem_patches=[(0x14, b'\x80')])
        self._test_play(b'\xA5\x15\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), mem_patches=[(0x15, b'\xFF')])

    def test_lda_zero_page_indexed_x(self):
        self._test_play(b'\xB5\x00\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x80\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x8F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\xFF\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x0E, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x12\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x21, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x13\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x02), mem_patches=[(0x22, b'\x00')], x=0x0F)
        self._test_play(b'\xB5\x14\x00', (0x8002, 0x00, 0x80, 0x0F, 0x00, 0x80), mem_patches=[(0x23, b'\x80')], x=0x0F)
        self._test_play(b'\xB5\x15\x00', (0x8002, 0x00, 0xFF, 0x0F, 0x00, 0x80), mem_patches=[(0x24, b'\xFF')], x=0x0F)
        
    def test_ldx_zero_page_indexed_y(self):
        self._test_play(b'\xB6\x00\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x80\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x8F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\xFF\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x0E, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x12\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x21, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x13\x00', (0x8002, 0x00, 0x00, 0x00, 0x0F, 0x02), mem_patches=[(0x22, b'\x00')], y=0x0F)
        self._test_play(b'\xB6\x14\x00', (0x8002, 0x00, 0x00, 0x80, 0x0F, 0x80), mem_patches=[(0x23, b'\x80')], y=0x0F)
        self._test_play(b'\xB6\x15\x00', (0x8002, 0x00, 0x00, 0xFF, 0x0F, 0x80), mem_patches=[(0x24, b'\xFF')], y=0x0F)

    def test_lda_absolute(self):
        self._test_play(b'\xAD\x00\x80\x00', (0x8003, 0x00, 0xAD, 0x00, 0x00, 0x80))
        self._test_play(b'\xAD\x00\xC0\x00', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0xC000, b'\x37')])
        self._test_play(b'\xAD\xFF\xFF\x00', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0xFFFF, b'\x37')])
        self._test_play(b'\xAD\x12\x80\x00', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), mem_patches=[(0x8012, b'\x37')])
        self._test_play(b'\xAD\x13\x80\x00', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x02), mem_patches=[(0x8013, b'\x00')])
        self._test_play(b'\xAD\x14\x80\x00', (0x8003, 0x00, 0x80, 0x00, 0x00, 0x80), mem_patches=[(0x8014, b'\x80')])
        self._test_play(b'\xAD\x15\x80\x00', (0x8003, 0x00, 0xFF, 0x00, 0x00, 0x80), mem_patches=[(0x8015, b'\xFF')])

    def test_lda_absolute_indexed_x(self):
        self._test_play(b'\xBD\x00\x80\x00', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x800F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x00\xC0\x00', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0xC00F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\xFF\xFF\x00', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x000E, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x12\x80\x00', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), mem_patches=[(0x8021, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x13\x80\x00', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x02), mem_patches=[(0x8022, b'\x00')], x=0x0F)
        self._test_play(b'\xBD\x14\x80\x00', (0x8003, 0x00, 0x80, 0x0F, 0x00, 0x80), mem_patches=[(0x8023, b'\x80')], x=0x0F)
        self._test_play(b'\xBD\x15\x80\x00', (0x8003, 0x00, 0xFF, 0x0F, 0x00, 0x80), mem_patches=[(0x8024, b'\xFF')], x=0x0F)

    def test_lda_absolute_indexed_y(self):
        self._test_play(b'\xB9\x00\x80\x00', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), x=0x0F, y=0x01)
        self._test_play(b'\xB9\x00\xC0\x00', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0xC001, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\xFF\xFF\x00', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x12\x80\x00', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x8013, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x13\x80\x00', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), mem_patches=[(0x8014, b'\x00')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x14\x80\x00', (0x8003, 0x00, 0x80, 0x0F, 0x01, 0x80), mem_patches=[(0x8015, b'\x80')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x15\x80\x00', (0x8003, 0x00, 0xFF, 0x0F, 0x01, 0x80), mem_patches=[(0x8016, b'\xFF')], x=0x0F, y=0x01)

    def test_lda_indexed_indirect(self):
        self._test_play(b'\xA1\x00\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x000F, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x000F, b'\xFF\xFF'), (0xFFFF, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xC0\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x00CF, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xFF\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), mem_patches=[(0x000E, b'\x00\xC0'), (0xC000, b'\x37')], x=0x0F, y=0x01)

    def test_lda_indirect_indexed(self):
        self._test_play(b'\xB1\x00\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), mem_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), mem_patches=[(0x0000, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), mem_patches=[(0x0000, b'\xFF\xFF'), (0x000E, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\xC0\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), mem_patches=[(0x00C0, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\xFF\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), mem_patches=[(0x00FF, b'\x00\xC0'), (0xC00F, b'\x37')], x=0x01, y=0x0F)

    # Test all operations (besides LDA which is tested thoroughly above)

    def test_ldx(self):
        self._test_play(b'\xA2\x37\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00))
        self._test_play(b'\xA6\x00\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00), mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB6\x00\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), mem_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xAE\x00\x80\x00', (0x8003, 0x00, 0x00, 0xAE, 0x00, 0x80))
        self._test_play(b'\xBE\x00\x80\x00', (0x8003, 0x00, 0x00, 0x00, 0x01, 0x02), x=0x0F, y=0x01)

    def test_ldy(self):
        self._test_play(b'\xA0\x37\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00))
        self._test_play(b'\xA4\x00\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00), mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB4\x00\x00', (0x8002, 0x00, 0x00, 0x0F, 0x37, 0x00), mem_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xAC\x00\x80\x00', (0x8003, 0x00, 0x00, 0x00, 0xAC, 0x80))
        self._test_play(b'\xBC\x00\x80\x00', (0x8003, 0x00, 0x00, 0x01, 0x00, 0x02), x=0x01, y=0x0F)

    def test_lsr(self):
        self._test_play(b'\x4A\x00', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x01), a=0x6F)
        self._test_play(b'\x4A\x00', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x00), a=0x6E)
        self._test_play(b'\x46\x00\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x01), mem_patches=[(0x00, b'\x6F')], expected_mem_patches=[(0x00, b'\x37')])
        self._test_play(b'\x56\x00\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x01), mem_patches=[(0x0F, b'\x6F')], x=0x0F, expected_mem_patches=[(0x0F, b'\x37')])
        self._test_play(b'\x4E\x00\x02\x00', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x01), mem_patches=[(0x0200, b'\x6F')], expected_mem_patches=[(0x0200, b'\x37')])
        self._test_play(b'\x5E\x00\x02\x00', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x01), mem_patches=[(0x020F, b'\x6F')], x=0x0F, expected_mem_patches=[(0x020F, b'\x37')])

    def test_adc(self):
        # TODO:
        # test permutations of carry flag
        # test permutations overflow
        # test all address modes in basic manor
        pass