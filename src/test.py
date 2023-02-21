import unittest
import array

from curt_nes import play, Mapper, PPU

class TestMapper(unittest.TestCase):
    sample_ram  = array.array('B', (0x00|(i&0xF) for i in range(0x800)))
    sample_vram = array.array('B', (0x10|(i&0xF) for i in range(0x800)))
    sample_prg_rom_banks = [
        array.array('B', (0x20|(i&0xF) for i in range(0x4000))),
        array.array('B', (0x30|(i&0xF) for i in range(0x4000)))
    ]
    sample_prg_ram = array.array('B', (0x40|(i&0xF) for i in range(0x2000)))
    sample_chr_rom = array.array('B', (0x50|(i&0xF) for i in range(0x2000)))
    sample_chr_ram = array.array('B', (0x60|(i&0xF) for i in range(0x2000)))
    sample_pals = array.array('B', (i&0xF for i in range(0x20)))

    def setUp(self):
        self.ppu_reads  = ppu_reads  = []
        self.ppu_writes = ppu_writes = []
        self.apu_reads  = apu_reads  = []
        self.apu_writes = apu_writes = []
        def ppu_read(addr):
            ppu_reads.append(addr)
            return 0x55
        def ppu_write(addr, value):
            ppu_writes.append((addr, value))
        def apu_read(addr):
            apu_reads.append(addr)
            return 0x55
        def apu_write(addr, value):
            apu_writes.append((addr, value))
        self.mapper = Mapper(
            self.sample_ram,
            self.sample_vram,
            sample_pals,
            self.sample_prg_rom_banks,
            self.sample_prg_ram,
            self.sample_chr_rom,
            None, # self.sample_chr_ram,
            ppu_read,
            ppu_write,
            apu_read,
            apu_write)

    def test_cpu_io_ram_reads(self):
        # $0000-$07FF	$0800	2KB internal RAM
        for i in range(0x0000, 0x0800):
            self.assertEqual(self.mapper.cpu_read(i), i&0xF)
        # $0800-$0FFF	$0800	Mirrors of $0000-$07FF
        for i in range(0x0800, 0x1000):
            self.assertEqual(self.mapper.cpu_read(i), i&0xF)
        # $1000-$17FF	$0800
        for i in range(0x1000, 0x1800):
            self.assertEqual(self.mapper.cpu_read(i), i&0xF)
        # $1800-$1FFF	$0800
        for i in range(0x1800, 0x2000):
            self.assertEqual(self.mapper.cpu_read(i), i&0xF)

    def test_cpu_io_ppu_mappings(self):
        # $2000-$2007	$0008	NES PPU registers
        for i in range(0x2000, 0x2008):
            self.mapper.cpu_read(i)
            self.mapper.cpu_write(i, 0xA0|(i&0xF))
        # $2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
        for i in range(0x2008, 0x4000):
            self.mapper.cpu_read(i)
            self.mapper.cpu_write(i, 0xB0|(i&0xF))
        self.assertEqual(
            self.ppu_reads,
            [(i%8) for i in range(0x2000, 0x4000)]
        )
        self.assertEqual(
            self.ppu_writes,
            [(i%8, 0xA0|(i&0xF)) for i in range(0x2000, 0x2008)]+
            [(i%8, 0xB0|(i&0xF)) for i in range(0x2008, 0x4000)]
        )

    def test_cpu_io_apu_mappings(self):
        # $4000-$4017	$0018	NES APU and I/O registers
        for i in range(0x4000, 0x4018):
            self.mapper.cpu_read(i)
        # $4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
        for i in range(0x4018, 0x4020):
            self.mapper.cpu_read(i)
        self.assertEqual(self.apu_reads, list(range(0x0000, 0x0020)))

    def test_cpu_io_misc_cart_mappings(self):
        for i in range(0x4020, 0x6000):
            self.assertEqual(self.mapper.cpu_read(i), 0)

    def test_cpu_io_prg_ram_mappings(self):
        for i in range(0x6000, 0x8000):
            self.assertEqual(self.mapper.cpu_read(i), 0x40|((i-0x6000)&0xF))

    def test_rom_one_bank_mappings(self):
        # $4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
        for i in range(0x8000, 0xC000):
            self.assertEqual(self.mapper.cpu_read(i), 0x20|((i-0x8000)&0xF))
        for i in range(0xC000, 0x10000):
            self.assertEqual(self.mapper.cpu_read(i), 0x30|((i-0xC000)&0xF))

class TestCPU(unittest.TestCase):
    def _build_mapper(self, prg_rom, chr_rom=None, mapper_cls=Mapper):
        self.ppu_reads  = []
        self.ppu_writes = []
        self.apu_reads  = []
        self.apu_writes = []
        ram  = array.array('B', (0 for _ in range(0x800)))
        vram = array.array('B', (0 for _ in range(0x800)))
        prg_ram = b''
        prg_rom_banks = [
            array.array('B', (0 for _ in range(0x4000))),
            array.array('B', (0 for _ in range(0x4000)))
        ]
        chr_rom_banks = chr_rom and [chr_rom]
        chr_ram = None
        pals = array.array('B', (i&0xF for i in range(0x20)))
        for i, value in enumerate(prg_rom):
            prg_rom_banks[0][i] = value
        def ppu_read(addr):
            self.ppu_reads.append(addr)
            return 0x55
        def ppu_write(addr, value):
            self.ppu_writes.append((addr, value))
        def apu_read(addr):
            self.apu_reads.append(addr)
            return 0x55
        def apu_write(addr, value):
            self.apu_writes.append((addr, value))
        return mapper_cls(
            ram,
            vram,
            pals,
            prg_rom_banks,
            prg_ram,
            chr_rom_banks,
            chr_ram,
            ppu_read,
            ppu_write,
            apu_read,
            apu_write)

    def _test_play(self, prg_rom, expected_regs, expected_t, pc=0x8000, s=0x00, a=0x00, x=0x00, y=0x00, p=0x00, ram_patches=[], expected_ram_patches=[], prg_rom_patches=[], expected_prg_rom_patches=[], expected_ppu_reads=[], expected_ppu_writes=[]):
        mapper = self._build_mapper(prg_rom)

        # Patch memory before running
        for i, values in ram_patches:
            for j, value in enumerate(values):
                mapper.ram[i+j] = value

        for i, values in prg_rom_patches:
            for j, value in enumerate(values):
                final_i = (i+j) - 0x8000
                mapper.prg_rom_banks[final_i//0x4000][final_i%0x4000] = value

        expected_ram = bytearray(mapper.ram)
        expected_prg_rom = bytearray(b''.join(mapper.prg_rom_banks))

        # Run program!
        regs, t = play(mapper, (pc, s, a, x, y, p), stop_on_brk=True)

        self.assertTupleEqual(regs, expected_regs)

        for start, expected_ram_bytes in expected_ram_patches:
            end = start + len(expected_ram_bytes)
            self.assertEqual(
                bytearray(mapper.ram[start:end]),
                expected_ram_bytes)
            expected_ram[start:end] = mapper.ram[start:end]

        actual_prg_rom = b''.join(mapper.prg_rom_banks)
        for start, expected_prg_rom_bytes in expected_prg_rom_patches:
            end = start + len(expected_prg_rom_bytes)
            self.assertEqual(
                bytearray(actual_prg_rom[start:end]),
                expected_prg_rom_bytes)
            expected_prg_rom[start:end] = actual_prg_rom[start:end]

        # Ensure memory other than what was patched, wasn't touched
        self.assertEqual(mapper.ram, expected_ram)
        self.assertEqual(b''.join(mapper.prg_rom_banks), expected_prg_rom)

        if expected_t is not None:
            self.assertEqual(t, expected_t)

        self.assertEqual(self.ppu_reads, expected_ppu_reads)
        self.assertEqual(self.ppu_writes, expected_ppu_writes)

    # Test NOP!

    def test_nop(self):
        self._test_play(b'\xEA', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x00), 2)
    
    # Test ALL read operations (via LDA/LDX)

    def test_lda_immediate(self):
        self._test_play(b'\xA9\x37', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 2)
        self._test_play(b'\xA9\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2)
        self._test_play(b'\xA9\x80', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80), 2)
        self._test_play(b'\xA9\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2)

    def test_lda_zero_page(self):
        self._test_play(b'\xA5\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, ram_patches=[(0x00, b'\x37')])
        self._test_play(b'\xA5\x80', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, ram_patches=[(0x80, b'\x37')])
        self._test_play(b'\xA5\xFF', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, ram_patches=[(0xFF, b'\x37')])
        self._test_play(b'\xA5\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x00), 3, ram_patches=[(0x12, b'\x37')])
        self._test_play(b'\xA5\x13', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 3, ram_patches=[(0x13, b'\x00')])
        self._test_play(b'\xA5\x14', (0x8002, 0x00, 0x80, 0x00, 0x00, 0x80), 3, ram_patches=[(0x14, b'\x80')])
        self._test_play(b'\xA5\x15', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 3, ram_patches=[(0x15, b'\xFF')])

    def test_lda_zero_page_indexed_x(self):
        self._test_play(b'\xB5\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, ram_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x80', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, ram_patches=[(0x8F, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\xFF', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, ram_patches=[(0x0E, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x12', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, ram_patches=[(0x21, b'\x37')], x=0x0F)
        self._test_play(b'\xB5\x13', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x02), 4, ram_patches=[(0x22, b'\x00')], x=0x0F)
        self._test_play(b'\xB5\x14', (0x8002, 0x00, 0x80, 0x0F, 0x00, 0x80), 4, ram_patches=[(0x23, b'\x80')], x=0x0F)
        self._test_play(b'\xB5\x15', (0x8002, 0x00, 0xFF, 0x0F, 0x00, 0x80), 4, ram_patches=[(0x24, b'\xFF')], x=0x0F)
        
    def test_ldx_zero_page_indexed_y(self):
        self._test_play(b'\xB6\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, ram_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x80', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, ram_patches=[(0x8F, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\xFF', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, ram_patches=[(0x0E, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x12', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, ram_patches=[(0x21, b'\x37')], y=0x0F)
        self._test_play(b'\xB6\x13', (0x8002, 0x00, 0x00, 0x00, 0x0F, 0x02), 4, ram_patches=[(0x22, b'\x00')], y=0x0F)
        self._test_play(b'\xB6\x14', (0x8002, 0x00, 0x00, 0x80, 0x0F, 0x80), 4, ram_patches=[(0x23, b'\x80')], y=0x0F)
        self._test_play(b'\xB6\x15', (0x8002, 0x00, 0x00, 0xFF, 0x0F, 0x80), 4, ram_patches=[(0x24, b'\xFF')], y=0x0F)

    def test_lda_absolute(self):
        self._test_play(b'\xAD\x00\x80', (0x8003, 0x00, 0xAD, 0x00, 0x00, 0x80), 4)
        self._test_play(b'\xAD\x00\xC0', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, prg_rom_patches=[(0xC000, b'\x37')])
        self._test_play(b'\xAD\xFF\xFF', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, prg_rom_patches=[(0xFFFF, b'\x37')])
        self._test_play(b'\xAD\x12\x80', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x00), 4, prg_rom_patches=[(0x8012, b'\x37')])
        self._test_play(b'\xAD\x13\x80', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x02), 4, prg_rom_patches=[(0x8013, b'\x00')])
        self._test_play(b'\xAD\x14\x80', (0x8003, 0x00, 0x80, 0x00, 0x00, 0x80), 4, prg_rom_patches=[(0x8014, b'\x80')])
        self._test_play(b'\xAD\x15\x80', (0x8003, 0x00, 0xFF, 0x00, 0x00, 0x80), 4, prg_rom_patches=[(0x8015, b'\xFF')])

        # test ppu registers (redundant to mapper test)
        self._test_play(b'\xAD\x00\x20', (0x8003, 0x00, 0x55, 0x00, 0x00, 0x00), 4, expected_ppu_reads=[0])

    def test_lda_absolute_indexed_x(self):
        self._test_play(b'\xBD\x00\x80', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, prg_rom_patches=[(0x800F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x00\xC0', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, prg_rom_patches=[(0xC00F, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\xFF\xFF', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 5, ram_patches=[(0x000E, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x12\x80', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x00), 4, prg_rom_patches=[(0x8021, b'\x37')], x=0x0F)
        self._test_play(b'\xBD\x13\x80', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x02), 4, prg_rom_patches=[(0x8022, b'\x00')], x=0x0F)
        self._test_play(b'\xBD\x14\x80', (0x8003, 0x00, 0x80, 0x0F, 0x00, 0x80), 4, prg_rom_patches=[(0x8023, b'\x80')], x=0x0F)
        self._test_play(b'\xBD\x15\x80', (0x8003, 0x00, 0xFF, 0x0F, 0x00, 0x80), 4, prg_rom_patches=[(0x8024, b'\xFF')], x=0x0F)

    def test_lda_absolute_indexed_y(self):
        self._test_play(b'\xB9\x00\x80', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), 4, x=0x0F, y=0x01)
        self._test_play(b'\xB9\x00\xC0', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 4, prg_rom_patches=[(0xC001, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\xFF\xFF', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 5, ram_patches=[(0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x12\x80', (0x8003, 0x00, 0x37, 0x0F, 0x01, 0x00), 4, prg_rom_patches=[(0x8013, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x13\x80', (0x8003, 0x00, 0x00, 0x0F, 0x01, 0x02), 4, prg_rom_patches=[(0x8014, b'\x00')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x14\x80', (0x8003, 0x00, 0x80, 0x0F, 0x01, 0x80), 4, prg_rom_patches=[(0x8015, b'\x80')], x=0x0F, y=0x01)
        self._test_play(b'\xB9\x15\x80', (0x8003, 0x00, 0xFF, 0x0F, 0x01, 0x80), 4, prg_rom_patches=[(0x8016, b'\xFF')], x=0x0F, y=0x01)

    def test_lda_indexed_indirect(self):
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, ram_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, ram_patches=[(0x000F, b'\x00\xC0')], prg_rom_patches=[(0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, ram_patches=[(0x000F, b'\xFF\xFF')], prg_rom_patches=[(0xFFFF, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xC0', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, ram_patches=[(0x00CF, b'\x00\xC0')], prg_rom_patches=[(0xC000, b'\x37')], x=0x0F, y=0x01)
        self._test_play(b'\xA1\xFF', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, ram_patches=[(0x000E, b'\x00\xC0')], prg_rom_patches=[(0xC000, b'\x37')], x=0x0F, y=0x01)

    def test_lda_indirect_indexed(self):
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, ram_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, ram_patches=[(0x0000, b'\x00\xC0')], prg_rom_patches=[(0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 6, ram_patches=[(0x0000, b'\xFF\xFF'), (0x000E, b'\x37')], x=0x01, y=0x0F) # page crossing
        self._test_play(b'\xB1\xC0', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, ram_patches=[(0x00C0, b'\x00\xC0')], prg_rom_patches=[(0xC00F, b'\x37')], x=0x01, y=0x0F)
        self._test_play(b'\xB1\xFF', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, ram_patches=[(0x00FF, b'\x00'), (0x0000, b'\xC0')], prg_rom_patches=[(0xC00F, b'\x37')], x=0x01, y=0x0F)

    # Test all operations (besides LDA which is tested thoroughly above)

    def test_ldx(self):
        self._test_play(b'\xA2\x37', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00), 2)
        self._test_play(b'\xA6\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x00), 3, ram_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB6\x00', (0x8002, 0x00, 0x00, 0x37, 0x0F, 0x00), 4, ram_patches=[(0x0F, b'\x37')], y=0x0F)
        self._test_play(b'\xAE\x00\x80', (0x8003, 0x00, 0x00, 0xAE, 0x00, 0x80), 4)
        self._test_play(b'\xBE\x00\x80', (0x8003, 0x00, 0x00, 0x00, 0x01, 0x02), 4, x=0x0F, y=0x01)

    def test_ldy(self):
        self._test_play(b'\xA0\x37', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00), 2)
        self._test_play(b'\xA4\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x00), 3, ram_patches=[(0x00, b'\x37')])
        self._test_play(b'\xB4\x00', (0x8002, 0x00, 0x00, 0x0F, 0x37, 0x00), 4, ram_patches=[(0x0F, b'\x37')], x=0x0F)
        self._test_play(b'\xAC\x00\x80', (0x8003, 0x00, 0x00, 0x00, 0xAC, 0x80), 4)
        self._test_play(b'\xBC\x00\x80', (0x8003, 0x00, 0x00, 0x01, 0x00, 0x02), 4, x=0x01, y=0x0F)

    def test_lsr(self):
        self._test_play(b'\x4A', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x01), 2, a=0x6F)
        self._test_play(b'\x4A', (0x8001, 0x00, 0x37, 0x00, 0x00, 0x00), 2, a=0x6E)
        self._test_play(b'\x46\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x01), 5, ram_patches=[(0x00, b'\x6F')], expected_ram_patches=[(0x00, b'\x37')])
        self._test_play(b'\x56\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x01), 6, ram_patches=[(0x0F, b'\x6F')], x=0x0F, expected_ram_patches=[(0x0F, b'\x37')])
        self._test_play(b'\x4E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x01), 6, ram_patches=[(0x0200, b'\x6F')], expected_ram_patches=[(0x0200, b'\x37')])
        self._test_play(b'\x5E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x01), 7, ram_patches=[(0x020F, b'\x6F')], x=0x0F, expected_ram_patches=[(0x020F, b'\x37')])

    def test_adc(self):
        self._test_play(b'\x69\x12', (0x8002, 0x00, 0x49, 0x00, 0x00, 0x00), 2, a=0x37)
        self._test_play(b'\x69\x12', (0x8002, 0x00, 0x4A, 0x00, 0x00, 0x00), 2, a=0x37, p=0x01) # borrow
        self._test_play(b'\x69\x02', (0x8002, 0x00, 0x01, 0x00, 0x00, 0x01), 2, a=0xFF) # carry
        self._test_play(b'\x69\x7E', (0x8002, 0x00, 0xFC, 0x00, 0x00, 0xC0), 2, a=0x7E) # overflow, negative
        self._test_play(b'\x69\xFE', (0x8002, 0x00, 0x7E, 0x00, 0x00, 0x41), 2, a=0x80) # overflow, carry
        self._test_play(b'\x69\xFE', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x02) # zero
        self._test_play(b'\x65\x00', (0x8002, 0x00, 0x49, 0x00, 0x00, 0x00), 3, a=0x37, ram_patches=[(0x00, b'\x12')])
        self._test_play(b'\x75\x00', (0x8002, 0x00, 0x49, 0x0F, 0x00, 0x00), 4, a=0x37, ram_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\x6D\x00\x02', (0x8003, 0x00, 0x49, 0x00, 0x00, 0x00), 4, a=0x37, ram_patches=[(0x0200, b'\x12')])
        self._test_play(b'\x7D\x00\x02', (0x8003, 0x00, 0x49, 0x0F, 0x00, 0x00), 4, a=0x37, ram_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\x79\x00\x02', (0x8003, 0x00, 0x49, 0x01, 0x0F, 0x00), 4, a=0x37, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x12')])
        self._test_play(b'\x61\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x00), 6, a=0x01, ram_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x36')], x=0x0F, y=0x01)
        self._test_play(b'\x71\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x00), 5, a=0x01, ram_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x36')], x=0x01, y=0x0F)

    def test_and(self):
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), 2, a=0xAA)
        self._test_play(b'\x29\xAA', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x29\xFF', (0x8002, 0x00, 0x55, 0x00, 0x00, 0x00), 2, a=0x55)
        self._test_play(b'\x29\xAA', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x55)
        self._test_play(b'\x25\x00', (0x8002, 0x00, 0x0A, 0x00, 0x00, 0x00), 3, a=0xAA, ram_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x35\x00', (0x8002, 0x00, 0x0A, 0x0F, 0x00, 0x00), 4, a=0xAA, ram_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x2D\x00\x02', (0x8003, 0x00, 0x0A, 0x00, 0x00, 0x00), 4, a=0xAA, ram_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x3D\x00\x02', (0x8003, 0x00, 0x0A, 0x0F, 0x00, 0x00), 4, a=0xAA, ram_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x39\x00\x02', (0x8003, 0x00, 0x0A, 0x01, 0x0F, 0x00), 4, a=0xAA, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x21\x00', (0x8002, 0x00, 0x0A, 0x0F, 0x01, 0x00), 6, a=0xAA, ram_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x31\x00', (0x8002, 0x00, 0x0A, 0x01, 0x0F, 0x00), 5, a=0xAA, ram_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

    def test_asl(self):
        self._test_play(b'\x0A', (0x8001, 0x00, 0x0A, 0x00, 0x00, 0x00), 2, a=0x05)
        self._test_play(b'\x0A', (0x8001, 0x00, 0xAA, 0x00, 0x00, 0x80), 2, a=0x55) # negative
        self._test_play(b'\x0A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x80) # zero, carry
        self._test_play(b'\x0A', (0x8001, 0x00, 0x40, 0x00, 0x00, 0x01), 2, a=0xA0) # carry
        self._test_play(b'\x0A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00) # zero
        self._test_play(b'\x06\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 5, ram_patches=[(0x00, b'\x55')], expected_ram_patches=[(0x00, b'\xAA')])
        self._test_play(b'\x16\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x80), 6, ram_patches=[(0x0F, b'\x55')], x=0x0F, expected_ram_patches=[(0x0F, b'\xAA')])
        self._test_play(b'\x0E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x80), 6, ram_patches=[(0x0200, b'\x55')], expected_ram_patches=[(0x0200, b'\xAA')])
        self._test_play(b'\x1E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x80), 7, ram_patches=[(0x020F, b'\x55')], x=0x0F, expected_ram_patches=[(0x020F, b'\xAA')])

    def test_bit(self):
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0x55, 0x00, 0x00, 0x00), 3, a=0x55, ram_patches=[(0x00, b'\x01')])
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x02), 3, a=0xAA, ram_patches=[(0x00, b'\x05')]) # zero
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x80), 3, a=0xAA, ram_patches=[(0x00, b'\xA0')]) # negative
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x40), 3, a=0xAA, ram_patches=[(0x00, b'\x42')]) # overflow
        self._test_play(b'\x24\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0xC0), 3, a=0xAA, ram_patches=[(0x00, b'\xF0')]) # negative, overflow
        self._test_play(b'\x2C\x00\x02', (0x8003, 0x00, 0x55, 0x00, 0x00, 0x00), 4, a=0x55, ram_patches=[(0x0200, b'\x01')])

    def test_sbc(self):
        self._test_play(b'\xE9\x12', (0x8002, 0x00, 0x25, 0x00, 0x00, 0x01), 2, a=0x37, p=0x01)
        self._test_play(b'\xE9\x12', (0x8002, 0x00, 0x24, 0x00, 0x00, 0x01), 2, a=0x37, p=0x00) # borrow-ed
        self._test_play(b'\xE9\x0F', (0x8002, 0x00, 0xF9, 0x00, 0x00, 0x80), 2, a=0x08, p=0x01) # borrow, negative
        self._test_play(b'\xE9\x01', (0x8002, 0x00, 0x7F, 0x00, 0x00, 0x41), 2, a=0x80, p=0x01) # overflow
        self._test_play(b'\xE9\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x02, p=0x01) # zero
        self._test_play(b'\xE5\x00', (0x8002, 0x00, 0x25, 0x00, 0x00, 0x01), 3, a=0x37, p=0x01, ram_patches=[(0x00, b'\x12')])
        self._test_play(b'\xF5\x00', (0x8002, 0x00, 0x25, 0x0F, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\xED\x00\x02', (0x8003, 0x00, 0x25, 0x00, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x0200, b'\x12')])
        self._test_play(b'\xFD\x00\x02', (0x8003, 0x00, 0x25, 0x0F, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\xF9\x00\x02', (0x8003, 0x00, 0x25, 0x01, 0x0F, 0x01), 4, a=0x37, p=0x01, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x12')])
        self._test_play(b'\xE1\x00', (0x8002, 0x00, 0x25, 0x0F, 0x01, 0x01), 6, a=0x37, p=0x01, ram_patches=[(0x000F, b'\x00\x02'), (0x0200, b'\x12')], x=0x0F, y=0x01)
        self._test_play(b'\xF1\x00', (0x8002, 0x00, 0x25, 0x01, 0x0F, 0x01), 5, a=0x37, p=0x01, ram_patches=[(0x0000, b'\x00\x02'), (0x020F, b'\x12')], x=0x01, y=0x0F)

    def test_bpl(self):
        self._test_play(b'\x10\x02', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 2, p=0x80) # no branch
        self._test_play(b'\x10\x02', (0x8004, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00) # branch
        self._test_play(b'', (0x8100, 0x00, 0x00, 0x00, 0x00, 0x00), 4, pc=0x807F, p=0x00, prg_rom_patches=[(0x807F, b'\x10\x7F')]) # branch / page crossed
        self._test_play(b'\x10\x02\x00\x00\xEA', (0x8005, 0x00, 0x00, 0x00, 0x00, 0x00), 5, p=0x00) # branch to nop (and execute that instruction too)
        self._test_play(b'\x00\x10\xFD', (0x8000, 0x00, 0x00, 0x00, 0x00, 0x00), 3, p=0x00, pc=0x8001) # branch negative address
        self._test_play(b'', (0x80FF, 0x00, 0x00, 0x00, 0x00, 0x00), 4, p=0x00, pc=0x8100, prg_rom_patches=[(0x8100, b'\x10\xFD')]) # branch negative address / page crossed

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
        self._test_play(b'\xC9\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), 2, a=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xC9\x12', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), 2, a=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xC9\x37', (0x8002, 0x00, 0x12, 0x00, 0x00, 0x80), 2, a=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xC9\x02', (0x8002, 0x00, 0x02, 0x00, 0x00, 0x03), 2, a=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xC5\x00', (0x8002, 0x00, 0x37, 0x00, 0x00, 0x01), 3, a=0x37, p=0x01, ram_patches=[(0x00, b'\x12')])
        self._test_play(b'\xD5\x00', (0x8002, 0x00, 0x37, 0x0F, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x0F, b'\x12')], x=0x0F)
        self._test_play(b'\xCD\x00\x02', (0x8003, 0x00, 0x37, 0x00, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x0200, b'\x12')])
        self._test_play(b'\xDD\x00\x02', (0x8003, 0x00, 0x37, 0x0F, 0x00, 0x01), 4, a=0x37, p=0x01, ram_patches=[(0x020F, b'\x12')], x=0x0F)
        self._test_play(b'\xD9\x00\x02', (0x8003, 0x00, 0x37, 0x01, 0x0F, 0x01), 4, a=0x37, p=0x01, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x12')])
        self._test_play(b'\xC1\x00', (0x8002, 0x00, 0x37, 0x0F, 0x01, 0x01), 6, a=0x37, p=0x01, ram_patches=[(0x000F, b'\x00\x02'), (0x0200, b'\x12')], x=0x0F, y=0x01)
        self._test_play(b'\xD1\x00', (0x8002, 0x00, 0x37, 0x01, 0x0F, 0x01), 5, a=0x37, p=0x01, ram_patches=[(0x0000, b'\x00\x02'), (0x020F, b'\x12')], x=0x01, y=0x0F)

    def test_cpx(self):
        self._test_play(b'\xE0\x12', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), 2, x=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xE0\x12', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), 2, x=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xE0\x37', (0x8002, 0x00, 0x00, 0x12, 0x00, 0x80), 2, x=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xE0\x02', (0x8002, 0x00, 0x00, 0x02, 0x00, 0x03), 2, x=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xE4\x00', (0x8002, 0x00, 0x00, 0x37, 0x00, 0x01), 3, x=0x37, p=0x01, ram_patches=[(0x00, b'\x12')])
        self._test_play(b'\xEC\x00\x02', (0x8003, 0x00, 0x00, 0x37, 0x00, 0x01), 4, x=0x37, p=0x01, ram_patches=[(0x0200, b'\x12')])

    def test_cpy(self):
        self._test_play(b'\xC0\x12', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), 2, y=0x37, p=0x01) # 0x37 > 0x12
        self._test_play(b'\xC0\x12', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), 2, y=0x37, p=0x00) # 0x37 > 0x12 (borrow-ed)
        self._test_play(b'\xC0\x37', (0x8002, 0x00, 0x00, 0x00, 0x12, 0x80), 2, y=0x12, p=0x01) # 0x12 < 0x37 (borrow, negative)
        self._test_play(b'\xC0\x02', (0x8002, 0x00, 0x00, 0x00, 0x02, 0x03), 2, y=0x02, p=0x01) # 0x02 = 0x02 (zero)
        self._test_play(b'\xC4\x00', (0x8002, 0x00, 0x00, 0x00, 0x37, 0x01), 3, y=0x37, p=0x01, ram_patches=[(0x00, b'\x12')])
        self._test_play(b'\xCC\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x37, 0x01), 4, y=0x37, p=0x01, ram_patches=[(0x0200, b'\x12')])
    
    def test_dec(self):
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 5, ram_patches=[(0x00, b'\x12')], expected_ram_patches=[(0x00, b'\x11')])
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 5, ram_patches=[(0x00, b'\x01')], expected_ram_patches=[(0x00, b'\x00')]) # zero
        self._test_play(b'\xC6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 5, ram_patches=[(0x00, b'\x00')], expected_ram_patches=[(0x00, b'\xFF')]) # negative
        self._test_play(b'\xD6\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x00), 6, ram_patches=[(0x0F, b'\x12')], x=0x0F, expected_ram_patches=[(0x0F, b'\x11')])
        self._test_play(b'\xCE\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x00), 6, ram_patches=[(0x0200, b'\x12')], expected_ram_patches=[(0x0200, b'\x11')])
        self._test_play(b'\xDE\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x00), 7, ram_patches=[(0x020F, b'\x12')], x=0x0F, expected_ram_patches=[(0x020F, b'\x11')])

    def test_eor(self):
        self._test_play(b'\x49\xFF', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0xFF)
        self._test_play(b'\x49\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00)
        self._test_play(b'\x49\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0x00)
        self._test_play(b'\x49\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x49\x55', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xAA)
        self._test_play(b'\x45\x00', (0x8002, 0x00, 0xF0, 0x00, 0x00, 0x80), 3, a=0xAA, ram_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x55\x00', (0x8002, 0x00, 0xF0, 0x0F, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x4D\x00\x02', (0x8003, 0x00, 0xF0, 0x00, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x5D\x00\x02', (0x8003, 0x00, 0xF0, 0x0F, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x59\x00\x02', (0x8003, 0x00, 0xF0, 0x01, 0x0F, 0x80), 4, a=0xAA, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x41\x00', (0x8002, 0x00, 0xF0, 0x0F, 0x01, 0x80), 6, a=0xAA, ram_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x51\x00', (0x8002, 0x00, 0xF0, 0x01, 0x0F, 0x80), 5, a=0xAA, ram_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

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
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x00), 5, ram_patches=[(0x00, b'\x12')], expected_ram_patches=[(0x00, b'\x13')])
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 5, ram_patches=[(0x00, b'\xFF')], expected_ram_patches=[(0x00, b'\x00')]) # zero
        self._test_play(b'\xE6\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x80), 5, ram_patches=[(0x00, b'\x80')], expected_ram_patches=[(0x00, b'\x81')]) # negative
        self._test_play(b'\xF6\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x00), 6, ram_patches=[(0x0F, b'\x12')], x=0x0F, expected_ram_patches=[(0x0F, b'\x13')])
        self._test_play(b'\xEE\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x00), 6, ram_patches=[(0x0200, b'\x12')], expected_ram_patches=[(0x0200, b'\x13')])
        self._test_play(b'\xFE\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x00), 7, ram_patches=[(0x020F, b'\x12')], x=0x0F, expected_ram_patches=[(0x020F, b'\x13')])

    def test_jmp(self):
        self._test_play(b'\x4C\x00\x02', (0x0200, 0x00, 0x00, 0x00, 0x00, 0x00), 3)
        self._test_play(b'\x6C\x00\x02', (0xC000, 0x00, 0x00, 0x00, 0x00, 0x00), 5, ram_patches=[(0x0200, b'\x00\xC0')])

    def test_jsr(self):
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFD, 0x00, 0x00, 0x00, 0x00), 6, s=0xFF, expected_ram_patches=[(0x01FE, b'\x02\x80')])
        self._test_play(b'\x20\x00\x02', (0x0200, 0x00, 0x00, 0x00, 0x00, 0x00), 6, s=0x02, expected_ram_patches=[(0x0101, b'\x02\x80')])
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFE, 0x00, 0x00, 0x00, 0x00), 6, s=0x00, expected_ram_patches=[(0x0100, b'\x80'), (0x01FF, b'\x02')]) # stack wrap-around mid address
        self._test_play(b'\x20\x00\x02', (0x0200, 0xFF, 0x00, 0x00, 0x00, 0x00), 6, s=0x01, expected_ram_patches=[(0x0100, b'\x02\x80')]) # stack wrap-around after address

    def test_ora(self):
        self._test_play(b'\x09\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x02), 2, a=0x00)
        self._test_play(b'\x09\xFF', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0x00)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x00', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xFF)
        self._test_play(b'\x09\x55', (0x8002, 0x00, 0xFF, 0x00, 0x00, 0x80), 2, a=0xAA)
        self._test_play(b'\x05\x00', (0x8002, 0x00, 0xFA, 0x00, 0x00, 0x80), 3, a=0xAA, ram_patches=[(0x00, b'\x5A')])
        self._test_play(b'\x15\x00', (0x8002, 0x00, 0xFA, 0x0F, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x0F, b'\x5A')], x=0x0F)
        self._test_play(b'\x0D\x00\x02', (0x8003, 0x00, 0xFA, 0x00, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x0200, b'\x5A')])
        self._test_play(b'\x1D\x00\x02', (0x8003, 0x00, 0xFA, 0x0F, 0x00, 0x80), 4, a=0xAA, ram_patches=[(0x020F, b'\x5A')], x=0x0F)
        self._test_play(b'\x19\x00\x02', (0x8003, 0x00, 0xFA, 0x01, 0x0F, 0x80), 4, a=0xAA, x=0x01, y=0x0F, ram_patches=[(0x020F, b'\x5A')])
        self._test_play(b'\x01\x00', (0x8002, 0x00, 0xFA, 0x0F, 0x01, 0x80), 6, a=0xAA, ram_patches=[(0x000F, b'\x00\x00'), (0x0000, b'\x5A')], x=0x0F, y=0x01)
        self._test_play(b'\x11\x00', (0x8002, 0x00, 0xFA, 0x01, 0x0F, 0x80), 5, a=0xAA, ram_patches=[(0x0000, b'\x00\x00'), (0x000F, b'\x5A')], x=0x01, y=0x0F)

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
        self._test_play(b'\x26\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x81), 5, p=0x01, ram_patches=[(0x00, b'\xD5')], expected_ram_patches=[(0x00, b'\xAB')])
        self._test_play(b'\x36\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x81), 6, p=0x01, ram_patches=[(0x0F, b'\xD5')], x=0x0F, expected_ram_patches=[(0x0F, b'\xAB')])
        self._test_play(b'\x2E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x81), 6, p=0x01, ram_patches=[(0x0200, b'\xD5')], expected_ram_patches=[(0x0200, b'\xAB')])
        self._test_play(b'\x3E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x81), 7, p=0x01, ram_patches=[(0x020F, b'\xD5')], x=0x0F, expected_ram_patches=[(0x020F, b'\xAB')])

    def test_ror(self):
        self._test_play(b'\x6A', (0x8001, 0x00, 0x01, 0x00, 0x00, 0x00), 2, a=0x02)
        self._test_play(b'\x6A', (0x8001, 0x00, 0x02, 0x00, 0x00, 0x01), 2, a=0x05) # rotate out carry
        self._test_play(b'\x6A', (0x8001, 0x00, 0x00, 0x00, 0x00, 0x03), 2, a=0x01) # rotate out carry, zero
        self._test_play(b'\x6A', (0x8001, 0x00, 0xC0, 0x00, 0x00, 0x81), 2, a=0x81, p=0x01) # rotate in/out carry
        self._test_play(b'\x6A', (0x8001, 0x00, 0x80, 0x00, 0x00, 0x80), 2, a=0x00, p=0x01) # rotate in
        self._test_play(b'\x66\x00', (0x8002, 0x00, 0x00, 0x00, 0x00, 0x81), 5, p=0x01, ram_patches=[(0x00, b'\xD5')], expected_ram_patches=[(0x00, b'\xEA')])
        self._test_play(b'\x76\x00', (0x8002, 0x00, 0x00, 0x0F, 0x00, 0x81), 6, p=0x01, ram_patches=[(0x0F, b'\xD5')], x=0x0F, expected_ram_patches=[(0x0F, b'\xEA')])
        self._test_play(b'\x6E\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0x00, 0x81), 6, p=0x01, ram_patches=[(0x0200, b'\xD5')], expected_ram_patches=[(0x0200, b'\xEA')])
        self._test_play(b'\x7E\x00\x02', (0x8003, 0x00, 0x00, 0x0F, 0x00, 0x81), 7, p=0x01, ram_patches=[(0x020F, b'\xD5')], x=0x0F, expected_ram_patches=[(0x020F, b'\xEA')])

    def test_rti(self):
        self._test_play(b'\x00', (0x8000, 0xFF, 0x00, 0x00, 0x00, 0xAA), 6, pc=0xC000, s=0xFC, ram_patches=[(0x1FD, b'\xAA\x00\x80')], prg_rom_patches=[(0xC000, b'\x40')])

    def test_rts(self):
        self._test_play(b'\x00', (0x8000, 0xFF, 0x00, 0x00, 0x00, 0x00), 6, pc=0xC000, s=0xFD, ram_patches=[(0x1FE, b'\xFF\x7F')], prg_rom_patches=[(0xC000, b'\x60')])

    def test_sta(self):
        self._test_play(b'\x85\x00', (0x8002, 0x00, 0xAA, 0x00, 0x00, 0x00), 3, a=0xAA, expected_ram_patches=[(0x00, b'\xAA')])
        self._test_play(b'\x95\x00', (0x8002, 0x00, 0xAA, 0x0F, 0x00, 0x00), 4, a=0xAA, expected_ram_patches=[(0x0F, b'\xAA')], x=0x0F)
        self._test_play(b'\x8D\x00\x02', (0x8003, 0x00, 0xAA, 0x00, 0x00, 0x00), 4, a=0xAA, expected_ram_patches=[(0x0200, b'\xAA')])
        self._test_play(b'\x9D\x00\x02', (0x8003, 0x00, 0xAA, 0x0F, 0x00, 0x00), 5, a=0xAA, expected_ram_patches=[(0x020F, b'\xAA')], x=0x0F)
        self._test_play(b'\x99\x00\x02', (0x8003, 0x00, 0xAA, 0x01, 0x0F, 0x00), 5, a=0xAA, x=0x01, y=0x0F, expected_ram_patches=[(0x020F, b'\xAA')])
        self._test_play(b'\x81\x00', (0x8002, 0x00, 0xAA, 0x0F, 0x01, 0x00), 6, a=0xAA, ram_patches=[(0x000F, b'\x00\x00')], expected_ram_patches=[(0x0000, b'\xAA')], x=0x0F, y=0x01)
        self._test_play(b'\x91\x00', (0x8002, 0x00, 0xAA, 0x01, 0x0F, 0x00), 6, a=0xAA, ram_patches=[(0x0000, b'\x00\x00')], expected_ram_patches=[(0x000F, b'\xAA')], x=0x01, y=0x0F)

        # test ppu registers (redundant to mapper test)
        self._test_play(b'\x8D\x00\x20', (0x8003, 0x00, 0xAA, 0x00, 0x00, 0x00), 4, a=0xAA, expected_ppu_writes=[(0, 0xAA)])

    def test_txs(self):
        self._test_play(b'\x9A', (0x8001, 0xAA, 0x00, 0xAA, 0x00, 0x00), 2, s=0x00, x=0xAA)

    def test_tsx(self):
        self._test_play(b'\xBA', (0x8001, 0xAA, 0x00, 0xAA, 0x00, 0x80), 2, s=0xAA, x=0x00)

    def test_pha(self):
        self._test_play(b'\x48', (0x8001, 0xFE, 0xAA, 0x00, 0x00, 0x00), 3, s=0xFF, a=0xAA, expected_ram_patches=[(0x01FF, b'\xAA')])

    def test_pla(self):
        self._test_play(b'\x68', (0x8001, 0xFF, 0x55, 0x00, 0x00, 0x00), 4, s=0xFE, a=0x00, ram_patches=[(0x01FF, b'\x55')])
        self._test_play(b'\x68', (0x8001, 0xFF, 0x00, 0x00, 0x00, 0x02), 4, s=0xFE, a=0x00, ram_patches=[(0x01FF, b'\x00')]) # zero
        self._test_play(b'\x68', (0x8001, 0xFF, 0xAA, 0x00, 0x00, 0x80), 4, s=0xFE, a=0x00, ram_patches=[(0x01FF, b'\xAA')]) # negative

    def test_php(self):
        self._test_play(b'\x08', (0x8001, 0xFE, 0x00, 0x00, 0x00, 0xAA), 3, s=0xFF, p=0xAA, expected_ram_patches=[(0x01FF, b'\xBA')])

    def test_plp(self):
        self._test_play(b'\x28', (0x8001, 0xFF, 0x00, 0x00, 0x00, 0xAA), 4, s=0xFE, p=0x00, ram_patches=[(0x01FF, b'\xAA')])

    def test_stx(self):
        self._test_play(b'\x86\x00', (0x8002, 0x00, 0x00, 0xAA, 0x00, 0x00), 3, x=0xAA, expected_ram_patches=[(0x00, b'\xAA')])
        self._test_play(b'\x96\x00', (0x8002, 0x00, 0x00, 0xAA, 0x0F, 0x00), 4, x=0xAA, expected_ram_patches=[(0x0F, b'\xAA')], y=0x0F)
        self._test_play(b'\x8E\x00\x02', (0x8003, 0x00, 0x00, 0xAA, 0x00, 0x00), 4, x=0xAA, expected_ram_patches=[(0x0200, b'\xAA')])

    def test_sty(self):
        self._test_play(b'\x84\x00', (0x8002, 0x00, 0x00, 0x00, 0xAA, 0x00), 3, y=0xAA, expected_ram_patches=[(0x00, b'\xAA')])
        self._test_play(b'\x94\x00', (0x8002, 0x00, 0x00, 0x0F, 0xAA, 0x00), 4, y=0xAA, expected_ram_patches=[(0x0F, b'\xAA')], x=0x0F)
        self._test_play(b'\x8C\x00\x02', (0x8003, 0x00, 0x00, 0x00, 0xAA, 0x00), 4, y=0xAA, expected_ram_patches=[(0x0200, b'\xAA')])

class TestPPU(unittest.TestCase):
    # TODO: clean this up and share between tests; it's a copy and paste job!
    def _build_mapper(self, prg_rom, chr_rom=b'', mapper_cls=Mapper):
        self.ppu_reads  = []
        self.ppu_writes = []
        self.apu_reads  = []
        self.apu_writes = []
        ram  = array.array('B', (0 for _ in range(0x800)))
        vram = array.array('B', (0 for _ in range(0x800)))
        prg_ram = b''
        prg_rom_banks = [
            array.array('B', (0 for _ in range(0x4000))),
            array.array('B', (0 for _ in range(0x4000)))
        ]
        chr_rom_banks = []
        chr_ram = b''
        pals = array.array('B', (i&0xF for i in range(0x20)))
        for i, value in enumerate(prg_rom):
            prg_rom_banks[0][i] = value
        def ppu_read(addr):
            self.ppu_reads.append(addr)
            return 0x55
        def ppu_write(addr, value):
            self.ppu_writes.append((addr, value))
        def apu_read(addr):
            self.apu_reads.append(addr)
            return 0x55
        def apu_write(addr, value):
            self.apu_writes.append((addr, value))
        return mapper_cls(
            ram,
            vram,
            pals,
            prg_rom_banks,
            prg_ram,
            chr_rom_banks,
            chr_ram,
            ppu_read,
            ppu_write,
            apu_read,
            apu_write)

    def _build_ppu(self, mapper):
        ppu = PPU()
        ppu.set_mapper(mapper)
        return ppu

    def test_ppuctrl(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial write
        self.assertEqual(ppu.ppu_ctrl, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        ppu.write_reg(0, 0xAA)
        self.assertEqual(ppu.ppu_ctrl, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        self.assertEqual(ppu.reg_io_write_state, 8) # TODO: figure out if this is expected (before 30k cycles no write, does tmp_addr write state change)
        # after 30000 cycles
        ppu.t = 30000
        ppu.reg_io_write_state = 0
        # initial write
        ppu.write_reg(0, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.ppu_ctrl, 0xAA)
        self.assertEqual(ppu.tmp_addr, 0x0800)
        # second write
        ppu.write_reg(0, 0x55)
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.ppu_ctrl, 0x55)
        self.assertEqual(ppu.tmp_addr, 0x0400)
        # write zero
        ppu.write_reg(0, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.ppu_ctrl, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        # read (latch value)
        ppu.reg_io_value = 0x55
        self.assertEqual(ppu.read_reg(0), 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        # flip bit to "generate an NMI", not already on, but not in vblank
        ppu.write_reg(0, 0x80)
        self.assertEqual(ppu.ppu_ctrl, 0x80)
        # flip bit to "generate an NMI", not already on, in vblank
        ppu.ppu_status = 0x80
        ppu.ppu_ctrl = 0x00
        self.assertRaises(ValueError, ppu.write_reg, 0, 0x80)
        # flip bit to "generate an NMI", already on, in vblank
        ppu.write_reg(0, 0x80)
        self.assertEqual(ppu.ppu_ctrl, 0x80)

    def test_ppumask(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial value
        self.assertEqual(ppu.ppu_mask, 0x00)
        # initial write
        ppu.write_reg(1, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.ppu_mask, 0xAA)
        # second write
        ppu.write_reg(1, 0x55)
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.ppu_mask, 0x55)
        # write zero
        ppu.write_reg(1, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.ppu_mask, 0x00)
        # read (latch value)
        ppu.reg_io_value = 0x55
        self.assertEqual(ppu.read_reg(1), 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)

    def test_ppustatus(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial value
        self.assertEqual(ppu.ppu_status, 0x00)
        # write attempt
        ppu.write_reg(2, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.ppu_status, 0x00)
        # initial read
        self.assertEqual(ppu.read_reg(2), 0x0A)
        self.assertEqual(ppu.reg_io_write_state, 0)
        # another read
        ppu.ppu_status = 0xFF
        self.assertEqual(ppu.read_reg(2), 0xEA)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.ppu_status, 0x7F)  # 7 bit (vblank) is cleared after read

    def test_oamaddr(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial value
        self.assertEqual(ppu.oam_addr, 0x00)
        # initial write
        ppu.write_reg(3, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.oam_addr, 0xAA)
        # second write
        ppu.write_reg(3, 0x55)
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.oam_addr, 0x55)
        # write zero
        ppu.write_reg(3, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.oam_addr, 0x00)
        # read (latch value)
        ppu.reg_io_value = 0x55
        self.assertEqual(ppu.read_reg(3), 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        # TODO: OAMADDR is set to 0 during each of ticks 257–320 (the sprite tile loading interval)
        # of the pre-render and visible scanlines

    def test_oamdata(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial values
        self.assertEqual(ppu.oam_addr, 0x00)
        # initial write
        ppu.write_reg(4, 0x55)
        self.assertEqual(ppu.oam[0x00], 0x55)
        self.assertEqual(ppu.oam_addr, 0x01)
        # second write
        ppu.write_reg(4, 0xAA)
        self.assertEqual(ppu.oam[0x01], 0xAA)
        self.assertEqual(ppu.oam_addr, 0x02)
        # third write, then wrap oamaddr
        ppu.oam_addr = 0xFF
        ppu.write_reg(4, 0x55)
        self.assertEqual(ppu.oam[0xFF], 0x55)
        self.assertEqual(ppu.oam_addr, 0x00)
        # reset oamaddr
        ppu.oam_addr = 0x00
        # first read
        self.assertEqual(ppu.read_reg(4), 0x55)
        self.assertEqual(ppu.oam_addr, 0x01)
        # second read
        self.assertEqual(ppu.read_reg(4), 0xAA)
        self.assertEqual(ppu.oam_addr, 0x02)
        # third read, then wrap oamaddr
        ppu.oam_addr = 0xFF
        self.assertEqual(ppu.read_reg(4), 0x55)
        self.assertEqual(ppu.oam_addr, 0x00)
        # TODO: reads during v/forced blanking do not increment oamaddr
        # TODO: the value of OAMADDR at tick 65 determines the starting address
        # for sprite evaluation for a visible scanline, which can cause the sprite
        # at OAMADDR to be treated as it was sprite 0, both for sprite-0 hit and priority.
        # If OAMADDR is unaligned and does not point to the Y position (first byte)
        # of an OAM entry, then whatever it points to (tile index, attribute, or X coordinate)
        # will be reinterpreted as a Y position, and the following bytes will be similarly reinterpreted.
        # No more sprites will be found once the end of OAM is reached,
        # effectively hiding any sprites before the starting OAMADDR.

    def test_ppuscroll(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial values
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        # first write
        ppu.write_reg(5, 0xFF)
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x001F)
        self.assertEqual(ppu.ppu_addr, 0x0000) # only gets updated during rendering, so always 0x0000 here
        self.assertEqual(ppu.fine_x_scroll, 0x07)
        # second write
        ppu.write_reg(5, 0xFF)
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x73FF)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x07)
        # write zero, first
        ppu.tmp_addr = 0xFFFF  # to check that unmasked bits don't change, not that it should ever matter since they aren't used
        ppu.write_reg(5, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0xFFE0)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # write zero, second
        ppu.write_reg(5, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x8C00)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        # first write
        ppu.write_reg(5, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x8C15)
        self.assertEqual(ppu.ppu_addr, 0x0000) # only gets updated during rendering, so always 0x0000 here
        self.assertEqual(ppu.fine_x_scroll, 0x02)
        # read (latch value)
        self.assertEqual(ppu.read_reg(5), 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 0)

    def test_ppuaddr(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial values
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # first write 0xFF
        ppu.write_reg(6, 0xFF)
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x3F00)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # second write 0xFF
        ppu.write_reg(6, 0xFF)
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x3FFF)
        self.assertEqual(ppu.ppu_addr, 0x3FFF)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # first write 0x00
        ppu.write_reg(6, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x00FF)
        self.assertEqual(ppu.ppu_addr, 0x3FFF)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # second write 0x00
        ppu.write_reg(6, 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # first write
        ppu.write_reg(6, 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x2A00)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # read (latch value)
        self.assertEqual(ppu.read_reg(6), 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 0)
   
    def test_ppudata(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # initial values
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        ppu.tmp_addr   = 0x2000
        ppu.ppu_addr   = 0x2000
        ppu.ppu_data   = 0x00
        mapper.vram[0x0000] = 0x00
        mapper.vram[0x0001] = 0xAA
        mapper.vram[0x0002] = 0xAA
        mapper.vram[0x0700] = 0x88
        mapper.pals[0x0000] = 0x77
        # write
        ppu.write_reg(7, 0xFF)
        self.assertEqual(tuple(mapper.vram[0:3]), (0xFF, 0xAA, 0xAA))
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2001)
        # second write
        ppu.write_reg(7, 0x55)
        self.assertEqual(tuple(mapper.vram[0:3]), (0xFF, 0x55, 0xAA))
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2002)
        # third write, after changing increment to 32
        ppu.ppu_ctrl |= 0x04 # set increment
        ppu.write_reg(7, 0xAA)
        self.assertEqual(tuple(mapper.vram[0:3]), (0xFF, 0x55, 0xAA))
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2022)
        ppu.ppu_ctrl ^= 0x04 # reset increment
        # read buffer garbage
        ppu.ppu_addr = 0x2001 # backup one
        self.assertEqual(ppu.read_reg(7), 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2002)
        # read 0x2001
        self.assertEqual(ppu.read_reg(7), 0x55)
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2003)
        # read 0x2002
        self.assertEqual(ppu.read_reg(7), 0xAA)
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x2004)
        # read mirror 0x3000
        ppu.ppu_data = 0x00
        ppu.ppu_addr = 0x3000 # jump to mirrors
        self.assertEqual(ppu.read_reg(7), 0x00)
        self.assertEqual(ppu.reg_io_value, 0x00)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x3001)
        self.assertEqual(ppu.ppu_data, 0xFF)
        # read mirror 0x3001
        self.assertEqual(ppu.read_reg(7), 0xFF)
        self.assertEqual(ppu.reg_io_value, 0xFF)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x3002)
        self.assertEqual(ppu.ppu_data, 0x55)
        # read directly from pal
        ppu.ppu_addr = 0x3F00 # jump to pal
        self.assertEqual(ppu.read_reg(7), 0x77)
        self.assertEqual(ppu.reg_io_value, 0x77)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        self.assertEqual(ppu.tmp_addr, 0x2000)
        self.assertEqual(ppu.ppu_addr, 0x3F01)
        self.assertEqual(ppu.ppu_data, 0x88) # nametable value mirrored "under" the palette
     
    def test_ppu_scrolling_example_from_wiki(self):
        mapper = self._build_mapper(b'')
        ppu = self._build_ppu(mapper)
        # full example from wiki, including other regs
        # initial
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x0000)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # $2000 write
        # t: ...GH.. ........ <- d: ......GH
        # <used elsewhere>    <- d: ABCDEF..
        # =>
        # t: ...11.. ........ <- d: ......11
        # <used elsewhere>    <- d: 000000..
        ppu.t = 30000
        ppu.write_reg(0, 0x03) # set nametable select = 1 1 = 0x2C00
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x0C00)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # $2002 read
        # w:                  <- 0
        ppu.read_reg(2) # reset write state
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x0C00)
        self.assertEqual(ppu.fine_x_scroll, 0x00)
        # $2005 first write (w is 0)
        # t: ....... ...ABCDE <- d: ABCDE...
        # x:              FGH <- d: .....FGH
        # w:                  <- 1
        # =>
        # t: ....... ...10101 <- d: 10101...
        # x:              010 <- d: .....010
        # w:                  <- 1
        ppu.write_reg(5, 0xAA) # write 
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x0C15)
        self.assertEqual(ppu.fine_x_scroll, 0x02)
        self.assertEqual(ppu.ppu_addr, 0x0000)
        # $2005 second write (w is 1)
        # t: FGH..AB CDE..... <- d: ABCDEFGH
        # w:                  <- 0
        # =>
        # t: 101..01 010..... <- d: 01010101
        # w:                  <- 0
        ppu.write_reg(5, 0x55) # write 
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x5D55)
        self.assertEqual(ppu.fine_x_scroll, 0x02)
        self.assertEqual(ppu.ppu_addr, 0x0000) # copied during rendering
        # $2006 first write (w is 0)
        # t: .CDEFGH ........ <- d: ..CDEFGH
        #        <unused>     <- d: AB......
        # t: Z...... ........ <- 0 (bit Z is cleared)
        # w:                  <- 1
        # =>
        # t: .CDEFGH ........ <- d: ..010101
        #        <unused>     <- d: 01......
        # t: Z...... ........ <- 0 (bit Z is cleared)
        ppu.write_reg(6, 0x55) # write 
        self.assertEqual(ppu.reg_io_value, 0x55)
        self.assertEqual(ppu.reg_io_write_state, 8)
        self.assertEqual(ppu.tmp_addr, 0x1555)
        self.assertEqual(ppu.fine_x_scroll, 0x02)
        self.assertEqual(ppu.ppu_addr, 0x0000)

        # $2006 second write (w is 1)
        # t: ....... ABCDEFGH <- d: ABCDEFGH
        # v: <...all bits...> <- t: <...all bits...>
        # w:                  <- 0
        # => 
        # t: ....... ABCDEFGH <- d: 10101010
        # v: <...all bits...> <- t: <...all bits...>
        ppu.write_reg(6, 0xAA) # write 
        self.assertEqual(ppu.reg_io_value, 0xAA)
        self.assertEqual(ppu.reg_io_write_state, 0)
        self.assertEqual(ppu.tmp_addr, 0x15AA)
        self.assertEqual(ppu.fine_x_scroll, 0x02)
        self.assertEqual(ppu.ppu_addr, 0x15AA)

if __name__ == "__main__":
    unittest.main()
