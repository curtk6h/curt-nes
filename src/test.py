import unittest
import array

from curt_nes import create_default_mapper_funcs, create_cpu_funcs, create_ppu_funcs, VMStop

class TestMapper(unittest.TestCase):
    sample_ram  = array.array('B', (0x00|(i&0xF) for i in range(0x800)))
    sample_vram = array.array('B', (0x10|(i&0xF) for i in range(0x800)))
    sample_prg_rom_banks = [
        array.array('B', (0x20|(i&0xF) for i in range(0x4000))),
        array.array('B', (0x30|(i&0xF) for i in range(0x4000)))
    ]
    sample_prg_ram = array.array('B', (0x40|(i&0xF) for i in range(0x2000)))
    sample_chr_rom_banks = [array.array('B', (0x50|(i&0xF) for i in range(0x2000)))]
    sample_chr_ram = array.array('B', (0x60|(i&0xF) for i in range(0x2000)))
    sample_pals = array.array('B', (i&0xF for i in range(0x20)))

    def setUp(self):
        self.cpu_transfer_page_to_oam_calls = cpu_transfer_page_to_oam_calls = []
        self.ppu_reads  = ppu_reads  = []
        self.ppu_writes = ppu_writes = []
        self.apu_reads  = apu_reads  = []
        self.apu_writes = apu_writes = []
        def ppu_read_reg(addr):
            ppu_reads.append(addr)
            return 0x55
        def ppu_write_reg(addr, value):
            ppu_writes.append((addr, value))
        def cpu_transfer_page_to_oam(page_num):
            cpu_transfer_page_to_oam_calls.append(page_num)
        def apu_read_reg(addr):
            apu_reads.append(addr)
            return 0x77
        def apu_write_reg(addr, value):
            apu_writes.append((addr, value))
        self.cpu_read, self.cpu_write, self.ppu_read, self.ppu_write = create_default_mapper_funcs(
            self.sample_ram,
            self.sample_vram,
            self.sample_pals,
            self.sample_prg_rom_banks,
            self.sample_prg_ram,
            self.sample_chr_rom_banks,
            None, # self.sample_chr_ram
            cpu_transfer_page_to_oam,
            ppu_read_reg,
            ppu_write_reg,
            apu_read_reg,
            apu_write_reg
        )

    def test_cpu_io_ram_reads(self):
        # $0000-$07FF	$0800	2KB internal RAM
        for i in range(0x0000, 0x0800):
            self.assertEqual(self.cpu_read(i), i&0xF)
        # $0800-$0FFF	$0800	Mirrors of $0000-$07FF
        for i in range(0x0800, 0x1000):
            self.assertEqual(self.cpu_read(i), i&0xF)
        # $1000-$17FF	$0800
        for i in range(0x1000, 0x1800):
            self.assertEqual(self.cpu_read(i), i&0xF)
        # $1800-$1FFF	$0800
        for i in range(0x1800, 0x2000):
            self.assertEqual(self.cpu_read(i), i&0xF)

    def test_cpu_io_ppu_mappings(self):
        # $2000-$2007	$0008	NES PPU registers
        for i in range(0x2000, 0x2008):
            self.cpu_read(i)
            self.cpu_write(i, 0xA0|(i&0xF))
        # $2008-$3FFF	$1FF8	Mirrors of $2000-2007 (repeats every 8 bytes)
        for i in range(0x2008, 0x4000):
            self.cpu_read(i)
            self.cpu_write(i, 0xB0|(i&0xF))
        self.assertEqual(
            self.ppu_reads,
            [(i%8) for i in range(0x2000, 0x4000)]
        )
        self.assertEqual(
            self.ppu_writes,
            [(i%8, 0xA0|(i&0xF)) for i in range(0x2000, 0x2008)]+
            [(i%8, 0xB0|(i&0xF)) for i in range(0x2008, 0x4000)]
        )

    def test_cpu_oamdma(self):
        self.cpu_write(0x4014, 0x0200)
        self.assertEqual(self.cpu_transfer_page_to_oam_calls, [0x0200])

    def test_cpu_io_apu_mappings(self):
        # $4000-$4017	$0018	NES APU and I/O registers
        for i in range(0x4000, 0x4018):
            self.cpu_read(i)
        # $4018-$401F	$0008	APU and I/O functionality that is normally disabled. See CPU Test Mode.
        for i in range(0x4018, 0x4020):
            self.cpu_read(i)
        self.assertEqual(self.apu_reads, list(range(0x0000, 0x0020)))

    def test_cpu_io_misc_cart_mappings(self):
        for i in range(0x4020, 0x6000):
            self.assertEqual(self.cpu_read(i), 0)

    def test_cpu_io_prg_ram_mappings(self):
        for i in range(0x6000, 0x8000):
            self.assertEqual(self.cpu_read(i), 0x40|((i-0x6000)&0xF))

    def test_rom_one_bank_mappings(self):
        # $4020-$FFFF	$BFE0	Cartridge space: PRG ROM, PRG RAM, and mapper registers (See Note)
        for i in range(0x8000, 0xC000):
            self.assertEqual(self.cpu_read(i), 0x20|((i-0x8000)&0xF))
        for i in range(0xC000, 0x10000):
            self.assertEqual(self.cpu_read(i), 0x30|((i-0xC000)&0xF))

class TestCPU(unittest.TestCase):
    def _build_mapper_and_cpu(self, prg_rom, regs):
        self.ppu_reads  = []
        self.ppu_writes = []
        self.ppu_oam_writes = []
        self.apu_reads  = []
        self.apu_writes = []
        self.ram  = array.array('B', (0 for _ in range(0x800)))
        self.vram = array.array('B', (0 for _ in range(0x800)))
        self.prg_ram = b''
        self.prg_rom_banks = [
            array.array('B', (0 for _ in range(0x4000))),
            array.array('B', (0 for _ in range(0x4000)))
        ]
        self.chr_rom_banks = []
        self.chr_ram = b''
        self.pals = array.array('B', (i&0xF for i in range(0x20)))
        for i, value in enumerate(prg_rom):
            self.prg_rom_banks[0][i] = value
        def ppu_read_reg(addr):
            self.ppu_reads.append(addr)
            return 0x55
        def ppu_write_reg(addr, value):
            self.ppu_writes.append((addr, value))
        def ppu_write_oam(value):
            self.ppu_oam_writes.append(value)
        def apu_read_reg(addr):
            self.apu_reads.append(addr)
            return 0x77
        def apu_write_reg(addr, value):
            self.apu_writes.append((addr, value))

        cpu_funcs = create_cpu_funcs(regs=regs, stop_on_brk=True)
        cpu_tick, cpu_trigger_nmi, cpu_trigger_reset, cpu_trigger_irq, cpu_transfer_page_to_oam, cpu_connect, cpu_inspect_regs = cpu_funcs
        cpu_read, cpu_write, ppu_read, ppu_write = mapper_funcs = create_default_mapper_funcs(
            self.ram,
            self.vram,
            self.pals,
            self.prg_rom_banks,
            self.prg_ram,
            self.chr_rom_banks,
            self.chr_ram,
            cpu_transfer_page_to_oam,
            ppu_read_reg,
            ppu_write_reg,
            apu_read_reg,
            apu_write_reg)

        cpu_connect(cpu_read, cpu_write, ppu_write_oam)

        return mapper_funcs, cpu_funcs

    def _test_play(self, prg_rom, expected_regs, expected_t, pc=0x8000, s=0x00, a=0x00, x=0x00, y=0x00, p=0x00, ram_patches=[], expected_ram_patches=[], prg_rom_patches=[], expected_prg_rom_patches=[], expected_ppu_reads=[], expected_ppu_writes=[], expected_ppu_oam_writes=[]):
        mapper_funcs, cpu_funcs = self._build_mapper_and_cpu(prg_rom, (pc, s, a, x, y, p))
        cpu_tick, cpu_trigger_nmi, cpu_trigger_reset, cpu_trigger_irq, cpu_transfer_page_to_oam, cpu_connect, cpu_insepect_regs = cpu_funcs

        def cpu_tick_forever():
            try:
                while True:
                    t = cpu_tick()
            except VMStop:
                return t

        # Patch memory before running
        for i, values in ram_patches:
            for j, value in enumerate(values):
                self.ram[i+j] = value

        for i, values in prg_rom_patches:
            for j, value in enumerate(values):
                final_i = (i+j) - 0x8000
                self.prg_rom_banks[final_i//0x4000][final_i%0x4000] = value

        expected_ram = bytearray(self.ram)
        expected_prg_rom = bytearray(b''.join(self.prg_rom_banks))

        # Run program!
        t = cpu_tick_forever()
        regs = cpu_insepect_regs()

        self.assertTupleEqual(regs, expected_regs)

        for start, expected_ram_bytes in expected_ram_patches:
            end = start + len(expected_ram_bytes)
            self.assertEqual(
                bytearray(self.ram[start:end]),
                expected_ram_bytes)
            expected_ram[start:end] = self.ram[start:end]

        actual_prg_rom = b''.join(self.prg_rom_banks)
        for start, expected_prg_rom_bytes in expected_prg_rom_patches:
            end = start + len(expected_prg_rom_bytes)
            self.assertEqual(
                bytearray(actual_prg_rom[start:end]),
                expected_prg_rom_bytes)
            expected_prg_rom[start:end] = actual_prg_rom[start:end]

        # Ensure memory other than what was patched, wasn't touched
        self.assertEqual(self.ram, expected_ram)
        self.assertEqual(b''.join(self.prg_rom_banks), expected_prg_rom)

        if expected_t is not None:
            self.assertEqual(t, expected_t)

        self.assertEqual(self.ppu_reads, expected_ppu_reads)
        self.assertEqual(self.ppu_writes, expected_ppu_writes)
        self.assertEqual(self.ppu_oam_writes, expected_ppu_oam_writes)

    # Test CPU/PPU registers

    def test_cpu_transfer_page_to_oam(self):
        sample_page = [(i+100)&0xFF for i in range(0x100)]
        self._test_play(b'\x8D\x14\x40', (0x8003, 0x00, 0x03, 0x00, 0x00, 0x00), 4+514, a=0x03, ram_patches=[(0x0300, bytearray(sample_page))], expected_ppu_oam_writes=sample_page)

    def test_ppu_write_reg(self):
        self._test_play(b'\x8D\x00\x20', (0x8003, 0x00, 0xAA, 0x00, 0x00, 0x00), 4, a=0xAA, expected_ppu_writes=[(0, 0xAA)])

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
    def _build_ppu_funcs(self, prg_rom, chr_rom=b'', **ppu_regs):
        self.cpu_trigger_nmi_calls = []
        self.cpu_transfer_page_to_oam_calls = []
        self.ppu_reads  = []
        self.ppu_writes = []
        self.apu_reads  = []
        self.apu_writes = []
        self.ram  = array.array('B', (0 for _ in range(0x800)))
        self.vram = array.array('B', (0 for _ in range(0x800)))
        self.prg_ram = b''
        self.prg_rom_banks = [
            array.array('B', (0 for _ in range(0x4000))),
            array.array('B', (0 for _ in range(0x4000)))
        ]
        self.chr_rom_banks = []
        self.chr_ram = b''
        self.pals = array.array('B', (i&0xF for i in range(0x20)))
        for i, value in enumerate(prg_rom):
            self.prg_rom_banks[0][i] = value
        def cpu_trigger_nmi():
            self.cpu_trigger_nmi_calls.append(None)
        def cpu_transfer_page_to_oam(page_num):
            self.cpu_transfer_page_to_oam_calls.append(page_num)
        def apu_read_reg(addr):
            self.apu_reads.append(addr)
            return 0x77
        def apu_write_reg(addr, value):
            self.apu_writes.append((addr, value))

        self.screen = array.array('B', (0 for _ in range(256*240)))
        self.ppu_funcs = create_ppu_funcs(self.screen, **ppu_regs)
        self.ppu_tick, self.ppu_read_reg, self.ppu_write_reg, self.ppu_write_oam, self.ppu_pals, self.ppu_connect, self.ppu_inspect_regs = self.ppu_funcs
        self.cpu_read, self.cpu_write, self.ppu_read, self.ppu_write = self.mapper_funcs = create_default_mapper_funcs(
            self.ram,
            self.vram,
            self.pals,
            self.prg_rom_banks,
            self.prg_ram,
            self.chr_rom_banks,
            self.chr_ram,
            cpu_transfer_page_to_oam,
            self.ppu_read_reg,
            self.ppu_write_reg,
            apu_read_reg,
            apu_write_reg)

        self.ppu_connect(self.ppu_read, self.ppu_write, cpu_trigger_nmi)

        return self.ppu_funcs

    def test_ppu_write_oam(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        sample_oam = [(i+80)&0xFF for i in range(0x100)]
        # test writing a whole page, as if through cpu oam dma
        for i, x in enumerate(sample_oam):
            ppu_write_oam(x)
            ppu = ppu_inspect_regs()
            self.assertEqual(ppu['oam_addr'], (i+1)&0xFF)
            self.assertEqual(ppu['reg_io_value'], x)
            self.assertEqual(ppu['reg_io_write_state'], 0)
            self.assertEqual(ppu['fine_x_scroll'], 0x00)
            self.assertEqual(ppu['tmp_addr'], 0x0000)
            self.assertEqual(ppu['ppu_addr'], 0x0000)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam'], bytearray(sample_oam))

    def test_ppuctrl_before_30000_cycles(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # check initial state
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['ppu_ctrl'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x0000)
        # initial write
        ppu_write_reg(0, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['ppu_ctrl'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x0000)
        self.assertEqual(ppu['reg_io_write_state'], 8) # TODO: figure out if this is expected (before 30k cycles no write, does tmp_addr write state change)

    def test_ppuctrl(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', t=30000)
        # initial write
        ppu_write_reg(0, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['ppu_ctrl'], 0xAA)
        self.assertEqual(ppu['tmp_addr'], 0x0800)
        # second write
        ppu_write_reg(0, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['ppu_ctrl'], 0x55)
        self.assertEqual(ppu['tmp_addr'], 0x0400)
        # read (latch value)
        self.assertEqual(ppu_read_reg(0), 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
        # write zero
        ppu_write_reg(0, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['ppu_ctrl'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x0000)
        # flip bit to "generate an NMI", not already on, but not in vblank
        ppu_write_reg(0, 0x80)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['ppu_ctrl'], 0x80)

    def test_ppuctrl_trigger_nmi(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', t=30000, ppu_status=0x80)
        # flip bit to "generate an NMI", not already on, in vblank
        ppu_write_reg(0, 0x80)
        self.assertEqual(self.cpu_trigger_nmi_calls, [None])
        # flip bit to "generate an NMI", already on, in vblank
        ppu_write_reg(0, 0x80)
        ppu = ppu_inspect_regs()
        self.assertEqual(self.cpu_trigger_nmi_calls, [None])
        self.assertEqual(ppu['ppu_ctrl'], 0x80)

    def test_ppumask(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # initial write
        ppu_write_reg(1, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['ppu_mask'], 0xAA)
        # second write
        ppu_write_reg(1, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['ppu_mask'], 0x55)
        # read (latch value)
        self.assertEqual(ppu_read_reg(1), 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
        # write zero
        ppu_write_reg(1, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['ppu_mask'], 0x00)

    def test_ppustatus(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # write attempt
        ppu_write_reg(2, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['ppu_status'], 0x00)
        # initial read
        self.assertEqual(ppu_read_reg(2), 0x0A)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)

    def test_ppustatus_clear_vblank(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', reg_io_value=0xAA, ppu_status=0xFF)
        # another read
        self.assertEqual(ppu_read_reg(2), 0xEA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['ppu_status'], 0x7F)  # 7 bit (vblank) is cleared after read

    def test_oamaddr(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # initial write
        ppu_write_reg(3, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['oam_addr'], 0xAA)
        # second write
        ppu_write_reg(3, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['oam_addr'], 0x55)
        # read (latch value)
        self.assertEqual(ppu_read_reg(3), 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
        # TODO: OAMADDR is set to 0 during each of ticks 257–320 (the sprite tile loading interval)
        # of the pre-render and visible scanlines
        # write zero
        ppu_write_reg(3, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['oam_addr'], 0x00)

    def test_oamdata(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # initial write
        ppu_write_reg(4, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam'][0x00], 0x55)
        self.assertEqual(ppu['oam_addr'], 0x01)
        # second write
        ppu_write_reg(4, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam'][0x01], 0xAA)
        self.assertEqual(ppu['oam_addr'], 0x02)
        # reset oamaddr
        ppu_write_reg(3, 0x00)
        # first read
        self.assertEqual(ppu_read_reg(4), 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam_addr'], 0x00)
        # second read
        ppu_write_reg(3, 0x01)
        self.assertEqual(ppu_read_reg(4), 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam_addr'], 0x01)
        # TODO: reads during v/forced blanking do not increment oamaddr
        # TODO: the value of OAMADDR at tick 65 determines the starting address
        # for sprite evaluation for a visible scanline, which can cause the sprite
        # at OAMADDR to be treated as it was sprite 0, both for sprite-0 hit and priority.
        # If OAMADDR is unaligned and does not point to the Y position (first byte)
        # of an OAM entry, then whatever it points to (tile index, attribute, or X coordinate)
        # will be reinterpreted as a Y position, and the following bytes will be similarly reinterpreted.
        # No more sprites will be found once the end of OAM is reached,
        # effectively hiding any sprites before the starting OAMADDR.

    def test_oamdata_wrap_oamaddr(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', oam_addr=0xFF)
        # third write, then wrap oamaddr
        ppu_write_reg(4, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['oam'][0xFF], 0x55)
        self.assertEqual(ppu['oam_addr'], 0x00)

    def test_ppuscroll(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # first write
        ppu_write_reg(5, 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x001F)
        self.assertEqual(ppu['ppu_addr'], 0x0000) # only gets updated during rendering, so always 0x0000 here
        self.assertEqual(ppu['fine_x_scroll'], 0x07)
        # second write
        ppu_write_reg(5, 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x73FF)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        self.assertEqual(ppu['fine_x_scroll'], 0x07)

    def test_ppuscroll_check_unused_bits(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0xFFFF)
        # write zero, first
        ppu_write_reg(5, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0xFFE0)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # write zero, second
        ppu_write_reg(5, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x8C00)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        # first write
        ppu_write_reg(5, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x8C15)
        self.assertEqual(ppu['ppu_addr'], 0x0000) # only gets updated during rendering, so always 0x0000 here
        self.assertEqual(ppu['fine_x_scroll'], 0x02)
        # read (latch value)
        self.assertEqual(ppu_read_reg(5), 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)

    def test_ppuaddr(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'')
        # first write 0xFF
        ppu_write_reg(6, 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x3F00)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # second write 0xFF
        ppu_write_reg(6, 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x3FFF)
        self.assertEqual(ppu['ppu_addr'], 0x3FFF)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # first write 0x00
        ppu_write_reg(6, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x00FF)
        self.assertEqual(ppu['ppu_addr'], 0x3FFF)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # second write 0x00
        ppu_write_reg(6, 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x0000)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # first write
        ppu_write_reg(6, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x2A00)
        self.assertEqual(ppu['ppu_addr'], 0x0000)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        # read (latch value)
        self.assertEqual(ppu_read_reg(6), 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
   
    def test_ppudata(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0x2000, ppu_addr=0x2000)
        # initialize values
        self.vram[0x0000] = 0x00
        self.vram[0x0001] = 0xAA
        self.vram[0x0002] = 0xAA
        # write
        ppu_write_reg(7, 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(tuple(self.vram[0:3]), (0xFF, 0xAA, 0xAA))
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2001)
        # second write
        ppu_write_reg(7, 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(tuple(self.vram[0:3]), (0xFF, 0x55, 0xAA))
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2002)
        
    def test_ppudata_increment_32(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0x2000, ppu_addr=0x2000, ppu_ctrl=0x04)
        # initialize values
        self.vram[0x0000] = 0xFF
        self.vram[0x0001] = 0x55
        self.vram[0x0002] = 0xAA
        # third write, after changing increment to 32
        ppu_write_reg(7, 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(tuple(self.vram[0:3]), (0xAA, 0x55, 0xAA))
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2020)

    def test_ppudata_buffer(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0x2000, ppu_addr=0x2001)
        # initialize values
        self.vram[0x0000] = 0xFF
        self.vram[0x0001] = 0x55
        self.vram[0x0002] = 0xAA
        # read buffer garbage
        self.assertEqual(ppu_read_reg(7), 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2002)
        # read 0x2001
        self.assertEqual(ppu_read_reg(7), 0x55)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2003)
        # read 0x2002
        self.assertEqual(ppu_read_reg(7), 0xAA)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x2004)

    def test_ppudata_read_mirrors(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0x2000, ppu_addr=0x3000)
        # initialize values
        self.vram[0x0000] = 0xFF
        self.vram[0x0001] = 0x55
        self.vram[0x0002] = 0xAA
        # read mirror 0x3000
        self.assertEqual(ppu_read_reg(7), 0x00)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x00)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x3001)
        self.assertEqual(ppu['ppu_data'], 0xFF)
        # read mirror 0x3001
        self.assertEqual(ppu_read_reg(7), 0xFF)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xFF)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x3002)
        self.assertEqual(ppu['ppu_data'], 0x55)

    def test_ppudata_read_pals(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', tmp_addr=0x2000, ppu_addr=0x3F00)
        # initialize values
        self.vram[0x0700] = 0x88
        self.pals[0x0000] = 0x77
        # read directly from pal
        self.assertEqual(ppu_read_reg(7), 0x77)
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x77)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)
        self.assertEqual(ppu['tmp_addr'], 0x2000)
        self.assertEqual(ppu['ppu_addr'], 0x3F01)
        self.assertEqual(ppu['ppu_data'], 0x88) # nametable value mirrored "under" the palette
     
    def test_ppu_scrolling_example_from_wiki(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', t=30000)

        # $2000 write
        # t: ...GH.. ........ <- d: ......GH
        # <used elsewhere>    <- d: ABCDEF..
        # =>
        # t: ...11.. ........ <- d: ......11
        # <used elsewhere>    <- d: 000000..
        ppu_write_reg(0, 0x03) # set nametable select = 1 1 = 0x2C00
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x0C00)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)

        # $2002 read
        # w:                  <- 0
        ppu_read_reg(2) # reset write state
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x0C00)
        self.assertEqual(ppu['fine_x_scroll'], 0x00)

        # $2005 first write (w is 0)
        # t: ....... ...ABCDE <- d: ABCDE...
        # x:              FGH <- d: .....FGH
        # w:                  <- 1
        # =>
        # t: ....... ...10101 <- d: 10101...
        # x:              010 <- d: .....010
        # w:                  <- 1
        ppu_write_reg(5, 0xAA) # write 
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x0C15)
        self.assertEqual(ppu['fine_x_scroll'], 0x02)
        self.assertEqual(ppu['ppu_addr'], 0x0000)

        # $2005 second write (w is 1)
        # t: FGH..AB CDE..... <- d: ABCDEFGH
        # w:                  <- 0
        # =>
        # t: 101..01 010..... <- d: 01010101
        # w:                  <- 0
        ppu_write_reg(5, 0x55) # write 
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x5D55)
        self.assertEqual(ppu['fine_x_scroll'], 0x02)
        self.assertEqual(ppu['ppu_addr'], 0x0000) # copied during rendering

        # $2006 first write (w is 0)
        # t: .CDEFGH ........ <- d: ..CDEFGH
        #        <unused>     <- d: AB......
        # t: Z...... ........ <- 0 (bit Z is cleared)
        # w:                  <- 1
        # =>
        # t: .CDEFGH ........ <- d: ..010101
        #        <unused>     <- d: 01......
        # t: Z...... ........ <- 0 (bit Z is cleared)
        ppu_write_reg(6, 0x55) # write 
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0x55)
        self.assertEqual(ppu['reg_io_write_state'], 8)
        self.assertEqual(ppu['tmp_addr'], 0x1555)
        self.assertEqual(ppu['fine_x_scroll'], 0x02)
        self.assertEqual(ppu['ppu_addr'], 0x0000)

        # $2006 second write (w is 1)
        # t: ....... ABCDEFGH <- d: ABCDEFGH
        # v: <...all bits...> <- t: <...all bits...>
        # w:                  <- 0
        # => 
        # t: ....... ABCDEFGH <- d: 10101010
        # v: <...all bits...> <- t: <...all bits...>
        ppu_write_reg(6, 0xAA) # write 
        ppu = ppu_inspect_regs()
        self.assertEqual(ppu['reg_io_value'], 0xAA)
        self.assertEqual(ppu['reg_io_write_state'], 0)
        self.assertEqual(ppu['tmp_addr'], 0x15AA)
        self.assertEqual(ppu['fine_x_scroll'], 0x02)
        self.assertEqual(ppu['ppu_addr'], 0x15AA)

    def verify_ppu_interal_regs(self, **regs):
        ppu_internal_regs = self.ppu_inspect_regs()
        self.assertEqual({k: ppu_internal_regs[k] for k in regs.keys()}, regs)

    def test_ppu_tick(self):
        ppu_tick, ppu_read_reg, ppu_write_reg, ppu_write_oam, ppu_pals, ppu_connect, ppu_inspect_regs = self._build_ppu_funcs(b'', t=30000)
        # Cycle 0
        self.verify_ppu_interal_regs(frame_num=0, scanline_num=0, scanline_t=0)
        # Cycle 1
        ppu_tick()
        self.verify_ppu_interal_regs(frame_num=0, scanline_num=0, scanline_t=1)
        # Cycle 2
        ppu_tick()
        self.verify_ppu_interal_regs(frame_num=0, scanline_num=0, scanline_t=2)
            
if __name__ == "__main__":
    unittest.main()
