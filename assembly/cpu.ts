import { Bus } from './bus'

enum FLAGS {
    C = (1 << 0),
    Z = (1 << 1),
    I = (1 << 2),
    D = (1 << 3),
    B = (1 << 4),
    U = (1 << 5),
    V = (1 << 6),
    N = (1 << 7),
}

interface Instruction {
    name: String
    operate: any
    addrmode: any
    cycles: u8
}

export class Cpu {

    public a: u8 = 0x00
    public x: u8 = 0x00
    public y: u8 = 0x00
    public stkp: u8 = 0x00
    public pc: u16 = 0x0000
    public status: u8 = 0x00

    private bus: Bus

    private fetched: u8 = 0x00
    private temp: u16 = 0x0000
    private addr_abs: u16 = 0x0000
    private addr_rel: u16 = 0x0000
    private opcode: u8 = 0x00
    private cycles: u8 = 0
    private clock_count: u8 = 0

    private INSTRUCTION: Array<Instruction> = [
        { name: "BRK", operate: this.BRK, addrmode: this.IMM, cycles: 7 }, { name: "ORA", operate: this.ORA, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 3 }, { name: "ORA", operate: this.ORA, addrmode: this.ZP0, cycles: 3 }, { name: "ASL", operate: this.ASL, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "PHP", operate: this.PHP, addrmode: this.IMP, cycles: 3 }, { name: "ORA", operate: this.ORA, addrmode: this.IMM, cycles: 2 }, { name: "ASL", operate: this.ASL, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "ORA", operate: this.ORA, addrmode: this.ABS, cycles: 4 }, { name: "ASL", operate: this.ASL, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BPL", operate: this.BPL, addrmode: this.REL, cycles: 2 }, { name: "ORA", operate: this.ORA, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "ORA", operate: this.ORA, addrmode: this.ZPX, cycles: 4 }, { name: "ASL", operate: this.ASL, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "CLC", operate: this.CLC, addrmode: this.IMP, cycles: 2 }, { name: "ORA", operate: this.ORA, addrmode: this.ABY, cycles: 4 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "ORA", operate: this.ORA, addrmode: this.ABX, cycles: 4 }, { name: "ASL", operate: this.ASL, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 },
        { name: "JSR", operate: this.JSR, addrmode: this.ABS, cycles: 6 }, { name: "AND", operate: this.AND, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "BIT", operate: this.BIT, addrmode: this.ZP0, cycles: 3 }, { name: "AND", operate: this.AND, addrmode: this.ZP0, cycles: 3 }, { name: "ROL", operate: this.ROL, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "PLP", operate: this.PLP, addrmode: this.IMP, cycles: 4 }, { name: "AND", operate: this.AND, addrmode: this.IMM, cycles: 2 }, { name: "ROL", operate: this.ROL, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "BIT", operate: this.BIT, addrmode: this.ABS, cycles: 4 }, { name: "AND", operate: this.AND, addrmode: this.ABS, cycles: 4 }, { name: "ROL", operate: this.ROL, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BMI", operate: this.BMI, addrmode: this.REL, cycles: 2 }, { name: "AND", operate: this.AND, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "AND", operate: this.AND, addrmode: this.ZPX, cycles: 4 }, { name: "ROL", operate: this.ROL, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "SEC", operate: this.SEC, addrmode: this.IMP, cycles: 2 }, { name: "AND", operate: this.AND, addrmode: this.ABY, cycles: 4 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "AND", operate: this.AND, addrmode: this.ABX, cycles: 4 }, { name: "ROL", operate: this.ROL, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 },
        { name: "RTI", operate: this.RTI, addrmode: this.IMP, cycles: 6 }, { name: "EOR", operate: this.EOR, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 3 }, { name: "EOR", operate: this.EOR, addrmode: this.ZP0, cycles: 3 }, { name: "LSR", operate: this.LSR, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "PHA", operate: this.PHA, addrmode: this.IMP, cycles: 3 }, { name: "EOR", operate: this.EOR, addrmode: this.IMM, cycles: 2 }, { name: "LSR", operate: this.LSR, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "JMP", operate: this.JMP, addrmode: this.ABS, cycles: 3 }, { name: "EOR", operate: this.EOR, addrmode: this.ABS, cycles: 4 }, { name: "LSR", operate: this.LSR, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BVC", operate: this.BVC, addrmode: this.REL, cycles: 2 }, { name: "EOR", operate: this.EOR, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "EOR", operate: this.EOR, addrmode: this.ZPX, cycles: 4 }, { name: "LSR", operate: this.LSR, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "CLI", operate: this.CLI, addrmode: this.IMP, cycles: 2 }, { name: "EOR", operate: this.EOR, addrmode: this.ABY, cycles: 4 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "EOR", operate: this.EOR, addrmode: this.ABX, cycles: 4 }, { name: "LSR", operate: this.LSR, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 },
        { name: "RTS", operate: this.RTS, addrmode: this.IMP, cycles: 6 }, { name: "ADC", operate: this.ADC, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 3 }, { name: "ADC", operate: this.ADC, addrmode: this.ZP0, cycles: 3 }, { name: "ROR", operate: this.ROR, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "PLA", operate: this.PLA, addrmode: this.IMP, cycles: 4 }, { name: "ADC", operate: this.ADC, addrmode: this.IMM, cycles: 2 }, { name: "ROR", operate: this.ROR, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "JMP", operate: this.JMP, addrmode: this.IND, cycles: 5 }, { name: "ADC", operate: this.ADC, addrmode: this.ABS, cycles: 4 }, { name: "ROR", operate: this.ROR, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BVS", operate: this.BVS, addrmode: this.REL, cycles: 2 }, { name: "ADC", operate: this.ADC, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "ADC", operate: this.ADC, addrmode: this.ZPX, cycles: 4 }, { name: "ROR", operate: this.ROR, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "SEI", operate: this.SEI, addrmode: this.IMP, cycles: 2 }, { name: "ADC", operate: this.ADC, addrmode: this.ABY, cycles: 4 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "ADC", operate: this.ADC, addrmode: this.ABX, cycles: 4 }, { name: "ROR", operate: this.ROR, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 },
        { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "STA", operate: this.STA, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "STY", operate: this.STY, addrmode: this.ZP0, cycles: 3 }, { name: "STA", operate: this.STA, addrmode: this.ZP0, cycles: 3 }, { name: "STX", operate: this.STX, addrmode: this.ZP0, cycles: 3 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 3 }, { name: "DEY", operate: this.DEY, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "TXA", operate: this.TXA, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "STY", operate: this.STY, addrmode: this.ABS, cycles: 4 }, { name: "STA", operate: this.STA, addrmode: this.ABS, cycles: 4 }, { name: "STX", operate: this.STX, addrmode: this.ABS, cycles: 4 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 },
        { name: "BCC", operate: this.BCC, addrmode: this.REL, cycles: 2 }, { name: "STA", operate: this.STA, addrmode: this.IZY, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "STY", operate: this.STY, addrmode: this.ZPX, cycles: 4 }, { name: "STA", operate: this.STA, addrmode: this.ZPX, cycles: 4 }, { name: "STX", operate: this.STX, addrmode: this.ZPY, cycles: 4 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 }, { name: "TYA", operate: this.TYA, addrmode: this.IMP, cycles: 2 }, { name: "STA", operate: this.STA, addrmode: this.ABY, cycles: 5 }, { name: "TXS", operate: this.TXS, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 5 }, { name: "STA", operate: this.STA, addrmode: this.ABX, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 },
        { name: "LDY", operate: this.LDY, addrmode: this.IMM, cycles: 2 }, { name: "LDA", operate: this.LDA, addrmode: this.IZX, cycles: 6 }, { name: "LDX", operate: this.LDX, addrmode: this.IMM, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "LDY", operate: this.LDY, addrmode: this.ZP0, cycles: 3 }, { name: "LDA", operate: this.LDA, addrmode: this.ZP0, cycles: 3 }, { name: "LDX", operate: this.LDX, addrmode: this.ZP0, cycles: 3 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 3 }, { name: "TAY", operate: this.TAY, addrmode: this.IMP, cycles: 2 }, { name: "LDA", operate: this.LDA, addrmode: this.IMM, cycles: 2 }, { name: "TAX", operate: this.TAX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "LDY", operate: this.LDY, addrmode: this.ABS, cycles: 4 }, { name: "LDA", operate: this.LDA, addrmode: this.ABS, cycles: 4 }, { name: "LDX", operate: this.LDX, addrmode: this.ABS, cycles: 4 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 },
        { name: "BCS", operate: this.BCS, addrmode: this.REL, cycles: 2 }, { name: "LDA", operate: this.LDA, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "LDY", operate: this.LDY, addrmode: this.ZPX, cycles: 4 }, { name: "LDA", operate: this.LDA, addrmode: this.ZPX, cycles: 4 }, { name: "LDX", operate: this.LDX, addrmode: this.ZPY, cycles: 4 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 }, { name: "CLV", operate: this.CLV, addrmode: this.IMP, cycles: 2 }, { name: "LDA", operate: this.LDA, addrmode: this.ABY, cycles: 4 }, { name: "TSX", operate: this.TSX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 }, { name: "LDY", operate: this.LDY, addrmode: this.ABX, cycles: 4 }, { name: "LDA", operate: this.LDA, addrmode: this.ABX, cycles: 4 }, { name: "LDX", operate: this.LDX, addrmode: this.ABY, cycles: 4 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 4 },
        { name: "CPY", operate: this.CPY, addrmode: this.IMM, cycles: 2 }, { name: "CMP", operate: this.CMP, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "CPY", operate: this.CPY, addrmode: this.ZP0, cycles: 3 }, { name: "CMP", operate: this.CMP, addrmode: this.ZP0, cycles: 3 }, { name: "DEC", operate: this.DEC, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "INY", operate: this.INY, addrmode: this.IMP, cycles: 2 }, { name: "CMP", operate: this.CMP, addrmode: this.IMM, cycles: 2 }, { name: "DEX", operate: this.DEX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "CPY", operate: this.CPY, addrmode: this.ABS, cycles: 4 }, { name: "CMP", operate: this.CMP, addrmode: this.ABS, cycles: 4 }, { name: "DEC", operate: this.DEC, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BNE", operate: this.BNE, addrmode: this.REL, cycles: 2 }, { name: "CMP", operate: this.CMP, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "CMP", operate: this.CMP, addrmode: this.ZPX, cycles: 4 }, { name: "DEC", operate: this.DEC, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "CLD", operate: this.CLD, addrmode: this.IMP, cycles: 2 }, { name: "CMP", operate: this.CMP, addrmode: this.ABY, cycles: 4 }, { name: "NOP", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "CMP", operate: this.CMP, addrmode: this.ABX, cycles: 4 }, { name: "DEC", operate: this.DEC, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 },
        { name: "CPX", operate: this.CPX, addrmode: this.IMM, cycles: 2 }, { name: "SBC", operate: this.SBC, addrmode: this.IZX, cycles: 6 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "CPX", operate: this.CPX, addrmode: this.ZP0, cycles: 3 }, { name: "SBC", operate: this.SBC, addrmode: this.ZP0, cycles: 3 }, { name: "INC", operate: this.INC, addrmode: this.ZP0, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 5 }, { name: "INX", operate: this.INX, addrmode: this.IMP, cycles: 2 }, { name: "SBC", operate: this.SBC, addrmode: this.IMM, cycles: 2 }, { name: "NOP", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.SBC, addrmode: this.IMP, cycles: 2 }, { name: "CPX", operate: this.CPX, addrmode: this.ABS, cycles: 4 }, { name: "SBC", operate: this.SBC, addrmode: this.ABS, cycles: 4 }, { name: "INC", operate: this.INC, addrmode: this.ABS, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 },
        { name: "BEQ", operate: this.BEQ, addrmode: this.REL, cycles: 2 }, { name: "SBC", operate: this.SBC, addrmode: this.IZY, cycles: 5 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 8 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "SBC", operate: this.SBC, addrmode: this.ZPX, cycles: 4 }, { name: "INC", operate: this.INC, addrmode: this.ZPX, cycles: 6 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 6 }, { name: "SED", operate: this.SED, addrmode: this.IMP, cycles: 2 }, { name: "SBC", operate: this.SBC, addrmode: this.ABY, cycles: 4 }, { name: "NOP", operate: this.NOP, addrmode: this.IMP, cycles: 2 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }, { name: "???", operate: this.NOP, addrmode: this.IMP, cycles: 4 }, { name: "SBC", operate: this.SBC, addrmode: this.ABX, cycles: 4 }, { name: "INC", operate: this.INC, addrmode: this.ABX, cycles: 7 }, { name: "???", operate: this.XXX, addrmode: this.IMP, cycles: 7 }
    ]

    constructor() { }

    connectBus(bus: Bus): void {
        this.bus = bus
    }

    public reset(): void {
        this.addr_abs = 0xFFFC
        const lo = this.read(this.addr_abs + 0)
        const hi = this.read(this.addr_abs + 1)

        this.pc = (hi << 8) | lo

        this.a = 0
        this.x = 0
        this.y = 0
        this.stkp = 0xFD
        this.status = 0x00 | FLAGS.U

        this.addr_rel = 0x0000
        this.addr_abs = 0x0000
        this.fetched = 0x00
        this.cycles = 8
    }

    public iqr(): void {
        if (this.getFlag(FLAGS.I) === 0) {

            this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
            this.stkp--
            this.write(0x0100 + this.stkp, this.pc & 0x00FF)

            this.setFlag(FLAGS.B, 0)
            this.setFlag(FLAGS.U, 1)
            this.setFlag(FLAGS.I, 1)
            this.stkp--

            this.addr_abs = 0xFFFE
            const lo = this.read(this.addr_abs + 0)
            const hi = this.read(this.addr_abs + 1)
            this.pc = (hi << 8) | lo

            this.cycles = 7
        }
    }

    public nmi(): void {
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
        this.stkp--
        this.write(0x0100 + this.stkp, this.pc & 0x00FF)
        this.stkp--

        this.setFlag(FLAGS.B, 0)
        this.setFlag(FLAGS.U, 1)
        this.setFlag(FLAGS.I, 1)
        this.write(0x0100 + this.stkp, this.status)
        this.stkp--

        this.addr_abs = 0xFFFA

        const lo = this.read(this.addr_abs + 0)
        const hi = this.read(this.addr_abs + 1)
        this.pc = (hi << 8) | lo

        this.cycles = 8

    }

    public clock(): void {
        if (this.cycles === 0) {
            this.opcode = this.read(this.pc)
            // LOG
            this.setFlag(FLAGS.U, true)
            this.pc++
            this.cycles = this.INSTRUCTION[this.opcode].cycles

            const additional_cycle1: u8 = this.INSTRUCTION[this.opcode].addrmode()
            const additional_cycle2: u8 = this.INSTRUCTION[this.opcode].operate()

            this.cycles += (additional_cycle1 & additional_cycle2)

            this.setFlag(FLAGS.U, true)

            this.clock_count++
            this.cycles--
        }
    }

    public complete(): bool { return this.cycles === 0 }

    private fetch(): u8 {
        if (this.INSTRUCTION[this.opcode].addrmode !== this.IMP()) {
            this.fetched = this.read(this.addr_abs)
        }
        return this.fetched
    }

    private write(addr: u16, data: u8): void {
        this.bus.write(addr, data)
    }

    private read(addr: u16): u8 {
        return this.bus.read(addr, false)
    }

    private getFlag(flag: FLAGS): u8 {
        return ((this.status & flag) ? 1 : 0)
    }

    private setFlag(flag: FLAGS, v: bool): void {
        if (v) {
            this.status |= flag
        } else {
            this.status &= ~flag
        }
    }

    //address modes
    private IMP(): u8 {
        this.fetched = this.a
        return 0
    }
    private IMM(): u8 {
        this.addr_abs = this.pc++
        return 0
    }
    private ZP0(): u8 {
        this.addr_abs = this.read(this.pc)
        this.pc++
        this.addr_abs &= 0x00FF
        return 0
    }
    private ZPX(): u8 {
        this.addr_abs = (this.read(this.pc) + this.x)
        this.pc++
        this.addr_abs &= 0x00FF
        return 0
    }
    private ZPY(): u8 {
        this.addr_abs = (this.read(this.pc) + this.y)
        this.pc++
        this.addr_abs &= 0x00FF
        return 0
    }
    private REL(): u8 {
        this.addr_rel = this.read(this.pc)
        this.pc++
        if (this.addr_rel & 0x80) this.addr_rel |= 0xFF00
        return 0
    }
    private ABS(): u8 {
        const lo: u16 = this.read(this.pc)
        this.pc++
        const hi: u16 = this.read(this.pc)
        this.pc++
        this.addr_abs = (hi << 8) | lo
        return 0
    }
    private ABX(): u8 {
        const lo: u16 = this.read(this.pc)
        this.pc++
        const hi: u16 = this.read(this.pc)
        this.pc++
        if ((this.addr_abs & 0xFF00) != (hi << 8)) return 1
        return 0
    }
    private ABY(): u8 {
        const lo: u16 = this.read(this.pc)
        this.pc++
        const hi: u16 = this.read(this.pc)
        this.pc++
        this.addr_abs = (hi << 8) | lo
        this.addr_abs += this.y
        if ((this.addr_abs & 0x00FF) != (hi << 8)) return 1
        return 0
    }
    private IND(): u8 {
        const ptr_lo: u16 = this.read(this.pc)
        this.pc++
        const ptr_hi: u16 = this.read(this.pc)
        this.pc++

        const ptr = (ptr_hi << 8) | ptr_lo

        if (ptr_lo === 0x00FF) {
            this.addr_abs = (this.read(ptr & 0xFF00 << 8) | this.read(ptr + 0))
        } else {
            this.addr_abs = (this.read(ptr & 1 << 8) | this.read(ptr + 0))
        }
        return 0
    }
    private IZX(): u8 {
        const t: u16 = this.read(this.pc)
        this.pc++

        const lo: u16 = this.read((u16)(t + this.x) & 0x00FF)
        const hi: u16 = this.read((u16)(t + this.x + 1) & 0x00FF)
        this.addr_abs = (hi << 8) | lo

        return 0
    }
    private IZY(): u8 {
        const t: u16 = this.read(this.pc)
        this.pc++

        const lo: u16 = this.read(t & 0x00FF)
        const hi: u16 = this.read((t + 1) & 0x00FF)

        this.addr_abs = (hi << 8) | lo
        this.addr_abs += this.y
        if ((this.addr_abs & 0x00FF) != (hi << 8)) return 1
        return 0
    }


    //opcodes
    private ADC(): u8 {
        this.fetch()
        const value: u16 = (this.fetched) ^ 0x00FF
        this.temp = this.a + value + this.getFlag(FLAGS.C)
        this.setFlag(FLAGS.C, this.temp & 0xFF00)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0)
        this.setFlag(FLAGS.V, ((this.temp ^ this.a)) & (this.temp ^ value) & 0x0080)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 1
    }
    private AND(): u8 {
        this.fetch()
        this.a &= this.fetched
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 1
    }
    private ASL(): u8 {
        this.fetch()
        this.temp = this.fetched << 1
        this.setFlag(FLAGS.C, (this.temp & 0xFF00) > 0)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x00)
        this.setFlag(FLAGS.N, (this.temp & 0x00FF) === 0x80)
        if (this.INSTRUCTION[this.opcode].addrmode === this.IMP) {
            this.a = this.temp & 0x00FF
        } else {
            this.write(this.addr_abs, this.temp & 0x00FF)
        }
        return 0
    }
    private BCC(): u8 {
        if (this.getFlag(FLAGS.C) === 0) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BCS(): u8 {
        if (this.getFlag(FLAGS.C) === 1) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BEQ(): u8 {
        if (this.getFlag(FLAGS.Z) === 1) {
            this.cycles++
            this.addr_abs = this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BIT(): u8 {
        this.fetch()
        this.temp = this.a & this.fetched
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x00)
        this.setFlag(FLAGS.N, this.fetched & (1 << 7))
        this.setFlag(FLAGS.V, this.fetched & (1 << 6))
        return 0
    }
    private BMI(): u8 {
        if (this.getFlag(FLAGS.N) === 1) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BNE(): u8 {
        if (this.getFlag(FLAGS.Z) === 0) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BPL(): u8 {
        if (this.getFlag(FLAGS.N) === 0) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel

            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00))
                this.cycles++

            this.pc = this.addr_abs
        }
        return 0
    }
    private BRK(): u8 {
        this.pc++

        this.setFlag(FLAGS.I, 1)
        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
        this.stkp--
        this.write(0x0100 + this.stkp, this.pc & 0x00FF)
        this.stkp--

        this.setFlag(FLAGS.B, 1)
        this.write(0x0100 + this.stkp, this.status)
        this.stkp--
        this.setFlag(FLAGS.B, 0)

        this.pc = this.read(0xFFFE) | (this.read(0xFFFF) << 8)
        return 0
    }
    private BVC(): u8 {
        if (this.getFlag(FLAGS.V) === 0) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private BVS(): u8 {
        if (this.getFlag(FLAGS.V) === 1) {
            this.cycles++
            this.addr_abs = this.pc + this.addr_rel
            if ((this.addr_abs & 0xFF00) !== (this.pc & 0xFF00)) this.cycles++
            this.pc = this.addr_abs
        }
        return 0
    }
    private CLC(): u8 {
        this.setFlag(FLAGS.C, false)
        return 0
    }
    private CLD(): u8 {
        this.setFlag(FLAGS.D, false)
        return 0
    }
    private CLI(): u8 {
        this.setFlag(FLAGS.I, false)
        return 0
    }
    private CLV(): u8 {
        this.setFlag(FLAGS.V, false)
        return 0
    }
    private CMP(): u8 {
        this.fetch()
        this.temp = this.a - this.fetched
        this.setFlag(FLAGS.C, this.a >= this.fetched)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 1
    }
    private CPX(): u8 {
        this.fetch()
        this.temp = this.x - this.fetched
        this.setFlag(FLAGS.C, this.x >= this.fetched)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 0
    }
    private CPY(): u8 {
        this.fetch()
        this.temp = this.y - this.fetched
        this.setFlag(FLAGS.C, this.y >= this.fetched)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 0
    }
    private DEC(): u8 {
        this.fetch()
        this.temp = this.fetched - 1
        this.write(this.addr_abs, this.temp & 0x00FF)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 0
    }
    private DEX(): u8 {
        this.x--
        this.setFlag(FLAGS.Z, this.x === 0x00)
        this.setFlag(FLAGS.N, this.x & 0x80)
        return 0
    }
    private DEY(): u8 {
        this.y--
        this.setFlag(FLAGS.Z, this.y === 0x00)
        this.setFlag(FLAGS.N, this.y & 0x80)
        return 0
    }
    private EOR(): u8 {
        this.fetch()
        this.a = this.a ^ this.fetched
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 1
    }
    private INC(): u8 {
        this.fetch()
        this.temp = this.fetched + 1
        this.write(this.addr_abs, this.temp & 0x00FF)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 0
    }
    private INX(): u8 {
        this.x++
        this.setFlag(FLAGS.Z, this.x === 0x00)
        this.setFlag(FLAGS.N, this.x & 0x80)
        return 0
    }
    private INY(): u8 {
        this.y++
        this.setFlag(FLAGS.Z, this.y === 0x00)
        this.setFlag(FLAGS.N, this.y & 0x80)
        return 0
    }
    private JMP(): u8 {
        this.pc = this.addr_abs
        return 0
    }
    private JSR(): u8 {
        this.pc--

        this.write(0x0100 + this.stkp, (this.pc >> 8) & 0x00FF)
        this.stkp--
        this.write(0x0100 + this.stkp, this.pc & 0x00FF)
        this.stkp--

        this.pc = this.addr_abs
        return 0
    }
    private LDA(): u8 {
        this.fetch()
        this.a = this.fetched
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 1
    }
    private LDX(): u8 {
        this.fetch()
        this.x = this.fetched
        this.setFlag(FLAGS.Z, this.x === 0x00)
        this.setFlag(FLAGS.N, this.x & 0x80)
        return 1
    }
    private LDY(): u8 {
        this.fetch()
        this.y = this.fetched
        this.setFlag(FLAGS.Z, this.y === 0x00)
        this.setFlag(FLAGS.N, this.y & 0x80)
        return 1
    }
    private LSR(): u8 {
        this.fetch()
        this.setFlag(FLAGS.C, this.fetched & 0x0001)
        this.temp = this.fetched >> 1
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        if (this.INSTRUCTION[this.opcode].addrmode === this.IMP) {
            this.a = this.temp & 0x00FF
        } else {
            this.write(this.addr_abs, this.temp & 0x00FF)
        }
        return 0
    }
    private NOP(): u8 {
        switch (this.opcode) {
            case 0x1C:
            case 0x3C:
            case 0x5C:
            case 0x7C:
            case 0xDC:
            case 0xFC:
                return 1
                break
        }
        return 0
    }
    private ORA(): u8 {
        this.fetch()
        this.a = this.a | this.fetched
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 1
    }
    private PHA(): u8 {
        this.write(0x0100 + this.stkp, this.a)
        this.stkp--
        return 0
    }
    private PHP(): u8 {
        this.write(0x0100 + this.stkp, this.status | FLAGS.B | FLAGS.U)
        this.setFlag(FLAGS.B, 0)
        this.setFlag(FLAGS.U, 0)
        this.stkp--
        return 0
    }
    private PLA(): u8 {
        this.stkp++
        this.a = this.read(0x0100 + this.stkp)
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 0
    }
    private PLP(): u8 {
        this.stkp++
        this.status = this.read(0x0100 + this.stkp)
        this.setFlag(FLAGS.U, 1)
        return 0
    }
    private ROL(): u8 {
        this.fetch()
        this.temp = (u16)(this.fetched << 1) | this.getFlag(FLAGS.C)
        this.setFlag(FLAGS.C, this.temp & 0xFF00)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x0000)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        if (this.INSTRUCTION[this.opcode].addrmode === this.IMP) {
            this.a = this.temp & 0x00FF
        } else {
            this.write(this.addr_abs, this.temp & 0x00FF)
        }
        return 0
    }
    private ROR(): u8 {
        this.fetch()
        this.temp = (u16)(this.getFlag(FLAGS.C) << 7) | (this.fetched >> 1)
        this.setFlag(FLAGS.C, this.fetched & 0x01)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0x00)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        if (this.INSTRUCTION[this.opcode].addrmode === this.IMP) {
            this.a = this.temp & 0x00FF
        } else {
            this.write(this.addr_abs, this.temp & 0x00FF)
        }
        return 0
    }
    private RTI(): u8 {
        this.stkp++
        this.status = this.read(0x0100 + this.stkp)
        this.status &= ~FLAGS.B
        this.status &= ~FLAGS.U

        this.stkp++
        this.pc = this.read(0x0100 + this.stkp)
        this.stkp++
        this.pc |= this.read(0x0100 + this.stkp) << 8
        return 0
    }
    private RTS(): u8 {
        this.stkp++
        this.pc = this.read(0x0100 + this.stkp)
        this.stkp++
        this.pc |= this.read(0x0100 + this.stkp) << 8

        this.pc++
        return 0
    }
    private SBC(): u8 {
        this.fetch()
        const value: u16 = (this.fetched) ^ 0x00FF
        this.temp = this.a + value + this.getFlag(FLAGS.C)
        this.setFlag(FLAGS.C, this.temp & 0xFF00)
        this.setFlag(FLAGS.Z, (this.temp & 0x00FF) === 0)
        this.setFlag(FLAGS.V, ((this.temp ^ this.a)) & (this.temp ^ value) & 0x0080)
        this.setFlag(FLAGS.N, this.temp & 0x0080)
        return 1
    }
    private SEC(): u8 {
        this.setFlag(FLAGS.C, true)
        return 0
    }
    private SED(): u8 {
        this.setFlag(FLAGS.D, true)
        return 0
    }
    private SEI(): u8 {
        this.setFlag(FLAGS.I, true)
        return 0
    }
    private STA(): u8 {
        this.write(this.addr_abs, this.a)
        return 0
    }
    private STX(): u8 {
        this.write(this.addr_abs, this.x)
        return 0
    }
    private STY(): u8 {
        this.write(this.addr_abs, this.y)
        return 0
    }
    private TAX(): u8 {
        this.x = this.a
        this.setFlag(FLAGS.Z, this.x === 0x00)
        this.setFlag(FLAGS.N, this.x & 0x80)
        return 0
    }
    private TAY(): u8 {
        this.y = this.a
        this.setFlag(FLAGS.Z, this.y === 0x00)
        this.setFlag(FLAGS.N, this.y & 0x80)
        return 0
    }
    private TSX(): u8 {
        this.x = this.stkp
        this.setFlag(FLAGS.Z, this.x === 0x00)
        this.setFlag(FLAGS.N, this.x & 0x80)
        return 0
    }
    private TXA(): u8 {
        this.a = this.x
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 0
    }
    private TXS(): u8 {
        this.stkp = this.x
        return 0
    }
    private TYA(): u8 {
        this.a = this.y
        this.setFlag(FLAGS.Z, this.a === 0x00)
        this.setFlag(FLAGS.N, this.a & 0x80)
        return 0
    }

    private XXX(): u8 { return 0 }

}