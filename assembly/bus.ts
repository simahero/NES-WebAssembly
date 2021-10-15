import { Cpu } from './cpu'

export class Bus {

    public ram: Array<u8>
    public cpu: Cpu

    constructor() {
        this.ram = new Array<u8>(64 * 1024)
        this.ram.forEach(address => {
            address = 0x00;
        })
        this.cpu.connectBus(this)
    }

    public write(addr: u16, data: u8): void{
        if(addr >= 0x0000 && addr <= 0xFFFF){
            this.ram[addr] = data
        }
    }
    
    public read(addr: u16, bReadOnly: bool = false): u8 {
        if(addr >= 0x0000 && addr <= 0xFFFF){
            return this.ram[addr]
        }
        return 0x00
    }

}