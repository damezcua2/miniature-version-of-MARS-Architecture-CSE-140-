#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem); 

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) { 
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) { 
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr)); 
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

void sign(DecodedInstr* d){
    if(d->op == 0x2 || d->op == 0X3){
        if(1 == d->regs.j.target >> 26){
            d->regs.j.target = (0xF2000000) | (d->regs.j.target);
        }
        else 
            d->regs.j.target = (0x00FFFFFF & (d->regs.j.target));
    
    }
    else{
        if(1 == d->regs.i.addr_or_immed >> 15){
            d->regs.i.addr_or_immed = (0xffff0000) | (d->regs.i.addr_or_immed);
        }
        else{
            d->regs.i.addr_or_immed = (0x0000FFFF & (d->regs.i.addr_or_immed));
        }
    }
}

void iformat(unsigned int instr, DecodedInstr* d, RegVals* rVals){
    d->regs.i.rs = instr << (6) >> (27);
    d->regs.i.rt = instr << 11 >> 27;
    d->regs.i.addr_or_immed = instr << 16 >> 16;
    rVals->R_rs = mips.registers[d->regs.r.rs]; 
    rVals->R_rt = mips.registers[d->regs.r.rt];
    sign(d);
    //printf("op: %u rs: %u rt: %u immid: %u \n", 
     //   d->op, d->regs.r.rs,d->regs.r.rt,d->regs.i.addr_or_immed);
    //getchar();
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    //unsigned int op = (int)(instr >> (32-6));
    d->op = (instr >> (32-6));
    //printf("op: %u \n", d->op);
    switch (d->op){
        case 0x0:{ //r format 
            d->regs.r.rs = instr << 6 >> 27;
            d->regs.r.rt = instr << 11 >> 27; 
            d->regs.r.rd = instr << 16 >> 27; 
            d->regs.r.funct = instr << (26) >> 26; 
            d->regs.r.shamt = instr << 21 >> 27; 
            rVals->R_rs = mips.registers[d->regs.r.rs];  
            rVals->R_rt = mips.registers[d->regs.r.rt];
            rVals->R_rd = 0;
            //printf("\n op: %u rs: %u rt: %u rd: %u shamt: %u funct: %u", 
            //d->op, d->regs.r.rs,d->regs.r.rt,d->regs.r.rd,d->regs.r.shamt,d->regs.r.funct);
            break;
        }
        case 0X9:{iformat(instr,d,rVals); break;} //addiu
        case 0xc:{iformat(instr,d,rVals); break;}//andi
        case 0xd:{iformat(instr,d,rVals); break;}//ori
        case 0xf:{iformat(instr,d,rVals); break;}//lui
        case 0x4:{iformat(instr,d,rVals); break;} //beq
        case 0x5:{iformat(instr,d,rVals); break;}//bne
        case 0x2:{d->regs.j.target = instr << 6 >> 6 << 2; sign(d); break;}//j
        case 0x3:{d->regs.j.target = instr << 6 >> 6 << 2; sign(d); break;}//jal
        case 0x23:{iformat(instr,d,rVals); break;} //lw
        case 0x2b:{iformat(instr,d,rVals); break;} //sw

    }

}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */
    switch(d->op){ 
        case 0x0:{
            switch(d->regs.r.funct){ 
                case 0x21:{/*addu*/ printf("addu\t$%u, $%u, $%u \n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); return;}
                case 0x23:{/*subu*/ printf("subu\t$%u, $%u, $%u \n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); return;}
                case 0x00:{ /*sll*/ printf("sll\t$%u, $%u, $%d \n",d->regs.r.rd, d->regs.r.rs, d->regs.r.shamt); return;} //srl Rdest, Rsrc, shamt
                case 0x02:{/*srl*/ printf("srl\t$%u, $%u, $%d \n",d->regs.r.rd, d->regs.r.rs, d->regs.r.shamt); return;}      
                case 0x24:{/*and*/ printf("and\t$%u, $%u, $%u \n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); return;}//and Rdest, Rsrc1, Rsrc2
                case 0x25:{/*or*/ printf("or\t$%u, $%u, $%u \n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); return;}
                case 0x2a:{/*slt*/ printf("slt\t$%u, $%u, $%u \n", d->regs.r.rd, d->regs.r.rs, d->regs.r.rt); return;}
                case 0x8:{/*jr*/ printf("jr\t$31\n"); return;}
                default: break;
            }
        }
        case 0X9:{printf("addiu\t$%u, $%u, %d \n",d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed); return;} //addiu
        case 0xc:{printf("andi\t$%u, $%u, 0x%x \n",d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);return;}//andi rt=rs+imm
        case 0xd:{printf("ori\t$%u, $%u, 0x%x \n",d->regs.i.rt, d->regs.i.rs, d->regs.i.addr_or_immed);return;}//ori rt=rs|imm
        case 0xf:{printf("lui\t$%u, 0x%x \n",d->regs.i.rt, d->regs.i.addr_or_immed<<16);return;}//lui rt=imm<<16
        case 0x4:{printf("beq\t$%u, $%u, 0x%8.8x \n", d->regs.i.rs, d->regs.i.rt, mips.pc + d->regs.i.addr_or_immed*4 + 4); return;} //beq if(rs==rt) pc+=offset*4
        case 0x5:{printf("bne\t$%u, $%u, 0x%8.8x \n", d->regs.i.rs, d->regs.i.rt, mips.pc + d->regs.i.addr_or_immed*4 + 4); return;}//bne
        case 0x2:{printf("j\t0x%8.8x \n", d->regs.j.target); return;}//j
        case 0x3:{printf("jal\t0x%8.8x \n",d->regs.j.target); return;}//jal
        case 0x23:{printf("lw\t$%u, %d($29)\n", d->regs.i.rt, d->regs.i.addr_or_immed); return;} //lw lw $1, 8($3)
        case 0x2b:{printf("sw\t$%u, %d($29)\n", d->regs.i.rt, d->regs.i.addr_or_immed); return;} //sw
        default: exit(1);//sb\t$10, -4($21)\n
    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */
    switch(d->op){
        case 0x0:
            switch(d->regs.r.funct){
                case 0x8:{return mips.registers[31]; break;} //jr pc = rs
                case 0x21:{ return rVals->R_rs + rVals->R_rt; break;} //addu 
                case 0x23:{ return rVals->R_rs - rVals->R_rt; break;} //subu
                case 0x00:{ return rVals->R_rt << d->regs.r.shamt; break;} //sll rd=rt<<sa
                case 0x02:{ return rVals->R_rt >> d->regs.r.shamt; break;} //srl rd=rt>>sa
                case 0x24:{ return rVals->R_rt & rVals->R_rs; break;} //and rd=rs&rt
                case 0x25:{ return rVals->R_rt | rVals->R_rs; break;} //or rd = rs | rt
                case 0x2a:{ if(rVals->R_rs < rVals->R_rt) //slt
                                return 1; 
                            else
                                return 0;
                            break;} //rd=rs<rt
            }
        case 0x23:{return d->regs.i.addr_or_immed + mips.registers[d->regs.i.rs]; break;} //lw  rt=*(int*)(offset+rs)
        case 0X9:{return rVals->R_rs + d->regs.i.addr_or_immed; break;} //addiu  rt=rs+imm
        case 0xc:{return rVals->R_rs & d->regs.i.addr_or_immed;break;}//andi rt=rs&imm
        case 0xd:{return rVals->R_rs | d->regs.i.addr_or_immed;break;}//ori rt=rs|imm
        case 0xf:{return d->regs.i.addr_or_immed << 16;break;}//lui rt=imm<<16
        case 0x2b:{return (d->regs.i.addr_or_immed + mips.registers[d->regs.i.rs]); break;} //sw   (offset+rs)=rt
        case 0x2:{return ((mips.pc >> 16 <<16) | d->regs.j.target); break;}//j pc=pc_upper|(target<<2)
        case 0x4:{
                if(mips.registers[d->regs.i.rs] == mips.registers[d->regs.i.rt]){return 1;}
                else{ return 0;} break;} //beq if(rs==rt) pc+=offset*4
        case 0x5:{if(mips.registers[d->regs.i.rs] != mips.registers[d->regs.i.rt]){
                    return 1;
                } 
                else{return 0;} break;}//bne
        case 0x3:{return mips.pc += 4; break;}//jal
        default: {return 0; break;}
    }

  return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    if(d->regs.r.funct == 0x8 && d->op == 0x0){
        mips.pc = mips.registers[31];
    }//jr
    switch(d->op){
        case 0x2:{mips.pc = val; break;}//j pc=pc_upper|(target<<2)
        case 0x3:{mips.pc = d->regs.j.target; break;}//jal
        case 0x4:{
            if(val == 1){ mips.pc += d->regs.i.addr_or_immed*4 ;}
            else {mips.pc = mips.pc;}
            break;} //beq if(rs==rt) pc+=offset*4
        case 0x5:{if(val == 1){ mips.pc += d->regs.i.addr_or_immed*4 ;}
            else {mips.pc = mips.pc;}
            break;}//bne
            default: return;
    }
    /* Your code goes here */
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */ 
    *changedMem = -1;
    switch(d->op){ 
        case 0x23:{
            return mips.memory[(val-0x00401000)/4];
        //printf("mem: %u \n",mips.memory[d->regs.i.addr_or_immed]); return mips.memory[d->regs.i.addr_or_immed];
         break;} //lw  rt=*(int*)(offset+rs)
     /*sw*/ case 0x2b:{
            *changedMem = val;
            mips.memory[(val - 0x00401000)/4] = mips.registers[d->regs.i.rt];
            return 0; break;}
        default: return val;
    }
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    *changedReg = -1;
    //int i = -1;
    if(d->regs.r.funct == 0x8 && d->op == 0x0){
        return;
    }
    switch(d->op){
        case 0X9:{*changedReg = d->regs.i.rt; mips.registers[d->regs.i.rt] = val;break;} //addiu  rt=rs+imm
        case 0x0:{*changedReg = d->regs.r.rd; mips.registers[d->regs.r.rd] = val; break;}
        case 0xc:{*changedReg = d->regs.i.rt; mips.registers[d->regs.i.rt] = val;break;}//andi rt=rs&imm
        case 0xd:{*changedReg = d->regs.i.rt; mips.registers[d->regs.i.rt] = val;break;}//ori rt=rs|imm
        case 0xf:{*changedReg = d->regs.i.rt; mips.registers[d->regs.i.rt] = val;break;}//lui rt=imm<<16
        case 0x23:{
            *changedReg = d->regs.i.rt;
            mips.registers[d->regs.i.rt] = val;
            //*changedReg = d->regs.i.rt; mips.registers[d->regs.i.rt] = val; 
        break;} //lw  rt=*(int*)(offset+rs)
        case 0x3:{*changedReg = 31; mips.registers[31] = val; break;}//jal
        default: return;
    }
}
