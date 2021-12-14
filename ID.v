`include "lib/defines.vh"
module ID(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,
    
    output wire stallreq,

    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,

    input wire [31:0] inst_sram_rdata,

    input wire [`WB_TO_RF_WD-1:0] wb_to_rf_bus,
    
    input wire [`EX_TO_ID_WD-1:0] ex_to_id_bus,
    
    input wire [`MEM_TO_ID_WD-1:0] mem_to_id_bus,
    
    input wire [`WB_TO_ID_WD-1:0] wb_to_id_bus,

    output wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,

    output wire [`BR_WD-1:0] br_bus
    
);

    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;
    reg [31:0] if_to_id_inst_r;//暂存下一条if的指令，防止其与load相关冲突
    reg [31:0] hilo_inst_r;
    reg inst_flag;//判断是载入下一条还是上一条load相关
    reg inst_hilo_flag;//判断是载入下一条还是上一条hilo寄存器相关指令
    wire [31:0] inst;
    wire [31:0] id_pc;
    wire ce;
    
    wire ex_rf_we;
    wire [4:0] ex_rf_waddr;
    wire [31:0] ex_result;
    wire [5:0] ex_op_i;
    
    wire mem_rf_we;
    wire [4:0] mem_rf_waddr;
    wire [31:0] mem_rf_wdata;

    wire wb_rf_we;
    wire [4:0] wb_rf_waddr;
    wire [31:0] wb_rf_wdata;
    
    wire hi_rf_we;
    wire [31:0] hi_rf_wdata;
    wire lo_rf_we;
    wire [31:0] lo_rf_wdata;
    
    wire hi_ex_to_id_we;
    wire [31:0] hi_ex_wdata;
    wire lo_ex_to_id_we;
    wire [31:0] lo_ex_wdata;
    
    wire hi_mem_we;
    wire [31:0] hi_mem_wdata;
    wire lo_mem_we;
    wire [31:0] lo_mem_wdata;
    
    wire hi_wb_we;
    wire [31:0] hi_wb_wdata;
    wire lo_wb_we;
    wire [31:0] lo_wb_wdata;
    
    wire ex_r_lo;
    wire [31:0] ex_r_lo_data;
    wire ex_r_hi;
    wire [31:0] ex_r_hi_data;
    

    always @ (posedge clk) begin
        if (rst) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;        
        end
        // else if (flush) begin
        //     ic_to_id_bus <= `IC_TO_ID_WD'b0;
        // end
        else if (stall[1]==`Stop && stall[2]==`NoStop) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;
        end
        else if (stall[1]==`NoStop) begin
            if_to_id_bus_r <= if_to_id_bus;
        end
    end
    
    
    
    always @ (posedge clk) begin
    //inst_flag <= 1'b0;
        
        if (stall[2]==`Stop && stall[3]==`NoStop) begin
            if_to_id_inst_r <= inst_sram_rdata;
            inst_flag <= 1'b1;
        end
        else if (stall[2]==`NoStop) begin
            if_to_id_inst_r <= 32'b0;
            inst_flag <= 1'b0;
        end
    end
    
    always @ (posedge clk) begin
    //inst_hilo_flag <= 1'b0;   
        if (stall[3]==`Stop && stall[4]==`NoStop && hilo_inst_r==32'b0) begin
            hilo_inst_r <= inst_sram_rdata;
            inst_hilo_flag <= 1'b1;
        end
        else if (stall[3]==`NoStop) begin
            hilo_inst_r <= 32'b0;
            inst_hilo_flag <= 1'b0;
        end
    end
    
    //(stall[2]==`NoStop)
    
    assign inst = inst_flag ? if_to_id_inst_r : 
                   inst_hilo_flag ? hilo_inst_r : 
                   inst_sram_rdata;
    assign {
        ce,
        id_pc
    } = if_to_id_bus_r;
    assign wb_to_rf_bus = {
        hi_rf_we,       
        hi_rf_wdata,    
        lo_rf_we,       
        lo_rf_wdata,
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    };
    assign {
        ex_r_lo,           // 141
        ex_r_lo_data,      // 140:109
        ex_r_hi,           // 108
        ex_r_hi_data,      // 107:76
        hi_ex_to_id_we,       
        hi_ex_wdata,    
        lo_ex_to_id_we,       
        lo_ex_wdata, 
        ex_rf_we,
        ex_rf_waddr,
        ex_result,
        ex_op_i
    } = ex_to_id_bus;
    assign {
        hi_mem_we,       
        hi_mem_wdata,    
        lo_mem_we,       
        lo_mem_wdata,  
        mem_rf_we,
        mem_rf_waddr,
        mem_rf_wdata
    } = mem_to_id_bus;
    assign {
        hi_wb_we,       
        hi_wb_wdata,    
        lo_wb_we,       
        lo_wb_wdata,
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    } = wb_to_id_bus;
        
    wire [5:0] opcode;
    wire [4:0] rs,rt,rd,sa;
    wire [5:0] func;
    wire [15:0] imm;
    wire [25:0] instr_index;
    wire [19:0] code;
    wire [4:0] base;
    wire [15:0] offset;
    wire [2:0] sel;

    wire [63:0] op_d, func_d;
    wire [31:0] rs_d, rt_d, rd_d, sa_d;

    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire [11:0] alu_op;

    wire data_ram_en;
    wire [3:0] data_ram_wen;
    
    wire rf_we;
    wire [4:0] rf_waddr;
    wire [4:0] rf_waddr_end;
    wire sel_rf_res;
    wire [2:0] sel_rf_dst;

    wire [31:0] rdata1, rdata2;
    wire r_hi;
    wire [31:0] r_hi_data;
    wire r_lo;
    wire [31:0] r_lo_data;
    wire hi_we_id;
    wire lo_we_id;


    regfile u_regfile(
    	.clk    (clk    ),
        .raddr1 (rs ),
        .rdata1 (rdata1 ),
        .raddr2 (rt ),
        .rdata2 (rdata2 ),
        .r_hi   (r_hi),
        .r_hi_data(r_hi_data),
        .r_lo   (r_lo),
        .r_lo_data(r_lo_data),
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  ),
        .hi_we  (hi_wb_we),
        .lo_we  (lo_wb_we),
        .hi_wdata(hi_wb_wdata),
        .lo_wdata(lo_wb_wdata),
        .ex_to_id_we     (ex_rf_we     ),
        .ex_to_id_waddr  (ex_rf_waddr  ),
        .ex_wdata  (ex_result  ),
        .hi_ex_to_id_we(hi_ex_to_id_we),
        .hi_ex_wdata  (hi_ex_wdata),
        .lo_ex_to_id_we(lo_ex_to_id_we),
        .lo_ex_wdata  (lo_ex_wdata),
        .mem_to_id_we     (mem_rf_we     ),
        .mem_to_id_waddr  (mem_rf_waddr  ),
        .mem_wdata  (mem_rf_wdata  ),
        .hi_mem_to_id_we(hi_mem_we),
        .hi_mem_wdata  (hi_mem_wdata),
        .lo_mem_to_id_we(lo_mem_we),
        .lo_mem_wdata  (lo_mem_wdata),
        .wb_to_id_we     (wb_rf_we     ),
        .wb_to_id_waddr  (wb_rf_waddr  ),
        .wb_wdata        (wb_rf_wdata  ),
        .hi_wb_to_id_we(hi_wb_we),
        .hi_wb_wdata  (hi_wb_wdata),
        .lo_wb_to_id_we(lo_wb_to_id_we),
        .lo_wb_wdata  (lo_wb_wdata)
    );
    

    assign opcode = inst[31:26];
    assign rs = inst[25:21];
    assign rt = inst[20:16];
    assign rd = inst[15:11];
    assign sa = inst[10:6];
    assign func = inst[5:0];
    assign imm = inst[15:0];
    assign instr_index = inst[25:0];
    assign code = inst[25:6];
    assign base = inst[25:21];
    assign offset = inst[15:0];
    assign sel = inst[2:0];

    wire inst_ori, inst_lui, inst_addiu, inst_beq, inst_subu,
         inst_jal, inst_jr, inst_addu, inst_or, inst_sll, inst_lw,
         inst_sw,inst_xor, inst_sltu, inst_bne, inst_slt, inst_slti,
         inst_sltiu, inst_j, inst_add, inst_sub, inst_and, inst_andi,
         inst_nor, inst_xori, inst_sllv, inst_sra, inst_srav, inst_srl,
         inst_srlv,inst_bgez,inst_bgtz,inst_blez,inst_bltz,inst_bltzal,inst_bgezal,inst_jalr,
         inst_mfhi,inst_mflo, inst_mthi, inst_mtlo, inst_div ,inst_divu, inst_lb;

    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    decoder_6_64 u0_decoder_6_64(
    	.in  (opcode  ),
        .out (op_d )
    );

    decoder_6_64 u1_decoder_6_64(
    	.in  (func  ),
        .out (func_d )
    );
    
    decoder_5_32 u2_decoder_5_32(
    	.in  (sa  ),
        .out (sa_d )
    );
    
    decoder_5_32 u0_decoder_5_32(
    	.in  (rs  ),
        .out (rs_d )
    );

    decoder_5_32 u1_decoder_5_32(
    	.in  (rt  ),
        .out (rt_d )
    );

    
    assign inst_ori     = op_d[6'b00_1101];
    assign inst_lui     = op_d[6'b00_1111];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_beq     = op_d[6'b00_0100];
    assign inst_subu    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0011];
    assign inst_jal     = op_d[6'b00_0011];
    assign inst_jr      = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b00_1000];
    assign inst_addu    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0001];
    assign inst_or      = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0101];
    assign inst_lw      = op_d[6'b10_0011];
    assign inst_sll     = op_d[6'b00_0000] && func_d[6'b00_0000];
    assign inst_sw      = op_d[6'b10_1011];
    assign inst_xor     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0110];
    assign inst_sltu    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_1011];
    assign inst_bne     = op_d[6'b00_0101];
    assign inst_slt     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_1010];
    assign inst_slti    = op_d[6'b00_1010];
    assign inst_sltiu   = op_d[6'b00_1011];
    assign inst_j       = op_d[6'b00_0010];
    assign inst_add     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0000];
    assign inst_addi    = op_d[6'b00_1000];
    assign inst_sub     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0010];
    assign inst_and     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0100];
    assign inst_andi    = op_d[6'b00_1100];
    assign inst_nor     = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b10_0111];
    assign inst_xori    = op_d[6'b00_1110];
    assign inst_sllv    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b00_0100];
    assign inst_sra     = op_d[6'b00_0000] && func_d[6'b00_0011];
    assign inst_srav    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b00_0111];
    assign inst_srl     = op_d[6'b00_0000] && func_d[6'b00_0010];
    assign inst_srlv    = op_d[6'b00_0000] && sa_d[5'b00_000] && func_d[6'b00_0110];
    assign inst_bgez    = op_d[6'b00_0001] &&(rt==5'b00_001);
    assign inst_bgtz    = op_d[6'b00_0111] ;
    assign inst_blez    = op_d[6'b00_0110] ;
    assign inst_bltz    = op_d[6'b00_0001] &&(rt==5'b00_000);
    assign inst_bltzal    = op_d[6'b00_0001] &&(rt==5'b10_000);
    assign inst_bgezal    = op_d[6'b00_0001] &&(rt==5'b10_001);
    assign inst_jalr      = op_d[6'b00_0000] &&(rt==5'b00_000)&& sa_d[5'b00_000] && func_d[6'b00_1001];
    assign inst_mfhi      = op_d[6'b00_0000] && func_d[6'b01_0000];
    assign inst_mflo      = op_d[6'b00_0000] && (func==6'b010010);
    assign inst_mthi      = op_d[6'b00_0000] && func_d[6'b01_0001];
    assign inst_mtlo      = op_d[6'b00_0000] && func_d[6'b01_0011];
    assign inst_div       = op_d[6'b00_0000] && func_d[6'b01_1010];
    assign inst_divu      = op_d[6'b00_0000] && func_d[6'b01_1011];
    assign inst_lb        = op_d[6'b10_0000];
    
    assign r_hi = inst_mfhi;
    assign r_lo = inst_mflo;
    
    assign hi_we_id = inst_div | inst_divu;
    assign lo_we_id = inst_div | inst_divu;
    
    // rs to reg1
    assign sel_alu_src1[0] = inst_ori | inst_addiu | inst_subu | inst_addu | inst_or |
                             inst_lw | inst_sw | inst_xor | inst_sltu | inst_slt | 
                             inst_slti | inst_sltiu | inst_add | inst_addi | inst_sub |
                             inst_and | inst_andi | inst_nor | inst_xori | inst_sllv |
                             inst_srav | inst_srlv | inst_div | inst_divu | inst_lb;

    // pc to reg1
    assign sel_alu_src1[1] = inst_jal | inst_bltzal | inst_bgezal |inst_jalr ;

    // sa_zero_extend to reg1
    assign sel_alu_src1[2] = inst_sll | inst_sra | inst_srl;

    
    // rt to reg2
    assign sel_alu_src2[0] = inst_subu | inst_addu | inst_or | inst_sll | inst_xor | inst_sltu |
                             inst_slt | inst_add | inst_sub | inst_and | inst_nor | inst_sllv |
                             inst_sra | inst_srav | inst_srl | inst_srlv | inst_div | inst_divu;
    
    // imm_sign_extend to reg2
    assign sel_alu_src2[1] = inst_lui | inst_addiu | inst_lw | inst_sw | inst_slti | inst_sltiu |
                              inst_addi | inst_lb;

    // 32'b8 to reg2
    assign sel_alu_src2[2] = inst_jal| inst_bltzal | inst_bgezal | inst_jalr;

    // imm_zero_extend to reg2
    assign sel_alu_src2[3] = inst_ori | inst_xori | inst_andi;



    assign op_add = inst_addiu | inst_jal | inst_addu | inst_lw | inst_sw | inst_add | inst_addi | inst_bltzal | inst_bgezal | inst_jalr | inst_lb;
    assign op_sub = inst_subu | inst_sub;
    assign op_slt = inst_slt | inst_slti;
    assign op_sltu = inst_sltu | inst_sltiu;
    assign op_and = inst_and | inst_andi;
    assign op_nor = inst_nor;
    assign op_or = inst_ori | inst_or;
    assign op_xor = inst_xor | inst_xori;
    assign op_sll = inst_sll | inst_sllv;
    assign op_srl = inst_srl | inst_srlv;
    assign op_sra = inst_sra | inst_srav;
    assign op_lui = inst_lui;

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui};



    // load and store enable 内存使能
    assign data_ram_en = inst_lw | inst_sw | inst_lb;
    
    wire inst_b;
    assign inst_b = inst_lb;

    // write enable
    assign data_ram_wen = inst_sw ? 4'b1111 :
                          4'b0;



    // regfile sotre enable
    assign rf_we = inst_ori | inst_lui | inst_addiu | inst_subu | inst_jal | inst_addu | 
                   inst_or | inst_sll | inst_lw | inst_xor | inst_sltu | inst_slt | inst_slti |
                   inst_sltiu | inst_add | inst_addi | inst_sub| inst_and | inst_andi | 
                   inst_nor | inst_xori | inst_sllv | inst_sra | inst_srav | inst_srl | 
                   inst_srlv |inst_bltzal | inst_bgezal | inst_jalr | inst_mfhi | inst_mflo |
                   inst_lb;



    // store in [rd]
    assign sel_rf_dst[0] = inst_subu | inst_addu | inst_or | inst_sll | inst_xor |inst_sltu |
                            inst_slt | inst_add | inst_sub | inst_and | inst_nor | inst_sllv |
                            inst_sra | inst_srav | inst_srl | inst_srlv | inst_jalr | inst_mfhi |
                            inst_mflo;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_ori | inst_lui | inst_addiu |inst_lw | inst_sw | inst_slti |
                            inst_sltiu | inst_addi | inst_andi | inst_xori | inst_lb;
    // store in [31]
    assign sel_rf_dst[2] = inst_jal | inst_bltzal | inst_bgezal;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;
                    

    // 0 from alu_res ; 1 from ld_res
    assign sel_rf_res = 1'b0; 

    assign id_to_ex_bus = {
        inst_b,         // 225
        r_lo,           // 224
        r_lo_data,      // 223:192
        r_hi,           // 191
        r_hi_data,      // 190:159
        id_pc,          // 158:127
        inst,           // 126:95
        alu_op,         // 94:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        //hi_we,          
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rdata1,         // 63:32
        rdata2          // 31:0
    };


    wire br_e;
    wire [31:0] br_addr;
    wire rs_eq_rt;
    wire jump_bgez;
    wire jump_bgtz;
    wire jump_blez;
    wire jump_bltz;
    wire rs_ge_z;
    wire rs_gt_z;
    wire rs_le_z;
    wire rs_lt_z;
    wire [31:0] pc_plus_4;
    wire [31:0] rdata1_end;
    wire [31:0] rdata2_end;
    
    assign rdata1_end = ex_r_lo ? ex_r_lo_data :
                         ex_r_hi ? ex_r_hi_data :
                         rdata1;
    assign rdata2_end = ex_r_lo ? ex_r_lo_data :
                         ex_r_hi ? ex_r_hi_data :
                         rdata2;
    
    assign pc_plus_4 = id_pc + 32'h4;

    assign rs_eq_rt = (rdata1_end == rdata2_end);
    assign jump_bgez = (rdata1_end[31]==1'b0);
    assign jump_bgtz=((rdata1_end[31]==1'b0 )&&(rdata1_end!=32'b0));
    assign jump_blez = ((rdata1_end[31]==1'b1 )|(rdata1_end==32'b0));
    assign jump_bltz = (rdata1_end[31]==1'b1);
    

    assign br_e    = (inst_beq & rs_eq_rt) | inst_jal | inst_jr | inst_jalr|(inst_bne & ~rs_eq_rt) | inst_j |(inst_bgez & jump_bgez) |(inst_bgtz & jump_bgtz)|(inst_blez & jump_blez)|(inst_bltz & jump_bltz)|(inst_bltzal & jump_bltz)|(inst_bgezal & jump_bgez);
    assign br_addr = inst_beq ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}) : 
                     inst_jal ? ({pc_plus_4[31:28],instr_index,2'b0}):
                     inst_jr  ?  rdata1 : 
                     inst_jalr ? rdata1 :
                     inst_bne ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bgez ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bgtz ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_blez ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bltz ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bltzal ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_bgezal ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}):
                     inst_j ? {pc_plus_4[31:28],instr_index,2'b0} :
                     32'b0;

    assign br_bus = {
        br_e,
        br_addr
    };
    
    assign stallreq = ((ex_op_i == 6'b100011)&&(ex_rf_we == 1'b1)&&((ex_rf_waddr == rs) | (ex_rf_waddr == rt))) ? 1'b1: 
                       1'b0;

endmodule