`include "lib/defines.vh"
module EX(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,
    
    output wire stallreq_for_ex,

    input wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,

    output wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,
    
    output wire [`EX_TO_ID_WD-1:0] ex_to_id_bus,

    output wire data_sram_en,
    output wire [3:0] data_sram_wen,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata
);

    reg [`ID_TO_EX_WD-1:0] id_to_ex_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
        // else if (flush) begin
        //     id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        // end
        else if (stall[2]==`Stop && stall[3]==`NoStop) begin
            id_to_ex_bus_r <= `ID_TO_EX_WD'b0;
        end
        else if (stall[2]==`NoStop) begin
            id_to_ex_bus_r <= id_to_ex_bus;
        end
    end

    wire [31:0] ex_pc, inst;
    wire [11:0] alu_op;
    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [31:0] rf_rdata1, rf_rdata2;
    wire br_e;
    wire [31:0] br_addr;
    wire r_hi;
    wire [31:0] r_hi_data;
    wire r_lo;
    wire [31:0] r_lo_data;
    reg is_in_delayslot;
    wire inst_b, inst_bu, inst_h, inst_hu, inst_sb_e, inst_sh_e, inst_lsae;

    assign {
        inst_lsae,
        inst_sh_e,
        inst_sb_e,      // 229
        inst_h,         // 228
        inst_hu,        // 227
        inst_b,         // 226
        inst_bu,        // 225
        r_lo,           // 224
        r_lo_data,      // 223:192
        r_hi,           // 191
        r_hi_data,      // 190:159
        ex_pc,          // 148:117
        inst,           // 116:85
        alu_op,         // 84:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        //hi_we,
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rf_rdata1,         // 63:32
        rf_rdata2          // 31:0
    } = id_to_ex_bus_r;

    wire [31:0] imm_sign_extend, imm_zero_extend, sa_zero_extend;
    wire [1:0] sa_2;
    assign imm_sign_extend = {{16{inst[15]}},inst[15:0]};
    assign imm_zero_extend = {16'b0, inst[15:0]};
    assign sa_zero_extend = {27'b0,inst[10:6]};
    assign sa_2 = inst[7:6];

    wire [31:0] alu_src1, alu_src2;
    wire [31:0] alu_result, ex_result;
    wire [5:0] ex_op_i;
    wire [2:0] sa_re;
    assign sa_re = sa_2 + 1'b1;
    

    assign alu_src1 = sel_alu_src1[1] ? ex_pc :
                      sel_alu_src1[2] ? sa_zero_extend : 
                      inst_lsae ? (rf_rdata1<<(sa_re)) :
                      rf_rdata1;

    assign alu_src2 = sel_alu_src2[1] ? imm_sign_extend :
                      sel_alu_src2[2] ? 32'd8 :
                      sel_alu_src2[3] ? imm_zero_extend : rf_rdata2;
                      
    
    alu u_alu(
    	.alu_control (alu_op ),
        .alu_src1    (alu_src1    ),
        .alu_src2    (alu_src2    ),
        .alu_result  (alu_result  )
    );

    
    
    assign ex_result = alu_result;
    
    assign data_sram_en = data_ram_en;
    assign data_sram_wen = (inst_sb_e && (ex_result[1:0]==2'b11) && data_ram_en) ? 4'b1000:
                            (inst_sb_e && (ex_result[1:0]==2'b10) && data_ram_en) ? 4'b0100:
                            (inst_sb_e && (ex_result[1:0]==2'b01) && data_ram_en) ? 4'b0010:
                            (inst_sb_e && (ex_result[1:0]==2'b00) && data_ram_en) ? 4'b0001:
                            
                            (inst_sh_e && (ex_result[1:0]==2'b10) && data_ram_en) ? 4'b1100:
                            (inst_sh_e && (ex_result[1:0]==2'b00) && data_ram_en) ? 4'b0011:
                            
                             data_ram_wen;
    assign data_sram_addr = ex_result;
    assign data_sram_wdata = (inst_sb_e && data_ram_en && (data_sram_wen == 4'b1000)) ? {4{rf_rdata2[7:0]}}:
                              (inst_sb_e && data_ram_en && (data_sram_wen == 4'b0100)) ? {4{rf_rdata2[7:0]}}:
                              (inst_sb_e && data_ram_en && (data_sram_wen == 4'b0010)) ? {4{rf_rdata2[7:0]}}:
                              (inst_sb_e && data_ram_en && (data_sram_wen == 4'b0001)) ? {4{rf_rdata2[7:0]}}:
                              
                              (inst_sh_e && data_ram_en && (data_sram_wen == 4'b1100)) ? {2{rf_rdata2[15:0]}}:
                              (inst_sh_e && data_ram_en && (data_sram_wen == 4'b0011)) ? {2{rf_rdata2[15:0]}}:
                              
                               rf_rdata2 ;
    
    assign ex_op_i = data_sram_en ? inst[31:26]:6'b000000;
    
    //乘除法器
    wire inst_div, inst_divu, inst_mult, inst_multu, inst_mthi, inst_mtlo;
    
    wire [5:0] opcode;
    wire [5:0] func;
    wire [63:0] op_d, func_d;
    assign opcode = inst[31:26];
    assign func = inst[5:0];
    
    decoder_6_64 u0_decoder_6_64(
    	.in  (opcode  ),
        .out (op_d )
    );

    decoder_6_64 u1_decoder_6_64(
    	.in  (func  ),
        .out (func_d )
    );
    
    assign inst_div       = op_d[6'b00_0000] && func_d[6'b01_1010];
    assign inst_divu      = op_d[6'b00_0000] && func_d[6'b01_1011];
    assign inst_mult      = op_d[6'b00_0000] && func_d[6'b01_1000];
    assign inst_multu     = op_d[6'b00_0000] && func_d[6'b01_1001];
    assign inst_mthi      = op_d[6'b00_0000] && func_d[6'b01_0001];
    assign inst_mtlo      = op_d[6'b00_0000] && func_d[6'b01_0011];
    
    // MUL&DIV part
    wire e_div;
    assign e_div = inst_div | inst_divu;
    wire [63:0] md_result;//结果
    
    wire md_ready_i;//准备信号
    reg md_ready_flag;
    reg stallreq_for_md;//乘除法暂停
    

    reg [31:0] md_opdata1_o;
    reg [31:0] md_opdata2_o;
    reg md_start_o;//开始信号
    reg signed_md_o;//有无符号
    

    div_mul u_div_mul(
        .div_e        (e_div),
        .clk          (clk          ),
        .rst          (rst          ),
        .e_signed     (signed_md_o ),
        .x            (md_opdata1_o    ),
        .y            (md_opdata2_o    ),
        .start        (md_start_o      ),
        .result       (md_result     ), // 除法结果 64bit
        .ready        (md_ready_i      )
    );
    
    
    
    always @ (*) begin
        if (rst) begin
            stallreq_for_md = `NoStop;
            md_opdata1_o = `ZeroWord;
            md_opdata2_o = `ZeroWord;
            md_start_o = `MDStop;
            signed_md_o = 1'b0;
        end
        else begin
            stallreq_for_md = `NoStop;
            md_opdata1_o = `ZeroWord;
            md_opdata2_o = `ZeroWord;
            md_start_o = `MDStop;
            signed_md_o = 1'b0;
            case ({(inst_mult | inst_div),(inst_multu | inst_divu)})
                2'b10:begin
                    if (md_ready_i == `ResultNotReady) begin
                        md_opdata1_o = rf_rdata1;
                        md_opdata2_o = rf_rdata2;
                        md_start_o = `Start;
                        signed_md_o = 1'b1;
                        stallreq_for_md = `Stop;
                    end
                    else if (md_ready_i == `ResultReady) begin
                        md_opdata1_o = rf_rdata1;
                        md_opdata2_o = rf_rdata2;
                        md_start_o = `MDStop;
                        signed_md_o = 1'b1;
                        stallreq_for_md = `NoStop;
                    end
                    else begin
                        md_opdata1_o = `ZeroWord;
                        md_opdata2_o = `ZeroWord;
                        md_start_o = `MDStop;
                        signed_md_o = 1'b0;
                        stallreq_for_md = `NoStop;
                    end
                end
                2'b01:begin
                    if (md_ready_i == `ResultNotReady) begin
                        md_opdata1_o = rf_rdata1;
                        md_opdata2_o = rf_rdata2;
                        md_start_o = `Start;
                        signed_md_o = 1'b0;
                        stallreq_for_md = `Stop;
                    end
                    else if (md_ready_i == `ResultReady) begin
                        md_opdata1_o = rf_rdata1;
                        md_opdata2_o = rf_rdata2;
                        md_start_o = `MDStop;
                        signed_md_o = 1'b0;
                        stallreq_for_md = `NoStop;
                    end
                    else begin
                        md_opdata1_o = `ZeroWord;
                        md_opdata2_o = `ZeroWord;
                        md_start_o = `MDStop;
                        signed_md_o = 1'b0;
                        stallreq_for_md = `NoStop;
                    end
                end
                default:begin
                end
            endcase
        end
    end
    
   
    assign stallreq_for_ex = stallreq_for_md;

   
    // mul_result 和 div_result 可以直接使用
    
    wire hi_ex_we;
    wire [31:0] hi_ex_wdata;
    wire lo_ex_we;
    wire [31:0] lo_ex_wdata;
    
    //assign div_result_end = div_flag ? div_result_reg : div_result;
    //assign div_result_end = div_result;
    
    assign hi_ex_wdata = (inst_div | inst_divu | inst_mult | inst_multu) ? md_result[63:32] : 
                          //(inst_mult | inst_multu) ? mul_result[63:32] : 
                           inst_mthi ? alu_src1 : 32'b0;
    assign hi_ex_we = inst_div | inst_divu | inst_mult | inst_multu | inst_mthi;
    assign lo_ex_wdata = (inst_div | inst_divu | inst_mult | inst_multu) ? md_result[31:0] : 
                          //(inst_mult | inst_multu) ? mul_result[31:0] : 
                          inst_mtlo ? alu_src1 : 32'b0;
    assign lo_ex_we = inst_div | inst_divu | inst_mult | inst_multu | inst_mtlo;
    
    
    
    assign ex_to_mem_bus = {
        inst_h,         // 211
        inst_hu,        // 210
        inst_b,         // 209
        inst_bu,        // 208
        hi_ex_we,       // 207
        hi_ex_wdata,    // 206:175
        lo_ex_we,       // 174
        lo_ex_wdata,    // 173:142
        r_lo,           // 141
        r_lo_data,      // 140:109
        r_hi,           // 108
        r_hi_data,      // 107:76
        ex_pc,          // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    };
    
    assign ex_to_id_bus = {
        r_lo,           // 141
        r_lo_data,      // 140:109
        r_hi,           // 108
        r_hi_data,      // 107:76
        hi_ex_we,       
        hi_ex_wdata,    
        lo_ex_we,       
        lo_ex_wdata,     
        rf_we,          
        rf_waddr,       
        ex_result,
        ex_op_i       
    };
    
    
    
    
endmodule