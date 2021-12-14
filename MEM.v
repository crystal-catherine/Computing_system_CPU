`include "lib/defines.vh"
module MEM(
    input wire clk,
    input wire rst,
    // input wire flush,
    input wire [`StallBus-1:0] stall,

    input wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,
    input wire [31:0] data_sram_rdata,

    output wire [`MEM_TO_WB_WD-1:0] mem_to_wb_bus,
    output wire [`MEM_TO_ID_WD-1:0] mem_to_id_bus
);

    reg [`EX_TO_MEM_WD-1:0] ex_to_mem_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        // else if (flush) begin
        //     ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        // end
        else if (stall[3]==`Stop && stall[4]==`NoStop) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        else if (stall[3]==`NoStop) begin
            ex_to_mem_bus_r <= ex_to_mem_bus;
        end
    end

    wire [31:0] mem_pc;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire sel_rf_res;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire [31:0] rf_wdata;
    wire [31:0] ex_result;
    wire [31:0] mem_result;
    wire r_hi;
    wire [31:0] r_hi_data;
    wire r_lo;
    wire [31:0] r_lo_data;
    wire hi_ex_we;
    wire [31:0] hi_ex_wdata;
    wire lo_ex_we;
    wire [31:0] lo_ex_wdata;
    wire inst_b, inst_bu;
    

    assign {
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
        mem_pc,         // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    } =  ex_to_mem_bus_r;

    
    assign rf_wdata = ((ex_result[1:0]==2'b11) && data_ram_en && inst_b) ? {{24{data_sram_rdata[31]}},data_sram_rdata[31:24]}:
                       ((ex_result[1:0]==2'b10) && data_ram_en && inst_b) ? {{24{data_sram_rdata[23]}},data_sram_rdata[23:16]}:
                       ((ex_result[1:0]==2'b01) && data_ram_en && inst_b) ? {{24{data_sram_rdata[15]}},data_sram_rdata[15:8]}:
                       ((ex_result[1:0]==2'b00) && data_ram_en && inst_b) ? {{24{data_sram_rdata[7]}},data_sram_rdata[7:0]}:
                       
                       ((ex_result[1:0]==2'b11) && data_ram_en && inst_bu) ? {24'b0,data_sram_rdata[31:24]}:
                       ((ex_result[1:0]==2'b10) && data_ram_en && inst_bu) ? {24'b0,data_sram_rdata[23:16]}:
                       ((ex_result[1:0]==2'b01) && data_ram_en && inst_bu) ? {24'b0,data_sram_rdata[15:8]}:
                       ((ex_result[1:0]==2'b00) && data_ram_en && inst_bu) ? {24'b0,data_sram_rdata[7:0]}:
                      (data_ram_en & (data_ram_wen == 4'b0000)) ? data_sram_rdata:
                      sel_rf_res ? mem_result : 
                      r_hi ? r_hi_data :
                      r_lo ? r_lo_data :
                      //(r_lo_data!=32'b0) ? r_lo_data :
                      ex_result;

    assign mem_to_wb_bus = {
        hi_ex_we,       // 107
        hi_ex_wdata,    // 106:75
        lo_ex_we,       // 74
        lo_ex_wdata,    // 73:42
        mem_pc,     // 41:38
        rf_we,      // 37
        rf_waddr,   // 36:32
        rf_wdata    // 31:0
    };
    
    
    assign mem_to_id_bus = {
        hi_ex_we,       
        hi_ex_wdata,    
        lo_ex_we,       
        lo_ex_wdata,    
        rf_we,      
        rf_waddr,   
        rf_wdata    
    };




endmodule