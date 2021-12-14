`include "defines.vh"
module regfile(
    input wire clk,
    input wire [4:0] raddr1,
    output wire [31:0] rdata1,
    input wire [4:0] raddr2,
    output wire [31:0] rdata2,
    
    input wire r_hi,
    output wire [31:0] r_hi_data,
    input wire r_lo,
    output wire [31:0] r_lo_data,
    
    input wire we,
    input wire [4:0] waddr,
    input wire [31:0] wdata,
    
    input wire hi_we,
    input wire lo_we,
    input wire [31:0] hi_wdata,
    input wire [31:0] lo_wdata,
    
    input wire ex_to_id_we,
    input wire [4:0] ex_to_id_waddr,
    input wire [31:0] ex_wdata,
    
    input wire hi_ex_to_id_we,
    input wire [31:0] hi_ex_wdata,
    
    input wire lo_ex_to_id_we,
    input wire [31:0] lo_ex_wdata,
    
    input wire mem_to_id_we,
    input wire [4:0] mem_to_id_waddr,
    input wire [31:0] mem_wdata,
    
    input wire hi_mem_to_id_we,
    input wire [31:0] hi_mem_wdata,
    
    input wire lo_mem_to_id_we,
    input wire [31:0] lo_mem_wdata,
    
    input wire wb_to_id_we,
    input wire [4:0] wb_to_id_waddr,
    input wire [31:0] wb_wdata,
    
    input wire hi_wb_to_id_we,
    input wire [31:0] hi_wb_wdata,
    
    input wire lo_wb_to_id_we,
    input wire [31:0] lo_wb_wdata
);
    reg [31:0] reg_array [31:0];
    reg [31:0] hi = 32'b0;
    reg [31:0] lo = 32'b0;
    // write
    always @ (posedge clk) begin
        if (we && waddr!=5'b0) begin
            reg_array[waddr] <= wdata;
        end
    end
    
    always @ (posedge clk) begin
        if (hi_we) begin
            hi <= hi_wdata;
        end
    end
    
    always @ (posedge clk) begin
        if (lo_we) begin
            lo <= lo_wdata;
        end
    end

    // read out 1
    assign rdata1 = (raddr1 == 5'b0) ? 32'b0 : 
                    ((ex_to_id_we == 1'b1)&&(ex_to_id_waddr == raddr1)) ? ex_wdata :
                    ((mem_to_id_we == 1'b1)&&(mem_to_id_waddr == raddr1)) ? mem_wdata :
                    ((wb_to_id_we == 1'b1)&&(wb_to_id_waddr == raddr1)) ? wb_wdata :
                     reg_array[raddr1];
                     
     assign r_hi_data = (hi_ex_to_id_we == 1'b1) ? hi_ex_wdata :
                   (hi_mem_to_id_we == 1'b1) ? hi_mem_wdata :
                   (hi_wb_to_id_we == 1'b1) ? hi_wb_wdata :
                    hi;

    // read out2
    assign rdata2 = (raddr2 == 5'b0) ? 32'b0 : 
                    ((ex_to_id_we == 1'b1)&&(ex_to_id_waddr == raddr2)) ? ex_wdata : 
                    ((mem_to_id_we == 1'b1)&&(mem_to_id_waddr == raddr2)) ? mem_wdata :
                    ((wb_to_id_we == 1'b1)&&(wb_to_id_waddr == raddr2)) ? wb_wdata :
                    reg_array[raddr2];
                    
    assign r_lo_data = (lo_ex_to_id_we == 1'b1) ? lo_ex_wdata :
                  (lo_mem_to_id_we == 1'b1) ? lo_mem_wdata :
                  (lo_wb_to_id_we == 1'b1) ? lo_wb_wdata :
                   lo;
endmodule
