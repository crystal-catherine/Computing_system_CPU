`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2021/12/13 14:21:49
// Design Name: 
// Module Name: mul2
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module mul2(
  input wire clk,
  input wire rst,
  input wire mul_signed, //signed is 1, unsigned is 0
  input wire [31:0] ina,
  input wire [31:0] inb,
  input wire start_m,
  output reg [63:0] result,
  output reg ready_m
);
    wire signed [31:0] x;
    wire signed [31:0] y;
    
    assign x = ina;
    assign y = inb;
    reg[31:0] temp_op1;
	reg[31:0] temp_op2;
    //parameter s0 = 0, s1 = 1, s2 = 2;//这个可以理解成C语言里面的宏定义
    reg [5:0] count;
    reg [1:0] state = 0;//相当于初始化了，开始赋值state = 0，执行case:0语句
    reg [63:0] P, T;
    reg [31:0] y_reg;
    always @(posedge clk) begin
        case (state)//时钟模块边沿触发之后就开始执行,<=也相当于赋值（非阻塞），阻塞和非阻塞的区别就是：阻塞：你不干完不准回来；非阻塞：你先干，我看看还有其他事没有
           `MulFree: begin
                if (start_m == `MulStart) begin
                state <= `MulOn;//state被赋值为1.开始执行下一个选择语句
                count <= 0;
                if(mul_signed == 1'b1 && x[31] == 1'b1) begin			//琚櫎鏁颁负璐熸暟
					temp_op1 = ~x + 1;
				end else begin
					temp_op1 = x;
				end
				if (mul_signed == 1'b1 && y[31] == 1'b1 ) begin			//闄ゆ暟涓鸿礋鏁�
				    temp_op2 = ~y + 1;
				end else begin
					temp_op2 = y;
				end
                P <= 0;
                y_reg <= temp_op2;
                T <= {{32{1'b0}}, temp_op1};//T的高32位是0，后面的8位是x，拼接运算
                end else begin
						ready_m <= `MulResultNotReady;
						result <= {`ZeroWord, `ZeroWord};
			   end
            end
            `MulOn: begin
                if(count == 6'b100000) begin
                    state <= `MulEnd;
                    count <= 6'b000000;
                    if ((mul_signed == 1'b1) && ((x[31] ^ y[31]) == 1'b1)) begin
					P[63:0] <= (~P[63:0] + 1);
				    end
                end else begin
                    if(y_reg[0] == 1'b1)//y_reg最低位是1的话，就会按照顺序向左进位进行求解
                        P <= P + T;
                    else
                        P <= P;
                    y_reg <= y_reg >> 1;
                    T <= T << 1;
                    count <= count + 1;
                    state <= `MulOn;
                end
            end
            `MulEnd: begin
                //if ((mul_signed == 1'b1) && ((x[31] ^ y[31]) == 1'b1)) begin
					//P[63:0] <= (~P[63:0] + 1);
				//end
				result <= P;
				//if ((mul_signed == 1'b1) && ((x[31] ^ y[31]) == 1'b1)) begin
					//result[63:0] <= (~result[63:0] + 1);
				//end
                
                ready_m <= `MulResultReady;
					if (start_m == `MulStop) begin
						state <= `MulFree;
						ready_m <= `MulResultReady;
						result <= {`ZeroWord, `ZeroWord};
					end
            end
            default: ;
        endcase
    end
endmodule

