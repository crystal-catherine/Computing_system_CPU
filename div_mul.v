`include "defines.vh"
`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2021/12/13 19:49:17
// Design Name: 
// Module Name: div_mul
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


module div_mul(
  input wire div_e,//1是div,0是mul
  input wire clk,
  input wire rst,
  input wire e_signed, //signed is 1, unsigned is 0
  input wire [31:0] x,
  input wire [31:0] y,
  input wire start,
  output reg [63:0] result,
  output reg ready
);

    reg[31:0] temp_op1;
	reg[31:0] temp_op2;
    reg [5:0] count;
    reg [1:0] state = 0;//相当于初始化了，开始赋值state = 0，执行case:0语句
    reg [63:0] m_end, m_temp;
    reg [31:0] y_reg;
    
    wire [32:0] div_temp;
	reg[64:0] dividend;						
	//reg[31:0] divisor;
	
	assign div_temp = {1'b0, dividend[63: 32]} - {1'b0, y_reg};
    
    always @(posedge clk) begin
        if (rst) begin
			state <= `Free;
			result <= {`ZeroWord,`ZeroWord};
			ready <= `ResultNotReady;
		end else begin
         case (state)
           `Free: begin//初始化状态
                if (start == `Start) begin
                  if(div_e && y == `ZeroWord) begin
					 state <= `DivByZero;//被除数是零
				  end else begin
				     state <= `On;
                     count <= 6'b000000;
                     if(e_signed == 1'b1 && x[31] == 1'b1) begin			
					    temp_op1 = ~x + 1;
				     end else begin
					    temp_op1 = x;
				     end
				     if (e_signed == 1'b1 && y[31] == 1'b1 ) begin			
				         temp_op2 = ~y + 1;
				     end else begin
					     temp_op2 = y;
				     end
                     m_end <= {`ZeroWord, `ZeroWord};
                     dividend <= {`ZeroWord, `ZeroWord};
					 dividend[32: 1] <= temp_op1;
					 //divisor <= temp_op2;
                     y_reg <= temp_op2;
                     m_temp <= {{32{1'b0}}, temp_op1};//temp的高32位是0，后面的32位是x，拼接运算
					 end
                end else begin
					ready <= `ResultNotReady;
					result <= {`ZeroWord, `ZeroWord};
			   end
            end
            
            `DivByZero: begin			//被除数为零状态
					dividend <= {`ZeroWord, `ZeroWord};
					state <= `End;
		    end
		    
            `On: begin//运算状态
                if(count == 6'b100000) begin//运算结束取反
                    
                    if ((e_signed == 1'b1) && ((x[31] ^ y[31]) == 1'b1))
                        if(div_e) dividend[31:0] <= (~dividend[31:0] + 1);//除法
                        
                        else m_end[63:0] <= (~m_end[63:0] + 1);//乘法
                        
                    if(div_e && (e_signed == 1'b1) && ((x[31] ^ dividend[64]) == 1'b1))
                        dividend[64:33] <= (~dividend[64:33] + 1);//除法
                    state <= `End;
                    count <= 6'b000000;    
                        
                        
                end else begin//运算开始
                  if(div_e) begin//除法
                    if (div_temp[32] == 1'b1) begin
						dividend <= {dividend[63:0],1'b0};
					end else begin
						dividend <= {div_temp[31:0],dividend[31:0], 1'b1};
					end
				  end else begin
					//乘法
                    if(y_reg[0] == 1'b1)//y_reg最低位是1的话，就会按照顺序向左进位进行求解
                        m_end <= m_end + m_temp;
                    else
                        m_end <= m_end;
                    y_reg <= y_reg >> 1;
                    m_temp <= m_temp << 1;
                  end
                    
                  count <= count + 1;
                  state <= `On;
                  
                end
            end
            
            `End: begin//结束状态
                if(div_e) begin//除法
                   result <= {dividend[64:33], dividend[31:0]};
                end else result <= m_end;//乘法
                ready <= `ResultReady;
					if (start == `MDStop) begin
						state <= `Free;
						ready <= `ResultNotReady;
						result <= {`ZeroWord, `ZeroWord};
					end
            end
            default: ;
        endcase
    end
   end
endmodule

