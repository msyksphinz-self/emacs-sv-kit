module top_block
  #(
    parameter int W = 8
  )
  (
    input  logic         i_clk,
    input  logic [W-1:0] i_data,
    output logic [W-1:0] o_data
  );

  logic [W-1:0] w_stage;

  sub_block #(.W (W)) u_first (
    .i_clk  (i_clk),
    .i_data (i_data),
    .o_data (w_stage)
  );

  sub_block #(.W (W)) u_second (
    .i_clk  (i_clk),
    .i_data (w_stage),
    .o_data (o_data)
  );

endmodule
