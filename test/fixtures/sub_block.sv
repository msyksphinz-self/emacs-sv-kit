module sub_block
  #(
    parameter int W = 8
  )
  (
    input  logic         i_clk,
    input  logic [W-1:0] i_data,
    output logic [W-1:0] o_data
  );

  assign o_data = i_data;

endmodule
