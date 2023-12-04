
module UartModel #(
    parameter BAUD = 115200,
    parameter CLK_FREQ = 90_000_000
) (
    input  wire clk,
    input  wire rxd,
    output wire txd,
    input wire start,
    input wire[7:0] data
);

  // TXD Side
  reg txd_start = 0;
  reg [7:0] txd_data;
  wire txd_busy;

  async_transmitter #(
      .ClkFrequency(CLK_FREQ),
      .Baud        (BAUD)
  ) uart_tx (
      .clk(clk),
      .TxD(txd),

      .TxD_start(txd_start),
      .TxD_data (txd_data),
      .TxD_busy (txd_busy)
  );

  always @(posedge clk) begin
    txd_start <= start;
    txd_data <= data;
  end

  task pc_send_byte;
    input [7:0] arg;
    begin
      @(posedge clk);
      txd_data  = arg;
      txd_start = 1;
      @(posedge clk);
      txd_start = 0;
      @(txd_busy == 0);
    end
  endtask

  // RXD Side
  wire rxd_data_ready;
  reg rxd_clear = 0;
  wire [7:0] rxd_data;

  async_receiver #(
      .ClkFrequency(CLK_FREQ),
      .Baud        (BAUD)
  ) uart_rx (
      .clk(clk),
      .RxD(rxd),

      .RxD_data_ready(rxd_data_ready),
      .RxD_clear     (rxd_clear),
      .RxD_data      (rxd_data)
  );


  always begin
    wait (rxd_data_ready == 1);
    $fwrite(32'h80000002, "[%0t]: uart received 0x%02x", $time, rxd_data);
    
    if (rxd_data >= 8'h21 && rxd_data <= 8'h7E)
      $fwrite(32'h80000002, ", ASCII: %c\n", rxd_data);
    else
      $fwrite(32'h80000002, "\n");
    
    @(posedge clk);
    rxd_clear = 1;
    @(posedge clk);
    rxd_clear = 0;

    wait(rxd_data_ready == 0);
  end

endmodule
