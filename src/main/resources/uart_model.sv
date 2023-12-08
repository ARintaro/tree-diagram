
module UartModel #(
    parameter BAUD = 115200,
    parameter CLK_FREQ = 90_000_000
) (
    input  wire clk,
    input wire rst,
    input  wire rxd,
    output wire txd,

    input wire start,
    input wire[7:0] data,
    output wire busy
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

  assign busy = txd_busy;

  always @(posedge clk) begin
    if (rst) begin
      txd_start <= 0;
    end else if (txd_start) begin
      $display("[uart model] sending data: 0x%x", txd_data);
    end
  end

  always @(posedge clk) begin
    txd_start <= start;
    txd_data <= data;
  end

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

  reg [1:0] state;

  always @(posedge clk) begin
    if (rst) begin
      state <= 0;
    end else if (state == 0) begin
      if (rxd_data_ready == 1) begin
        $display("[uart model] received data: 0x%x", rxd_data);
        state <= 1;
      end
    end else if (state == 1) begin
      rxd_clear <= 1;
      state <= 2;
    end else if (state == 2) begin
      rxd_clear <= 0;
      state <= 3;
    end else begin
      if (rxd_data_ready == 0) begin
        state <= 0;
      end
    end
  end


endmodule
