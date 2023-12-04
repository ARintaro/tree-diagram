
module UartControllerBlackBox #(
    parameter ADDR_WIDTH = 32,
    parameter DATA_WIDTH = 32,

    parameter CLK_FREQ = 80_000_000,
    parameter BAUD = 115200
) (
    // clk and reset
    input wire clk_i,
    input wire rst_i,

    // wishbone slave interface
    input wire wb_cyc_i,
    input wire wb_stb_i,
    output reg wb_ack_o,
    input wire [ADDR_WIDTH-1:0] wb_adr_i,
    input wire [DATA_WIDTH-1:0] wb_dat_i,
    output reg [DATA_WIDTH-1:0] wb_dat_o,
    input wire [DATA_WIDTH/8-1:0] wb_sel_i,
    input wire wb_we_i,

    // uart interface
    output reg uart_txd_o,
    input  wire uart_rxd_i
);

  localparam REG_DATA = 8'h00;
  localparam REG_STATUS = 8'h04;

  // uart transmitter
  logic txd_start;
  logic txd_busy;
  logic [7:0] txd_data;

  async_transmitter #(
      .ClkFrequency(CLK_FREQ),
      .Baud        (BAUD)
  ) u_async_transmitter (
      .clk      (clk_i),
      .TxD_start(txd_start),
      .TxD_data (txd_data),
      .TxD      (uart_txd_o),
      .TxD_busy (txd_busy)
  );

  // uart receiver
  logic rxd_data_ready;
  logic [7:0] rxd_data;
  logic rxd_clear;

  async_receiver #(
      .ClkFrequency(CLK_FREQ),
      .Baud        (BAUD)
  ) u_async_receiver (
      .clk           (clk_i),
      .RxD           (uart_rxd_i),
      .RxD_data_ready(rxd_data_ready),
      .RxD_clear     (rxd_clear),
      .RxD_data      (rxd_data)
  );

  /*-- internal registers --*/
  wire [7:0] reg_status = {2'b0, ~txd_busy, 4'b0, rxd_data_ready};

  /*-- wishbone fsm --*/
  always_ff @(posedge clk_i) begin
    if (rst_i)
      wb_ack_o <= 0;
    else
      // every request get ACK-ed immediately
      if (wb_ack_o) begin
        wb_ack_o <= 0;
      end else begin
        wb_ack_o <= wb_stb_i;
      end

      if (rxd_data_ready) begin
        // $fwrite(32'h80000002, "[%0t]: uart data ready!! 0x%02x\n", $time, rxd_data);
      end

  end

  // write logic
  always_ff @(posedge clk_i) begin
    if (rst_i) begin
      txd_start <= 0;
    end else if(wb_stb_i && wb_we_i) begin
      case (wb_adr_i[7:0])
        REG_DATA: begin
          if(wb_sel_i[0]) begin
            $fwrite(32'h80000002, "[%0t]: uart writed data!! 0x%02x\n", $time, wb_dat_i[7:0]);
            txd_data  <= wb_dat_i[7:0];
            txd_start <= 1;
          end
        end

        default: ;  // do nothing
      endcase
    end else begin
      txd_start <= 0;
    end
  end

  // read logic
  always_ff @(posedge clk_i) begin
    if(rst_i) begin
      rxd_clear <= 1;  // clear rxd to initialize dataready
    end else if(wb_stb_i && !wb_we_i) begin
      case (wb_adr_i[7:0])
        REG_DATA: begin
          $fwrite(32'h80000002, "[%0t]: uart data readed 0x%02x\n", $time, rxd_data);
          if (wb_sel_i[0]) wb_dat_o[7:0] <= rxd_data;
          if (wb_sel_i[1]) wb_dat_o[15:8] <= rxd_data;
          if (wb_sel_i[2]) wb_dat_o[23:16] <= rxd_data;
          if (wb_sel_i[3]) wb_dat_o[31:24] <= rxd_data;

          rxd_clear <= 1;
        end

        REG_STATUS: begin
          if (wb_sel_i[0]) wb_dat_o[7:0] <= reg_status;
          if (wb_sel_i[1]) wb_dat_o[15:8] <= reg_status;
          if (wb_sel_i[2]) wb_dat_o[23:16] <= reg_status;
          if (wb_sel_i[3]) wb_dat_o[31:24] <= reg_status;
        end

        default: ;  // do nothing
      endcase
    end else begin
      rxd_clear <= 0;
    end
  end

endmodule



module async_transmitter(
	input wire clk,
	input wire TxD_start,
	input wire [7:0] TxD_data,
	output wire TxD,
	output wire TxD_busy
);

// Assert TxD_start for (at least) one clock cycle to start transmission of TxD_data
// TxD_data is latched so that it doesn't have to stay valid while it is being sent

parameter ClkFrequency = 25000000;	// 25MHz
parameter Baud = 115200;

// generate
// 	if(ClkFrequency<Baud*8 && (ClkFrequency % Baud!=0)) ASSERTION_ERROR PARAMETER_OUT_OF_RANGE("Frequency incompatible with requested Baud rate");
// endgenerate

////////////////////////////////
`ifdef SIMULATION
wire BitTick = 1'b1;  // output one bit per clock cycle
`else
wire BitTick;
BaudTickGen #(ClkFrequency, Baud) tickgen(.clk(clk), .enable(TxD_busy), .tick(BitTick));
`endif

reg [3:0] TxD_state = 0;
wire TxD_ready = (TxD_state==0);
assign TxD_busy = ~TxD_ready;

reg [7:0] TxD_shift = 0;
always @(posedge clk)
begin
	if(TxD_ready & TxD_start)
		TxD_shift <= TxD_data;
	else
	if(TxD_state[3] & BitTick)
		TxD_shift <= (TxD_shift >> 1);

	case(TxD_state)
		4'b0000: if(TxD_start) TxD_state <= 4'b0100;
		4'b0100: if(BitTick) TxD_state <= 4'b1000;  // start bit
		4'b1000: if(BitTick) TxD_state <= 4'b1001;  // bit 0
		4'b1001: if(BitTick) TxD_state <= 4'b1010;  // bit 1
		4'b1010: if(BitTick) TxD_state <= 4'b1011;  // bit 2
		4'b1011: if(BitTick) TxD_state <= 4'b1100;  // bit 3
		4'b1100: if(BitTick) TxD_state <= 4'b1101;  // bit 4
		4'b1101: if(BitTick) TxD_state <= 4'b1110;  // bit 5
		4'b1110: if(BitTick) TxD_state <= 4'b1111;  // bit 6
		4'b1111: if(BitTick) TxD_state <= 4'b0010;  // bit 7
		4'b0010: if(BitTick) TxD_state <= 4'b0000;  // stop1
		//4'b0011: if(BitTick) TxD_state <= 4'b0000;  // stop2
		default: if(BitTick) TxD_state <= 4'b0000;
	endcase
end

assign TxD = (TxD_state<4) | (TxD_state[3] & TxD_shift[0]);  // put together the start, data and stop bits
endmodule


////////////////////////////////////////////////////////
module async_receiver(
	input wire clk,
	input wire RxD,
	output reg RxD_data_ready,
	input wire RxD_clear,
	output reg [7:0] RxD_data  // data received, valid only (for one clock cycle) when RxD_data_ready is asserted
);

parameter ClkFrequency = 25000000; // 25MHz
parameter Baud = 115200;

parameter Oversampling = 8;  // needs to be a power of 2
// we oversample the RxD line at a fixed rate to capture each RxD data bit at the "right" time
// 8 times oversampling by default, use 16 for higher quality reception

// generate
// 	if(ClkFrequency<Baud*Oversampling) ASSERTION_ERROR PARAMETER_OUT_OF_RANGE("Frequency too low for current Baud rate and oversampling");
// 	if(Oversampling<8 || ((Oversampling & (Oversampling-1))!=0)) ASSERTION_ERROR PARAMETER_OUT_OF_RANGE("Invalid oversampling value");
// endgenerate

////////////////////////////////

// We also detect if a gap occurs in the received stream of characters
// That can be useful if multiple characters are sent in burst
//  so that multiple characters can be treated as a "packet"
wire RxD_idle;  // asserted when no data has been received for a while
reg RxD_endofpacket; // asserted for one clock cycle when a packet has been detected (i.e. RxD_idle is going high)


reg [3:0] RxD_state = 0;

`ifdef SIMULATION
wire RxD_bit = RxD;
wire sampleNow = 1'b1;  // receive one bit per clock cycle

`else
wire OversamplingTick;
BaudTickGen #(ClkFrequency, Baud, Oversampling) tickgen(.clk(clk), .enable(1'b1), .tick(OversamplingTick));

// synchronize RxD to our clk domain
reg [1:0] RxD_sync = 2'b11;
always @(posedge clk) if(OversamplingTick) RxD_sync <= {RxD_sync[0], RxD};

// and filter it
reg [1:0] Filter_cnt = 2'b11;
reg RxD_bit = 1'b1;

always @(posedge clk)
if(OversamplingTick)
begin
	if(RxD_sync[1]==1'b1 && Filter_cnt!=2'b11) Filter_cnt <= Filter_cnt + 1'd1;
	else 
	if(RxD_sync[1]==1'b0 && Filter_cnt!=2'b00) Filter_cnt <= Filter_cnt - 1'd1;

	if(Filter_cnt==2'b11) RxD_bit <= 1'b1;
	else
	if(Filter_cnt==2'b00) RxD_bit <= 1'b0;
end

// and decide when is the good time to sample the RxD line
function integer log2(input integer v); begin log2=0; while(v>>log2) log2=log2+1; end endfunction
localparam l2o = log2(Oversampling);
reg [l2o-2:0] OversamplingCnt = 0;
always @(posedge clk) if(OversamplingTick) OversamplingCnt <= (RxD_state==0) ? 1'd0 : OversamplingCnt + 1'd1;
wire sampleNow = OversamplingTick && (OversamplingCnt==Oversampling/2-1);
`endif

// now we can accumulate the RxD bits in a shift-register
always @(posedge clk)
case(RxD_state)
	4'b0000: if(~RxD_bit) RxD_state <= `ifdef SIMULATION 4'b1000 `else 4'b0001 `endif;  // start bit found?
	4'b0001: if(sampleNow) RxD_state <= 4'b1000;  // sync start bit to sampleNow
	4'b1000: if(sampleNow) RxD_state <= 4'b1001;  // bit 0
	4'b1001: if(sampleNow) RxD_state <= 4'b1010;  // bit 1
	4'b1010: if(sampleNow) RxD_state <= 4'b1011;  // bit 2
	4'b1011: if(sampleNow) RxD_state <= 4'b1100;  // bit 3
	4'b1100: if(sampleNow) RxD_state <= 4'b1101;  // bit 4
	4'b1101: if(sampleNow) RxD_state <= 4'b1110;  // bit 5
	4'b1110: if(sampleNow) RxD_state <= 4'b1111;  // bit 6
	4'b1111: if(sampleNow) RxD_state <= 4'b0010;  // bit 7
	4'b0010: if(sampleNow) RxD_state <= 4'b0000;  // stop bit
	default: RxD_state <= 4'b0000;
endcase

always @(posedge clk)
if(sampleNow && RxD_state[3]) RxD_data <= {RxD_bit, RxD_data[7:1]};

//reg RxD_data_error = 0;
always @(posedge clk)
begin
	if(RxD_clear)
		RxD_data_ready <= 0;
	else
		RxD_data_ready <= RxD_data_ready | (sampleNow && RxD_state==4'b0010 && RxD_bit);  // make sure a stop bit is received
	//RxD_data_error <= (sampleNow && RxD_state==4'b0010 && ~RxD_bit);  // error if a stop bit is not received
end

`ifdef SIMULATION
assign RxD_idle = 0;
`else
reg [l2o+1:0] GapCnt = 0;
always @(posedge clk) if (RxD_state!=0) GapCnt<=0; else if(OversamplingTick & ~GapCnt[log2(Oversampling)+1]) GapCnt <= GapCnt + 1'h1;
assign RxD_idle = GapCnt[l2o+1];
always @(posedge clk) RxD_endofpacket <= OversamplingTick & ~GapCnt[l2o+1] & &GapCnt[l2o:0];
`endif

endmodule


////////////////////////////////////////////////////////
// dummy module used to be able to raise an assertion in Verilog
//module ASSERTION_ERROR();
//endmodule


////////////////////////////////////////////////////////
module BaudTickGen(
	input  wire clk, enable,
	output wire tick  // generate a tick at the specified baud rate * oversampling
);
parameter ClkFrequency = 25000000;
parameter Baud = 115200;
parameter Oversampling = 1;

function integer log2(input integer v); begin log2=0; while(v>>log2) log2=log2+1; end endfunction
localparam AccWidth = log2(ClkFrequency/Baud)+8;  // +/- 2% max timing error over a byte
reg [AccWidth:0] Acc = 0;
localparam ShiftLimiter = log2(Baud*Oversampling >> (31-AccWidth));  // this makes sure Inc calculation doesn't overflow
localparam Inc = ((Baud*Oversampling << (AccWidth-ShiftLimiter))+(ClkFrequency>>(ShiftLimiter+1)))/(ClkFrequency>>ShiftLimiter);
always @(posedge clk) if(enable) Acc <= Acc[AccWidth-1:0] + Inc[AccWidth:0]; else Acc <= Inc[AccWidth:0];
assign tick = Acc[AccWidth];
endmodule


////////////////////////////////////////////////////////
