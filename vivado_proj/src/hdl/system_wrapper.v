`timescale 1 ps / 1 ps
`include "axi.vh"

module system_wrapper(
  // input wire UART_rxd,
  input wire sys_clk_clk_p,
  input wire sys_clk_clk_n,
  output wire UART_txd,
  output wire [0:0] led,
  output wire mdio_mdc,
  inout wire mdio_mdio_io,
  output wire phy_reset_n,
  input wire [3:0]rgmii_rd,
  input wire rgmii_rx_ctl,
  input wire rgmii_rxc,
  output wire [3:0]rgmii_td,
  output wire rgmii_tx_ctl,
  output wire rgmii_txc
);

  // XXV_DMA_S_AXI
  `axi_wire(DMA_AXI, 64, 16);
  
  // MEM_AXI  
  wire        M_AXI_awready;
  wire        M_AXI_awvalid;
  wire [5:0]  M_AXI_awid;
  wire [63:0] M_AXI_awaddr;
  wire [7:0]  M_AXI_awlen;
  wire [2:0]  M_AXI_awsize;
  wire [1:0]  M_AXI_awburst;
  wire        M_AXI_awlock;
  wire [3:0]  M_AXI_awcache;
  wire [2:0]  M_AXI_awprot;
  wire [3:0]  M_AXI_awqos;
  wire        M_AXI_wready;
  wire        M_AXI_wvalid;
  wire [127:0] M_AXI_wdata;
  wire [7:0]  M_AXI_wstrb;
  wire        M_AXI_wlast;
  wire        M_AXI_bready;
  wire        M_AXI_bvalid;
  wire [5:0]  M_AXI_bid;
  wire [1:0]  M_AXI_bresp;
  wire        M_AXI_arready;
  wire        M_AXI_arvalid;
  wire [5:0]  M_AXI_arid;
  wire [63:0] M_AXI_araddr;
  wire [7:0]  M_AXI_arlen;
  wire [2:0]  M_AXI_arsize;
  wire [1:0]  M_AXI_arburst;
  wire        M_AXI_arlock;
  wire [3:0]  M_AXI_arcache;
  wire [2:0]  M_AXI_arprot;
  wire [3:0]  M_AXI_arqos;
  wire        M_AXI_rready;
  wire        M_AXI_rvalid;
  wire [5:0]  M_AXI_rid;
  wire [127:0] M_AXI_rdata;
  wire [1:0]  M_AXI_rresp;
  wire        M_AXI_rlast;

  // MMIO_AXI
  wire        M_MMIO_AXI_awready;
  wire        M_MMIO_AXI_awvalid;
  wire [3:0]  M_MMIO_AXI_awid;
  wire [31:0] M_MMIO_AXI_awaddr;
  wire [7:0]  M_MMIO_AXI_awlen;
  wire [2:0]  M_MMIO_AXI_awsize;
  wire [1:0]  M_MMIO_AXI_awburst;
  wire        M_MMIO_AXI_awlock;
  wire [3:0]  M_MMIO_AXI_awcache;
  wire [2:0]  M_MMIO_AXI_awprot;
  wire [3:0]  M_MMIO_AXI_awqos;
  wire        M_MMIO_AXI_wready;
  wire        M_MMIO_AXI_wvalid;
  wire [63:0] M_MMIO_AXI_wdata;
  wire [3:0]  M_MMIO_AXI_wstrb;
  wire        M_MMIO_AXI_wlast;
  wire        M_MMIO_AXI_bready;
  wire        M_MMIO_AXI_bvalid;
  wire [3:0]  M_MMIO_AXI_bid;
  wire [1:0]  M_MMIO_AXI_bresp;
  wire        M_MMIO_AXI_arready;
  wire        M_MMIO_AXI_arvalid;
  wire [3:0]  M_MMIO_AXI_arid;
  wire [31:0] M_MMIO_AXI_araddr;
  wire [7:0]  M_MMIO_AXI_arlen;
  wire [2:0]  M_MMIO_AXI_arsize;
  wire [1:0]  M_MMIO_AXI_arburst;
  wire        M_MMIO_AXI_arlock;
  wire [3:0]  M_MMIO_AXI_arcache;
  wire [2:0]  M_MMIO_AXI_arprot;
  wire [3:0]  M_MMIO_AXI_arqos;
  wire        M_MMIO_AXI_rready;
  wire        M_MMIO_AXI_rvalid;
  wire [3:0]  M_MMIO_AXI_rid;
  wire [63:0] M_MMIO_AXI_rdata;
  wire [1:0]  M_MMIO_AXI_rresp;
  wire        M_MMIO_AXI_rlast;

  wire [31:0] mem_awaddr;
  wire [31:0] mem_araddr;
  assign M_AXI_awaddr = {32'd0, 4'd1, mem_awaddr[27:0]};
  assign M_AXI_araddr = {32'd0, 4'd1, mem_araddr[27:0]};

  wire [30:0] mmio_awaddr;
  wire [30:0] mmio_araddr;
  assign M_MMIO_AXI_awaddr = {1'b0, mmio_awaddr};
  assign M_MMIO_AXI_araddr = {1'b0, mmio_araddr};

  wire reset, sys_reset, clock;
  wire [5:0] ext_intrs;
  wire mdio_mdio_i;
  wire mdio_mdio_o;
  wire mdio_mdio_t;
  IOBUF mdio_mdio_iobuf
       (.I(mdio_mdio_o),
        .IO(mdio_mdio_io),
        .O(mdio_mdio_i),
        .T(mdio_mdio_t));

  soc system(
    .clock              (clock              ),
    .reset              (reset              ),
    .sys_reset          (sys_reset          ),
    .ext_intrs          (ext_intrs          ),
    .led                (led                ),

    .UART_txd           (UART_txd           ),
    // .UART_rxd           (UART_rxd           ),
    
    `axi_connect_if(M_DMA_AXI, DMA_AXI),
    
    // ethernet
    .sys_clk_clk_p(sys_clk_clk_p),
    .sys_clk_clk_n(sys_clk_clk_n),
    .mdio_mdc(mdio_mdc),
    .mdio_mdio_i(mdio_mdio_i),
    .mdio_mdio_o(mdio_mdio_o),
    .mdio_mdio_t(mdio_mdio_t),
    .phy_reset_n(phy_reset_n),
    .rgmii_rd(rgmii_rd),
    .rgmii_rx_ctl(rgmii_rx_ctl),
    .rgmii_rxc(rgmii_rxc),
    .rgmii_td(rgmii_td),
    .rgmii_tx_ctl(rgmii_tx_ctl),
    .rgmii_txc(rgmii_txc),

    // slave AXI interface (fpga = master, zynq = slave) connected directly to DDR controller
    .S_AXI_awready      (M_AXI_awready      ),
    .S_AXI_awvalid      (M_AXI_awvalid      ),
    .S_AXI_awid         (M_AXI_awid         ),
    .S_AXI_awaddr       (M_AXI_awaddr[48:0] ),
    .S_AXI_awlen        (M_AXI_awlen        ),
    .S_AXI_awsize       (M_AXI_awsize       ),
    .S_AXI_awburst      (M_AXI_awburst      ),
    .S_AXI_awlock       (M_AXI_awlock       ),
    .S_AXI_awcache      (M_AXI_awcache      ),
    .S_AXI_awprot       (M_AXI_awprot       ),
    .S_AXI_awqos        (M_AXI_awqos        ),
    .S_AXI_awuser       (),
    .S_AXI_wready       (M_AXI_wready       ),
    .S_AXI_wvalid       (M_AXI_wvalid       ),
    .S_AXI_wdata        (M_AXI_wdata        ),
    .S_AXI_wstrb        (M_AXI_wstrb        ),
    .S_AXI_wlast        (M_AXI_wlast        ),
    .S_AXI_bready       (M_AXI_bready       ),
    .S_AXI_bvalid       (M_AXI_bvalid       ),
    .S_AXI_bid          (M_AXI_bid          ),
    .S_AXI_bresp        (M_AXI_bresp        ),
    .S_AXI_arready      (M_AXI_arready      ),
    .S_AXI_arvalid      (M_AXI_arvalid      ),
    .S_AXI_arid         (M_AXI_arid         ),
    .S_AXI_araddr       (M_AXI_araddr[48:0] ),
    .S_AXI_arlen        (M_AXI_arlen        ),
    .S_AXI_arsize       (M_AXI_arsize       ),
    .S_AXI_arburst      (M_AXI_arburst      ),
    .S_AXI_arlock       (M_AXI_arlock       ),
    .S_AXI_arcache      (M_AXI_arcache      ),
    .S_AXI_arprot       (M_AXI_arprot       ),
    .S_AXI_arqos        (M_AXI_arqos        ),
    .S_AXI_aruser       (),
    .S_AXI_rready       (M_AXI_rready       ),
    .S_AXI_rvalid       (M_AXI_rvalid       ),
    .S_AXI_rid          (M_AXI_rid          ),
    .S_AXI_rdata        (M_AXI_rdata        ),
    .S_AXI_rresp        (M_AXI_rresp        ),
    .S_AXI_rlast        (M_AXI_rlast        ),
    
    // slave AXI interface (fpga = master, zynq = slave) connected directly to UART
    .S_MMIO_AXI_awready (M_MMIO_AXI_awready ),
    .S_MMIO_AXI_awvalid (M_MMIO_AXI_awvalid ),
    .S_MMIO_AXI_awid    (M_MMIO_AXI_awid    ),
    .S_MMIO_AXI_awaddr  (M_MMIO_AXI_awaddr  ),
    .S_MMIO_AXI_awlen   (M_MMIO_AXI_awlen   ),
    .S_MMIO_AXI_awsize  (M_MMIO_AXI_awsize  ),
    .S_MMIO_AXI_awburst (M_MMIO_AXI_awburst ),
    .S_MMIO_AXI_awlock  (M_MMIO_AXI_awlock  ),
    .S_MMIO_AXI_awcache (M_MMIO_AXI_awcache ),
    .S_MMIO_AXI_awprot  (M_MMIO_AXI_awprot  ),
    .S_MMIO_AXI_awqos   (M_MMIO_AXI_awqos   ),
    .S_MMIO_AXI_awregion(),
    .S_MMIO_AXI_wready  (M_MMIO_AXI_wready  ),
    .S_MMIO_AXI_wvalid  (M_MMIO_AXI_wvalid  ),
    .S_MMIO_AXI_wdata   (M_MMIO_AXI_wdata   ),
    .S_MMIO_AXI_wstrb   (M_MMIO_AXI_wstrb   ),
    .S_MMIO_AXI_wlast   (M_MMIO_AXI_wlast   ),
    .S_MMIO_AXI_bready  (M_MMIO_AXI_bready  ),
    .S_MMIO_AXI_bvalid  (M_MMIO_AXI_bvalid  ),
    .S_MMIO_AXI_bid     (M_MMIO_AXI_bid     ),
    .S_MMIO_AXI_bresp   (M_MMIO_AXI_bresp   ),
    .S_MMIO_AXI_arready (M_MMIO_AXI_arready ),
    .S_MMIO_AXI_arvalid (M_MMIO_AXI_arvalid ),
    .S_MMIO_AXI_arid    (M_MMIO_AXI_arid    ), 
    .S_MMIO_AXI_araddr  (M_MMIO_AXI_araddr  ),
    .S_MMIO_AXI_arlen   (M_MMIO_AXI_arlen   ),
    .S_MMIO_AXI_arsize  (M_MMIO_AXI_arsize  ),
    .S_MMIO_AXI_arburst (M_MMIO_AXI_arburst ),
    .S_MMIO_AXI_arlock  (M_MMIO_AXI_arlock  ),
    .S_MMIO_AXI_arcache (M_MMIO_AXI_arcache ),
    .S_MMIO_AXI_arprot  (M_MMIO_AXI_arprot  ),
    .S_MMIO_AXI_arqos   (M_MMIO_AXI_arqos   ),
    .S_MMIO_AXI_arregion(),
    .S_MMIO_AXI_rready  (M_MMIO_AXI_rready  ),
    .S_MMIO_AXI_rvalid  (M_MMIO_AXI_rvalid  ),
    .S_MMIO_AXI_rid     (M_MMIO_AXI_rid     ),
    .S_MMIO_AXI_rdata   (M_MMIO_AXI_rdata   ),
    .S_MMIO_AXI_rresp   (M_MMIO_AXI_rresp   ),
    .S_MMIO_AXI_rlast   (M_MMIO_AXI_rlast   )
  );

  Top top(
    .clock                        (clock              ),
    .reset                        (reset              ),
    .io_sys_reset                 (sys_reset          ),
    .io_ext_intrs                 (ext_intrs          ),

    // MEM
    .io_mem_axi4_aw_ready         (M_AXI_awready      ),
    .io_mem_axi4_aw_valid         (M_AXI_awvalid      ),
    .io_mem_axi4_aw_bits_id       (M_AXI_awid[3:0]    ),
    .io_mem_axi4_aw_bits_addr     (mem_awaddr         ),
    .io_mem_axi4_aw_bits_len      (M_AXI_awlen        ),
    .io_mem_axi4_aw_bits_size     (M_AXI_awsize       ),
    .io_mem_axi4_aw_bits_burst    (M_AXI_awburst      ),
    .io_mem_axi4_aw_bits_lock     (M_AXI_awlock       ),
    .io_mem_axi4_aw_bits_cache    (M_AXI_awcache      ),
    .io_mem_axi4_aw_bits_prot     (M_AXI_awprot       ),
    .io_mem_axi4_aw_bits_qos      (M_AXI_awqos        ),
    .io_mem_axi4_w_ready          (M_AXI_wready       ),
    .io_mem_axi4_w_valid          (M_AXI_wvalid       ),
    .io_mem_axi4_w_bits_data      (M_AXI_wdata[63:0]  ),
    .io_mem_axi4_w_bits_strb      (M_AXI_wstrb        ),
    .io_mem_axi4_w_bits_last      (M_AXI_wlast        ),
    .io_mem_axi4_b_ready          (M_AXI_bready       ),
    .io_mem_axi4_b_valid          (M_AXI_bvalid       ),
    .io_mem_axi4_b_bits_id        (M_AXI_bid[3:0]     ),
    .io_mem_axi4_b_bits_resp      (M_AXI_bresp        ),
    .io_mem_axi4_ar_ready         (M_AXI_arready      ),
    .io_mem_axi4_ar_valid         (M_AXI_arvalid      ),
    .io_mem_axi4_ar_bits_id       (M_AXI_arid[3:0]    ),
    .io_mem_axi4_ar_bits_addr     (mem_araddr         ),
    .io_mem_axi4_ar_bits_len      (M_AXI_arlen        ),
    .io_mem_axi4_ar_bits_size     (M_AXI_arsize       ),
    .io_mem_axi4_ar_bits_burst    (M_AXI_arburst      ),
    .io_mem_axi4_ar_bits_lock     (M_AXI_arlock       ),
    .io_mem_axi4_ar_bits_cache    (M_AXI_arcache      ),
    .io_mem_axi4_ar_bits_prot     (M_AXI_arprot       ),
    .io_mem_axi4_ar_bits_qos      (M_AXI_arqos        ),
    .io_mem_axi4_r_ready          (M_AXI_rready       ),
    .io_mem_axi4_r_valid          (M_AXI_rvalid       ),
    .io_mem_axi4_r_bits_id        (M_AXI_rid[3:0]     ),
    .io_mem_axi4_r_bits_data      (M_AXI_rdata[63:0]  ),
    .io_mem_axi4_r_bits_resp      (M_AXI_rresp        ),
    .io_mem_axi4_r_bits_last      (M_AXI_rlast        ),

    // MMIO
    .io_mmio_axi4_aw_ready        (M_MMIO_AXI_awready ),
    .io_mmio_axi4_aw_valid        (M_MMIO_AXI_awvalid ),
    .io_mmio_axi4_aw_bits_id      (M_MMIO_AXI_awid    ),
    .io_mmio_axi4_aw_bits_addr    (mmio_awaddr        ),
    .io_mmio_axi4_aw_bits_len     (M_MMIO_AXI_awlen   ),
    .io_mmio_axi4_aw_bits_size    (M_MMIO_AXI_awsize  ),
    .io_mmio_axi4_aw_bits_burst   (M_MMIO_AXI_awburst ),
    .io_mmio_axi4_aw_bits_lock    (M_MMIO_AXI_awlock  ),
    .io_mmio_axi4_aw_bits_cache   (M_MMIO_AXI_awcache ),
    .io_mmio_axi4_aw_bits_prot    (M_MMIO_AXI_awprot  ),
    .io_mmio_axi4_aw_bits_qos     (M_MMIO_AXI_awqos   ),
    .io_mmio_axi4_w_ready         (M_MMIO_AXI_wready  ),
    .io_mmio_axi4_w_valid         (M_MMIO_AXI_wvalid  ),
    .io_mmio_axi4_w_bits_data     (M_MMIO_AXI_wdata   ),
    .io_mmio_axi4_w_bits_strb     (M_MMIO_AXI_wstrb   ),
    .io_mmio_axi4_w_bits_last     (M_MMIO_AXI_wlast   ),
    .io_mmio_axi4_b_ready         (M_MMIO_AXI_bready  ),
    .io_mmio_axi4_b_valid         (M_MMIO_AXI_bvalid  ),
    .io_mmio_axi4_b_bits_id       (M_MMIO_AXI_bid     ),
    .io_mmio_axi4_b_bits_resp     (M_MMIO_AXI_bresp   ),
    .io_mmio_axi4_ar_ready        (M_MMIO_AXI_arready ),
    .io_mmio_axi4_ar_valid        (M_MMIO_AXI_arvalid ),
    .io_mmio_axi4_ar_bits_id      (M_MMIO_AXI_arid    ),
    .io_mmio_axi4_ar_bits_addr    (mmio_araddr        ),
    .io_mmio_axi4_ar_bits_len     (M_MMIO_AXI_arlen   ),
    .io_mmio_axi4_ar_bits_size    (M_MMIO_AXI_arsize  ),
    .io_mmio_axi4_ar_bits_burst   (M_MMIO_AXI_arburst ),
    .io_mmio_axi4_ar_bits_lock    (M_MMIO_AXI_arlock  ),
    .io_mmio_axi4_ar_bits_cache   (M_MMIO_AXI_arcache ),
    .io_mmio_axi4_ar_bits_prot    (M_MMIO_AXI_arprot  ),
    .io_mmio_axi4_ar_bits_qos     (M_MMIO_AXI_arqos   ),
    .io_mmio_axi4_r_ready         (M_MMIO_AXI_rready  ),
    .io_mmio_axi4_r_valid         (M_MMIO_AXI_rvalid  ),
    .io_mmio_axi4_r_bits_id       (M_MMIO_AXI_rid     ),
    .io_mmio_axi4_r_bits_data     (M_MMIO_AXI_rdata   ),
    .io_mmio_axi4_r_bits_resp     (M_MMIO_AXI_rresp   ),
    .io_mmio_axi4_r_bits_last     (M_MMIO_AXI_rlast   ),
    
    // DMA
    .io_l2_frontend_bus_axi4_aw_ready        (DMA_AXI_awready ),
    .io_l2_frontend_bus_axi4_aw_valid        (DMA_AXI_awvalid ),
    .io_l2_frontend_bus_axi4_aw_bits_id      (DMA_AXI_awid    ),
    .io_l2_frontend_bus_axi4_aw_bits_addr    (DMA_AXI_awaddr  ),
    .io_l2_frontend_bus_axi4_aw_bits_len     (DMA_AXI_awlen   ),
    .io_l2_frontend_bus_axi4_aw_bits_size    (DMA_AXI_awsize  ),
    .io_l2_frontend_bus_axi4_aw_bits_burst   (DMA_AXI_awburst ),
    .io_l2_frontend_bus_axi4_aw_bits_lock    (DMA_AXI_awlock  ),
    .io_l2_frontend_bus_axi4_aw_bits_cache   (DMA_AXI_awcache ),
    .io_l2_frontend_bus_axi4_aw_bits_prot    (DMA_AXI_awprot  ),
    .io_l2_frontend_bus_axi4_aw_bits_qos     (DMA_AXI_awqos   ),
    .io_l2_frontend_bus_axi4_w_ready         (DMA_AXI_wready  ),
    .io_l2_frontend_bus_axi4_w_valid         (DMA_AXI_wvalid  ),
    .io_l2_frontend_bus_axi4_w_bits_data     (DMA_AXI_wdata   ),
    .io_l2_frontend_bus_axi4_w_bits_strb     (DMA_AXI_wstrb   ),
    .io_l2_frontend_bus_axi4_w_bits_last     (DMA_AXI_wlast   ),
    .io_l2_frontend_bus_axi4_b_ready         (DMA_AXI_bready  ),
    .io_l2_frontend_bus_axi4_b_valid         (DMA_AXI_bvalid  ),
    .io_l2_frontend_bus_axi4_b_bits_id       (DMA_AXI_bid     ),
    .io_l2_frontend_bus_axi4_b_bits_resp     (DMA_AXI_bresp   ),
    .io_l2_frontend_bus_axi4_ar_ready        (DMA_AXI_arready ),
    .io_l2_frontend_bus_axi4_ar_valid        (DMA_AXI_arvalid ),
    .io_l2_frontend_bus_axi4_ar_bits_id      (DMA_AXI_arid    ),
    .io_l2_frontend_bus_axi4_ar_bits_addr    (DMA_AXI_araddr  ),
    .io_l2_frontend_bus_axi4_ar_bits_len     (DMA_AXI_arlen   ),
    .io_l2_frontend_bus_axi4_ar_bits_size    (DMA_AXI_arsize  ),
    .io_l2_frontend_bus_axi4_ar_bits_burst   (DMA_AXI_arburst ),
    .io_l2_frontend_bus_axi4_ar_bits_lock    (DMA_AXI_arlock  ),
    .io_l2_frontend_bus_axi4_ar_bits_cache   (DMA_AXI_arcache ),
    .io_l2_frontend_bus_axi4_ar_bits_prot    (DMA_AXI_arprot  ),
    .io_l2_frontend_bus_axi4_ar_bits_qos     (DMA_AXI_arqos   ),
    .io_l2_frontend_bus_axi4_r_ready         (DMA_AXI_rready  ),
    .io_l2_frontend_bus_axi4_r_valid         (DMA_AXI_rvalid  ),
    .io_l2_frontend_bus_axi4_r_bits_id       (DMA_AXI_rid     ),
    .io_l2_frontend_bus_axi4_r_bits_data     (DMA_AXI_rdata   ),
    .io_l2_frontend_bus_axi4_r_bits_resp     (DMA_AXI_rresp   ),
    .io_l2_frontend_bus_axi4_r_bits_last     (DMA_AXI_rlast   )
  );

endmodule