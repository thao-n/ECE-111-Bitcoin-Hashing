module bitcoin_hash (input logic clk, reset_n, start, 
									input logic[15:0] message_addr, output_addr,
									output logic done, mem_clk, mem_we,
									output logic [15:0] mem_addr,
									output logic [31:0] mem_write_data,
									input logic [31:0] mem_read_data);
									
  enum logic[3:0] {IDLE, BUFFER, ASSIGN_1, PRE_COMPUTE1, HASH_1, ASSIGN_2, PRE_COMPUTE2, HASH_2, ASSIGN_3, PRE_COMPUTE3, HASH_3, CHECKING, WRITE} state;
	
  logic [31:0] h0[8];
  logic [31:0] h1[8];
  logic [31:0] h2[8];
  logic [31:0] w[16];
  logic [31:0] A, B, C, D, E, F, G, H;
  logic [31:0] tmp_value;
  logic [7:0] counter; 
  logic [7:0] t;

  	////////THIS IS THE FINAL CODE//////
  parameter int k[0:63] = '{
  32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
  32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
  32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
  32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
  32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
  32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070, 
  32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
  32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
  };
  
  function logic [31:0] Wtnew;
    logic [31:0] s0, s1;
	 
	 s0 = rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1]>>3);
	 s1 = rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14]>>10);
	 Wtnew = w[0] + s0 + w[9] + s1;
  endfunction
  
  function logic [255:0] sha256_op(input logic [31:0] A, B, C, D, E, F, G, tmp_value);
  logic [31:0] S1, S0, ch, maj, t1, t2; 
  begin
    S1 = rightrotate(E, 6) ^ rightrotate(E, 11) ^ rightrotate(E, 25);
    ch = (E&F)^((~E)&G);
    t1 = S1 + ch + tmp_value;
    S0 = rightrotate(A, 2) ^ rightrotate(A, 13) ^ rightrotate(A, 22);
    maj = (A&B)^(A&C)^(B&C);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, A, B, C, D + t1, E, F, G};
  end
  endfunction
  
  function logic [31:0] rightrotate(input logic [31:0] x,
                                    input logic [7:0] r);
  begin
    rightrotate = (x >> r) | (x << (32-r));
  end
  endfunction
  
  
  assign mem_clk=clk;	
  assign done = (state == IDLE);
  
  
  always_ff @(posedge clk, negedge reset_n)
  begin
    if(!reset_n) begin
		state <= IDLE;
	end 
	 
	else  	
		case(state)
			IDLE:
				if(start) begin
				
					h0[0] <= 32'h6a09e667;
					h0[1] <= 32'hbb67ae85;
					h0[2] <= 32'h3c6ef372;
					h0[3] <= 32'ha54ff53a;
					h0[4] <= 32'h510e527f;
					h0[5] <= 32'h9b05688c;
					h0[6] <= 32'h1f83d9ab;
					h0[7] <= 32'h5be0cd19; 
					
					mem_we <= 0;
					mem_addr <= message_addr;
					counter <= 0;
					state <= BUFFER;
			
				end
		 
			BUFFER: begin
				mem_addr <= message_addr;
				mem_addr <= mem_addr + 1;	
				state <= ASSIGN_1;	
			end
			
			ASSIGN_1: begin
				A <= h0[0];
				B <= h0[1]; 
				C <= h0[2]; 
				D <= h0[3]; 
				E <= h0[4]; 
				F <= h0[5]; 
				G <= h0[6]; 
				H <= h0[7]; 
				w[15] <= mem_read_data;
				
				t <= 0;
				mem_addr <= mem_addr + 1;
				state <= PRE_COMPUTE1;
			end
		
			PRE_COMPUTE1: begin
			
				tmp_value <= w[15] + k[t] + H;
				
				for (int n = 0; n <15; n++) begin 
					w[n] <= w[n+1];
				end 
			
				
				mem_addr <= mem_addr + 1;
				w[15] <= mem_read_data;
				t <= t + 1;
				state <= HASH_1;

			end	 
						
			HASH_1: begin
				if (t<65) begin 
					for (int n = 0; n < 15; n++) begin
					w[n] <= w[n+1];
					end 
					
					if (t<15) begin
						w[15] <= mem_read_data;
						mem_addr <= mem_addr + 1;
					end 
				
				
					else begin
						w[15] <= Wtnew();
						mem_addr <= message_addr + 16;
					end
					
					tmp_value <= w[15] + k[t] + G;
					t <= t + 1;
					{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, tmp_value);
				
				  
				end 
				
				else begin 
					h1[0] <= h0[0] + A; 
					h1[1] <= h0[1] + B;
					h1[2] <= h0[2] + C;
					h1[3] <= h0[3] + D; 
					h1[4] <= h0[4] + E; 
					h1[5] <= h0[5] + F; 
					h1[6] <= h0[6] + G; 
					h1[7] <= h0[7] + H;
				  
					mem_addr <= message_addr;
					mem_addr <= mem_addr + 1;
					state <= ASSIGN_2;
				end 
				
				
			end
		
		
			ASSIGN_2: begin
				A <= h1[0];
				B <= h1[1];
				C <= h1[2];
				D <= h1[3];
				E <= h1[4];
				F <= h1[5];
				G <= h1[6];
				H <= h1[7];
				w[15] <= mem_read_data;
			 
				t <= 0;
				mem_addr <= mem_addr + 1;
				state <= PRE_COMPUTE2; 
				 
			end
		
			PRE_COMPUTE2: begin
				tmp_value <= w[15] + k[t] + H;
			 
				for (int n = 0; n < 15; n++) 
					w[n] <= w[n+1];
				
				w[15] <= mem_read_data;
				mem_addr <= mem_addr + 1;
				t <= t + 1;
				state <= HASH_2;
				
			end
		
			HASH_2: begin
				if (t<=64) begin 
					for (int n = 0; n < 15; n++) 
						w[n] <= w[n+1];	
					if (t<15) begin
						if (t<2) begin
							w[15] <= mem_read_data;
							mem_addr <= mem_addr + 1;
						end 
						else if (t == 2 ) begin
							w[15] <= counter;
						end
						
						else if (t == 3 ) begin
							w[15] <= 32'h80000000;
						end 
						
						else if (t>3 && t<14) begin
							w[15] <= 32'h00000000;
						end 
						
						else begin
							w[15] <= 32'd640;
						end
					
					end 
				
					else begin
						w[15] <= Wtnew();
					end
				
				  t <= t + 1;
				  tmp_value <= w[15] + k[t] + G;
				  {A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, tmp_value);
				  
				end 
				
				else begin
				
					t <= 0;
					
				  h2[0] <= h1[0] + A;
				  h2[1] <= h1[1] + B;
				  h2[2] <= h1[2] + C;
				  h2[3] <= h1[3] + D;
				  h2[4] <= h1[4] + E;
				  h2[5] <= h1[5] + F;
				  h2[6] <= h1[6] + G;
				  h2[7] <= h1[7] + H;
				  
				  h0[0] <= 32'h6a09e667;
				  h0[1] <= 32'hbb67ae85;
				  h0[2] <= 32'h3c6ef372;   
				  h0[3] <= 32'ha54ff53a;
				  h0[4] <= 32'h510e527f;
				  h0[5] <= 32'h9b05688c;
				  h0[6] <= 32'h1f83d9ab;
				  h0[7] <= 32'h5be0cd19; 
				  
				  state <= ASSIGN_3;
				  
				end
			end
		
		
			ASSIGN_3: begin
				A <= h0[0];
				B <= h0[1];
				C <= h0[2];
				D <= h0[3];
				E <= h0[4];
				F <= h0[5];
				G <= h0[6];
				H <= h0[7];
				w[15] <= h2[t];

				state <= PRE_COMPUTE3;

			end
		
			PRE_COMPUTE3: begin
		
				tmp_value <= w[15] + k[t] + H;
					for (int n = 0; n < 15; n++)begin 
						w[n] <= w[n+1];
					end 
				
				w[15] <= h2[t+1];
				t <= t + 1;
				state <= HASH_3;
			end
		
			CHECKING: begin
				mem_addr <= mem_addr + 1;
				h0[0] <= h1[0];
				h0[1] <= h1[1];
				h0[2] <= h1[2];
				h0[3] <= h1[3];
				h0[4] <= h1[4];
				h0[5] <= h1[5];
				h0[6] <= h1[6];
				h0[7] <= h1[7];
				state <= ASSIGN_2;
			end 
		
			HASH_3: begin
				if (t<=64) begin 
					for (int n = 0; n < 15; n++) 
						w[n] <= w[n+1];
					if (t<15) begin
						if (t<7) begin
							w[15] <= h2[t+1];
						end 
				    
						else if (t == 7 ) begin
							w[15] <= 32'h80000000;
						end 
						else if (t>7 && t<14) begin
							w[15] <= 32'h00000000;
						end 
						else begin
							w[15] <= 32'd256;
						end
					end 
				
					else begin
							w[15] <= Wtnew();
					end
				
					tmp_value <= w[15] + k[t] + G;
					t <= t + 1;
					{A, B, C, D, E, F, G, H} <= sha256_op(A, B, C, D, E, F, G, tmp_value);
					state <= HASH_3;
				  
				end 
			
				else begin
					mem_we <= 1;
					mem_addr <= output_addr + counter;
					state <= WRITE;
					mem_write_data <= h0[0] + A;
				end
			end

	
			WRITE: begin
		
				mem_we <= 0;
				mem_addr <= message_addr + 16;	 
		
				if (counter < 15) begin
					counter <= counter + 1;
					state<=CHECKING;
				end 			
			 
				else begin
					state <= IDLE;
				end
				
			end
				
		endcase	 
	end									
endmodule
