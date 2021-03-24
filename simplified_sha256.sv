module simplified_sha256 #(parameter integer NUM_OF_WORDS = 20)(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, PROCESS_1, PROCESS_2, READ, BLOCK, COMPUTE, WRITE} state;

// Local variables
logic [31:0] w[64];  //it was recommended to implement w[16] but i dont know how to do that
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7; //hash 8
logic [31:0] a, b, c, d, e, f, g, h;//
logic [ 7:0] i, j;

logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;

logic        cur_we; 
logic [15:0] cur_addr;
logic [31:0] cur_write_data;
logic [31:0] tmp[8];
logic[512:0] memory_block; 
logic [ 7:0] tstep; //used in program

// SHA256 K constants
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

assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
assign tstep = (i - 1);

function logic [15:0] determine_num_blocks(input logic [31:0] size);
	logic [31:0] test, blocks, temp;
		
		test=(size*32)/512; 
		temp=(size*32)%512; 
		
		if(temp) 
			begin 
				blocks=test+1; //more than 1 block is required 
			end 
		else 
			begin 
				blocks=test; 
			end 
	determine_num_blocks=blocks; 

endfunction

//Hash logic core
// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
	// all is given on sha 256 pdf pg 23
    S0 =rightrotate(a,2)^rightrotate(a,13)^rightrotate(a,22); 
    ch = (e&f)^((~e)&g); 
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
	t1 = h+S1+ch+k[t]+w; 
    maj = (a&b)^(a&c)^(b&c);  
    t2 = S0+maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;


function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
							
	begin rightrotate=(x>>r)|(x<<(32-r)); end

endfunction

//Message schedule logic 

function logic [31:0] Wtnew; //step 4 sha 256 pdf pg 21
	logic [31:0] S0, S1; 
		begin 
			///given on sha optimization pg 20
			S0= rightrotate(w[1], 7) ^ rightrotate(w[1], 18) ^ (w[1] >> 3);
			S1= rightrotate(w[14], 17) ^ rightrotate(w[14], 19) ^ (w[14] >> 10);
			Wtnew = w[0] + S0 + w[9] + S1;
		end 
endfunction
	
// SHA-256 FSM 
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0; //by default
    state <= IDLE;
  end 
  else case (state)
    IDLE: begin 
       if(start) begin
		cur_addr<=message_addr; 
		 
	//This is where we define initual hash value. The initial hash value consists of 8 words, each 32 bits in length
		
		h0 <= 32'h6a09e667;
		h1 <= 32'hbb67ae85;
		h2 <= 32'h3c6ef372;
		h3 <= 32'ha54ff53a;
		h4 <= 32'h510e527f;
		h5 <= 32'h9b05688c;
		h6 <= 32'h1f83d9ab;
		h7 <= 32'h5be0cd19;

		a <= 32'h6a09e667;
		b <= 32'hbb67ae85;
		c <= 32'h3c6ef372;
		d <= 32'ha54ff53a;
		e <= 32'h510e527f;
		f <= 32'h9b05688c;
		g <= 32'h1f83d9ab;
		h <= 32'h5be0cd19;
		
		
		cur_we<=1'b0; 
		offset<=32'b0; 
		i<=8'b0; 
		j<=8'b0; 
		state<=PROCESS_1; 
		
       end
    end
	
	PROCESS_1: begin 
	
		state<=READ;
		
		end 
		
		
	READ: begin 
		if (offset<NUM_OF_WORDS) begin 
			w[offset]<=mem_read_data; 
			offset<=offset+1; //
			state<=PROCESS_1; 
		end
		
		else begin 
			w[31]<=32'd640;  
			w[20]<=32'h80000000; 
	
			for (int t=21; t<31; t=t+1) begin 
				w[t]<=32'h0; 
			end 
			
			offset<=0; 
			state<=BLOCK; 
		end
	end 
		

    BLOCK: begin
		if (i<num_blocks) begin //
			for (int t=0; t<16; t=t+1) begin 
				w[t]<=w[t+(i*16)];
				i<=i+1; 
				state<=COMPUTE; 
			end 
		end 
		else begin 
			state<=PROCESS_2; 
		end
		
    end
	
	PROCESS_2: begin 
		cur_we<=1; 
		cur_addr<=output_addr; 
		cur_write_data<=h0; 
		state<=WRITE; 
		
		tmp[0]<=h0; 
		tmp[1]<=h1;
		tmp[2]<=h2;
		tmp[3]<=h3;
		tmp[4]<=h4;
		tmp[5]<=h5;
		tmp[6]<=h6;
		tmp[7]<=h7;
		
	end 
		
    COMPUTE: begin
	
        if (j < 64) begin
				{a, b, c, d, e, f, g, h} <= sha256_op(a,b,c,d,e,f,g,h,w[0],j);
					for(int n=0;n<15;n++) begin //given on optimization pg 9
						w[n] <= w[n+1];
					end
						w[15] <= Wtnew;
						j <= j + 1;
				 
						state <= COMPUTE;	
			end 
			
		else begin//this is second time hashing 
			state <= BLOCK;
			j <= 0;
		
		 h0 <= a + h0;
		 h1 <= b + h1;
		 h2 <= c + h2;
		 h3 <= d + h3;
		 h4 <= e + h4;
		 h5 <= f + h5;
		 h6 <= g + h6;
		 h7 <= h + h7;
		 
		 a <= a + h0;
		 b <= b + h1;
		 c <= c + h2;
		 d <= d + h3;
		 e <= e + h4;
		 f <= f + h5;
		 g <= g + h6;
		 h <= h + h7;
		 
       end
    end

 
    WRITE: begin
		if(offset<=7) begin
			offset<=offset+1;		
			cur_write_data<=tmp[offset+1];  
			state<=WRITE; 
		end 
		
		else begin 
		state<=IDLE; 
		end 
  
    end
   
	endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
