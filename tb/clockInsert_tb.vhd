----------------------------------------------------------------------------------------------------
-- brief: Testbench for entity clockInsert
-- file: clockInsert_tb.vhd
-- author: Lukas Leuenberger
----------------------------------------------------------------------------------------------------
-- Copyright (c) 2025 by OST - Eastern Switzerland University of Applied Sciences (www.ost.ch)
-- This code is licensed under the MIT license (see LICENSE for details)
----------------------------------------------------------------------------------------------------

-- Standard library ieee
library ieee; 
-- This package defines the basic std_logic data types and a few functions.
use ieee.std_logic_1164.all;
-- This package provides arithmetic functions for vectors.
use ieee.numeric_std.all;
-- This package provides file specific functions.
use std.textio.all;
-- This package provides file specific functions for the std_logic types.
use ieee.std_logic_textio.all;
-- This package provides arithmetic functions for real values.
use ieee.math_real.all;
-- VUnit test library
library vunit_lib;
context vunit_lib.vunit_context;

-- This block implements the testbench for the block **clockInsert**.
entity clockInsert_tb is
	generic(
		runner_cfg : string := runner_cfg_default; -- Generic for VUnit -- @suppress
		tb_path : string := "D:\Projekte\2023_PQC\GlitchGenerator\tb\clockInsert\"  -- Generic for VUnit -- @suppress
	);
end clockInsert_tb;

-- This is the testbench architecture of the **clockInsert** block.
architecture tb of clockInsert_tb is	
	----------------------------------------------------------------------------------------------------
	-- Internal constants
	----------------------------------------------------------------------------------------------------
	constant PATH_TESTVECTOR_C : string := tb_path & "clockInsert_tb.csv"; -- Path to the testvector file
	constant F_CLK_C : real := 100.0E6; -- Define the frequency of the clock
	constant T_CLK_C : time := (1.0 sec) / F_CLK_C; -- Calculate the period of a clockcycle
	constant DTS_C : time := 2 ns; -- Wait time before applying the stimulus 
	constant DTR_C : time := 6 ns; -- Wait time before reading response

	constant MAX_COUNT_VAL_C : positive := 3; -- Constant for the generic MAX_COUNT_VAL_G

	----------------------------------------------------------------------------------------------------
	-- Internal signals
	----------------------------------------------------------------------------------------------------
	-- Input signals
	signal tb_clk_in : std_logic; -- Internal signal for input signal clk_in @suppress
	signal tb_reset_in : std_logic; -- Internal signal for input signal reset_in @suppress
	signal tb_start_in : std_logic; -- Internal signal for input signal start_in @suppress
	signal tb_stop_in : std_logic; -- Internal signal for input signal stop_in @suppress
	signal tb_repeat_in : std_logic; -- Internal signal for input signal repeat_in @suppress
	signal tb_delay_in : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_C)))) - 1 downto 0); -- Internal signal for input signal delay_in @suppress
	signal tb_repetition_in : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_C)))) - 1 downto 0); -- Internal signal for input signal repetition_in @suppress
	signal tb_bypass_in : std_logic; -- Internal signal for input signal bypass_in @suppress

	-- Output signals
	signal tb_running_out : std_logic; -- Internal signal for output signal running_out @suppress
	signal tb_insertGlitch_out : std_logic; -- Internal signal for output signal insertGlitch_out @suppress

	-- Expected responses signals
	signal tb_running_out_exp : std_logic; -- Expected response for output signal running_out @suppress
	signal tb_insertGlitch_out_exp : std_logic; -- Expected response for output signal insertGlitch_out @suppress

	----------------------------------------------------------------------------------------------------
	-- Internal constants, types and functions to convert a vector to a string
	----------------------------------------------------------------------------------------------------
	type charIndexedByMVL9_t is array (std_ulogic) of character; -- Type to convert a vector to a string
	constant MVL9_TO_CHAR_C : charIndexedByMVL9_t := "UX01ZWLH-"; -- Constant to convert a vector to a string

	-- This function converts a __std_ulogic_vector__ into a string.
	function to_string_std_ulogic_vector(value : std_ulogic_vector) return STRING is -- @suppress
		alias ivalue    : std_ulogic_vector(1 to value'length) is value;
		variable result_v : string(1 to value'length + 2);
	begin
		if value'length < 1 then
			return "";
		else
			result_v(1) := ''';
			result_v(value'length + 2) := ''';
			for i in ivalue'range loop
				result_v(i + 1) := MVL9_TO_CHAR_C(ivalue(i));
			end loop;
			return result_v;
		end if;
	end function to_string_std_ulogic_vector;

	-- This function converts a __std_logic_vector__ into a string.
	function to_string_std_logic_vector(value : std_logic_vector) return string is -- @suppress
	begin
		return to_string_std_ulogic_vector(to_stdulogicvector(value));
	end function to_string_std_logic_vector;

	-- This function converts an unsigned value into a string.
	function to_string_unsigned(value : unsigned) return string is -- @suppress
	begin
		return to_string_std_ulogic_vector(std_ulogic_vector(value)) & " / " & Integer'image(to_integer(value));
	end function to_string_unsigned;

	-- This function converts a signed value into a string.
	function to_string_signed(value : signed) return string is -- @suppress
	begin
		return to_string_std_ulogic_vector(std_ulogic_vector(value)) & " / " & Integer'image(to_integer(value));
	end function to_string_signed;

begin
	-- Make a dut and map the ports to the correct signals
	inst_DUT: entity work.clockInsert
		generic map (
			MAX_COUNT_VAL_G => MAX_COUNT_VAL_C -- Map constant MAX_COUNT_VAL_C to the generic MAX_COUNT_VAL_G
		)
		port map (
			clk_in => tb_clk_in, -- Map signal tb_clk_in to the port clk_in
			reset_in => tb_reset_in, -- Map signal tb_reset_in to the port reset_in
			start_in => tb_start_in, -- Map signal tb_start_in to the port start_in
			stop_in => tb_stop_in, -- Map signal tb_stop_in to the port stop_in
			repeat_in => tb_repeat_in, -- Map signal tb_repeat_in to the port repeat_in
			delay_in => tb_delay_in, -- Map signal tb_delay_in to the port delay_in
			repetition_in => tb_repetition_in, -- Map signal tb_repetition_in to the port repetition_in
			bypass_in => tb_bypass_in, -- Map signal tb_bypass_in to the port bypass_in
			running_out => tb_running_out, -- Map signal tb_running_out to the port running_out
			insertGlitch_out => tb_insertGlitch_out -- Map signal tb_insertGlitch_out to the port insertGlitch_out
		);

	-- Apply a clock to the DUT
	proc_stimuliClk : process
	begin
		tb_clk_in <= '0';
		loop
			wait for (T_CLK_C / 2.0);
			tb_clk_in <= not tb_clk_in;
		end loop;
		wait;
	end process;

	-- Apply the testvectors to the DUT
	proc_applyTestvector : process
		-- declare and open file with test vectors
		file testVectorFile : text open read_mode 
		is PATH_TESTVECTOR_C;

		variable lineBuffer_v : line; -- Text line buffer, current line
		variable lineDelimChar_v : character; -- buffer for the delimitier char @suppress
		variable vectorNr_v : integer := 0; -- Variable used to count the testvectors @suppress
		variable errorCounter_v : integer := 0; -- Variable used to count the number of occured errors @suppress
		variable reset_in_v : std_logic; -- Internal variable for signal reset_in @suppress
		variable start_in_v : std_logic; -- Internal variable for signal start_in @suppress
		variable stop_in_v : std_logic; -- Internal variable for signal stop_in @suppress
		variable repeat_in_v : std_logic; -- Internal variable for signal repeat_in @suppress
		variable delay_in_v : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_C)))) - 1 downto 0); -- Internal variable for signal delay_in @suppress
		variable repetition_in_v : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_C)))) - 1 downto 0); -- Internal variable for signal repetition_in @suppress
		variable bypass_in_v : std_logic; -- Internal variable for signal bypass_in @suppress
		variable running_out_v : std_logic; -- Internal variable for signal running_out @suppress
		variable insertGlitch_out_v : std_logic; -- Internal variable for signal insertGlitch_out @suppress
	begin
		 -- Setup the VUnit Runner
		test_runner_setup(runner, runner_cfg);

		-- Set all signals to a default value
		tb_reset_in <= '0';
		tb_start_in <= '0';
		tb_stop_in <= '0';
		tb_repeat_in <= '0';
		tb_delay_in <= (others => '0');
		tb_repetition_in <= (others => '0');
		tb_bypass_in <= '0';

		-- Reset DUT
		tb_reset_in <= '1'; -- Perform a reset
		wait until (rising_edge (tb_clk_in)); -- Wait for the first active clock edge
		tb_reset_in <= '0'; -- Clear the reset

		-- Loop through the whole file
		while not endfile(testVectorFile) loop -- Read individual lines until the end of the file
			readline (testVectorFile, lineBuffer_v); -- Start reading a new line with stimulus / response pair

			vectorNr_v := vectorNr_v + 1; -- Increment the test vector number

			next when lineBuffer_v.all(1) = '-'; -- Jump over comments

			wait for DTS_C; -- Wait for time point of application stimuli

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, reset_in_v); -- Read input stimuli of signal reset_in
			tb_reset_in <= reset_in_v; -- Interprete stimuli of signal reset_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, start_in_v); -- Read input stimuli of signal start_in
			tb_start_in <= start_in_v; -- Interprete stimuli of signal start_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, stop_in_v); -- Read input stimuli of signal stop_in
			tb_stop_in <= stop_in_v; -- Interprete stimuli of signal stop_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, repeat_in_v); -- Read input stimuli of signal repeat_in
			tb_repeat_in <= repeat_in_v; -- Interprete stimuli of signal repeat_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, delay_in_v); -- Read input stimuli of signal delay_in
			tb_delay_in <= delay_in_v; -- Interprete stimuli of signal delay_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, repetition_in_v); -- Read input stimuli of signal repetition_in
			tb_repetition_in <= repetition_in_v; -- Interprete stimuli of signal repetition_in

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, bypass_in_v); -- Read input stimuli of signal bypass_in
			tb_bypass_in <= bypass_in_v; -- Interprete stimuli of signal bypass_in


			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, running_out_v); -- Read expected reponse of signal running_out
			tb_running_out_exp <= running_out_v; -- Interprete expected response of signal running_out

			read (lineBuffer_v, lineDelimChar_v); -- Read a delim char
			read (lineBuffer_v, insertGlitch_out_v); -- Read expected reponse of signal insertGlitch_out
			tb_insertGlitch_out_exp <= insertGlitch_out_v; -- Interprete expected response of signal insertGlitch_out

			wait for DTR_C; -- Wait for a valid response

			-- Compare all results with the expected results
			check (tb_running_out_exp = tb_running_out, "Error with running_out in test vector " & Integer'image(vectorNr_v)  & ". Expected = " & std_logic'image(tb_running_out_exp) & ", was = " & std_logic'image(tb_running_out), warning);
			check (tb_insertGlitch_out_exp = tb_insertGlitch_out, "Error with insertGlitch_out in test vector " & Integer'image(vectorNr_v)  & ". Expected = " & std_logic'image(tb_insertGlitch_out_exp) & ", was = " & std_logic'image(tb_insertGlitch_out), warning);

			-- Increment the error counter
			if (tb_running_out_exp /= tb_running_out) then
				errorCounter_v := errorCounter_v + 1;
			end if;
			if (tb_insertGlitch_out_exp /= tb_insertGlitch_out) then
				errorCounter_v := errorCounter_v + 1;
			end if;

			wait until (rising_edge (tb_clk_in)); -- Wait for the next active clock edge
		end loop;

		-- Terminate the simulation
		check (errorCounter_v = 0, "Simulation completed with " & Integer'image(errorCounter_v) & " errors.", error);

		-- Cleanup the VUnit Runner
		test_runner_cleanup(runner);

		-- Wait forever
		wait;
	end process proc_applyTestvector;
end tb;
