----------------------------------------------------------------------------------------------------
-- brief: Testbench for entity clockLogic
-- file: clockLogic_tb.vhd
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

-- This block implements the testbench for the block **clockLogic**.
entity clockLogic_tb is
    generic(
        runner_cfg : string := runner_cfg_default; -- Generic for VUnit -- @suppress
        tb_path    : string := "d:\Projekte\2023_PQC\GlitchGenerator\tb\clockLogic\" -- Generic for VUnit -- @suppress
    );
end clockLogic_tb;

-- This is the testbench architecture of the **clockLogic** block.
architecture tb of clockLogic_tb is
    ----------------------------------------------------------------------------------------------------
    -- Internal constants
    ----------------------------------------------------------------------------------------------------
    constant F_CLK_C : real := 200.0E6; -- Define the frequency of the clock    
    constant T_CLK_C : time := (1.0 sec) / F_CLK_C; -- Calculate the period of a clockcycle

    constant CLKIN1_PERIOD_C : real    := 5.0; -- Constant for the generic CLKIN1_PERIOD_G
    constant CLKFBOUT_MULT_C : real    := 4.0; -- Constant for the generic CLKFBOUT_MULT_G  
    constant DIVCLK_DIVIDE_C : integer := 1; -- Constant for the generic DIVCLK_DIVIDE_G  
    constant CLKOUT_DIVIDE_C : real    := 4.0; -- Constant for the generic CLKOUT_DIVIDE_G  

    constant T_CLKCHECK_C : time := (1.0 sec) / (F_CLK_C * CLKFBOUT_MULT_C / real(DIVCLK_DIVIDE_C) / CLKOUT_DIVIDE_C); -- Calculate the period of a clockcycle

    ----------------------------------------------------------------------------------------------------
    -- Internal signals
    ----------------------------------------------------------------------------------------------------
    -- Input signals
    signal tb_clk_in      : std_logic;  -- Internal signal for input signal clk_in @suppress
    signal tb_reset_in    : std_logic;  -- Internal signal for input signal reset_in @suppress
    signal tb_psIncDec_in : std_logic;  -- Internal signal for input signal psIncDec_in @suppress
    signal tb_psEnA_in    : std_logic;  -- Internal signal for input signal psEnA_in @suppress
    signal tb_psEnB_in    : std_logic;  -- Internal signal for input signal psEnB_in @suppress
    signal tb_psEnC_in    : std_logic;  -- Internal signal for input signal psEnC_in @suppress
    signal tb_enA_in      : std_logic;  -- Internal signal for input signal enA_in @suppress
    signal tb_enB_in      : std_logic;  -- Internal signal for input signal enB_in @suppress
    signal tb_enC_in      : std_logic;  -- Internal signal for input signal enC_in @suppress
    signal tb_enGlitch_in : std_logic;  -- Internal signal for input signal enGlitch_in @suppress
    signal tb_DADDR_in    : std_logic_vector(6 downto 0); -- Internal signal for input signal DADDR_in @suppress
    signal tb_DCLK_in     : std_logic;  -- Internal signal for input signal DCLK_in @suppress
    signal tb_DEN_in      : std_logic;  -- Internal signal for input signal DEN_in @suppress
    signal tb_DI_in       : std_logic_vector(15 downto 0); -- Internal signal for input signal DI_in @suppress
    signal tb_DWE_in      : std_logic;  -- Internal signal for input signal DWE_in @suppress
    signal tb_DRPRESET_in : std_logic;  -- Internal signal for input signal DRPRESET_in @suppress
    signal tb_DRPSEL_in   : std_logic_vector(1 downto 0); -- Internal signal for input signal DRPSEL_in @suppress
    signal tb_drpStart    : std_logic;  -- Starts DRP reconfiguration @suppress

    -- Output signals
    signal tb_clk_out       : std_logic; -- Internal signal for output signal clk_out @suppress
    signal tb_clkC_out      : std_logic; -- Internal signal for output signal clkC_out @suppress
    signal tb_psDoneA_out   : std_logic; -- Internal signal for output signal psDoneA_out @suppress
    signal tb_lockedA_out   : std_logic; -- Internal signal for output signal lockedA_out @suppress
    signal tb_psDoneB_out   : std_logic; -- Internal signal for output signal psDoneB_out @suppress
    signal tb_lockedB_out   : std_logic; -- Internal signal for output signal lockedB_out @suppress
    signal tb_psDoneC_out   : std_logic; -- Internal signal for output signal psDoneC_out @suppress
    signal tb_lockedC_out   : std_logic; -- Internal signal for output signal lockedC_out @suppress
    signal tb_DO_out        : std_logic_vector(15 downto 0); -- Internal signal for output signal DO_out @suppress
    signal tb_DRDY_out      : std_logic; -- Internal signal for output signal DRDY_out @suppress
    signal tb_DRPLOCKED_out : std_logic; -- Internal signal for output signal DRPLOCKED_out @suppress
    signal tb_drpReady      : std_logic; -- Signalizes finished DRP reconfiguration @suppress

    ----------------------------------------------------------------------------------------------------
    -- Internal constants, types and functions to convert a vector to a string
    ----------------------------------------------------------------------------------------------------
    type charIndexedByMVL9_t is array (std_ulogic) of character; -- Type to convert a vector to a string
    constant MVL9_TO_CHAR_C : charIndexedByMVL9_t := "UX01ZWLH-"; -- Constant to convert a vector to a string

    -- This function converts a __std_ulogic_vector__ into a string.
    function to_string_std_ulogic_vector(value : std_ulogic_vector) return STRING is -- @suppress
        alias ivalue      : std_ulogic_vector(1 to value'length) is value;
        variable result_v : string(1 to value'length + 2);
    begin
        if value'length < 1 then
            return "";
        else
            result_v(1)                := ''';
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

    -- This procedure measures the time between two edges of a signal.
    procedure calcPeriod(signal clk : in std_logic; variable period : out time) is
        variable timeStart_v : time;
        variable timeEnd_v   : time;
    begin
        wait until (rising_edge(clk));  -- Wait for the active clock edge
        timeStart_v := now;
        wait until (rising_edge(clk));  -- Wait for the active clock edge
        timeEnd_v   := now;
        period      := timeEnd_v - timeStart_v;
    end procedure calcPeriod;

    -- This procedure measures the phase shift between two signals
    procedure calcPhase(signal clkA : in std_logic; signal clkB : in std_logic; variable phase : out real) is
        variable timeA_v  : time;
        variable timeB_v  : time;
        variable period_v : time;
    begin
        calcPeriod(clkA, period_v);
        wait until (rising_edge(clkA)); -- Wait for the active clock edge
        timeA_v := now;
        wait until (rising_edge(clkB)); -- Wait for the active clock edge
        timeB_v := now;
        phase   := (real((timeB_v - timeA_v) / 1 ps) / real(period_v / 1 ps)) * 360.0;
    end procedure calcPhase;

begin
    -- Make a dut and map the ports to the correct signals
    inst_DUT : entity work.clockLogic
        generic map(
            CLKIN1_PERIOD_G => CLKIN1_PERIOD_C, -- Map constant CLKIN1_PERIOD_C to the generic CLKIN1_PERIOD_G
            CLKFBOUT_MULT_G => CLKFBOUT_MULT_C, -- Map constant CLKFBOUT_MULT_C to the generic CLKFBOUT_MULT_G
            CLKOUT_DIVIDE_G => CLKOUT_DIVIDE_C, -- Map constant CLKOUT_DIVIDE_C to the generic CLKOUT_DIVIDE_G
            DIVCLK_DIVIDE_G => DIVCLK_DIVIDE_C -- Map constant DIVCLK_DIVIDE_C to the generic DIVCLK_DIVIDE_G
        )
        port map(
            clk_in        => tb_clk_in, -- Map signal tb_clk_in to the port clk_in
            clk_out       => tb_clk_out, -- Map signal tb_clk_out to the port clk_out
            clkC_out      => tb_clkC_out, -- Map signal tb_clkC_out to the port clkC_out
            reset_in      => tb_reset_in, -- Map signal tb_reset_in to the port reset_in
            psIncDec_in   => tb_psIncDec_in, -- Map signal tb_psIncDec_in to the port psIncDec_in
            psEnA_in      => tb_psEnA_in, -- Map signal tb_psEnA_in to the port psEnA_in
            psDoneA_out   => tb_psDoneA_out, -- Map signal tb_psDoneA_out to the port psDoneA_out
            lockedA_out   => tb_lockedA_out, -- Map signal tb_lockedA_out to the port lockedA_out
            psEnB_in      => tb_psEnB_in, -- Map signal tb_psEnB_in to the port psEnB_in
            psDoneB_out   => tb_psDoneB_out, -- Map signal tb_psDoneB_out to the port psDoneB_out
            lockedB_out   => tb_lockedB_out, -- Map signal tb_lockedB_out to the port lockedB_out
            psEnC_in      => tb_psEnC_in, -- Map signal tb_psEnC_in to the port psEnC_in
            psDoneC_out   => tb_psDoneC_out, -- Map signal tb_psDoneC_out to the port psDoneC_out
            lockedC_out   => tb_lockedC_out, -- Map signal tb_lockedC_out to the port lockedC_out
            enA_in        => tb_enA_in, -- Map signal tb_enA_in to the port enA_in
            enB_in        => tb_enB_in, -- Map signal tb_enB_in to the port enB_in
            enC_in        => tb_enC_in, -- Map signal tb_enC_in to the port enC_in
            enGlitch_in   => tb_enGlitch_in, -- Map signal tb_enGlitch_in to the port enGlitch_in
            DO_out        => tb_DO_out, -- Map signal tb_DO_out to the port DO_out
            DRDY_out      => tb_DRDY_out, -- Map signal tb_DRDY_out to the port DRDY_out
            DRPLOCKED_out => tb_DRPLOCKED_out, -- Map signal tb_DRPLOCKED_out to the port DRPLOCKED_out
            DADDR_in      => tb_DADDR_in, -- Map signal tb_DADDR_in to the port DADDR_in
            DCLK_in       => tb_DCLK_in, -- Map signal tb_DCLK_in to the port DCLK_in
            DEN_in        => tb_DEN_in, -- Map signal tb_DEN_in to the port DEN_in
            DI_in         => tb_DI_in,  -- Map signal tb_DI_in to the port DI_in
            DWE_in        => tb_DWE_in, -- Map signal tb_DWE_in to the port DWE_in
            DRPRESET_in   => tb_DRPRESET_in, -- Map signal tb_DRPRESET_in to the port DRPRESET_in
            DRPSEL_in     => tb_DRPSEL_in -- Map signal tb_DRPSEL_in to the port DRPSEL_in
        );

    -- Add DRP Interface
    inst_drp : entity work.clk_wiz_0_mmcm_drp
        generic map(
            S1_CLKFBOUT_MULT    => CLKFBOUT_MULT_C,
            S1_CLKFBOUT_PHASE   => 0,
            S1_CLKFBOUT_FRAC    => 0,
            S1_CLKFBOUT_FRAC_EN => 0,
            S1_BANDWIDTH        => "HIGH",
            S1_DIVCLK_DIVIDE    => DIVCLK_DIVIDE_C,
            S1_CLKOUT0_DIVIDE   => CLKOUT_DIVIDE_C,
            S1_CLKOUT0_PHASE    => 0,
            S1_CLKOUT0_DUTY     => 50000,
            S1_CLKOUT0_FRAC     => 0,
            S1_CLKOUT0_FRAC_EN  => 0
        )
        port map(
            S2_CLKFBOUT_MULT    => std_logic_vector(to_unsigned(integer(CLKFBOUT_MULT_C), 8)),
            S2_CLKFBOUT_PHASE   => x"00000000",
            S2_CLKFBOUT_FRAC    => "0000000000",
            S2_CLKFBOUT_FRAC_EN => '0',
            S2_DIVCLK_DIVIDE    => std_logic_vector(to_unsigned(DIVCLK_DIVIDE_C, 8)),
            S2_CLKOUT0_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT0_PHASE    => x"00000000",
            S2_CLKOUT0_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT0_FRAC     => "0000000000",
            S2_CLKOUT0_FRAC_EN  => '0',
            S2_CLKOUT1_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT1_PHASE    => x"00000000",
            S2_CLKOUT1_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT2_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT2_PHASE    => x"00000000",
            S2_CLKOUT2_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT3_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT3_PHASE    => x"00000000",
            S2_CLKOUT3_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT4_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT4_PHASE    => x"00000000",
            S2_CLKOUT4_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT5_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT5_PHASE    => x"00000000",
            S2_CLKOUT5_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            S2_CLKOUT6_DIVIDE   => std_logic_vector(to_unsigned(integer(CLKOUT_DIVIDE_C * 2.0), 8)),
            S2_CLKOUT6_PHASE    => x"00000000",
            S2_CLKOUT6_DUTY     => std_logic_vector(to_unsigned(50000, 32)),
            LOAD                => '1',
            SADDR               => '1',
            SEN                 => tb_drpStart,
            SCLK                => tb_clk_in,
            RST                 => tb_reset_in,
            SRDY                => tb_drpReady,
            DO                  => tb_DO_out,
            DRDY                => tb_DRDY_out,
            LOCKED              => tb_DRPLOCKED_out,
            DWE                 => tb_DWE_in,
            DEN                 => tb_DEN_in,
            DADDR               => tb_DADDR_in,
            DI                  => tb_DI_in,
            DCLK                => tb_DCLK_in,
            RST_MMCM_PLL        => tb_DRPRESET_in
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
        variable period_v     : time;
        variable phase_v      : real;
        variable phaseOld_v   : real;
        variable singleWrap_v : boolean;
    begin
        -- Setup the VUnit Runner
        test_runner_setup(runner, runner_cfg);

        -- Set all signals to a default value
        tb_reset_in    <= '0';
        tb_psIncDec_in <= '0';
        tb_psEnA_in    <= '0';
        tb_psEnB_in    <= '0';
        tb_psEnC_in    <= '0';
        tb_enA_in      <= '0';
        tb_enB_in      <= '0';
        tb_enC_in      <= '0';
        tb_enGlitch_in <= '0';
        tb_drpStart    <= '0';
        tb_DRPSEL_in   <= (others => '0');

        -- Reset DUT
        tb_reset_in <= '1';             -- Perform a reset
        wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
        tb_reset_in <= '0';             -- Clear the reset

        -- Loop through all tests
        while test_suite loop
            -- Test clock A
            if run("test_clockA") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                 wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable clock A
                tb_enA_in      <= '1';
                tb_enB_in      <= '0';
                tb_enC_in      <= '0';
                tb_enGlitch_in <= '1';

                -- Wait for the clock to become locked
                if tb_lockedA_out /= '1' then
                    wait until (tb_lockedA_out = '1');
                end if;

                -- Phase shift the clock into one direction
                phaseOld_v   := 0.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnA_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnA_in    <= '0';
                    wait until (rising_edge(tb_psDoneA_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock A is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v < phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock A to be incrementing, but was decrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;

                -- Phase shift the clock into the other direction
                phaseOld_v   := 359.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '1';
                    tb_psEnA_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnA_in    <= '0';
                    wait until (rising_edge(tb_psDoneA_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock A is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v > phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock A to be decrementing, but was incrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;

            -- Test clock B
            elsif run("test_clockB") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                 wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable clock B
                tb_enA_in      <= '0';
                tb_enB_in      <= '1';
                tb_enC_in      <= '0';
                tb_enGlitch_in <= '1';

                -- Wait for the clock to become locked
                if tb_lockedB_out /= '1' then
                    wait until (tb_lockedB_out = '1');
                end if;

                -- Phase shift the clock into one direction
                phaseOld_v   := 179.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnB_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnB_in    <= '0';
                    wait until (rising_edge(tb_psDoneB_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock B is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v < phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock B to be incrementing, but was decrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;

                -- Phase shift the clock into the other direction
                phaseOld_v   := 359.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '1';
                    tb_psEnB_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnB_in    <= '0';
                    wait until (rising_edge(tb_psDoneB_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock B is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v > phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock B to be decrementing, but was incrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;

            -- Test clock C
            elsif run("test_clockC") then
                 -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                 wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable clock B
                tb_enA_in      <= '0';
                tb_enB_in      <= '0';
                tb_enC_in      <= '1';
                tb_enGlitch_in <= '0';

                -- Wait for the clock to become locked
                if tb_lockedC_out /= '1' then
                    wait until (tb_lockedC_out = '1');
                end if;

                -- Phase shift the clock into one direction
                phaseOld_v   := 179.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnC_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnC_in    <= '0';
                    wait until (rising_edge(tb_psDoneC_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock C is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v < phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock C to be incrementing, but was decrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;

                -- Phase shift the clock into the other direction
                phaseOld_v   := 359.0;
                singleWrap_v := false;
                for i in 0 to 56 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '1';
                    tb_psEnC_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnC_in    <= '0';
                    wait until (rising_edge(tb_psDoneC_out)); -- Wait for end of phase shift

                    -- Measure Phase and Period of the clock
                    calcPhase(tb_clk_out, tb_clk_in, phase_v);
                    calcPeriod(tb_clk_out, period_v);

                    -- Check phase and period
                    check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock C is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));
                    if (phase_v > phaseOld_v) then
                        if (singleWrap_v = false) then
                            singleWrap_v := true;
                        else
                            error("Expected phase of clock C to be decrementing, but was incrementing.");
                        end if;
                    end if;
                    phaseOld_v := phase_v;
                end loop;
                
            -- Test glitch
            elsif run("test_glitch") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                 wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable all clocks
                tb_enA_in      <= '1';
                tb_enB_in      <= '1';
                tb_enC_in      <= '1';
                tb_enGlitch_in <= '1';

                -- Wait for the clocks to become locked
                if tb_lockedA_out /= '1' then
                    wait until (tb_lockedA_out = '1');
                end if;
                if tb_lockedB_out /= '1' then
                    wait until (tb_lockedB_out = '1');
                end if;
                if tb_lockedC_out /= '1' then
                    wait until (tb_lockedC_out = '1');
                end if;

                -- Phase shift clock A for 270 degree
                for i in 0 to 56 / 4 * 1 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnA_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnA_in    <= '0';
                    wait until (rising_edge(tb_psDoneA_out)); -- Wait for end of phase shift
                end loop;

                -- Phase shift clock B for 300 degree
                for i in 0 to 56 / 6 * 2 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnB_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnB_in    <= '0';
                    wait until (rising_edge(tb_psDoneB_out)); -- Wait for end of phase shift
                end loop;

                -- Wait two cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Period of the clock
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check(period_v = (2147 ps), "Clock period of clock glitch is wrong. Expected = " & time'image((2138 ps)) & ", was = " & time'image(period_v));

                -- Second Period of the clock
                wait until (rising_edge(tb_clk_out)); -- Wait for  active clock edge
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check(period_v = (2853 ps), "Clock period of clock glitch is wrong. Expected = " & time'image((2853 ps)) & ", was = " & time'image(period_v));

            -- Test bypass
            elsif run("test_bypass") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable all clocks
                tb_enA_in      <= '1';
                tb_enB_in      <= '1';
                tb_enC_in      <= '1';
                tb_enGlitch_in <= '0';

                -- Wait for the clocks to become locked
                if tb_lockedA_out /= '1' then
                    wait until (tb_lockedA_out = '1');
                end if;
                if tb_lockedB_out /= '1' then
                    wait until (tb_lockedB_out = '1');
                end if;
                if tb_lockedC_out /= '1' then
                    wait until (tb_lockedC_out = '1');
                end if;

                -- Phase shift clock A for 270 degree
                for i in 0 to 56 / 4 * 3 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnA_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnA_in    <= '0';
                    wait until (rising_edge(tb_psDoneA_out)); -- Wait for end of phase shift
                end loop;

                -- Phase shift clock B for 300 degree
                for i in 0 to 56 / 6 * 5 * integer(CLKFBOUT_MULT_C) - 1 loop
                    -- Initiate a phase shift
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psIncDec_in <= '0';
                    tb_psEnB_in    <= '1';
                    wait until (rising_edge(tb_clk_in)); -- Wait for clock edge
                    tb_psEnB_in    <= '0';
                    wait until (rising_edge(tb_psDoneB_out)); -- Wait for end of phase shift
                end loop;

                -- Wait two cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Period of the clock
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock bypass is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));

                -- Second Period of the clock
                wait until (rising_edge(tb_clk_out)); -- Wait for  active clock edge
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check((T_CLKCHECK_C - period_v <= (1 ps)) and (T_CLKCHECK_C - period_v >= (-1 ps)), "Clock period of clock bypass is wrong. Expected = " & time'image(T_CLKCHECK_C) & ", was = " & time'image(period_v));

            -- Test reconfiguration clkA
            elsif run("test_reconfiguration_clockA") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable all clocks
                tb_enA_in      <= '1';
                tb_enB_in      <= '0';
                tb_enC_in      <= '0';
                tb_enGlitch_in <= '1';
                tb_DRPSEL_in   <= "00";

                -- Wait for the clocks to become locked
                if tb_lockedA_out /= '1' then
                    wait until (tb_lockedA_out = '1');
                end if;
                
                -- Wait some cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Start the reconfiguration
                tb_drpStart <= '1';

                -- Wait one cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge                

                -- Wait for the reconfiguration to finish
                tb_drpStart <= '0';
                wait until (tb_drpReady = '1'); -- Wait for ready signal
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Period of the clock
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check(((T_CLKCHECK_C * 2.0) - period_v <= (1 ps)) and ((T_CLKCHECK_C * 2.0) - period_v >= (-1 ps)), "Clock period of reconfigured clock A is wrong. Expected = " & time'image((T_CLKCHECK_C * 2.0)) & ", was = " & time'image(period_v));

            -- Test reconfiguration clkB
            elsif run("test_reconfiguration_clockB") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable all clocks
                tb_enA_in      <= '0';
                tb_enB_in      <= '1';
                tb_enC_in      <= '0';
                tb_enGlitch_in <= '1';
                tb_DRPSEL_in   <= "01";

                -- Wait for the clocks to become locked
                if tb_lockedB_out /= '1' then
                    wait until (tb_lockedB_out = '1');
                end if;
                
                -- Wait some cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Start the reconfiguration
                tb_drpStart <= '1';

                -- Wait one cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge                

                -- Wait for the reconfiguration to finish
                tb_drpStart <= '0';
                wait until (tb_drpReady = '1'); -- Wait for ready signal
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Period of the clock
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check(((T_CLKCHECK_C * 2.0) - period_v <= (1 ps)) and ((T_CLKCHECK_C * 2.0) - period_v >= (-1 ps)), "Clock period of reconfigured clock B is wrong. Expected = " & time'image((T_CLKCHECK_C * 2.0)) & ", was = " & time'image(period_v));
            
            -- Test reconfiguration clkC
            elsif run("test_reconfiguration_clockC") then
                -- Perform a reset
                tb_reset_in <= '1';     -- Perform a reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                tb_reset_in <= '0';     -- Clear the reset
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Enable all clocks
                tb_enA_in      <= '0';
                tb_enB_in      <= '0';
                tb_enC_in      <= '1';
                tb_enGlitch_in <= '0';
                tb_DRPSEL_in   <= "10";

                -- Wait for the clocks to become locked
                if tb_lockedC_out /= '1' then
                    wait until (tb_lockedC_out = '1');
                end if;
                
                -- Wait some cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Start the reconfiguration
                tb_drpStart <= '1';

                -- Wait one cycles
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge                

                -- Wait for the reconfiguration to finish
                tb_drpStart <= '0';
                wait until (tb_drpReady = '1'); -- Wait for ready signal
                wait until (rising_edge(tb_clk_in)); -- Wait for the first active clock edge

                -- Period of the clock
                calcPeriod(tb_clk_out, period_v);

                -- Check period
                check(((T_CLKCHECK_C * 2.0) - period_v <= (1 ps)) and ((T_CLKCHECK_C * 2.0) - period_v >= (-1 ps)), "Clock period of reconfigured clock C is wrong. Expected = " & time'image((T_CLKCHECK_C * 2.0)) & ", was = " & time'image(period_v));
            
            end if;
        end loop;

        -- Cleanup the VUnit Runner
        test_runner_cleanup(runner);

        -- Wait forever
        wait;
    end process proc_applyTestvector;
end tb;
