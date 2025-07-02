----------------------------------------------------------------------------------------------------
-- brief: This block implements the gltich insert logic for the glitchgenerator.
-- file: clockInsert.vhd
-- author: Lukas Leuenberger
----------------------------------------------------------------------------------------------------
-- Copyright (c) 2025 by OST - Eastern Switzerland University of Applied Sciences (www.ost.ch)
-- This code is licensed under the MIT license (see LICENSE for details)
----------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
-- Library declarations
------------------------------------------------------------------------------------------------
library ieee;
-- This package defines the basic std_logic data types and a few functions.
use ieee.std_logic_1164.all;
-- This package provides arithmetic functions for vectors.  
use ieee.numeric_std.all;
-- This package provides functions for the calculation with real values.
use ieee.math_real.all;
-- Vivado Components library
library unisim;
-- This package contains the vivado components
use unisim.vcomponents.all;
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
-- This block implements the gltich insert logic for the glitchgenerator.
------------------------------------------------------------------------------------------------
entity clockInsert is
    generic(
        MAX_COUNT_VAL_G : positive := 128 -- Defines the maximum value for the ports **delay_in** and **repetition_in**.
    );
    port(
        -- Clock and reset
        clk_in           : in  std_logic; -- Clock
        reset_in         : in  std_logic; -- Reset

        -- Settings
        start_in         : in  std_logic; -- Start Signal
        stop_in          : in  std_logic; -- Stop Signal
        repeat_in        : in  std_logic; -- Signalizes if single shot (**repeat_in** = '0') or multishot (**repeat_in** = '1') shall be used.
        delay_in         : in  std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0); -- Defines the delay from start until a glitch is inserted.
        repetition_in    : in  std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0); -- Defines the number of repeated glitches that are inserted.
        bypass_in        : in  std_logic; -- Bypass the normal clock (**bypass_in** = '1') when the **clockInsert** block is not running.
        running_out      : out std_logic; -- Signalizes that glitches are generated.
        insertGlitch_out : out std_logic; -- Control signal for the **clockLogic** block. When = '1', a glitch is inserted.
        trigger_out      : out std_logic -- Signalizes the start of a new litch insertion
    );
end entity clockInsert;

------------------------------------------------------------------------------------------------
-- This is the behavioral architecture of the **clockInsert** block.
------------------------------------------------------------------------------------------------
architecture behavioral of clockInsert is
    --------------------------------------------------------------------------------------------
    -- Internal Types
    --------------------------------------------------------------------------------------------

    -- Define the different states of the statemachine  
    type fsmState_t is (IDLE, COUNTDELAY, INSERTGLITCH, WAITSTART);
    -- State type for the statemachine.
    type states_t is record
        state             : fsmState_t; -- State
        counterDelay      : signed(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) downto 0); -- Counter for the delay until a glicht is inserted.
        counterRepetition : signed(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) downto 0); -- Counter for the glitch repetitions.
        repeat            : std_logic;  -- Single shot or multishot mode.
        delay             : signed(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) downto 0); -- Defines the delay from start until a glitch is inserted.
        repetition        : signed(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) downto 0); -- Defines the number of repeated glitches that are inserted.
    end record states_t;

    --------------------------------------------------------------------------------------------
    -- Init State of the statemachine.
    --------------------------------------------------------------------------------------------
    constant INIT_STATE_C : states_t := (
        state             => IDLE,
        counterDelay      => (others => '0'),
        counterRepetition => (others => '0'),
        repeat            => '0',
        delay             => (others => '0'),
        repetition        => (others => '0')
    );

    --------------------------------------------------------------------------------------------
    -- Internal signals
    --------------------------------------------------------------------------------------------
    signal p : states_t := INIT_STATE_C; -- Preset State Signal of the statemachine.
    signal n : states_t := INIT_STATE_C; -- Next State Signal of the statemachine.

begin
    ------------------------------------------------------------------------------------------------
    -- This process controls the next state logic of the statemachine.
    ------------------------------------------------------------------------------------------------
    nextStateLogic : process(p, delay_in, repeat_in, repetition_in, start_in, stop_in, bypass_in)
    begin
        -- Default assignements
        n <= p;

        running_out      <= '1';
        insertGlitch_out <= bypass_in;
        trigger_out      <= '0';

        -- Conditional assignements     
        case p.state is
            ----------------------------------------------------------------------------------------
            when IDLE =>
                ------------------------------------------------------------------------------------
                -- Check if the Start port is set
                if start_in = '1' and to_integer(unsigned(repetition_in)) > 0 then
                    if (to_integer(unsigned(delay_in)) = 0) then
                        -- Signal start_in is set and delay_in is zero
                        n.state <= INSERTGLITCH;
                    else
                        -- Signal start_in is set and delay_in is not zero
                        n.state <= COUNTDELAY;
                    end if;
                end if;

                -- Reset the counter
                n.counterDelay      <= signed("0" & delay_in) - 2;
                n.counterRepetition <= signed("0" & repetition_in) - 2;

                -- Save the values
                n.repeat     <= repeat_in;
                n.delay      <= signed("0" & delay_in) - 2;
                n.repetition <= signed("0" & repetition_in) - 2;

                -- We are in idle state
                running_out <= '0';

            ----------------------------------------------------------------------------------------
            when COUNTDELAY =>
                ------------------------------------------------------------------------------------
                -- Check if we reached the end
                if (p.counterDelay < 0) then
                    -- Delay has been reached
                    n.state <= INSERTGLITCH;
                end if;

                -- Switch to normal clock
                insertGlitch_out <= '0';

                -- Reset the counter
                n.counterRepetition <= p.repetition;

                -- Increment the counter
                n.counterDelay <= p.counterDelay - 1;

                -- Check if the Stop port is set
                if stop_in = '1' then
                    -- Signal stop_in is set.
                    n.state <= IDLE;
                end if;

            ----------------------------------------------------------------------------------------
            when INSERTGLITCH =>
                ------------------------------------------------------------------------------------
                trigger_out <= '1';

                -- Check if we reached the end
                if (p.counterRepetition < 0) then
                    -- Change the state
                    if (p.repeat = '1') then
                        if (p.delay + 2 > 0) then
                            -- Number of glitches has been reached and multishot glitch was requested
                            n.state <= COUNTDELAY;
                        end if;
                    else
                        -- Single shot glitch was requested
                        -- Check that start is gone to zero --> In case of much faster clock 
                        if start_in = '1' then
                            n.state <= WAITSTART;
                        else
                            n.state <= IDLE;
                        end if;
                    end if;
                end if;

                -- Reset the counter
                n.counterDelay <= p.delay;

                -- Increment the counter
                n.counterRepetition <= p.counterRepetition - 1;

                -- Switch to glitch clock
                insertGlitch_out <= '1';

                -- Check if the Stop port is set
                if stop_in = '1' then
                    -- Signal stop_in is set.
                    n.state <= IDLE;
                end if;

            ----------------------------------------------------------------------------------------
            when WAITSTART =>
                ------------------------------------------------------------------------------------
                if start_in = '0' then
                    -- Change back to start state
                    n.state <= IDLE;
                end if;
        end case;
    end process nextStateLogic;

    ------------------------------------------------------------------------------------------------
    -- This process controls the stateregister of the statemachine.
    ------------------------------------------------------------------------------------------------
    stateRegister : process(clk_in)
    begin
        if rising_edge(clk_in) then
            if (reset_in = '1') then
                p <= INIT_STATE_C;
            else
                p <= n;
            end if;
        end if;
    end process stateRegister;

end behavioral;
