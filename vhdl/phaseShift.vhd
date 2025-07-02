----------------------------------------------------------------------------------------------------
-- brief: This block does a phase shift
-- file: phaseshift.vhd
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
-- This block implements multiple averaging blocks
------------------------------------------------------------------------------------------------
entity phaseShift is
    generic(
        DATA_SIZE_G : positive := 16    -- Size of the data
    );
    port(
        -- Clock and reset
        clk_in        : in  std_logic;  -- Clock
        reset_in      : in  std_logic;  -- Reset

        -- Settings
        start_in      : in  std_logic;  -- Start Signal
        number_in     : in  std_logic_vector(DATA_SIZE_G - 1 downto 0); -- Number of shifts to perform
        running_out   : out std_logic;  -- Signalizes that a phase shift is performed.
        psEnBypass_in : in  std_logic;  -- Bypass for calibration
        psEn_out      : out std_logic;
        psDone_in     : in  std_logic
    );
end entity phaseShift;

------------------------------------------------------------------------------------------------
-- This is the behavioral architecture of the **multiAverage** block.
------------------------------------------------------------------------------------------------
architecture behavioral of phaseShift is

    --------------------------------------------------------------------------------------------
    -- Internal Types
    --------------------------------------------------------------------------------------------

    -- Define the different states of the statemachine  
    type fsmState_t is (IDLE, START, WAITSHIFT);
    -- State type for the statemachine.
    type states_t is record
        state   : fsmState_t;           -- State
        counter : unsigned(DATA_SIZE_G - 1 downto 0); -- Counter for the sum
        number  : unsigned(DATA_SIZE_G - 1 downto 0); -- Sum for all values
    end record states_t;

    --------------------------------------------------------------------------------------------
    -- Init State of the statemachine.
    --------------------------------------------------------------------------------------------
    constant INIT_STATE_C : states_t := (
        state   => IDLE,
        counter => (others => '0'),
        number  => (others => '0')
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
    nextStateLogic : process(p, start_in, number_in, psDone_in, psEnBypass_in)
    begin
        -- Default assignements
        n <= p;

        -- Default outputs
        running_out <= '1';
        psEn_out    <= psEnBypass_in;

        -- Conditional assignements     
        case p.state is
            ----------------------------------------------------------------------------------------
            when IDLE =>
                ------------------------------------------------------------------------------------
                -- Check if the Start port is set
                if start_in = '1' then
                    -- Signal start_in is set
                    n.state <= START;
                end if;

                -- Reset the counter
                n.counter <= (others => '0');

                -- Save the values
                n.number <= unsigned(number_in);

                -- We are in idle state
                running_out <= '0';

            ----------------------------------------------------------------------------------------
            when START =>
                ------------------------------------------------------------------------------------
                -- Start the phase shift
                psEn_out <= '1';

                -- Change to the wait state
                n.state <= WAITSHIFT;

            ----------------------------------------------------------------------------------------
            when WAITSHIFT =>
                ------------------------------------------------------------------------------------
                -- Do no set the enable 
                psEn_out <= '0';

                -- Check if the wait is over
                if psDone_in = '1' then
                    -- Increment the counter
                    n.counter <= p.counter + 1;

                    -- Check if we reached the end
                    if (p.counter >= p.number) then
                        -- Maximum shift performed
                        n.state <= IDLE;

                    else
                        -- Next shift
                        n.state <= START;
                    end if;
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
