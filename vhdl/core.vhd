----------------------------------------------------------------------------------------------------
-- brief: Implements a simple core for the clock glitch generator.
-- file: core.vhd
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
-- This package provides functions for the calcualtion with real values.
use ieee.math_real.all;
-- Vivado Components library
library unisim;
-- This package contains the iobuf component.
use unisim.vcomponents.all;
-- Vivado XPM library
library xpm;
use xpm.vcomponents.all;

------------------------------------------------------------------------------------------------
-- Entity declarations
------------------------------------------------------------------------------------------------
entity core is
    generic(
        MAX_COUNT_VAL_G        : integer  := 1024;
        MAX_PHASE_SHIFT_SIZE_G : positive := 16;
        CLK_FREQ_G             : integer  := 100; -- Frequency of input clock
        CLKFBOUT_MULT_G        : real     := 12.0; -- Multiplicator for clock feedback   
        DIVCLK_DIVIDE_G        : integer  := 1; -- Divdider of input clock 
        CLKOUT_DIVIDE_G        : real     := 4.0
    );
    port(
        -----------------------------------------------------------------------------------------------
        -- Clocks and reset 
        -----------------------------------------------------------------------------------------------
        clk_in                   : in  std_logic; -- Clock
        reset_in                 : in  std_logic; -- Reset
        clk_out                  : out std_logic; -- Clock with glitches out
        -----------------------------------------------------------------------------------------------
        -- Clock Glitch Ports
        -----------------------------------------------------------------------------------------------
        trigger_out              : out std_logic;
        startClockGlitch_in      : in  std_logic;
        stopClockGlitch_in       : in  std_logic;
        repeatClockGlitch_in     : in  std_logic;
        delayClockGlitch_in      : in  std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0);
        repetitionClockGlitch_in : in  std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0);
        clockGlitchRunning_out   : out std_logic;
        -----------------------------------------------------------------------------------------------
        -- Calibration and phase shift ports
        -----------------------------------------------------------------------------------------------
        clockLogicLockedA_out    : out std_logic;
        clockLogicLockedB_out    : out std_logic;
        clockLogicLockedC_out    : out std_logic;
        clockLogicAPSRunning_out : out std_logic;
        clockLogicBPSRunning_out : out std_logic;
        clockLogicCPSRunning_out : out std_logic;
        clockLogicEnable_in      : in  std_logic_vector(2 downto 0);
        clockLogicIncDec_in      : in  std_logic;
        clockLogicEnA_in         : in  std_logic;
        clockLogicEnB_in         : in  std_logic;
        clockLogicEnC_in         : in  std_logic;
        bypassClockGlitch_in     : in  std_logic;
        drpSel_in                : in  std_logic_vector(1 downto 0);
        drpStart_in              : in  std_logic;
        clockLogicPSNumber_in    : in  std_logic_vector(MAX_PHASE_SHIFT_SIZE_G - 1 downto 0);
        clockLogicAPSStart_in    : in  std_logic;
        clockLogicBPSStart_in    : in  std_logic;
        clockLogicCPSStart_in    : in  std_logic;
        S2_CLKFBOUT_MULT_in      : in  std_logic_vector(15 downto 8); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKFBOUT_PHASE_in     : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKFBOUT_FRAC_in      : in  std_logic_vector(25 downto 16); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKFBOUT_FRAC_EN_in   : in  std_logic; -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_DIVCLK_DIVIDE_in      : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT0_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT0_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT0_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT0_FRAC_in       : in  std_logic_vector(17 downto 8); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT0_FRAC_EN_in    : in  std_logic; -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT1_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT1_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT1_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT2_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT2_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT2_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT3_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT3_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT3_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT4_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT4_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT4_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT5_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT5_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT5_DUTY_in       : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT6_DIVIDE_in     : in  std_logic_vector(7 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT6_PHASE_in      : in  std_logic_vector(31 downto 0); -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
        S2_CLKOUT6_DUTY_in       : in  std_logic_vector(31 downto 0) -- @suppress "Naming convention violation: input port name should match pattern '[a-zA-Z0-9]*_in'"
    );
end core;

------------------------------------------------------------------------------------------------
-- Architecture
------------------------------------------------------------------------------------------------
architecture behavioral of core is
    ---------------------------------------------------------------------------
    --  Internal constants
    ---------------------------------------------------------------------------
    constant CLKIN1_PERIOD_C : real := 1.0 / real(CLK_FREQ_G) * 1000.0; -- Period of port **clk_in** in ns

    ---------------------------------------------------------------------------
    --  Internal signals
    ---------------------------------------------------------------------------
    signal clkGlitch : std_logic;
    signal clkC      : std_logic;

    signal clockLogicDoneACalib : std_logic;
    signal clockLogicDoneBCalib : std_logic;
    signal clockLogicDoneCCalib : std_logic;
    signal clockLogicEnAPS      : std_logic;
    signal clockLogicEnBPS      : std_logic;
    signal clockLogicEnCPS      : std_logic;

    signal clockGlitchInsert : std_logic;

    signal resetC                 : std_logic;
    signal startClockGlitchC      : std_logic;
    signal stopClockGlitchC       : std_logic;
    signal repeatClockGlitchC     : std_logic;
    signal delayClockGlitchC      : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0);
    signal repetitionClockGlitchC : std_logic_vector(integer(ceil(log2(real(MAX_COUNT_VAL_G)))) - 1 downto 0);
    signal bypassClockGlitchC     : std_logic;
    signal clockGlitchRunningC    : std_logic;

    signal drpDo        : std_logic_vector(15 downto 0);
    signal drpDrdy      : std_logic;
    signal drpDaddr     : std_logic_vector(6 downto 0);
    signal drpDclk      : std_logic;
    signal drpDen       : std_logic;
    signal drpDi        : std_logic_vector(15 downto 0);
    signal drpDwe       : std_logic;
    signal drpReset     : std_logic;
    signal drpLocked    : std_logic;
    signal drpReady     : std_logic;
    signal drpReadySave : std_logic;

begin
    ---------------------------------------------------------------------------
    --  clock Logic
    ---------------------------------------------------------------------------
    inst_clockLogic : entity work.clockLogic
        generic map(
            CLKIN1_PERIOD_G => CLKIN1_PERIOD_C,
            CLKFBOUT_MULT_G => CLKFBOUT_MULT_G,
            CLKOUT_DIVIDE_G => CLKOUT_DIVIDE_G,
            DIVCLK_DIVIDE_G => DIVCLK_DIVIDE_G
        )
        port map(
            clk_in        => clk_in,
            clk_out       => clkGlitch,
            clkC_out      => clkC,
            reset_in      => reset_in,
            psIncDec_in   => clockLogicIncDec_in,
            psEnA_in      => clockLogicEnAPS,
            psDoneA_out   => clockLogicDoneACalib,
            lockedA_out   => clockLogicLockedA_out,
            psEnB_in      => clockLogicEnBPS,
            psDoneB_out   => clockLogicDoneBCalib,
            lockedB_out   => clockLogicLockedB_out,
            psEnC_in      => clockLogicEnCPS,
            psDoneC_out   => clockLogicDoneCCalib,
            lockedC_out   => clockLogicLockedC_out,
            enA_in        => clockLogicEnable_in(0),
            enB_in        => clockLogicEnable_in(1),
            enC_in        => clockLogicEnable_in(2),
            enGlitch_in   => clockGlitchInsert,
            DO_out        => drpDo,
            DRDY_out      => drpDrdy,
            DRPLOCKED_out => drpLocked,
            DADDR_in      => drpDaddr,
            DCLK_in       => drpDclk,
            DEN_in        => drpDen,
            DI_in         => drpDi,
            DWE_in        => drpDwe,
            DRPRESET_in   => drpReset,
            DRPSEL_in     => drpSel_in
        );

    inst_phaseShiftA : entity work.phaseShift
        generic map(
            DATA_SIZE_G => MAX_PHASE_SHIFT_SIZE_G
        )
        port map(
            clk_in        => clk_in,
            reset_in      => reset_in,
            start_in      => clockLogicAPSStart_in,
            number_in     => clockLogicPSNumber_in,
            running_out   => clockLogicAPSRunning_out,
            psEnBypass_in => clockLogicEnA_in,
            psEn_out      => clockLogicEnAPS,
            psDone_in     => clockLogicDoneACalib
        );

    inst_phaseShiftB : entity work.phaseShift
        generic map(
            DATA_SIZE_G => MAX_PHASE_SHIFT_SIZE_G
        )
        port map(
            clk_in        => clk_in,
            reset_in      => reset_in,
            start_in      => clockLogicBPSStart_in,
            number_in     => clockLogicPSNumber_in,
            running_out   => clockLogicBPSRunning_out,
            psEnBypass_in => clockLogicEnB_in,
            psEn_out      => clockLogicEnBPS,
            psDone_in     => clockLogicDoneBCalib
        );

    inst_phaseShiftC : entity work.phaseShift
        generic map(
            DATA_SIZE_G => MAX_PHASE_SHIFT_SIZE_G
        )
        port map(
            clk_in        => clk_in,
            reset_in      => reset_in,
            start_in      => clockLogicCPSStart_in,
            number_in     => clockLogicPSNumber_in,
            running_out   => clockLogicCPSRunning_out,
            psEnBypass_in => clockLogicEnC_in,
            psEn_out      => clockLogicEnCPS,
            psDone_in     => clockLogicDoneCCalib
        );

    ---------------------------------------------------------------------------
    -- DRP
    ---------------------------------------------------------------------------
    inst_drp : entity work.clk_wiz_0_mmcm_drp
        generic map(
            S1_CLKFBOUT_MULT    => CLKFBOUT_MULT_G,
            S1_CLKFBOUT_PHASE   => 0,
            S1_CLKFBOUT_FRAC    => 0,
            S1_CLKFBOUT_FRAC_EN => 0,
            S1_BANDWIDTH        => "HIGH",
            S1_DIVCLK_DIVIDE    => DIVCLK_DIVIDE_G,
            S1_CLKOUT0_DIVIDE   => CLKOUT_DIVIDE_G,
            S1_CLKOUT0_PHASE    => 0,
            S1_CLKOUT0_DUTY     => 50000,
            S1_CLKOUT0_FRAC     => 0,
            S1_CLKOUT0_FRAC_EN  => 0
        )
        port map(
            S2_CLKFBOUT_MULT    => S2_CLKFBOUT_MULT_in,
            S2_CLKFBOUT_PHASE   => S2_CLKFBOUT_PHASE_in,
            S2_CLKFBOUT_FRAC    => S2_CLKFBOUT_FRAC_in,
            S2_CLKFBOUT_FRAC_EN => S2_CLKFBOUT_FRAC_EN_in,
            S2_DIVCLK_DIVIDE    => S2_DIVCLK_DIVIDE_in,
            S2_CLKOUT0_DIVIDE   => S2_CLKOUT0_DIVIDE_in,
            S2_CLKOUT0_PHASE    => S2_CLKOUT0_PHASE_in,
            S2_CLKOUT0_DUTY     => S2_CLKOUT0_DUTY_in,
            S2_CLKOUT0_FRAC     => S2_CLKOUT0_FRAC_in,
            S2_CLKOUT0_FRAC_EN  => S2_CLKOUT0_FRAC_EN_in,
            S2_CLKOUT1_DIVIDE   => S2_CLKOUT1_DIVIDE_in,
            S2_CLKOUT1_PHASE    => S2_CLKOUT1_PHASE_in,
            S2_CLKOUT1_DUTY     => S2_CLKOUT1_DUTY_in,
            S2_CLKOUT2_DIVIDE   => S2_CLKOUT2_DIVIDE_in,
            S2_CLKOUT2_PHASE    => S2_CLKOUT2_PHASE_in,
            S2_CLKOUT2_DUTY     => S2_CLKOUT2_DUTY_in,
            S2_CLKOUT3_DIVIDE   => S2_CLKOUT3_DIVIDE_in,
            S2_CLKOUT3_PHASE    => S2_CLKOUT3_PHASE_in,
            S2_CLKOUT3_DUTY     => S2_CLKOUT3_DUTY_in,
            S2_CLKOUT4_DIVIDE   => S2_CLKOUT4_DIVIDE_in,
            S2_CLKOUT4_PHASE    => S2_CLKOUT4_PHASE_in,
            S2_CLKOUT4_DUTY     => S2_CLKOUT4_DUTY_in,
            S2_CLKOUT5_DIVIDE   => S2_CLKOUT5_DIVIDE_in,
            S2_CLKOUT5_PHASE    => S2_CLKOUT5_PHASE_in,
            S2_CLKOUT5_DUTY     => S2_CLKOUT5_DUTY_in,
            S2_CLKOUT6_DIVIDE   => S2_CLKOUT6_DIVIDE_in,
            S2_CLKOUT6_PHASE    => S2_CLKOUT6_PHASE_in,
            S2_CLKOUT6_DUTY     => S2_CLKOUT6_DUTY_in,
            LOAD                => '1',
            SADDR               => '1',
            SEN                 => drpStart_in,
            SCLK                => clk_in,
            RST                 => reset_in,
            SRDY                => drpReady,
            DO                  => drpDo,
            DRDY                => drpDrdy,
            LOCKED              => drpLocked,
            DWE                 => drpDwe,
            DEN                 => drpDen,
            DADDR               => drpDaddr,
            DI                  => drpDi,
            DCLK                => drpDclk,
            RST_MMCM_PLL        => drpReset
        );

    ---------------------------------------------------------------------------
    -- Generate drp ready signal
    ---------------------------------------------------------------------------   
    proc_drpReadyReg : process(clk_in)
    begin
        if rising_edge(clk_in) then
            if reset_in = '1' then
                drpReadySave <= '0';
            else
                if (drpStart_in = '1') then
                    drpReadySave <= '0';
                elsif (drpReady = '1') then
                    drpReadySave <= '1';
                else
                    drpReadySave <= drpReadySave;
                end if;
            end if;
        end if;
    end process;

    ---------------------------------------------------------------------------
    --  clock insert Logic
    ---------------------------------------------------------------------------
    inst_clockInsert : entity work.clockInsert
        generic map(
            MAX_COUNT_VAL_G => MAX_COUNT_VAL_G
        )
        port map(
            clk_in           => clkC,
            reset_in         => resetC,
            start_in         => startClockGlitchC,
            stop_in          => stopClockGlitchC,
            repeat_in        => repeatClockGlitchC,
            delay_in         => delayClockGlitchC,
            repetition_in    => repetitionClockGlitchC,
            bypass_in        => bypassClockGlitchC,
            running_out      => clockGlitchRunningC,
            insertGlitch_out => clockGlitchInsert,
            trigger_out      => trigger_out
        );

    clk_out <= clkGlitch;

    ---------------------------------------------------------------------------
    --  CDC
    ---------------------------------------------------------------------------
    inst_running : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clkC,
            src_in   => clockGlitchRunningC,
            dest_clk => clk_in,
            dest_out => clockGlitchRunning_out
        );

    inst_delayClockGlitch : xpm_cdc_array_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1,
            WIDTH          => delayClockGlitch_in'length
        )
        port map(
            src_clk  => clk_in,
            src_in   => delayClockGlitch_in,
            dest_clk => clkC,
            dest_out => delayClockGlitchC
        );

    inst_repetitionClockGlitch : xpm_cdc_array_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1,
            WIDTH          => repetitionClockGlitch_in'length
        )
        port map(
            src_clk  => clk_in,
            src_in   => repetitionClockGlitch_in,
            dest_clk => clkC,
            dest_out => repetitionClockGlitchC
        );

    inst_reset : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clk_in,
            src_in   => reset_in,
            dest_clk => clkC,
            dest_out => resetC
        );

    inst_start : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clk_in,
            src_in   => startClockGlitch_in,
            dest_clk => clkC,
            dest_out => startClockGlitchC
        );

    inst_stop : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clk_in,
            src_in   => stopClockGlitch_in,
            dest_clk => clkC,
            dest_out => stopClockGlitchC
        );

    inst_repeat : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clk_in,
            src_in   => repeatClockGlitch_in,
            dest_clk => clkC,
            dest_out => repeatClockGlitchC
        );

    inst_bypass : xpm_cdc_single
        generic map(
            DEST_SYNC_FF   => 2,
            INIT_SYNC_FF   => 0,
            SIM_ASSERT_CHK => 0,
            SRC_INPUT_REG  => 1
        )
        port map(
            src_clk  => clk_in,
            src_in   => bypassClockGlitch_in,
            dest_clk => clkC,
            dest_out => bypassClockGlitchC
        );

end behavioral;
