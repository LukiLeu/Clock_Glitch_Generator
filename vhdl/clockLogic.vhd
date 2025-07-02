----------------------------------------------------------------------------------------------------
-- brief: Implement the clock logic for the glitch generator.
-- file: glitchgenerator.vhd
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
-- This block implements the required clocking ressources for the glitch generator.
------------------------------------------------------------------------------------------------
entity clockLogic is
    generic(
        CLKIN1_PERIOD_G : real    := 0.000; -- Period of port **clk_in**
        CLKFBOUT_MULT_G : real    := 11.000; -- Multiplicator for clock feedback
        CLKOUT_DIVIDE_G : real    := 11.000; -- Divider for clock
        DIVCLK_DIVIDE_G : integer := 1  -- Divider of input clock
    );
    port(
        -- Clock and reset
        clk_in        : in  std_logic;  -- Clock
        clk_out       : out std_logic;  -- Output clock that contains glitch
        clkC_out      : out std_logic;  -- Output clock C
        reset_in      : in  std_logic;  -- Reset 

        -- Port for Clock A and B
        psIncDec_in   : in  std_logic;  -- Phase Shift: Select increment or decrement

        -- Ports for Clock A
        psEnA_in      : in  std_logic;  -- Phase Shift: Perform a phase shift in MMCM A
        psDoneA_out   : out std_logic;  -- Phase Shift: Phase shift for MMCM A finished        
        lockedA_out   : out std_logic;  -- MMCM A is locked 

        -- Ports for Clock B
        psEnB_in      : in  std_logic;  -- Phase Shift: Perform a phase shift in MMCM B
        psDoneB_out   : out std_logic;  -- Phase Shift: Phase shift for MMCM B finished        
        lockedB_out   : out std_logic;  -- MMCM B is locked

        -- Ports for Clock C      
        psEnC_in      : in  std_logic;  -- Phase Shift: Perform a phase shift in MMCM C
        psDoneC_out   : out std_logic;  -- Phase Shift: Phase shift for MMCM C finished  
        lockedC_out   : out std_logic;  -- MMCM C is locked

        -- Enable Signals
        enA_in        : in  std_logic;  -- Enable MMCM A
        enB_in        : in  std_logic;  -- Enable MMCM B
        enC_in        : in  std_logic;  -- Enable the input clock (clock C)
        enGlitch_in   : in  std_logic;  -- Enable the glitch generation. if disabled clock C is fordwarded to port **clk_out**.

        -- DRP Interface
        DO_out        : out std_logic_vector(15 downto 0);
        DRDY_out      : out std_logic;
        DRPLOCKED_out : out std_logic;
        DADDR_in      : in  std_logic_vector(6 downto 0);
        DCLK_in       : in  std_logic;
        DEN_in        : in  std_logic;
        DI_in         : in  std_logic_vector(15 downto 0);
        DWE_in        : in  std_logic;
        DRPRESET_in   : in  std_logic;
        DRPSEL_in     : in  std_logic_vector(1 downto 0)
    );
end entity clockLogic;

------------------------------------------------------------------------------------------------
-- This is the behavioral architecture of the **clockLogic** block.
------------------------------------------------------------------------------------------------
architecture behavioral of clockLogic is
    -----------------------------------------------------------------------------------------------
    -- Internal constants
    -----------------------------------------------------------------------------------------------

    -----------------------------------------------------------------------------------------------
    -- Internal alias
    -----------------------------------------------------------------------------------------------

    -----------------------------------------------------------------------------------------------
    -- Internal types
    -----------------------------------------------------------------------------------------------

    -----------------------------------------------------------------------------------------------
    -- Internal signals
    -----------------------------------------------------------------------------------------------
    signal clkFBA      : std_logic;     -- Clock Feedback of MMCM A
    signal clkA        : std_logic;     -- Clock of MMCM A
    signal clkFBB      : std_logic;     -- Clock Feedback of MMCM B
    signal clkB        : std_logic;     -- Clock of MMCM B
    signal clkGlitch   : std_logic;     -- Glitch Signal that will be inserted into clock C
    signal clkC        : std_logic;     -- Guarded clock C
    signal clknC       : std_logic;     -- Guarded inverse of clock C
    signal clkGlitchEn : std_logic;     -- Guarded clock Glitch
    signal clkFBC      : std_logic;     -- Clock Feedback of MMCM C
    signal clkCO       : std_logic;     -- Clock of MMCM C
    signal clkCnO      : std_logic;     -- Clock of MMCM Cn

    signal doA     : std_logic_vector(15 downto 0);
    signal drdyA   : std_logic;
    signal daddrA  : std_logic_vector(6 downto 0);
    signal dclkA   : std_logic;
    signal denA    : std_logic;
    signal diA     : std_logic_vector(15 downto 0);
    signal dweA    : std_logic;
    signal resetA  : std_logic;
    signal lockedA : std_logic;
    signal doB     : std_logic_vector(15 downto 0);
    signal drdyB   : std_logic;
    signal daddrB  : std_logic_vector(6 downto 0);
    signal dclkB   : std_logic;
    signal denB    : std_logic;
    signal diB     : std_logic_vector(15 downto 0);
    signal dweB    : std_logic;
    signal resetB  : std_logic;
    signal lockedB : std_logic;
    signal doC     : std_logic_vector(15 downto 0);
    signal drdyC   : std_logic;
    signal daddrC  : std_logic_vector(6 downto 0);
    signal dclkC   : std_logic;
    signal denC    : std_logic;
    signal diC     : std_logic_vector(15 downto 0);
    signal dweC    : std_logic;
    signal resetC  : std_logic;
    signal lockedC : std_logic;

begin
    -----------------------------------------------------------------------------------------------
    -- DRP muxing
    -----------------------------------------------------------------------------------------------
    DO_out        <= doA when DRPSEL_in = "00" else
                     doB when DRPSEL_in = "01" else
                     doC;
    DRDY_out      <= drdyA when DRPSEL_in = "00" else
                     drdyB when DRPSEL_in = "01" else
                     drdyC;
    DRPLOCKED_out <= lockedA when DRPSEL_in = "00" else
                     lockedB when DRPSEL_in = "01" else
                     lockedC;
    lockedA_out   <= lockedA;
    lockedB_out   <= lockedB;
    lockedC_out   <= lockedC;
    daddrA        <= DADDR_in when DRPSEL_in = "00" else (others => '0');
    daddrB        <= DADDR_in when DRPSEL_in = "01" else (others => '0');
    daddrC        <= DADDR_in when DRPSEL_in = "10" else (others => '0');
    dclkA         <= DCLK_in when DRPSEL_in = "00" else '0';
    dclkB         <= DCLK_in when DRPSEL_in = "01" else '0';
    dclkC         <= DCLK_in when DRPSEL_in = "10" else '0';
    denA          <= DEN_in when DRPSEL_in = "00" else '0';
    denB          <= DEN_in when DRPSEL_in = "01" else '0';
    denC          <= DEN_in when DRPSEL_in = "10" else '0';
    diA           <= DI_in when DRPSEL_in = "00" else (others => '0');
    diB           <= DI_in when DRPSEL_in = "01" else (others => '0');
    diC           <= DI_in when DRPSEL_in = "10" else (others => '0');
    dweA          <= DWE_in when DRPSEL_in = "00" else '0';
    dweB          <= DWE_in when DRPSEL_in = "01" else '0';
    dweC          <= DWE_in when DRPSEL_in = "10" else '0';
    resetA        <= DRPRESET_in when DRPSEL_in = "00" else reset_in;
    resetB        <= DRPRESET_in when DRPSEL_in = "01" else reset_in;
    resetC        <= DRPRESET_in when DRPSEL_in = "10" else reset_in;

    -----------------------------------------------------------------------------------------------
    -- Instantiate the two clocking wizards to generate the glitch signal
    -----------------------------------------------------------------------------------------------

    -- Instantiate the MMCM A
    inst_clkWizA : MMCME2_ADV
        generic map(
            BANDWIDTH            => "HIGH",
            CLKFBOUT_MULT_F      => CLKFBOUT_MULT_G,
            CLKFBOUT_PHASE       => 0.0,
            CLKFBOUT_USE_FINE_PS => false,
            CLKIN1_PERIOD        => CLKIN1_PERIOD_G,
            CLKIN2_PERIOD        => 0.0,
            CLKOUT0_DIVIDE_F     => CLKOUT_DIVIDE_G,
            CLKOUT0_DUTY_CYCLE   => 0.5,
            CLKOUT0_PHASE        => 0.0,
            CLKOUT0_USE_FINE_PS  => true,
            CLKOUT1_DIVIDE       => 1,
            CLKOUT1_DUTY_CYCLE   => 0.5,
            CLKOUT1_PHASE        => 0.0,
            CLKOUT1_USE_FINE_PS  => false,
            CLKOUT2_DIVIDE       => 1,
            CLKOUT2_DUTY_CYCLE   => 0.5,
            CLKOUT2_PHASE        => 0.0,
            CLKOUT2_USE_FINE_PS  => false,
            CLKOUT3_DIVIDE       => 1,
            CLKOUT3_DUTY_CYCLE   => 0.5,
            CLKOUT3_PHASE        => 0.0,
            CLKOUT3_USE_FINE_PS  => false,
            CLKOUT4_CASCADE      => false,
            CLKOUT4_DIVIDE       => 1,
            CLKOUT4_DUTY_CYCLE   => 0.5,
            CLKOUT4_PHASE        => 0.0,
            CLKOUT4_USE_FINE_PS  => false,
            CLKOUT5_DIVIDE       => 1,
            CLKOUT5_DUTY_CYCLE   => 0.5,
            CLKOUT5_PHASE        => 0.0,
            CLKOUT5_USE_FINE_PS  => false,
            CLKOUT6_DIVIDE       => 1,
            CLKOUT6_DUTY_CYCLE   => 0.5,
            CLKOUT6_PHASE        => 0.0,
            CLKOUT6_USE_FINE_PS  => false,
            COMPENSATION         => "ZHOLD",
            DIVCLK_DIVIDE        => DIVCLK_DIVIDE_G,
            IS_CLKINSEL_INVERTED => '0',
            IS_PSEN_INVERTED     => '0',
            IS_PSINCDEC_INVERTED => '0',
            IS_PWRDWN_INVERTED   => '0',
            IS_RST_INVERTED      => '0',
            REF_JITTER1          => 0.0,
            REF_JITTER2          => 0.0,
            SS_EN                => "FALSE",
            SS_MODE              => "CENTER_HIGH",
            SS_MOD_PERIOD        => 10000,
            STARTUP_WAIT         => false
        )
        port map(
            CLKFBOUT     => clkFBA,
            CLKFBOUTB    => open,
            CLKFBSTOPPED => open,
            CLKINSTOPPED => open,
            CLKOUT0      => clkA,
            CLKOUT0B     => open,
            CLKOUT1      => open,
            CLKOUT1B     => open,
            CLKOUT2      => open,
            CLKOUT2B     => open,
            CLKOUT3      => open,
            CLKOUT3B     => open,
            CLKOUT4      => open,
            CLKOUT5      => open,
            CLKOUT6      => open,
            DO           => doA,
            DRDY         => drdyA,
            LOCKED       => lockedA,
            PSDONE       => psDoneA_out,
            CLKFBIN      => clkFBA,
            CLKIN1       => clk_in,
            CLKIN2       => '0',
            CLKINSEL     => '1',
            DADDR        => daddrA,
            DCLK         => dclkA,
            DEN          => denA,
            DI           => diA,
            DWE          => dweA,
            PSCLK        => clk_in,
            PSEN         => psEnA_in,
            PSINCDEC     => psIncDec_in,
            PWRDWN       => "not"(enA_in),
            RST          => resetA
        );

    -- Instantiate the MMCM B
    inst_clkWizB : MMCME2_ADV
        generic map(
            BANDWIDTH            => "HIGH",
            CLKFBOUT_MULT_F      => CLKFBOUT_MULT_G,
            CLKFBOUT_PHASE       => 0.0,
            CLKFBOUT_USE_FINE_PS => false,
            CLKIN1_PERIOD        => CLKIN1_PERIOD_G,
            CLKIN2_PERIOD        => 0.0,
            CLKOUT0_DIVIDE_F     => CLKOUT_DIVIDE_G,
            CLKOUT0_DUTY_CYCLE   => 0.5,
            CLKOUT0_PHASE        => 0.0,
            CLKOUT0_USE_FINE_PS  => true,
            CLKOUT1_DIVIDE       => 1,
            CLKOUT1_DUTY_CYCLE   => 0.5,
            CLKOUT1_PHASE        => 0.0,
            CLKOUT1_USE_FINE_PS  => false,
            CLKOUT2_DIVIDE       => 1,
            CLKOUT2_DUTY_CYCLE   => 0.5,
            CLKOUT2_PHASE        => 0.0,
            CLKOUT2_USE_FINE_PS  => false,
            CLKOUT3_DIVIDE       => 1,
            CLKOUT3_DUTY_CYCLE   => 0.5,
            CLKOUT3_PHASE        => 0.0,
            CLKOUT3_USE_FINE_PS  => false,
            CLKOUT4_CASCADE      => false,
            CLKOUT4_DIVIDE       => 1,
            CLKOUT4_DUTY_CYCLE   => 0.5,
            CLKOUT4_PHASE        => 0.0,
            CLKOUT4_USE_FINE_PS  => false,
            CLKOUT5_DIVIDE       => 1,
            CLKOUT5_DUTY_CYCLE   => 0.5,
            CLKOUT5_PHASE        => 0.0,
            CLKOUT5_USE_FINE_PS  => false,
            CLKOUT6_DIVIDE       => 1,
            CLKOUT6_DUTY_CYCLE   => 0.5,
            CLKOUT6_PHASE        => 0.0,
            CLKOUT6_USE_FINE_PS  => false,
            COMPENSATION         => "ZHOLD",
            DIVCLK_DIVIDE        => DIVCLK_DIVIDE_G,
            IS_CLKINSEL_INVERTED => '0',
            IS_PSEN_INVERTED     => '0',
            IS_PSINCDEC_INVERTED => '0',
            IS_PWRDWN_INVERTED   => '0',
            IS_RST_INVERTED      => '0',
            REF_JITTER1          => 0.0,
            REF_JITTER2          => 0.0,
            SS_EN                => "FALSE",
            SS_MODE              => "CENTER_HIGH",
            SS_MOD_PERIOD        => 10000,
            STARTUP_WAIT         => false
        )
        port map(
            CLKFBOUT     => clkFBB,
            CLKFBOUTB    => open,
            CLKFBSTOPPED => open,
            CLKINSTOPPED => open,
            CLKOUT0      => clkB,
            CLKOUT0B     => open,
            CLKOUT1      => open,
            CLKOUT1B     => open,
            CLKOUT2      => open,
            CLKOUT2B     => open,
            CLKOUT3      => open,
            CLKOUT3B     => open,
            CLKOUT4      => open,
            CLKOUT5      => open,
            CLKOUT6      => open,
            DO           => doB,
            DRDY         => drdyB,
            LOCKED       => lockedB,
            PSDONE       => psDoneB_out,
            CLKFBIN      => clkFBB,
            CLKIN1       => clk_in,
            CLKIN2       => '0',
            CLKINSEL     => '1',
            DADDR        => daddrB,
            DCLK         => dclkB,
            DEN          => denB,
            DI           => diB,
            DWE          => dweB,
            PSCLK        => clk_in,
            PSEN         => psEnB_in,
            PSINCDEC     => psIncDec_in,
            PWRDWN       => "not"(enB_in),
            RST          => resetB
        );

    -- Instantiate the MMCM C
    inst_clkWizC : MMCME2_ADV
        generic map(
            BANDWIDTH            => "HIGH",
            CLKFBOUT_MULT_F      => CLKFBOUT_MULT_G,
            CLKFBOUT_PHASE       => 0.0,
            CLKFBOUT_USE_FINE_PS => false,
            CLKIN1_PERIOD        => CLKIN1_PERIOD_G,
            CLKIN2_PERIOD        => 0.0,
            CLKOUT0_DIVIDE_F     => CLKOUT_DIVIDE_G,
            CLKOUT0_DUTY_CYCLE   => 0.5,
            CLKOUT0_PHASE        => 0.0,
            CLKOUT0_USE_FINE_PS  => true,
            CLKOUT1_DIVIDE       => 1,
            CLKOUT1_DUTY_CYCLE   => 0.5,
            CLKOUT1_PHASE        => 0.0,
            CLKOUT1_USE_FINE_PS  => false,
            CLKOUT2_DIVIDE       => 1,
            CLKOUT2_DUTY_CYCLE   => 0.5,
            CLKOUT2_PHASE        => 0.0,
            CLKOUT2_USE_FINE_PS  => false,
            CLKOUT3_DIVIDE       => 1,
            CLKOUT3_DUTY_CYCLE   => 0.5,
            CLKOUT3_PHASE        => 0.0,
            CLKOUT3_USE_FINE_PS  => false,
            CLKOUT4_CASCADE      => false,
            CLKOUT4_DIVIDE       => 1,
            CLKOUT4_DUTY_CYCLE   => 0.5,
            CLKOUT4_PHASE        => 0.0,
            CLKOUT4_USE_FINE_PS  => false,
            CLKOUT5_DIVIDE       => 1,
            CLKOUT5_DUTY_CYCLE   => 0.5,
            CLKOUT5_PHASE        => 0.0,
            CLKOUT5_USE_FINE_PS  => false,
            CLKOUT6_DIVIDE       => 1,
            CLKOUT6_DUTY_CYCLE   => 0.5,
            CLKOUT6_PHASE        => 0.0,
            CLKOUT6_USE_FINE_PS  => false,
            COMPENSATION         => "ZHOLD",
            DIVCLK_DIVIDE        => DIVCLK_DIVIDE_G,
            IS_CLKINSEL_INVERTED => '0',
            IS_PSEN_INVERTED     => '0',
            IS_PSINCDEC_INVERTED => '0',
            IS_PWRDWN_INVERTED   => '0',
            IS_RST_INVERTED      => '0',
            REF_JITTER1          => 0.0,
            REF_JITTER2          => 0.0,
            SS_EN                => "FALSE",
            SS_MODE              => "CENTER_HIGH",
            SS_MOD_PERIOD        => 10000,
            STARTUP_WAIT         => false
        )
        port map(
            CLKFBOUT     => clkFBC,
            CLKFBOUTB    => open,
            CLKFBSTOPPED => open,
            CLKINSTOPPED => open,
            CLKOUT0      => clkCO,
            CLKOUT0B     => clkCnO,
            CLKOUT1      => open,
            CLKOUT1B     => open,
            CLKOUT2      => open,
            CLKOUT2B     => open,
            CLKOUT3      => open,
            CLKOUT3B     => open,
            CLKOUT4      => open,
            CLKOUT5      => open,
            CLKOUT6      => open,
            DO           => doC,
            DRDY         => drdyC,
            LOCKED       => lockedC,
            PSDONE       => psDoneC_out,
            CLKFBIN      => clkFBC,
            CLKIN1       => clk_in,
            CLKIN2       => '0',
            CLKINSEL     => '1',
            DADDR        => daddrC,
            DCLK         => dclkC,
            DEN          => denC,
            DI           => diC,
            DWE          => dweC,
            PSCLK        => clk_in,
            PSEN         => psEnC_in,
            PSINCDEC     => psIncDec_in,
            PWRDWN       => '0',
            RST          => resetC
        );

    -- Output Clock C, required for glitch insert logic
    clkC_out <= clkA;

    -----------------------------------------------------------------------------------------------
    -- Generate the select signal for the clock glitching
    -----------------------------------------------------------------------------------------------
    inst_glitchSignal : BUFGCTRL
        generic map(
            CE_TYPE_CE0         => "SYNC",
            CE_TYPE_CE1         => "SYNC",
            INIT_OUT            => 0,
            IS_CE0_INVERTED     => '0',
            IS_CE1_INVERTED     => '0',
            IS_I0_INVERTED      => '0',
            IS_I1_INVERTED      => '0',
            IS_IGNORE0_INVERTED => '0',
            IS_IGNORE1_INVERTED => '0',
            IS_S0_INVERTED      => '1',
            IS_S1_INVERTED      => '0',
            PRESELECT_I0        => true,
            PRESELECT_I1        => false,
            SIM_DEVICE          => "7SERIES",
            STARTUP_SYNC        => "FALSE"
        )
        port map(
            O       => clkGlitch,
            CE0     => '1',
            CE1     => '1',
            I0      => clkA,
            I1      => '1',
            IGNORE0 => '1',
            IGNORE1 => '1',
            S0      => clkB,
            S1      => clkB
        );

    -----------------------------------------------------------------------------------------------
    -- Generate the guarded signal for the clock glitching
    -----------------------------------------------------------------------------------------------
    inst_guardGlitchSignal : BUFGCTRL
        generic map(
            CE_TYPE_CE0         => "SYNC",
            CE_TYPE_CE1         => "SYNC",
            INIT_OUT            => 0,
            IS_CE0_INVERTED     => '0',
            IS_CE1_INVERTED     => '0',
            IS_I0_INVERTED      => '0',
            IS_I1_INVERTED      => '0',
            IS_IGNORE0_INVERTED => '0',
            IS_IGNORE1_INVERTED => '0',
            IS_S0_INVERTED      => '1',
            IS_S1_INVERTED      => '0',
            PRESELECT_I0        => true,
            PRESELECT_I1        => false,
            SIM_DEVICE          => "7SERIES",
            STARTUP_SYNC        => "FALSE"
        )
        port map(
            O       => clkGlitchEn,
            CE0     => '1',
            CE1     => '1',
            I0      => '1',
            I1      => clkGlitch,
            IGNORE0 => '1',
            IGNORE1 => '1',
            S0      => enGlitch_in,
            S1      => enGlitch_in
        );

    -----------------------------------------------------------------------------------------------
    -- Add two buffers to enable/disable the ports **clk_in** and **clkn_in**
    -----------------------------------------------------------------------------------------------

    -- Enable Buffer for port **clk_in**.
    inst_enClk : BUFGCTRL
        generic map(
            CE_TYPE_CE0         => "SYNC",
            CE_TYPE_CE1         => "SYNC",
            INIT_OUT            => 0,
            IS_CE0_INVERTED     => '0',
            IS_CE1_INVERTED     => '0',
            IS_I0_INVERTED      => '0',
            IS_I1_INVERTED      => '0',
            IS_IGNORE0_INVERTED => '0',
            IS_IGNORE1_INVERTED => '0',
            IS_S0_INVERTED      => '1',
            IS_S1_INVERTED      => '0',
            PRESELECT_I0        => true,
            PRESELECT_I1        => false,
            SIM_DEVICE          => "7SERIES",
            STARTUP_SYNC        => "FALSE"
        )
        port map(
            O       => clkC,
            CE0     => '1',
            CE1     => '1',
            I0      => '0',
            I1      => clkCO,
            IGNORE0 => '1',
            IGNORE1 => '1',
            S0      => enC_in,
            S1      => enC_in
        );

    -- Enable Buffer for port **clkn_in**.
    inst_enClkn : BUFGCTRL
        generic map(
            CE_TYPE_CE0         => "SYNC",
            CE_TYPE_CE1         => "SYNC",
            INIT_OUT            => 0,
            IS_CE0_INVERTED     => '0',
            IS_CE1_INVERTED     => '0',
            IS_I0_INVERTED      => '0',
            IS_I1_INVERTED      => '0',
            IS_IGNORE0_INVERTED => '0',
            IS_IGNORE1_INVERTED => '0',
            IS_S0_INVERTED      => '1',
            IS_S1_INVERTED      => '0',
            PRESELECT_I0        => true,
            PRESELECT_I1        => false,
            SIM_DEVICE          => "7SERIES",
            STARTUP_SYNC        => "FALSE"
        )
        port map(
            O       => clknC,
            CE0     => '1',
            CE1     => '1',
            I0      => '1',
            I1      => clkCnO,
            IGNORE0 => '1',
            IGNORE1 => '1',
            S0      => enC_in,
            S1      => enC_in
        );

    -----------------------------------------------------------------------------------------------
    -- Generate the final clock with glitches in it
    -----------------------------------------------------------------------------------------------
    -- Enable Buffer for port **clk_in**.
    inst_genGlitch : BUFGCTRL
        generic map(
            CE_TYPE_CE0         => "SYNC",
            CE_TYPE_CE1         => "SYNC",
            INIT_OUT            => 0,
            IS_CE0_INVERTED     => '0',
            IS_CE1_INVERTED     => '0',
            IS_I0_INVERTED      => '0',
            IS_I1_INVERTED      => '0',
            IS_IGNORE0_INVERTED => '0',
            IS_IGNORE1_INVERTED => '0',
            IS_S0_INVERTED      => '1',
            IS_S1_INVERTED      => '0',
            PRESELECT_I0        => true,
            PRESELECT_I1        => false,
            SIM_DEVICE          => "7SERIES",
            STARTUP_SYNC        => "FALSE"
        )
        port map(
            O       => clk_out,
            CE0     => '1',
            CE1     => '1',
            I0      => clkC,
            I1      => clknC,
            IGNORE0 => '1',
            IGNORE1 => '1',
            S0      => clkGlitchEn,
            S1      => clkGlitchEn
        );

end behavioral;
